package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._
import scala.collection.mutable.ArrayBuffer

class PrefetchCtl(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val req = Decoupled(new Bundle{
    val address = UInt(params.inner.bundle.addressBits.W)
    val trunk = Bool()
  })
  val perf = Flipped(Vec(params.nPerfCounters, UInt(params.perfCounterBits.W)))
  val sel = UInt(2.W)
  val args = Vec(params.nArgs, UInt(64.W))
}

object AccessState {
  val bits = 2
  def MISS = 0.U(bits.W)
  def HIT = 1.U(bits.W)
  def PREFETCH_HIT = 2.U(bits.W)
  def LATE_HIT = 3.U(bits.W)

  def eligible(state: UInt): Bool = {
    state.isOneOf(MISS, PREFETCH_HIT, LATE_HIT)
  }
}

class PrefetchTrain(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val tag = UInt(params.tagBits.W)
  val set = UInt(params.setBits.W)
  val state = UInt(AccessState.bits.W)
}

class PrefetcherResp(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val tag = UInt(params.tagBits.W)
  val set = UInt(params.setBits.W)
  val grant = Bool() // fetches data from lower level
}

class PrefetchAccess(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val address = UInt(params.inner.bundle.addressBits.W)
  val state = UInt(AccessState.bits.W)
}

abstract class AbstractPrefetcher(params: InclusiveCacheParameters) extends Module
{
  val addressBits = params.inner.bundle.addressBits
  val pageOffsetBits = 12

  val io = IO(new Bundle {
    // L2 access
    val access = Flipped(Decoupled(new PrefetchAccess(params)))
    // Prefetch request
    val request = Decoupled(UInt(addressBits.W))
    // Completed prefetch
    val response = Flipped(Decoupled(UInt(addressBits.W)))

    // Is this prefetcher enabled?
    val enable = Input(Bool())
    // Optional arguments.
    // Args are valid only when enable is true.
    val args = Input(Vec(params.nArgs, UInt(64.W)))
  })
}

class NullPrefetcher(params: InclusiveCacheParameters)
  extends AbstractPrefetcher(params) {
  io.access.ready := true.B
  io.response.ready := true.B
  io.request.valid := false.B
  io.request.bits := DontCare
}

class Prefetcher(params: InclusiveCacheParameters) extends Module
{
  val io = IO(new Bundle {
    val train = Flipped(Decoupled(new PrefetchTrain(params)))
    val req = Decoupled(new FullRequest(params))
    val resp = Flipped(Decoupled(new PrefetcherResp(params)))
    val ctl = Flipped(new PrefetchCtl(params))
  })

  val reqs = Module(new Queue(new FullRequest(params), params.reqQueueEntries, flow = true))
  io.req <> reqs.io.deq

  val reqArb = Module(new Arbiter(new FullRequest(params), 2))
  reqs.io.enq <> reqArb.io.out

  def make_req() = {
    val req = Wire(new FullRequest(params))
    req.prio    := VecInit(1.U(3.W).asBools)
    req.control := false.B
    req.prefetch:= true.B
    req.opcode  := TLMessages.Hint
    req.param   := TLHints.PREFETCH_READ
    // see SinkX for why size and source should be set as below
    req.size    := params.offsetBits.U
    req.source  := params.inner.client.clients.map(_.sourceId.start).min.U
    req.tag     := DontCare
    req.set     := DontCare
    req.offset  := 0.U
    req.put     := DontCare
    req
  }

  val ctl_blkaddr = ~(~io.ctl.req.bits.address | ((BigInt(1) << params.offsetBits) - 1).U)
  val ctl_cacheable = params.outer.manager.supportsAcquireBSafe(ctl_blkaddr, params.offsetBits.U)
  io.ctl.req.ready := reqArb.io.in(0).ready || !ctl_cacheable

  val ctl_req = make_req()
  reqArb.io.in(0).bits := ctl_req
  reqArb.io.in(0).valid := io.ctl.req.valid && ctl_cacheable

  val (ctl_tag, ctl_set, _) = params.parseAddress(io.ctl.req.bits.address)
  ctl_req.param := Mux(io.ctl.req.bits.trunk, TLHints.PREFETCH_WRITE, TLHints.PREFETCH_READ)
  ctl_req.tag := ctl_tag
  ctl_req.set := ctl_set

  val cycle = freechips.rocketchip.util.WideCounter(32).value
  when (reqArb.io.in(0).fire) {
    printf("{event:Prefetcher.ctl,cycle:%d,address:0x%x,tag:0x%x,set:0x%x}\n",
      cycle, io.ctl.req.bits.address, io.req.bits.tag, io.req.bits.set)
  }

  println("params.inner.bundle.addressBits", params.inner.bundle.addressBits)
  println("params.tagBits", params.tagBits)
  println("params.setBits", params.setBits)
  println("params.offsetBits", params.offsetBits)
  println("params.addressMapping", params.addressMapping)

  def restore_address(tag: UInt, set: UInt): UInt = {
    val expanded = params.expandAddress(tag, set, 0.U)
    // We MUST restore the address after expansion, otherwise the high bit of the
    // address may not be properly set. (I spent 3 whole days on this!!!
    params.restoreAddress(expanded)
  }

  val prefetchers = Seq[AbstractPrefetcher](
    Module(new NullPrefetcher(params)),
    Module(new NextNLinePrefetcher(params)),
    Module(new BestOffsetPrefetcher(params)),
    Module(new SignaturePathPrefetcher(params))
  )

  // Broadcast train and response to all prefetchers
  // TODO: should train and resp be able to back-pressure MSHRs?
  val miss_addr = restore_address(io.train.bits.tag, io.train.bits.set)
  for (pf <- prefetchers) {
    pf.io.access.bits.address := miss_addr
    pf.io.access.bits.state := io.train.bits.state
    pf.io.access.valid := io.train.valid
  }
  io.train.ready := true.B

  val resp_addr = restore_address(io.resp.bits.tag, io.resp.bits.set)
  for (pf <- prefetchers) {
    pf.io.response.valid := io.resp.valid
    pf.io.response.bits := resp_addr
  }
  io.resp.ready := true.B

  // Accept requests from only the selected prefetcher.
  // Requests from other prefetchers are not blocked but discarded
  // silently, as if they were consumed from the prefetchers' point
  // of view.
  val pred_addr = VecInit(prefetchers.map(_.io.request.bits))(io.ctl.sel)
  val pred_valid = VecInit(prefetchers.map(_.io.request.valid))(io.ctl.sel)
  val cacheable = params.outer.manager.supportsAcquireBSafe(pred_addr, params.offsetBits.U)
  val pred_req = make_req()
  val (pred_tag, pred_set, _) = params.parseAddress(pred_addr)
  pred_req.tag := pred_tag
  pred_req.set := pred_set
  reqArb.io.in(1).bits := pred_req
  reqArb.io.in(1).valid := pred_valid && cacheable

  for ((pf, i) <- prefetchers.zipWithIndex) {
    pf.io.request.ready := Mux(i.U === io.ctl.sel,
      reqArb.io.in(1).ready || !cacheable, true.B)
    pf.io.enable := i.U === io.ctl.sel
    pf.io.args := io.ctl.args
  }

  // Performance counters
  val perf_events = ArrayBuffer(
    "train"       -> io.train.fire,
    "train hit"   -> (io.train.fire && io.train.bits.state === AccessState.HIT),
    "train miss"  -> (io.train.fire && io.train.bits.state === AccessState.MISS),
    "train late"  -> (io.train.fire && io.train.bits.state === AccessState.LATE_HIT),
    "pred"        -> reqArb.io.in(1).fire,
    "pred grant"  -> (io.resp.fire && io.resp.bits.grant),
  )
  val counters = perf_events.map { case (_, e) =>
    val counter = WideCounter(params.perfCounterBits, RegNext(e.asUInt))
    counter.value
  }
  io.ctl.perf zip counters map { case (o, c) => o := c }
  println("params.nPerfCounters", params.nPerfCounters)
}
