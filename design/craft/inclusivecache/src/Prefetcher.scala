package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.WideCounter
import scala.collection.mutable.ArrayBuffer

class PrefetchCtl(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val req = Decoupled(new Bundle{
    val address = UInt(params.inner.bundle.addressBits.W)
    val trunk = Bool()
  })
  val perf = Flipped(Vec(params.nPerfCounters, UInt(params.perfCounterBits.W)))
  val enable = Bool()
  val next_n = UInt(log2Ceil(params.maxNextN).W)
}

class PrefetchTrain(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val tag = UInt(params.tagBits.W)
  val set = UInt(params.setBits.W)
  val hit = Bool()
}

class PrefetcherResp(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val grant = Bool()
}

class Prefetcher(params: InclusiveCacheParameters) extends Module
{
  val io = IO(new Bundle {
    val train = Flipped(Decoupled(new PrefetchTrain(params)))
    val req = Decoupled(new FullRequest(params))
    val resp = Flipped(Decoupled(new PrefetcherResp(params)))
    val ctl = Flipped(new PrefetchCtl(params))
  })

  io.resp.ready := true.B

  val trains = Module(new Queue(new PrefetchTrain(params), params.trainQueueEntries, flow = true))
  trains.io.enq <> io.train
  // Discard input when train queue is full
  io.train.ready := true.B

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

  val train = trains.io.deq.bits
  // We MUST restore the address after expansion, otherwise the high bit of the
  // address may not be properly set. (I spent 3 whole days on this!!!
  val miss_addr_unrestored = params.expandAddress(train.tag, train.set, 0.U)
  val miss_addr = params.restoreAddress(miss_addr_unrestored)

  val cur_n = RegInit(1.U(log2Ceil(params.maxNextN + 1).W))
  val cur_delta = cur_n << params.offsetBits.U
  val pred_addr = miss_addr + cur_delta
  val cacheable = params.outer.manager.supportsAcquireBSafe(pred_addr, params.offsetBits.U)

  val pred_req = make_req()
  val (pred_tag, pred_set, _) = params.parseAddress(pred_addr)
  pred_req.tag := pred_tag
  pred_req.set := pred_set
  reqArb.io.in(1).bits := pred_req
  reqArb.io.in(1).valid := false.B
  trains.io.deq.ready := false.B

  // Next-N-line generation loop
  val pred_valid = cacheable && cur_n <= io.ctl.next_n
  val can_step = reqArb.io.in(1).ready || !pred_valid
  when (trains.io.deq.valid) {
    reqArb.io.in(1).valid := pred_valid
    when (can_step) {
      when (cur_n < io.ctl.next_n) {
        cur_n := cur_n + 1.U
      } .otherwise {
        cur_n := 1.U
      }
    }
  }
  when (can_step && cur_n >= io.ctl.next_n) {
    trains.io.deq.ready := true.B // decouple ready from valid
  }

  when (reqArb.io.in(1).fire) {
    printf("{event:Prefetcher.pred,cycle:%d,address:0x%x,tag:0x%x,set:0x%x}\n",
      cycle, pred_addr, io.req.bits.tag, io.req.bits.set)
  }

  val perf_events = ArrayBuffer(
    "train"       -> io.train.fire,
    "train hit"   -> (io.train.fire && io.train.bits.hit),
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
