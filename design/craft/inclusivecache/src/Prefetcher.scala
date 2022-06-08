package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class PrefetcherCtl(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params) {
  val req = Decoupled(new Bundle{
    val address = UInt(params.inner.bundle.addressBits.W)
    val trunk = Bool()
  })
  val enable = Bool()
}

class Prefetcher(params: InclusiveCacheParameters) extends Module
{
  val io = IO(new Bundle {
    val train = Flipped(Decoupled(new FullRequest(params)))
    val req = Decoupled(new FullRequest(params))
    val ctl = Flipped(new PrefetcherCtl(params))
  })

  val reqArb = Module(new Arbiter(new FullRequest(params), 2))
  io.req <> reqArb.io.out
  io.train.ready := true.B
  io.ctl.req.ready := reqArb.io.in(0).ready

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

  val ctl_req = make_req()
  reqArb.io.in(0).bits := ctl_req
  reqArb.io.in(0).valid := io.ctl.req.valid

  val (ctl_tag, ctl_set, _) = params.parseAddress(io.ctl.req.bits.address)
  ctl_req.param := Mux(io.ctl.req.bits.trunk, TLHints.PREFETCH_WRITE, TLHints.PREFETCH_READ)
  ctl_req.tag := ctl_tag
  ctl_req.set := ctl_set

  val cycle = freechips.rocketchip.util.WideCounter(32).value
  when (io.ctl.req.fire) {
    printf("{event:Prefetcher.ctl,cycle:%d,address:0x%x,tag:0x%x,set:0x%x}\n",
      cycle, io.ctl.req.bits.address, io.req.bits.tag, io.req.bits.set)
  }

  val pred_req = make_req()
  val pred_valid = RegInit(false.B)
  reqArb.io.in(1).bits := pred_req
  reqArb.io.in(1).valid := pred_valid

  val pred_addr = Reg(UInt(params.inner.bundle.addressBits.W))
  val (pred_tag, pred_set, _) = params.parseAddress(pred_addr)
  ctl_req.tag := ctl_tag
  ctl_req.set := ctl_set

  val miss_addr = params.expandAddress(io.train.bits.tag, io.train.bits.set, 0.U)
  val next_addr = miss_addr + params.cache.blockBytes.U
  val cacheable = params.outer.manager.supportsAcquireBSafe(next_addr, params.offsetBits.U)

  when (io.train.valid && cacheable && io.ctl.enable) {
    pred_valid := true.B
    pred_addr := next_addr
  } .elsewhen (reqArb.io.in(1).fire) {
    pred_valid := false.B
  }

  when (reqArb.io.in(1).fire) {
    printf("{event:Prefetcher.pred,cycle:%d,address:0x%x,tag:0x%x,set:0x%x}\n",
      cycle, pred_addr, io.req.bits.tag, io.req.bits.set)
  }
}
