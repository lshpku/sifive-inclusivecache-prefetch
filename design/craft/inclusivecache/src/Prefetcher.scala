package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class Prefetcher(params: InclusiveCacheParameters) extends Module
{
  val io = IO(new Bundle {
    val train = Flipped(Decoupled(new FullRequest(params)))
    val req = Decoupled(new FullRequest(params))
    val ctl = Flipped(Decoupled(UInt(64.W)))
  })

  io.train.ready := true.B
  io.req.valid := false.B
  io.ctl.ready := true.B
  when (io.ctl.valid) {
    val cycle = freechips.rocketchip.util.WideCounter(32).value
    printf("{source:ctlnode_prefetch,cycle:%d,address:0x%x}\n", cycle, io.ctl.bits)
  }

  io.req.bits.prio    := VecInit(1.U(3.W).asBools)
  io.req.bits.control := false.B
  io.req.bits.prefetch:= true.B
  io.req.bits.opcode  := TLMessages.Hint
  io.req.bits.param   := TLHints.PREFETCH_READ
  // see SinkX for why size and source should be set as below
  io.req.bits.size    := params.offsetBits.U
  io.req.bits.source  := params.inner.client.clients.map(_.sourceId.start).min.U
  io.req.bits.offset  := 0.U
  io.req.bits.set     := 0.U
  io.req.bits.tag     := 0.U
  io.req.bits.put     := DontCare
}
