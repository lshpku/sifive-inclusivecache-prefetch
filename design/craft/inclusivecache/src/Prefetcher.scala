package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class Prefetcher(params: InclusiveCacheParameters) extends Module
{
  val io = IO(new Bundle {
    val train = Flipped(Decoupled(new FullRequest(params)))
    val req = Decoupled(new FullRequest(params))
  })

  io.train.ready := true.B
  io.req.valid := false.B

  io.req.bits.prio    := VecInit(1.U(3.W).asBools)
  io.req.bits.control := false.B
  io.req.bits.prefetch:= true.B
  io.req.bits.opcode  := TLMessages.Hint
  io.req.bits.param   := TLHints.PREFETCH_READ
  io.req.bits.size    := params.cache.blockBytes.U
  io.req.bits.source  := DontCare
  io.req.bits.offset  := 0.U
  io.req.bits.set     := 0.U
  io.req.bits.tag     := 0.U
  io.req.bits.put     := DontCare
}
