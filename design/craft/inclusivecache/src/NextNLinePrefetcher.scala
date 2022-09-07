package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

class NextNLinePrefetcher(params: InclusiveCacheParameters)
  extends AbstractPrefetcher(params) {

  val idle = RegInit(true.B)
  val cycle = freechips.rocketchip.util.WideCounter(32).value

  // Miss address
  val miss_q = Module(new Queue(UInt(addressBits.W), 4))
  miss_q.io.enq.bits := io.access.bits.address
  miss_q.io.enq.valid := io.access.valid && AccessState.eligible(io.access.bits.state)
  val miss_addr = miss_q.io.deq.bits

  // User-specified next-line bit vector
  val bv = Reg(UInt(64.W))

  val cur_bv = Mux(idle, Mux(io.enable, io.args(0), 0.U), bv)
  val cur_mask = PriorityMux(
    cur_bv.asBools.zipWithIndex.map { case (b, i) => (b, (1.U << i).asUInt) })
  val cur_step = PriorityMux(
    cur_bv.asBools.zipWithIndex.map { case (b, i) => (b, (i + 1).U) })

  def getPPN(address: UInt) = address(addressBits - 1, pageOffsetBits)

  val pred_addr = miss_addr + (cur_step << params.offsetBits)
  val same_page = getPPN(miss_addr) === getPPN(pred_addr)
  val cur_valid = cur_bv =/= 0.U && same_page
  io.request.bits := pred_addr
  io.request.valid := miss_q.io.deq.valid && cur_valid
  when (io.request.valid) {
    printf("{event:NextNLine.request,cycle:%d,base:0x%x,pred:0x%x,cur_bv:0x%b,cur_step:%d}\n",
      cycle, miss_addr, pred_addr, cur_bv, cur_step)
  }

  val next_bv = Mux(cur_valid, cur_bv ^ cur_mask, 0.U)
  bv := next_bv
  miss_q.io.deq.ready := next_bv === 0.U

  when (miss_q.io.deq.valid) {
    idle := false.B
  }
  when (miss_q.io.deq.fire) {
    idle := true.B
    printf("{event:NextNLine.miss_q.fire,cycle:%d}\n", cycle)
  }

  io.access.ready := true.B
  io.response.ready := true.B
}
