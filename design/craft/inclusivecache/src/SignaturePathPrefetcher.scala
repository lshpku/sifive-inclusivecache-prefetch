package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer
import freechips.rocketchip.util.SetAssocLRU

object SPPParams {
  val pageOffsetBits = 12

  val lgSTEntries = 8
  val lgSTWays = 2
  val nSTWays = 1 << lgSTWays
  val lgSTSets = lgSTEntries - lgSTWays
  val nSTSets = 1 << lgSTSets
  val signatureBits = 12

  val nDeltas = 4
  val counterBits = 4
  val maxCounter = (1 << counterBits) - 1

  // Confidence is a fix-point number between 0 and 1, where 7'b0 = 0
  // and 7'b1111111 = 1.
  val cfdBits = 7
  val maxCfd = (1 << cfdBits) - 1

  val prefThresRatio = 0.25
  val prefThres = (prefThresRatio * maxCfd).toInt

  val divTab = ArrayBuffer[Int]()
  for (i <- 0 to maxCounter) {
    for (j <- 0 to maxCounter) {
      val r = i.toFloat / j.toFloat
      divTab += (r * maxCfd).toInt
    }
  }
  println("divTab", divTab)
  require(divTab.size == 1 << (counterBits * 2))

  def divide(a: UInt, b: UInt): UInt = {
    val index = Cat(a(counterBits - 1, 0), b(counterBits - 1, 0))
    VecInit(divTab.map(_.U))(index)
  }
}

class STBundle(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params) {
  val tag = UInt((params.inner.bundle.addressBits - SPPParams.pageOffsetBits - SPPParams.lgSTSets).W)
  val lastOffset = UInt((SPPParams.pageOffsetBits - params.offsetBits).W)
  val signature = UInt(SPPParams.signatureBits.W)
}

class PTBundle(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params) {
  val delta = Vec(SPPParams.nDeltas, UInt((SPPParams.pageOffsetBits - params.offsetBits + 1).W))
  val cDelta = Vec(SPPParams.nDeltas, UInt(SPPParams.counterBits.W))
  val cSig = UInt(SPPParams.counterBits.W)
}

class SignaturePathPrefetcher(params: InclusiveCacheParameters)
  extends AbstractPrefetcher(params) {

  // io.access -> miss_q
  val miss_q = Queue(io.access, 4)
  io.access.ready := true.B
  miss_q.ready := false.B
  // pred_q -> io.request
  val pred_q = Module(new Queue(UInt(addressBits.W), 4))
  io.request <> pred_q.io.deq
  pred_q.io.enq.valid := false.B
  pred_q.io.enq.bits := DontCare
  // io.response -> /dev/null
  io.response.ready := true.B

  val ppnBits = addressBits - SPPParams.pageOffsetBits
  val tagBits = ppnBits - SPPParams.lgSTSets
  val offsetBits = SPPParams.pageOffsetBits - params.offsetBits
  val deltaBits = offsetBits + 1

  val ppn = miss_q.bits(addressBits - 1, SPPParams.pageOffsetBits)
  val offset = miss_q.bits(SPPParams.pageOffsetBits - 1, params.offsetBits)
  val tag = ppn(ppnBits - 1, SPPParams.lgSTSets)
  val set = ppn(SPPParams.lgSTSets - 1, 0)

  // Signature Table
  val st = SyncReadMem(SPPParams.nSTSets, Vec(SPPParams.nSTWays, new STBundle(params)))
  val st_ren = WireInit(false.B)
  val st_read = RegEnable(st.read(set, st_ren), RegNext(st_ren))

  // ST entry of the current page
  val st_ppn = Reg(UInt(ppnBits.W))
  val st_tag = st_ppn(ppnBits - 1, SPPParams.lgSTSets)
  val st_delta = Reg(UInt(deltaBits.W))
  val st_sig = Reg(UInt(SPPParams.signatureBits.W))
  val st_lru = new SetAssocLRU(SPPParams.nSTSets, SPPParams.nSTWays, "lru")
  val st_replace_way = RegEnable(st_lru.way(RegNext(set)), RegNext(st_ren))

  // Pattern Table
  val pt = SyncReadMem(1 << SPPParams.signatureBits, new PTBundle(params))
  val pt_ren = WireInit(false.B)
  val pt_index = Wire(UInt(SPPParams.signatureBits.W))
  pt_index := DontCare
  val pt_read = RegEnable(pt.read(pt_index, pt_ren), RegNext(pt_ren))

  // Lookahead status
  val cur_sig = Reg(UInt(SPPParams.signatureBits.W))
  val cur_offset = Reg(UInt(offsetBits.W))
  val cur_cfd = Reg(UInt(SPPParams.cfdBits.W))

  def tree_compare(in: Vec[UInt], begin: Int, end: Int, cmp: (UInt, UInt) => Bool): UInt = {
    require(end > begin, "invalid range")
    if (end - begin == 1) {
      begin.U
    } else {
      val mid = (begin + end) / 2
      val left = tree_compare(in, begin, mid, cmp)
      val right = tree_compare(in, mid, end, cmp)
      Mux(cmp(in(left), in(right)), left, right)
    }
  }

  val s_idle :: s_st_1 :: s_st_2 :: s_pt_1 :: s_pt_2 :: s_pt_3 :: s_pf_1 :: s_pf_2 :: Nil = Enum(8)
  val state = RegInit(s_idle)

  val cycle = freechips.rocketchip.util.WideCounter(32).value

  when(state === s_idle && miss_q.valid) {
    // init st_entry and cfd
    st_ppn := ppn
    cur_offset := offset
    cur_cfd := SPPParams.maxCfd.U

    // read ST
    st_ren := true.B
    state := s_st_1

    printf("{cycle:%d,object:SPP,state:idle,ppn:0x%x,offset:%d}\n", cycle, ppn, offset)
  }.elsewhen(state === s_st_1) {
    state := s_st_2
    printf("{cycle:%d,object:SPP,state:st_1}\n", cycle)
  }.elsewhen(state === s_st_2) {
    // find the hit way
    // Note: there may be multiple hits because ST doesn't have valid bits.
    val hit_vec = st_read.map(_.tag === st_tag)
    val hit = VecInit(hit_vec).asUInt =/= 0.U
    val hit_way = PriorityMux(hit_vec.zipWithIndex.map { case (h, i) => (h, i.U) })

    // update ST
    val lastOffset = Mux(hit, st_read(hit_way).lastOffset, 0.U)
    val sig = Mux(hit, st_read(hit_way).signature, 0.U)
    val delta = cur_offset - lastOffset
    val new_sig = (sig << 3).asUInt ^ delta
    st_sig := sig
    st_delta := delta
    cur_sig := new_sig

    val update_way = Mux(hit, hit_way, st_replace_way)
    val update_set = WireInit(st_read)
    update_set(update_way).tag := st_tag
    update_set(update_way).lastOffset := cur_offset
    update_set(update_way).signature := new_sig
    st.write(RegNext(RegNext(set)), update_set)
    when(hit) {
      st_lru.access(RegNext(RegNext(set)), hit_way)
    }

    // read PT
    pt_index := sig
    pt_ren := true.B
    state := s_pt_1

    printf("{cycle:%d,object:SPP,state:st_2,hit_vec:0b%b,hit_way:%d,lastOffset:%d,lastSig:0x%x,delta:%d,newSig:0x%x}\n",
      cycle, VecInit(hit_vec).asUInt, hit_way, lastOffset, sig, delta, new_sig)
  }.elsewhen(state === s_pt_1) {
    state := s_pt_2
    printf("{cycle:%d,object:SPP,state:pt_1}\n", cycle)
  }.elsewhen(state === s_pt_2) {
    // find the matching delta
    // Note: there may be multiple matching deltas
    val hit_vec = pt_read.delta.map(_ === st_delta)
    val hit = VecInit(hit_vec).asUInt =/= 0.U
    val hit_way = PriorityMux(hit_vec.zipWithIndex.map { case (h, i) => (h, i.U) })
    val min_way = tree_compare(pt_read.cDelta, 0, SPPParams.nDeltas, _ < _)

    // update PT
    val update_way = Mux(hit, hit_way, min_way)
    val cDelta = Mux(hit, pt_read.cDelta(hit_way), 0.U)
    val new_entry = WireInit(pt_read)
    when(cDelta === SPPParams.maxCounter.U || pt_read.cSig === SPPParams.maxCounter.U) {
      new_entry.cDelta zip pt_read.cDelta map { case (n, o) => n := o >> 1 }
      new_entry.cDelta(update_way) := (cDelta +& 1.U) >> 1
      new_entry.cSig := (pt_read.cSig +& 1.U) >> 1
    }.otherwise {
      new_entry.cDelta(update_way) := cDelta + 1.U
      new_entry.cSig := pt_read.cSig + 1.U
    }
    new_entry.delta(update_way) := st_delta
    pt.write(st_sig, new_entry)
    state := s_pt_3
    printf("{cycle:%d,object:SPP,state:pt_2}\n", cycle)
  }.elsewhen(state === s_pt_3) {
    // read PT
    pt_index := cur_sig
    pt_ren := true.B
    state := s_pf_1
    printf("{cycle:%d,object:SPP,state:pt_3}\n", cycle)
  }.elsewhen(state === s_pf_1) {
    state := s_pf_2
    printf("{cycle:%d,object:SPP,state:pf_1}\n", cycle)
  }.elsewhen(state === s_pf_2) {
    // find the delta with the max counter value
    val max_way = tree_compare(pt_read.cDelta, 0, SPPParams.nDeltas, _ > _)
    val delta = pt_read.delta(max_way)
    val cDelta = pt_read.cDelta(max_way)

    // compute confidence
    val rDelta = SPPParams.divide(cDelta, pt_read.cSig)
    val cfd = (cur_cfd * rDelta) >> SPPParams.cfdBits
    cur_cfd := cfd

    // compute prefetch address
    val sig = (cur_sig << 3).asUInt ^ delta
    val offset = cur_offset + delta
    cur_offset := offset
    val cross_page = offset(offsetBits)
    val line_offset = 0.U(params.offsetBits.W)
    pred_q.io.enq.bits := Cat(st_ppn, offset(offsetBits - 1, 0), line_offset)

    // decide whether to make prefetching and look ahead
    when(cfd.asUInt >= SPPParams.prefThres.U && !cross_page) {
      pred_q.io.enq.valid := true.B
      pt_index := sig
      pt_ren := true.B
      cur_sig := sig
      state := s_pf_1
    }.otherwise {
      miss_q.ready := true.B
      state := s_idle
    }

    printf("{cycle:%d,object:SPP,state:pf_2,maxWay:%d,delta:%d,cDelta:%d,cSig:%d,rDelta:%d,cfd:%d}\n",
      cycle, max_way, delta, cDelta, pt_read.cSig, rDelta, cfd)
  }
}
