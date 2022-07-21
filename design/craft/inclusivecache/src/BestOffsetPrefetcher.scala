package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

class BestOffsetPrefetcher(params: InclusiveCacheParameters)
  extends AbstractPrefetcher(params) {
  io.access.ready := true.B
  io.response.ready := true.B

  def generateOffsetList(): Array[Int] = {
    val maxOffset = 16

    val isPrime = Array.fill[Boolean](maxOffset + 1)(true)
    for (i <- 2 to maxOffset) {
      if (isPrime(i)) {
        var j = i + i
        while (j <= maxOffset) {
          isPrime(j) = false
          j += i
        }
      }
    }

    val excluded = Array.fill[Boolean](maxOffset + 1)(false)
    for (i <- 6 to maxOffset) {
      if (isPrime(i)) {
        var j = i
        while (j <= maxOffset) {
          excluded(j) = true
          j += i
        }
      }
    }

    val offsetList = ArrayBuffer[Int]()
    for (i <- 1 to maxOffset) {
      if (!excluded(i)) {
        offsetList += i
      }
    }
    offsetList.toArray
  }

  val cycle = freechips.rocketchip.util.WideCounter(32).value

  val offsetList = generateOffsetList()
  println(offsetList.mkString("(offsetList,Array(", ", ", "))"))

  val offsetIndexBits = log2Ceil(offsetList.length)
  val offsetVec = VecInit(offsetList.map(_.U))

  // Current offset
  val Di = Reg(UInt(offsetIndexBits.W))
  val D = offsetVec(Di)
  // Current testing offset index
  val di = Reg(UInt(offsetIndexBits.W))
  val d = offsetVec(di)

  val rrIndexBits = 8
  val rrTagBits = addressBits - params.offsetBits - rrIndexBits
  val rrEntries = 1 << rrIndexBits

  // Recent requests
  val rr = SyncReadMem(rrEntries, UInt(rrTagBits.W))

  val maxScore = 15
  val maxRounds = 50
  val badScore = 1
  val maxScoreBits = log2Ceil(maxScore + 1)

  // Candidate offset scores
  val scores = Reg(Vec(offsetList.length, UInt(maxScoreBits.W)))
  val scoreIndex = Reg(UInt(log2Ceil(offsetList.length).W))
  val curMaxScore = Reg(UInt(maxScoreBits.W))
  val curMaxScoreDi = Reg(UInt(offsetIndexBits.W))

  // Current number of rounds
  val nRounds = Reg(UInt(log2Ceil(maxRounds + 1).W))

  val s_init :: s_learning :: s_updating :: Nil = Enum(3)
  val state = RegInit(s_init)

  def gotoUpdating() = {
    state := s_updating
    scoreIndex := 0.U
    curMaxScore := 0.U
    curMaxScoreDi := 0.U
  }

  // Handle misses and prefetch hits
  {
    // 1. make prediction
    val line = io.access.bits(addressBits - 1, params.offsetBits)
    val page = io.access.bits(addressBits - 1, pageOffsetBits)
    val predLine = line + D
    val predAddr = Cat(predLine, 0.U(params.offsetBits.W))
    val predPage = predAddr(addressBits - 1, pageOffsetBits)
    when (io.access.valid) {
      printf("{event:BOP.access,cycle:%d,line:0x%x,D:%d,d:%d}\n", cycle, line, D, d)
    }
    io.request.valid := false.B
    io.request.bits := DontCare
    when (io.access.valid && page === predPage && state =/= s_init) {
      io.request.valid := true.B
      io.request.bits := predAddr
      printf("{event:BOP.request,cycle:%d,line:0x%x}\n", cycle, predLine)
    }

    // 2. lookup base line
    val baseLine = line - d
    val baseAddr = Cat(baseLine, 0.U(params.offsetBits.W))
    val basePage = baseAddr(addressBits - 1, pageOffsetBits)
    val index = baseLine(rrIndexBits - 1, 0)
    val tag = baseLine(addressBits - params.offsetBits - 1, rrIndexBits)
    val rrReadEn = io.access.valid && page === basePage && state === s_learning
    val rrTag = rr.read(index, rrReadEn)

    // 3. increment score
    when (RegNext(rrReadEn) && RegNext(tag) === rrTag) {
      val score = scores(RegNext(di)) + 1.U
      scores(RegNext(di)) := score
      printf("{event:BOP.incScore,cycle:%d,di:%d,score:%d}\n", cycle, RegNext(di), score)
      when (score === maxScore.U) {
        gotoUpdating()
        printf("{event:BOP.reachScoreMax,cycle:%d}\n", cycle)
      }
    }

    // 4. increment offset index
    when (io.access.valid && state === s_learning) {
      when (di =/= (offsetList.length - 1).U) {
        di := di + 1.U
      } .otherwise {
        di := 0.U
        nRounds := nRounds + 1.U
        printf("{event:BOP.incRound,cycle:%d,round:%d}\n", cycle, nRounds)
      }
    }

    // 5. check for round limit
    when (nRounds === maxRounds.U && state === s_learning) {
      gotoUpdating()
      printf("{event:BOP.reachRoundMax,cycle:%d}\n", cycle)
    }
  }

  // Handle completed prefetches
  when (io.response.valid) {
    val line = io.response.bits(addressBits - 1, params.offsetBits)
    val page = io.response.bits(addressBits - 1, pageOffsetBits)
    val baseLine = line - D
    val baseAddr = Cat(baseLine, 0.U(params.offsetBits.W))
    val basePage = baseAddr(addressBits - 1, pageOffsetBits)

    printf("{event:BOP.response,cycle:%d,line:0x%x,D:%d}\n", cycle, line, D)

    when (page === basePage && state === s_learning) {
      val index = baseLine(rrIndexBits - 1, 0)
      val tag = baseLine(addressBits - params.offsetBits - 1, rrIndexBits)
      rr.write(index, tag)
      printf("{event:BOP.recordBase,cycle:%d,line:0x%x}\n", cycle, baseLine)
    }
  }

  // Update the best offset
  when (state === s_updating) {
    // Find the max score
    val score = scores(scoreIndex)
    val newMaxScore = WireInit(curMaxScore)
    val newMaxScoreDi = WireInit(curMaxScoreDi)
    when (score > curMaxScore) {
      newMaxScore := score
      newMaxScoreDi := scoreIndex
    }
    curMaxScore := newMaxScore
    curMaxScoreDi := newMaxScoreDi

    // Finish updating
    when (scoreIndex === (offsetList.length - 1).U) {
      state := s_learning
      Di := newMaxScoreDi
      di := 0.U
      nRounds := 0.U
      printf("{event:BOP.finishUpdating,cycle:%d,maxScore:%d,Di:%d}\n", cycle, newMaxScore, newMaxScoreDi)
    } .otherwise {
      scoreIndex := scoreIndex + 1.U
    }

    // Clear score
    scores(scoreIndex) := 0.U
  }

  // Initialize
  when (state === s_init) {
    scores(0) := maxScore.U
    gotoUpdating()
  }
}
