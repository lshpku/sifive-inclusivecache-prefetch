package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

object BOPParams {
  val maxOffset = 16
  val maxScore = 15
  val maxRounds = 50
  val badScore = 1
  val rrIndexBits = 8

  def generateOffsetList(): Array[Int] = {
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
}

class BestOffsetPrefetcher(params: InclusiveCacheParameters)
  extends AbstractPrefetcher(params) {
  io.access.ready := true.B
  io.response.ready := true.B

  val cycle = freechips.rocketchip.util.WideCounter(32).value

  val offsetList = BOPParams.generateOffsetList()
  println(offsetList.mkString("(offsetList,Array(", ", ", "))"))

  val offsetIndexBits = log2Ceil(offsetList.length)
  val offsetVec = VecInit(offsetList.map(_.U))

  // Current offset
  val Di = Reg(UInt(offsetIndexBits.W))
  val D = offsetVec(Di)
  // Current testing offset index
  val di = Reg(UInt(offsetIndexBits.W))
  val d = offsetVec(di)

  val maxScoreBits = log2Ceil(BOPParams.maxScore + 1)
  val lineBits = addressBits - params.offsetBits
  val rrTagBits = lineBits - BOPParams.rrIndexBits
  val rrEntries = 1 << BOPParams.rrIndexBits

  // Recent requests
  val rr = SyncReadMem(rrEntries, UInt(rrTagBits.W))

  // Candidate offset scores
  val scores = Reg(Vec(offsetList.length, UInt(maxScoreBits.W)))
  val maxScore = Reg(UInt(maxScoreBits.W))
  val maxScoreDi = Reg(UInt(offsetIndexBits.W))
  val scoreResetDi = Reg(UInt(offsetIndexBits.W))
  val nextMaxScore = WireInit(maxScore)
  val nextMaxScoreDi = WireInit(maxScoreDi)
  maxScore := nextMaxScore
  maxScoreDi := nextMaxScoreDi

  // Current number of rounds
  val nRounds = Reg(UInt(log2Ceil(BOPParams.maxRounds).W))

  val s_learn :: s_reset :: Nil = Enum(2)
  val state = RegInit(s_reset)
  val enable = RegInit(false.B)

  def gotoReset() = {
    state := s_reset
    scoreResetDi := 0.U
    enable := nextMaxScore > BOPParams.badScore.U
    Di := nextMaxScoreDi
    printf("{event:BOP.gotoReset,cycle:%d,enable:%d,Di:%d}\n",
      cycle, nextMaxScore > BOPParams.badScore.U, nextMaxScoreDi)
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
      printf("{event:BOP.access,cycle:%d,line:0x%x,Di:%d,di:%d}\n", cycle, line, Di, di)
    }
    io.request.valid := false.B
    io.request.bits := DontCare
    when (io.access.valid && page === predPage && enable) {
      io.request.valid := true.B
      io.request.bits := predAddr
      printf("{event:BOP.request,cycle:%d,line:0x%x}\n", cycle, predLine)
    }

    // 2. lookup base line
    val baseLine = line - d
    val baseAddr = Cat(baseLine, 0.U(params.offsetBits.W))
    val basePage = baseAddr(addressBits - 1, pageOffsetBits)
    val index = baseLine(BOPParams.rrIndexBits - 1, 0)
    val tag = baseLine(lineBits - 1, BOPParams.rrIndexBits)
    val rrReadEn = io.access.valid && page === basePage && state === s_learn
    val rrTag = rr.read(index, rrReadEn)

    // 3. increment score
    when (RegNext(rrReadEn) && RegNext(tag) === rrTag) {
      val scoreDi = RegNext(di)
      val score = scores(scoreDi) + 1.U
      scores(scoreDi) := score
      // When two offsets have the same score, use the smaller offset.
      val updateMax = WireInit(false.B)
      when (score > maxScore || (score === maxScore && scoreDi < maxScoreDi)) {
        nextMaxScore := score
        nextMaxScoreDi := scoreDi
        updateMax := true.B
      }
      printf("{event:BOP.incScore,cycle:%d,score:%d,scoreDi:%d,updateMax:%d}\n",
        cycle, score, scoreDi, updateMax)
      when (score === BOPParams.maxScore.U) {
        gotoReset()
      }
    }

    // 4. increment offset index and round number
    when (io.access.valid && state === s_learn) {
      when (di =/= (offsetList.length - 1).U) {
        di := di + 1.U
      } .otherwise {
        di := 0.U
        when (nRounds =/= (BOPParams.maxRounds - 1).U) {
          nRounds := nRounds + 1.U
        } .otherwise {
          gotoReset()
        }
        printf("{event:BOP.incRound,cycle:%d,round:%d}\n", cycle, nRounds)
      }
    }
  }

  // Update RR table
  {
    val baseLine = Wire(UInt(lineBits.W))
    baseLine := DontCare
    val rrWriteEn = WireInit(false.B)

    // 1. when prefetch is on, record Y - D
    when (enable && io.response.valid) {
      val line = io.response.bits(addressBits - 1, params.offsetBits)
      val page = io.response.bits(addressBits - 1, pageOffsetBits)
      baseLine := line - D
      val baseAddr = Cat(baseLine, 0.U(params.offsetBits.W))
      val basePage = baseAddr(addressBits - 1, pageOffsetBits)
      rrWriteEn := page === basePage
      printf("{event:BOP.response,cycle:%d,line:0x%x,D:%d}\n", cycle, line, D)
    }

    // 2. when prefetch is off, record Y itself
    when (!enable && io.access.valid) {
      baseLine := io.access.bits(addressBits - 1, params.offsetBits)
      rrWriteEn := true.B
    }

    when (rrWriteEn) {
      val index = baseLine(BOPParams.rrIndexBits - 1, 0)
      val tag = baseLine(lineBits - 1, BOPParams.rrIndexBits)
      rr.write(index, tag)
      printf("{event:BOP.recordBase,cycle:%d,line:0x%x}\n", cycle, baseLine)
    }
  }

  // Reset all scores to 0
  when (state === s_reset) {
    scores(scoreResetDi) := 0.U
    when (scoreResetDi === (offsetList.length - 1).U) {
      state := s_learn
      di := 0.U
      nRounds := 0.U
      maxScore := 0.U
      printf("{event:BOP.finishReseting,cycle:%d}\n", cycle)
    } .otherwise {
      scoreResetDi := scoreResetDi + 1.U
    }
  }
}
