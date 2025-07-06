/** *************************************************************************************
  * Utility stubs for HuanCun to be compatible with Chisel3/Chipyard
  */

package utility

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field}
import freechips.rocketchip.util.BundleKeyBase

// Bundle Key for HuanCun compatibility - simple object for compatibility
object ReqSourceKey

// MemReqSource 관련
object MemReqSource {
  val reqSourceBits = 6
  def apply(): UInt = 0.U
  def apply(i: Int): MemReqSource = new MemReqSource(i)
  
  object ReqSourceCount {
    val id = 16 // 적절한 값으로 설정
  }
  
  // Source entries that HuanCun expects
  object NoWhere {
    val id = 0.U(reqSourceBits.W)
  }
  object CPULoadData {
    val id = 1.U(reqSourceBits.W)
  }
  object CPUStoreData {
    val id = 2.U(reqSourceBits.W)
  }
  object Prefetch2L2BOP {
    val id = 3.U(reqSourceBits.W)
  }
  object Prefetch2L2SMS {
    val id = 4.U(reqSourceBits.W)
  }
  object Prefetch2L2TP {
    val id = 5.U(reqSourceBits.W)
  }
  object Prefetch2L2Stream {
    val id = 6.U(reqSourceBits.W)
  }
  object Prefetch2L3Unknown {
    val id = 7.U(reqSourceBits.W)
  }
}

class MemReqSource(val id: Int) {
  override def toString: String = s"Source$id"
}

// 기본 유틸리티들
class FastArbiter[T <: Data](gen: T, n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(n, Flipped(DecoupledIO(gen)))
    val out = DecoupledIO(gen)
    val chosen = UInt(log2Ceil(n).W)
  })
  
  val arb = Module(new RRArbiter(gen, n))
  arb.io.in <> io.in
  io.out <> arb.io.out
  io.chosen := arb.io.chosen
}

object FastArbiter {
  def apply[T <: Data](gen: T, n: Int): FastArbiter[T] = Module(new FastArbiter(gen, n))
}

class Pipeline[T <: Data](gen: T, latency: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)
    // Legacy compatibility
    def in = enq
    def out = deq
  })
  
  if (latency <= 0) {
    io.deq <> io.enq
  } else {
    val pipe = RegInit(VecInit(Seq.fill(latency)(0.U.asTypeOf(Valid(gen)))))
    val valid = RegInit(VecInit(Seq.fill(latency)(false.B)))
    
    for (i <- 1 until latency) {
      when(io.enq.fire || !valid(i)) {
        pipe(i) := pipe(i-1)
        valid(i) := valid(i-1)
      }
    }
    
    when(io.enq.fire || !valid(0)) {
      pipe(0).bits := io.enq.bits
      pipe(0).valid := io.enq.fire
      valid(0) := io.enq.fire
    }
    
    io.enq.ready := !valid(0) || io.deq.ready
    io.deq.valid := valid(latency-1)
    io.deq.bits := pipe(latency-1).bits
  }
}

object Pipeline {
  def apply[T <: Data](gen: T, latency: Int): Pipeline[T] = Module(new Pipeline(gen, latency))
  
  // Multiple overloads to handle different parameter patterns
  def apply[T <: Data](gen: T, depth: Int = 1, pipe: Boolean = true, name: Option[String] = None): T = {
    if (depth > 0 && pipe) {
      val pipeline = Module(new Pipeline(gen.cloneType, depth))
      pipeline.io.enq.bits := gen
      pipeline.io.enq.valid := true.B
      pipeline.io.deq.ready := true.B
      pipeline.io.deq.bits
    } else {
      gen
    }
  }
  
  def pipeTo[T <: Data](in: DecoupledIO[T]): DecoupledIO[T] = {
    // Simple passthrough when no depth specified
    in
  }
  
  def pipeTo[T <: Data](in: DecoupledIO[T], depth: Int, pipe: Boolean = true): DecoupledIO[T] = {
    if (depth > 0 && pipe) {
      val pipeModule = Module(new Pipeline(in.bits.cloneType, depth))
      pipeModule.io.enq <> in
      pipeModule.io.deq
    } else {
      in
    }
  }
  
  def pipeTo[T <: Data](in: DecoupledIO[T], depth: Int, pipe: Boolean, name: Option[String]): DecoupledIO[T] = {
    pipeTo(in, depth, pipe)
  }
}

// ResetGen 스텁
object ResetGen {
  def apply(resetIn: Reset, cycles: Int = 1): Reset = {
    // Simple reset generation
    resetIn
  }
}

// RegNextN 스텁
object RegNextN {
  def apply[T <: Data](in: T, n: Int): T = {
    if (n <= 0) in
    else if (n == 1) RegNext(in)
    else RegNext(RegNextN(in, n-1))
  }
  
  def apply[T <: Data](in: T, n: Int, init: T): T = {
    if (n <= 0) in
    else if (n == 1) RegNext(in, init)
    else RegNext(RegNextN(in, n-1, init), init)
  }
  
  def apply[T <: Data](in: T, n: Int, initOpt: Option[T]): T = {
    initOpt match {
      case Some(init) => apply(in, n, init)
      case None => apply(in, n)
    }
  }
}

// ParallelPriorityMux 스텁
object ParallelPriorityMux {
  def apply[T <: Data](in: Seq[(Bool, T)]): T = PriorityMux(in)
}

// XSPerfAccumulate 스텁  
object XSPerfAccumulate {
  def apply(perfName: String, perfCnt: UInt): Unit = {
    // Performance counter stub - do nothing
  }
}

// XSPerfHistogram 스텁
object XSPerfHistogram {
  def apply(perfName: String, perfCnt: UInt, enable: Bool, start: Int, stop: Int, step: Int): Unit = {
    // Performance histogram stub - do nothing
  }
  
  def apply(perfName: String, perfCnt: UInt, enable: Bool, start: Int, stop: Int, step: Int, right_strict: Boolean): Unit = {
    // Performance histogram stub with right_strict parameter - do nothing
  }
}

// ClockGate 스텁
object ClockGate {
  def apply(clock: Clock, enable: Bool): Clock = clock
  def apply(enable: Bool, clk_en: Bool, clock: Clock): Clock = clock  // 3-parameter version
}

// Code 스텁 
class Code {
  def width(k: Int): Int = 8 // ECC width stub
  def encode(data: UInt): Seq[UInt] = Seq(0.U) // ECC encode stub
  def decode(data: UInt): DecodeResult = new DecodeResult // ECC decode stub
}

class DecodeResult {
  val error = false.B
  val corrected = 0.U
  val correctable = false.B
  val uncorrectable = false.B
}

object Code {
  def apply(): Code = new Code
  def fromString(s: String): Code = new Code
  def fromString(sOpt: Option[String]): Code = new Code
}

// ParallelOR 등
object ParallelOR {
  def apply(in: Seq[Bool]): Bool = in.reduce(_ || _)
}

object ParallelMax {
  def apply[T <: Data](in: Seq[T])(implicit ev: T <:< UInt): T = in.reduce((a, b) => Mux(a >= b, a, b))
}

object ParallelMin {
  def apply[T <: Data](in: Seq[T])(implicit ev: T <:< UInt): T = in.reduce((a, b) => Mux(a <= b, a, b))
}

// Select 관련
object Select {
  def apply[T <: Data](sel: UInt, in: Seq[T]): T = Mux1H(sel, in)
}

// HoldUnless
object HoldUnless {
  def apply[T <: Data](in: T, enable: Bool): T = RegEnable(in, enable)
}

// UIntToOH1
object UIntToOH1 {
  def apply(in: UInt, width: Int): UInt = UIntToOH(in, width)
}

// GTimer
object GTimer {
  def apply(): UInt = RegInit(0.U(64.W))
}

// XSPerfMax
object XSPerfMax {
  def apply(perfName: String, perfCnt: UInt, enable: Bool): Unit = {
    // Stub implementation
  }
}

// ValidIODelay
object ValidIODelay {
  def apply[T <: Data](in: ValidIO[T], delay: Int): ValidIO[T] = {
    val delayed = Wire(ValidIO(chiselTypeOf(in.bits)))
    delayed.valid := ShiftRegister(in.valid, delay)
    delayed.bits := ShiftRegister(in.bits, delay)
    delayed
  }
}

// LatchFastArbiter
class LatchFastArbiter[T <: Data](gen: T, n: Int) extends FastArbiter(gen, n) {
  // Same as FastArbiter for now
}

// SRAMTemplate - standard Chisel interface with proper Bundle definitions
class SRAMTemplate[T <: Data](gen: T, sets: Int, ways: Int = 1, singlePort: Boolean = false, shouldReset: Boolean = false, input_clk_div_by_2: Boolean = false) extends Module {
  def this(gen: T, sets: Int, shouldReset: Boolean, singlePort: Boolean) = this(gen, sets, 1, singlePort, shouldReset, false)
  def this(gen: T, sets: Int, ways: Int, singlePort: Boolean, input_clk_div_by_2: Boolean) = this(gen, sets, ways, singlePort, false, input_clk_div_by_2)
  
  // Define Bundle classes to avoid clone issues
  class ReadReqBundle extends Bundle {
    val setIdx = UInt(log2Ceil(sets).W)
    val waymask = if (ways > 1) Some(UInt(ways.W)) else None
  }
  
  class ReadRespBundle extends Bundle {
    val data = Vec(ways.max(1), gen.cloneType)
  }
  
  class WriteReqBundle extends Bundle {
    val setIdx = UInt(log2Ceil(sets).W) 
    val data = gen.cloneType
    val waymask = if (ways > 1) Some(UInt(ways.W)) else None
  }
  
  val io = IO(new Bundle {
    val r = new Bundle {
      val req = Flipped(Valid(new ReadReqBundle))
      val resp = Valid(new ReadRespBundle)
    }
    val w = new Bundle {
      val req = Flipped(Valid(new WriteReqBundle))
    }
  })
  
  // Simple SRAM implementation stub
  io.r.resp.valid := RegNext(io.r.req.valid)
  io.r.resp.bits.data := VecInit(Seq.fill(ways.max(1))(0.U.asTypeOf(gen.cloneType)))
}

// SRAMWrapper - updated with more parameters
class SRAMWrapper[T <: chisel3.Data](gen: T, set: Int, way: Int, shouldReset: Boolean, holdRead: Boolean, singlePort: Boolean) extends Module {
  val io = IO(new Bundle {
    val r = new Bundle {
      val req = Flipped(Valid(new Bundle {
        val setIdx = UInt(log2Ceil(set).W)
      }))
      val resp = Valid(new Bundle {
        val data = Vec(way, gen)
      })
    }
    val w = new Bundle {
      val req = Flipped(Valid(new Bundle {
        val setIdx = UInt(log2Ceil(set).W)
        val data = Vec(way, gen)
        val waymask = UInt(way.W)
      }))
    }
  })
  
  val mem = SyncReadMem(set, Vec(way, gen))
  
  // Read logic
  io.r.resp.valid := RegNext(io.r.req.valid)
  when(RegNext(io.r.req.valid)) {
    io.r.resp.bits.data := mem.read(RegNext(io.r.req.bits.setIdx))
  }.otherwise {
    io.r.resp.bits.data := DontCare
  }
  
  // Write logic
  when(io.w.req.valid) {
    mem.write(io.w.req.bits.setIdx, io.w.req.bits.data)
  }
}



// C-like types stubs
trait HasCLikeTypes {
  def uint64_t: UInt = UInt(64.W)
  def uint32_t: UInt = UInt(32.W)
  def uint16_t: UInt = UInt(16.W)
  def uint8_t: UInt = UInt(8.W)
}

// EccInfo stubs
class EccInfo extends Bundle {
  val errCode = UInt(8.W)
  val addr = UInt(64.W)
}

object EccInfo {
  val ERR_DATA = 1.U(8.W)
}
