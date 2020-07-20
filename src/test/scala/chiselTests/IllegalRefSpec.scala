// See LICENSE for license details.

package chiselTests

import chisel3._
import chisel3.stage.ChiselStage

object IllegalRefSpec {
  class IllegalRefInner extends RawModule {
    val io = IO(new Bundle {
      val i = Input(Bool())
      val o = Output(Bool())
    })
    val x = io.i & io.i
    io.o := io.i
  }

  class IllegalRefOuter(useConnect: Boolean) extends RawModule {
    val io = IO(new Bundle {
      val a = Input(Bool())
      val b = Input(Bool())
      val out = Output(Bool())
    })

    val inst = Module(new IllegalRefInner)
    io.out := inst.io.o
    inst.io.i := io.a
    val x = WireInit(io.b)
    if (useConnect) {
      val z = WireInit(inst.x) // oops
    } else {
      val z = inst.x & inst.x // oops
    }
  }

  class CrossWhenConnect extends RawModule {
    val io = IO(new Bundle {
      val i = Input(Bool())
      val o = Output(Bool())
    })
    private var tmp: Option[Data] = None
    when (io.i) {
      val x = io.i & io.i
      tmp = Some(x)
    }
    io.o := tmp.get
  }
}

class IllegalRefSpec extends ChiselFlatSpec with Utils {
  import IllegalRefSpec._

  "Illegal cross-module references in connect" should "fail" in {
    a [ChiselException] should be thrownBy extractCause[ChiselException] {
      ChiselStage.elaborate { new IllegalRefOuter(true) }
    }
  }

  "Using a signal that has escaped its enclosing when scope" should "fail" in {
    a [ChiselException] should be thrownBy extractCause[ChiselException] {
      ChiselStage.elaborate { new CrossWhenConnect }
    }
  }
}
