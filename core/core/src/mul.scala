import chisel3._
import bundles._

class mult_gen_0 extends BlackBox {
    val io = IO(new Bundle {
        val CLK = Input(Clock())
        val A = Input(UInt(32.W))
        val B = Input(UInt(32.W))
        val P = Output(UInt(32.W))
    })
}