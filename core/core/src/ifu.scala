import chisel3._
import chisel3.util._
import bundles._

class ifu extends Module {
    val io = IO(new Bundle {
        val base_in = Flipped(Decoupled(new bus_in()))
        val base_out = Decoupled(new bus_out())
        val pc = Input(UInt(32.W))
        val next = Decoupled(new ifu_idu())
    })

    val fetch = RegInit("h80000000".U(32.W))

    val ready = io.base_out.ready && io.next.ready

    when(ready) {
        fetch := fetch + 4.U
        io.base_out.valid := 1.B
    }
    .otherwise {
        fetch := fetch
        io.base_out.valid := 0.B
    }

    io.base_out.bits.addr := fetch
    io.base_out.bits.oe_n := 0.U
    io.base_out.bits.ce_n := 0.U
    io.base_out.bits.we_n := 1.U
    io.base_out.bits.be_n := 0.U
    io.base_out.bits.data_wen := 0.U
    io.base_out.bits.data_in := 0.U
    
    io.base_in.ready := 1.B

    io.next.bits.pc := fetch
    io.next.bits.inst := io.base_in.bits.data_out
    io.next.valid := io.base_out.ready
}

