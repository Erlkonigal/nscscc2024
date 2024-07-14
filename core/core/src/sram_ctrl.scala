import chisel3._
import chisel3.util._
import bundles._

class sram_ctrl extends Module {
    val io = IO(new Bundle {
        val in = Flipped(Decoupled(new bus_out()))
        val out = Decoupled(new bus_in())
        val sram = new sram()
    })
    when(io.in.valid) {
        io.sram.data_wen := io.in.bits.data_wen
        io.sram.data_in := io.in.bits.data_in
        io.sram.addr := io.in.bits.addr(21, 2) // 4MB / 4B = 1M
        io.sram.be_n := io.in.bits.be_n
        io.sram.ce_n := io.in.bits.ce_n
        io.sram.oe_n := io.in.bits.oe_n
        io.sram.we_n := io.in.bits.we_n
        io.out.bits.data_out := io.sram.data_out
    }
    .otherwise {
        io.sram.data_wen := 0.U
        io.sram.data_in := 0.U
        io.sram.addr := 0.U
        io.sram.be_n := 0.U
        io.sram.ce_n := 1.U
        io.sram.oe_n := 1.U
        io.sram.we_n := 1.U
        io.out.bits.data_out := 0.U
    }

    io.in.ready := 1.B
    io.out.valid := io.in.valid
}