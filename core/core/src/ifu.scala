import chisel3._
import chisel3.util._
import bundles._

class ifu extends Module {
    val io = IO(new Bundle {
        val base_in = Flipped(Decoupled(new bus_in()))
        val base_out = Decoupled(new bus_out())
        val InstFetch = Input(Bool()) // 多周期需要WBU通知取指
        val pc = Input(UInt(32.W))
        val next = Decoupled(new ifu_idu())
    })
    when(io.InstFetch) {
        io.base_out.bits.addr := io.pc
        io.base_out.bits.oe_n := 0.U
        io.base_out.bits.ce_n := 0.U
    }
    .otherwise {
        io.base_out.bits.addr := 0.U
        io.base_out.bits.oe_n := 1.U
        io.base_out.bits.ce_n := 1.U
    }
    io.base_out.bits.we_n := 1.U
    io.base_out.bits.be_n := 0.U
    io.base_out.bits.data_wen := 0.U
    io.base_out.bits.data_in := 0.U
    
    io.base_out.valid := io.InstFetch
    io.base_in.ready := 1.B

    io.next.bits.pc := io.pc
    io.next.bits.inst := io.base_in.bits.data_out
    io.next.valid := io.InstFetch
}

