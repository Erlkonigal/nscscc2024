import chisel3._
import chisel3.util._
import bundles._

class ifu extends Module {
    val io = IO(new Bundle {
        // mem
        val base_in = Flipped(Decoupled(new bus_in()))
        val base_out = Decoupled(new bus_out())
        // pipe
        val next = Decoupled(new ifu_idu())
        // pipe signal
        val flush = Input(UInt(1.W))
        val stall = Input(UInt(1.W))
        // nextpc
        val nextPC = Input(UInt(32.W))
    })

    val pc = RegInit("h80000000".U(32.W))
    when(io.flush === 1.U) {
        pc := io.nextPC
    }
    .elsewhen(io.stall === 1.U) {
        pc := pc
    }
    .elsewhen(io.base_out.ready) {
        pc := pc + 4.U
    }
    .otherwise {
        pc := pc
    }

    io.base_out.valid := io.base_out.ready
    io.base_out.bits.addr := pc
    io.base_out.bits.oe_n := 0.U
    io.base_out.bits.ce_n := 0.U
    io.base_out.bits.we_n := 1.U
    io.base_out.bits.be_n := 0.U
    io.base_out.bits.data_wen := 0.U
    io.base_out.bits.data_in := 0.U
    
    io.base_in.ready := 1.B

    io.next.bits.pc := pc
    io.next.bits.inst := io.base_in.bits.data_out
    io.next.valid := io.base_out.ready
}

// static predict: back taken forward not taken
class spred extends Module {
    val io = IO(new Bundle {
        // input signals
        val pc = Input(UInt(32.W))
        val nextPC = Input(UInt(32.W))
        val memready = Input(UInt(1.W))
        // pipe signals
        val stall = Input(UInt(1.W))
        val flush = Input(UInt(1.W))
        // ram data(instruction)
        val inst = Input(UInt(32.W))
        // output next pc
        val npc = Output(UInt(32.W))
    })
}

class btb extends Module {

}

