import chisel3._
import chisel3.util._
import bundles._

class async_transmitter extends BlackBox(Map(
    "ClkFrequency" -> 50000000,
    "Baud" -> 9600
)) {
    val io = IO(new Bundle {
        val clk = Input(Bool())
        val TxD_start = Input(Bool())
        val TxD_data = Input(UInt(8.W))
        val TxD = Output(Bool())
        val TxD_busy = Output(Bool())
    })
}

class async_receiver extends BlackBox(Map(
    "ClkFrequency" -> 50000000,
    "Baud" -> 9600
)) {
    val io = IO(new Bundle {
        val clk = Input(Bool())
        val RxD = Input(Bool())
        val RxD_data_ready = Output(Bool())
        val RxD_clear = Input(Bool())
        val RxD_data = Output(UInt(8.W))
    })
}

class uart_ctrl extends Module {
    val io = IO(new Bundle {
        val in = Flipped(Decoupled(new bus_out()))
        val out = Decoupled(new bus_in())
        val uart = new uart()
    })
    val tran = Module(new async_transmitter())
    val recv = Module(new async_receiver())
    
    val tran_fifo = Module(new Queue(UInt(8.W), 16))
    val recv_fifo = Module(new Queue(UInt(8.W), 16))

    tran.io.clk := clock.asBool
    tran.io.TxD_start := ~tran.io.TxD_busy & tran_fifo.io.deq.valid
    tran_fifo.io.deq.ready := tran.io.TxD_start
    tran.io.TxD_data := tran_fifo.io.deq.bits
    io.uart.tx := tran.io.TxD

    recv.io.clk := clock.asBool
    recv.io.RxD := io.uart.rx
    recv.io.RxD_clear := recv.io.RxD_data_ready & recv_fifo.io.enq.ready
    recv_fifo.io.enq.valid := recv.io.RxD_data_ready
    recv_fifo.io.enq.bits := recv.io.RxD_data
// [7:0] rw, addr
// [8] r, tran_fifo ~full
// [9] r, recv_fifo ~empty
    val master_write = ~io.in.bits.ce_n & ~io.in.bits.we_n & io.in.bits.oe_n
    val master_read = ~io.in.bits.ce_n & io.in.bits.we_n & ~io.in.bits.oe_n

    tran_fifo.io.enq.bits := io.in.bits.data_in(7, 0)

    when(io.in.valid & io.in.bits.addr === 0.U) {
        tran_fifo.io.enq.valid := master_write & tran_fifo.io.enq.ready
        recv_fifo.io.deq.ready := master_read & recv_fifo.io.deq.valid
        io.out.bits.data_out := Cat(Fill(24, 0.B), recv_fifo.io.deq.bits)
    }
    .elsewhen(io.in.valid & io.in.bits.addr === 4.U) {
        tran_fifo.io.enq.valid := 0.B
        recv_fifo.io.deq.ready := 0.B
        io.out.bits.data_out := Cat(Fill(30, 0.B), recv_fifo.io.deq.valid, tran_fifo.io.enq.ready)
    }
    .otherwise {
        tran_fifo.io.enq.valid := 0.B
        recv_fifo.io.deq.ready := 0.B
        io.out.bits.data_out := DontCare
    }

    io.in.ready := 1.B
    io.out.valid := io.in.valid
}