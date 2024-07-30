import chisel3._
import chisel3.util._
import bundles._

class async_transmitter extends BlackBox(Map(
    "ClkFrequency" -> 100000000,
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
    "ClkFrequency" -> 100000000,
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
    // write state machine
    val state = RegInit(0.U(2.W))
    val tx_data_buf = RegInit(0.U(8.W))
    val tx_addr_buf = RegInit(0.B)
    val uart_rd_buf = RegInit(0.B) // 0: write, 1: read

    val tran = Module(new async_transmitter())
    val recv = Module(new async_receiver())
    
    val tran_fifo = Module(new Queue(UInt(8.W), 16))
    val recv_fifo = Module(new Queue(UInt(8.W), 16))

    val master_write = ~io.in.bits.ce_n & ~io.in.bits.we_n & io.in.bits.oe_n
    val master_read = ~io.in.bits.ce_n & io.in.bits.we_n & ~io.in.bits.oe_n

    state := MuxLookup(state, 0.U) (Seq(
        0.U -> Mux(io.in.valid, 1.U, 0.U),
        1.U -> Mux(io.in.valid, 2.U, 0.U),
        2.U -> Mux(io.in.valid, 1.U, 0.U)
    ))
    tx_data_buf := io.in.bits.data_in(7, 0)
    tx_addr_buf := io.in.bits.addr(2)
    uart_rd_buf := master_read

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

    tran_fifo.io.enq.bits := tx_data_buf

    when(state === 2.U && ~tx_addr_buf) {
        tran_fifo.io.enq.valid := ~uart_rd_buf & tran_fifo.io.enq.ready
        recv_fifo.io.deq.ready := uart_rd_buf & recv_fifo.io.deq.valid
    }
    .otherwise {
        tran_fifo.io.enq.valid := 0.B
        recv_fifo.io.deq.ready := 0.B
    }

    val recv_data = Cat(Fill(24, 0.B), recv_fifo.io.deq.bits)
    val uart_state = Cat(Fill(30, 0.B), recv_fifo.io.deq.valid, tran_fifo.io.enq.ready)

    io.out.bits.data_out := Mux(io.in.bits.addr(2), uart_state, recv_data)

    io.in.ready := 1.B
    io.out.valid := state
}