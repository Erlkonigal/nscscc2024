import chisel3._
import chisel3.util._
import bundles._

class core extends Module {
    val io = IO(new Bundle {
        val bus_in = Flipped(Decoupled(new bus_in()))
        val bus_out = Decoupled(new bus_out())
    })
    val ifu = Module(new ifu())
    val ifu_idu = Module(new buffer(new ifu_idu()))
    val idu = Module(new idu())
    val idu_exu = Module(new buffer(new idu_exu()))
    val exu = Module(new exu())
    val exu_lsu = Module(new buffer(new exu_lsu()))
    val lsu = Module(new lsu())
    val lsu_wbu = Module(new buffer(new lsu_wbu()))
    val wbu = Module(new wbu())

    val state = RegInit(0.U(2.W)) // 0: reset, 1: start, 2: idle
    state := MuxLookup(state, 0.U) (Seq(
        0.U -> 1.U,
        1.U -> 2.U,
        2.U -> 2.U
    ))
    val IFetch = RegInit(0.U(1.W))
    IFetch := Mux(wbu.io.InstFetch || state === 1.U, 1.U, 0.U)

    val abt = Module(new abt(2))
    abt.io.in.DataIn(0) <> ifu.io.base_out
    abt.io.in.DataIn(1) <> lsu.io.ext_out
    abt.io.out.DataOut <> io.bus_out
    abt.io.in.BusIn <> io.bus_in
    ifu.io.base_in <> abt.io.out.BusOut(0)
    lsu.io.ext_in <> abt.io.out.BusOut(1)

    ifu.io.InstFetch := IFetch
    ifu.io.pc := wbu.io.pc
    idu.io.wen := wbu.io.wen
    idu.io.waddr := wbu.io.waddr
    idu.io.wdata := wbu.io.wdata // write back

    ifu.io.next <> ifu_idu.io.prev
    ifu_idu.io.next <> idu.io.prev
    idu.io.next <> idu_exu.io.prev
    idu_exu.io.next <> exu.io.prev
    exu.io.next <> exu_lsu.io.prev
    exu_lsu.io.next <> lsu.io.prev
    lsu.io.next <> lsu_wbu.io.prev
    lsu_wbu.io.next <> wbu.io.prev
}

class buffer[T <: Bundle](gen: T) extends Module {
    val io = IO(new Bundle {
        val prev = Flipped(Decoupled(gen))
        val next = Decoupled(gen)
    })
    val state = RegInit(0.B) // 0: idle, 1: valid
    state := Mux(io.prev.valid, 1.B, 0.B)

    val regs = Reg(gen)
    when(io.prev.valid) {
        regs := io.prev.bits
    }
    io.next.bits := regs

    io.prev.ready := io.next.ready
    io.next.valid := state === 1.B
}