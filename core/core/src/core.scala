import chisel3._
import chisel3.util._
import bundles._

class core extends Module {
    val io = IO(new Bundle {
        val base_in = Flipped(Decoupled(new bus_in()))
        val base_out = Decoupled(new bus_out())
        val ext_in = Flipped(Decoupled(new bus_in()))
        val ext_out = Decoupled(new bus_out())
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

    ifu.io.base_in <> io.base_in
    lsu.io.ext_in <> io.ext_in
    io.base_out <> ifu.io.base_out
    io.ext_out <> lsu.io.ext_out

    val fwctrl = Module(new fwctrl())
    fwctrl.io.RA := exu.io.RA
    fwctrl.io.RB := exu.io.RB
    fwctrl.io.RDst := exu.io.RDst
    fwctrl.io.RStore := exu.io.RStore
    fwctrl.io.wbSel := exu.io.wbSel
    ifu_idu.io.stall := fwctrl.io.stall(3)
    idu_exu.io.stall := fwctrl.io.stall(2)
    exu_lsu.io.stall := fwctrl.io.stall(1)
    lsu_wbu.io.stall := fwctrl.io.stall(0)
    ifu_idu.io.flush := fwctrl.io.flush(3)
    idu_exu.io.flush := fwctrl.io.flush(2)
    exu_lsu.io.flush := fwctrl.io.flush(1)
    lsu_wbu.io.flush := fwctrl.io.flush(0)
    exu.io.FwASrc := fwctrl.io.FwALUA
    exu.io.FwBSrc := fwctrl.io.FwALUB
    exu.io.FwStore := fwctrl.io.FwStore
    exu.io.ELALU := exu_lsu.io.next.bits.ALUOut
    exu.io.LWALU := lsu_wbu.io.next.bits.ALUOut
    exu.io.LWMEM := lsu_wbu.io.next.bits.MemOut
    exu.io.MULP := exu.io.P
    exu.io.WBData := idu.io.wbdata

    ifu.io.pc := wbu.io.pc
    idu.io.wen := wbu.io.wen
    idu.io.waddr := wbu.io.waddr
    idu.io.wdata := wbu.io.wdata // write back
    wbu.io.P := exu.io.P

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
        val stall = Input(UInt(1.W))
        val flush = Input(UInt(1.W))
    })
    val state = RegInit(0.B) // 0: idle, 1: valid
    val regs = Reg(gen)

    when(io.flush === 1.U) {
        regs := 0.U.asTypeOf(gen)
    }
    .elsewhen(io.stall === 1.U) {
        regs := regs
    }
    .elsewhen(io.prev.valid) {
        state := 1.B
        regs := io.prev.bits
    }

    io.next.bits := regs
    io.prev.ready := io.next.ready && io.stall === 0.U && io.flush === 0.U
    io.next.valid := state === 1.B && io.stall === 0.U && io.flush === 0.U
}