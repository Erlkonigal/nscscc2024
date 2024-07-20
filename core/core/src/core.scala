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

    val flush = idu.io.prev.bits.pc =/= exu.io.nextPC &&
                exu.io.prev.valid &&
                exu.io.stall === 0.U

    val fwctrl = Module(new forwarding())
    fwctrl.io.RD := idu.io.RD
    fwctrl.io.RJ := idu.io.RJ
    fwctrl.io.RK := idu.io.RK
    fwctrl.io.RDst := idu.io.RDst
    fwctrl.io.RSel := idu.io.RSel
    fwctrl.io.flush := flush

    ifu.io.flush := flush
    ifu_idu.io.stall := fwctrl.io.stall
    ifu_idu.io.flush := flush
    idu.io.stall := fwctrl.io.stall
    idu.io.flush := flush
    idu_exu.io.stall := 0.U
    idu_exu.io.flush := 0.U
    exu.io.stall := 0.U
    exu.io.flush := 0.U
    exu_lsu.io.stall := 0.U
    exu_lsu.io.flush := 0.U
    lsu.io.stall := 0.U
    lsu.io.flush := 0.U
    lsu_wbu.io.stall := 0.U
    lsu_wbu.io.flush := 0.U

    ifu.io.nextPC := exu.io.nextPC
    idu.io.wen := wbu.io.wen
    idu.io.waddr := wbu.io.waddr
    idu.io.wdata := wbu.io.wdata // write back
    idu.io.FwID_RD := fwctrl.io.FwID_RD
    idu.io.FwID_RJ := fwctrl.io.FwID_RJ
    idu.io.FwID_RK := fwctrl.io.FwID_RK
    idu.io.FwEX_RD := fwctrl.io.FwEX_RD
    idu.io.FwEX_RJ := fwctrl.io.FwEX_RJ
    idu.io.FwEX_RK := fwctrl.io.FwEX_RK
    idu.io.ELALU := exu_lsu.io.next.bits.ALUOut
    idu.io.LWALU := lsu_wbu.io.next.bits.ALUOut
    idu.io.LWMEM := lsu_wbu.io.next.bits.MemOut
    idu.io.MULP := exu.io.P
    exu.io.ELALU := exu_lsu.io.next.bits.ALUOut
    exu.io.LWALU := lsu_wbu.io.next.bits.ALUOut
    exu.io.LWMEM := lsu_wbu.io.next.bits.MemOut
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
    val ready = io.stall === 0.U && io.flush === 0.U

    when(io.flush === 1.U) {
        state := 0.B
        regs := 0.U.asTypeOf(gen)
    }
    .elsewhen(io.stall === 1.U) {
        state := state
        regs := regs
    }
    .elsewhen(io.prev.valid) {
        state := 1.B
        regs := io.prev.bits
    }
    .otherwise {
        state := 0.B
        regs := 0.U.asTypeOf(gen)
    }

    io.next.bits := regs
    io.prev.ready := ready
    io.next.valid := state === 1.B
}