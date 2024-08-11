import chisel3._
import chisel3.util._
import bundles._

class core extends Module {
    val io = IO(new Bundle {
        val ifu_in = Flipped(Decoupled(new bus_in()))
        val ifu_out = Decoupled(new bus_out())
        val lsu_in = Flipped(Decoupled(new bus_in()))
        val lsu_out = Decoupled(new bus_out())
    })
    val ifu = Module(new ifu())
    val ifu_idu = Module(new buffer(new ifu_idu()))
    val idu = Module(new idu())
    val idu_exu = Module(new buffer(new idu_exu()))
    val exu = Module(new exu())
    val exu_lsu = Module(new buffer(new exu_lsu()))
    val exu_bru = Module(new buffer(new exu_bru()))
    val lsu = Module(new lsu())
    val bru = Module(new bru())
    val lsu_wbu = Module(new buffer(new lsu_wbu()))
    val wbu = Module(new wbu())

    ifu.io.base_in <> io.ifu_in
    ifu.io.base_out <> io.ifu_out
    lsu.io.ext_in <> io.lsu_in
    lsu.io.ext_out <> io.lsu_out

    val forward = Module(new forward())
    forward.io.RD := idu.io.RD
    forward.io.RJ := idu.io.RJ
    forward.io.RK := idu.io.RK
    forward.io.ex_Dst := exu.io.ex_Dst
    forward.io.ex_Sel := exu.io.ex_Sel
    forward.io.ls_Dst := lsu.io.ls_Dst
    forward.io.ls_Sel := lsu.io.ls_Sel
    forward.io.wb_Dst := wbu.io.wb_Dst
    forward.io.wb_Sel := wbu.io.wb_Sel

    ifu.io.flush := bru.io.flush
    ifu_idu.io.stall := forward.io.stall || lsu.io.stall
    ifu_idu.io.flush := bru.io.flush
    idu_exu.io.stall := lsu.io.stall
    idu_exu.io.flush := bru.io.flush
    exu.io.stall := lsu.io.stall // for pipeline multiplier
    exu_lsu.io.stall := lsu.io.stall
    exu_lsu.io.flush := bru.io.flush
    exu_bru.io.stall := lsu.io.stall
    exu_bru.io.flush := bru.io.flush
    bru.io.stall := lsu.io.stall
    lsu.io.flush := 0.B
    lsu_wbu.io.stall := lsu.io.stall
    lsu_wbu.io.flush := 0.B
    wbu.io.stall := lsu.io.stall

    ifu.io.nextPC := bru.io.nextPC
    ifu.io.update := bru.io.update
    ifu.io.u_branch := bru.io.u_branch
    ifu.io.u_type := bru.io.u_type
    ifu.io.u_pc := bru.io.u_pc
    ifu.io.u_target := bru.io.u_target
    idu.io.stall := forward.io.stall
    idu.io.wen := wbu.io.wen
    idu.io.waddr := wbu.io.waddr
    idu.io.wdata := wbu.io.wdata // write back
    idu.io.FwID_RD := forward.io.FwID_RD
    idu.io.FwID_RJ := forward.io.FwID_RJ
    idu.io.FwID_RK := forward.io.FwID_RK
    idu.io.FwEX_RD := forward.io.FwEX_RD
    idu.io.FwEX_RJ := forward.io.FwEX_RJ
    idu.io.FwEX_RK := forward.io.FwEX_RK
    idu.io.ls_ALU := lsu.io.ls_ALU
    idu.io.wb_ALU := wbu.io.wb_ALU
    idu.io.wb_Mem := wbu.io.wb_Mem
    idu.io.wb_Mul := exu.io.MulOut
    exu.io.ls_ALU := lsu.io.ls_ALU
    exu.io.wb_ALU := wbu.io.wb_ALU
    exu.io.wb_Mem := wbu.io.wb_Mem
    wbu.io.MulOut := exu.io.MulOut

    ifu.io.next <> ifu_idu.io.prev
    ifu_idu.io.next <> idu.io.prev
    idu.io.next <> idu_exu.io.prev
    idu_exu.io.next <> exu.io.prev
    exu.io.next <> exu_lsu.io.prev
    exu.io.bru <> exu_bru.io.prev
    exu_lsu.io.next <> lsu.io.prev
    exu_bru.io.next <> bru.io.prev
    lsu.io.next <> lsu_wbu.io.prev
    lsu_wbu.io.next <> wbu.io.prev
}

class buffer[T <: Bundle](gen: T) extends Module {
    val io = IO(new Bundle {
        val prev = Flipped(Decoupled(gen))
        val next = Decoupled(gen)
        val stall = Input(Bool())
        val flush = Input(Bool())
    })
    val state = RegInit(0.B) // 0: idle, 1: valid
    val ready = Wire(Bool())
    val regs = Reg(gen)

    when(io.flush) {
        state := 0.B
        ready := 0.B
    }
    .elsewhen((state && ~io.next.ready) || io.stall) {
        ready := 0.B
        regs := regs
    }
    .otherwise {
        state := io.prev.valid
        ready := 1.B
        regs := Mux(io.prev.valid, io.prev.bits, regs)
    }

    io.next.bits := regs
    io.prev.ready := ready
    io.next.valid := state
}