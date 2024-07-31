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
    forward.io.l1_Dst := lsu.io.l1_Dst
    forward.io.l1_Sel := lsu.io.l1_Sel
    forward.io.l1_stall := lsu.io.stall
    forward.io.l2_Dst := lsu.io.l2_Dst
    forward.io.l2_Sel := lsu.io.l2_Sel
    forward.io.wb_Dst := wbu.io.wb_Dst
    forward.io.wb_Sel := wbu.io.wb_Sel

    ifu.io.flush := bru.io.flush
    ifu_idu.io.stall := forward.io.stall
    ifu_idu.io.flush := bru.io.flush
    idu_exu.io.stall := 0.B
    idu_exu.io.flush := bru.io.flush
    exu_lsu.io.stall := lsu.io.stall
    exu_lsu.io.flush := bru.io.flush
    exu_bru.io.stall := lsu.io.stall
    exu_bru.io.flush := bru.io.flush
    bru.io.stall := lsu.io.stall
    lsu.io.flush := 0.B
    lsu_wbu.io.stall := 0.B
    lsu_wbu.io.flush := 0.B

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
    idu.io.Fw_RD := forward.io.Fw_RD
    idu.io.Fw_RJ := forward.io.Fw_RJ
    idu.io.Fw_RK := forward.io.Fw_RK
    idu.io.ex_ALU := exu.io.ex_ALU
    idu.io.l1_ALU := lsu.io.l1_ALU
    idu.io.l2_ALU := lsu.io.l2_ALU
    idu.io.wb_ALU := wbu.io.wb_ALU
    idu.io.l1_Mem := lsu.io.l1_Mem
    idu.io.l2_Mem := lsu.io.l2_Mem
    idu.io.wb_Mem := wbu.io.wb_Mem
    idu.io.wb_Mul := wbu.io.wb_Mul
    wbu.io.P := exu.io.P

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
    .elsewhen(state && (io.stall || ~io.next.ready)) {
        state := 1.B
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