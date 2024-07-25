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
        val update = Input(Bool())
        val u_branch = Input(Bool())
        val u_type = Input(Bool())
        val u_pc = Input(UInt(32.W))
        val u_target = Input(UInt(32.W))
        val nextPC = Input(UInt(32.W))
    })

    val pc = RegInit("h80000000".U(32.W))
    val btb = Module(new btb())
    val npc = Wire(UInt(32.W))
    
    btb.io.pc := pc
    btb.io.update := io.update
    btb.io.u_branch := io.u_branch
    btb.io.u_type := io.u_type
    btb.io.u_pc := io.u_pc
    btb.io.u_target := io.u_target

    when(io.flush === 1.U) {
        npc := io.nextPC
    }
    .elsewhen(io.stall === 1.U) {
        npc := pc
    }
    .elsewhen(io.base_out.ready) {
        npc := Mux(btb.io.branch, btb.io.btarget, pc + 4.U)
    }
    .otherwise {
        npc := pc
    }

    pc := npc

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
    io.next.bits.npc := npc
    io.next.bits.inst := io.base_in.bits.data_out
    io.next.valid := io.base_out.ready
}

class btb extends Module {
    val io = IO(new Bundle {
        val pc = Input(UInt(32.W))
        val branch = Output(Bool())
        val btarget = Output(UInt(32.W))
        
        val update = Input(Bool())
        val u_branch = Input(Bool())
        val u_type = Input(Bool()) // 0.direct 1.judge
        val u_pc = Input(UInt(32.W))
        val u_target = Input(UInt(32.W))
    })
    val btb = Module(new dist_mem())
    val pht = RegInit(VecInit(Seq.fill(64)(0.U(2.W))))

    val s_index = io.pc(7, 2)
    val s_valid = btb.io.dpo(47)
    val s_type = btb.io.dpo(46)
    val s_tag = btb.io.dpo(45, 32)
    val s_target = btb.io.dpo(31, 0)

    btb.io.clk := clock
    btb.io.dpra := s_index
    io.btarget := s_target
    when(s_valid && io.pc(21, 8) === s_tag) { // valid
        when(s_type && pht(s_index)(1)) { // judge && predict taken
            io.branch := 1.B
        }
        .elsewhen(!s_type) { // direct
            io.branch := 1.B
        }
        .otherwise {
            io.branch := 0.B
        }
    }
    .otherwise {
        io.branch := 0.B
    }

    btb.io.a := io.u_pc(7, 2)
    btb.io.d := Cat(io.update, io.u_type, io.u_pc(21, 8), io.u_target)
    btb.io.we := io.update

    val branchSeq = Seq(
        0.U -> 1.U,
        1.U -> 2.U,
        2.U -> 3.U,
        3.U -> 3.U
    )
    val unbranchSeq = Seq(
        0.U -> 0.U,
        1.U -> 0.U,
        2.U -> 1.U,
        3.U -> 2.U
    )

    when(io.update && io.u_type) {
        pht(io.u_pc(7, 2)) := MuxCase(pht(io.u_pc(7, 2)), Seq(
            io.u_branch -> MuxLookup(pht(io.u_pc(7, 2)), pht(io.u_pc(7, 2))) (branchSeq),
            ~io.u_branch -> MuxLookup(pht(io.u_pc(7, 2)), pht(io.u_pc(7, 2))) (unbranchSeq)
        ))
    }
}
// |47       47|46      46|45      32|31          0|
// |valid(1bit)|type(1bit)|tag(14bit)|target(32bit)|
class dist_mem extends BlackBox {
    val io = IO(new Bundle {
        val clk = Input(Clock())
        val a = Input(UInt(6.W))
        val d = Input(UInt(48.W))
        val we = Input(Bool())
        val spo = Output(UInt(48.W))
        val dpra = Input(UInt(6.W))
        val dpo = Output(UInt(48.W))
    })
}