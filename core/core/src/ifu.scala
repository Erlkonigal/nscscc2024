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
        val flush = Input(Bool())
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
    val icache = Module(new icache())

    val npc = Wire(UInt(32.W))
    val state = RegInit(0.B)

    val valid = io.next.ready && 
              ((io.base_out.ready && state) || icache.io.valid)

    state := Mux(io.flush, 0.B, MuxLookup(state, 0.B) (Seq(
        0.B -> Mux(io.base_out.ready && ~icache.io.valid, 1.B, state),
        1.B -> Mux(io.next.ready || ~io.base_out.ready, 0.B, state)
    )))

    icache.io.pc := pc
    icache.io.update := state && io.base_out.ready
    icache.io.u_pc := pc
    icache.io.u_inst := io.base_in.bits.data_out


    btb.io.pc := pc
    btb.io.update := io.update
    btb.io.u_branch := io.u_branch
    btb.io.u_type := io.u_type
    btb.io.u_pc := io.u_pc
    btb.io.u_target := io.u_target

    when(io.flush) {
        npc := io.nextPC
    }
    .elsewhen(valid) {
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
    io.next.bits.inst := Mux(icache.io.valid, icache.io.inst, io.base_in.bits.data_out)
    io.next.valid := valid
}

class icache extends Module {
    val io = IO(new Bundle {
        val pc = Input(UInt(32.W))
        val valid = Output(Bool())
        val inst = Output(UInt(32.W))

        val update = Input(Bool())
        val u_pc = Input(UInt(32.W))
        val u_inst = Input(UInt(32.W))
    })
    val cache = Module(new dist_mem())
    val s_index = io.pc(7, 2)
    val s_valid = cache.io.dpo(46)
    val s_tag = cache.io.dpo(45, 32)
    val s_inst = cache.io.dpo(31, 0)

    cache.io.clk := clock
    cache.io.dpra := s_index
    io.inst := s_inst
    io.valid := s_valid && ~(io.pc(21, 8) ^ s_tag).orR

    cache.io.a := io.u_pc(7, 2)
    cache.io.d := Cat(0.B, io.update, io.u_pc(21, 8), io.u_inst)
    cache.io.we := io.update
}
// |      47|46       46|45      32|31        0|
// |preserve|valid(1bit)|tag(14bit)|inst(32bit)|

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
    when(s_valid && ~(io.pc(21, 8) ^ s_tag).orR) { // valid
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