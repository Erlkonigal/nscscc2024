import chisel3._
import chisel3.util._
import bundles._

class bru extends Module {
    val io = IO(new Bundle {
        // pipe
        val prev = Flipped(Decoupled(new exu_bru))
        // pipe signal
        val stall = Input(Bool())
        val flush = Output(Bool())
        // branch
        val update = Output(Bool())
        val u_branch = Output(Bool())
        val u_type = Output(Bool())
        val u_pc = Output(UInt(32.W))
        val u_target = Output(UInt(32.W))
        val nextPC = Output(UInt(32.W))
    })

    val Zero = io.prev.bits.Zero
    val SLess = io.prev.bits.SLess
    val ULess = io.prev.bits.ULess
    val pcadd4 = io.prev.bits.pcadd4
    val pcoff = io.prev.bits.pcoff
    val jirlPC = io.prev.bits.jirlpc

    val branch = MuxLookup(io.prev.bits.branchOp, 0.B) (Seq(
        Branch.jirl -> 1.B,
        Branch.b -> 1.B,
        Branch.bl -> 1.B,
        Branch.beq -> Mux(Zero, 1.B, 0.B),
        Branch.bne -> Mux(~Zero, 1.B, 0.B),
        Branch.blt -> Mux(SLess, 1.B, 0.B),
        Branch.bge -> Mux(~SLess, 1.B, 0.B),
        Branch.bltu -> Mux(ULess, 1.B, 0.B),
        Branch.bgeu -> Mux(~ULess, 1.B, 0.B),
    ))
    val btarget = MuxLookup(io.prev.bits.branchOp, pcoff) (Seq(
        Branch.jirl -> jirlPC,
    ))
    val nextPC = Mux(branch, btarget, pcadd4)

    val flush = ~Compare.equals(nextPC, io.prev.bits.npc) && io.prev.valid && ~io.stall
    val update = ~Compare.equals(io.prev.bits.branchOp.asUInt, Branch.other.asUInt) && io.prev.valid && ~io.stall
    val utype = MuxLookup(io.prev.bits.branchOp, 1.B) (Seq(
        Branch.jirl -> 0.B,
        Branch.b -> 0.B,
        Branch.bl -> 0.B,
    ))

    io.flush := flush
    io.update := update
    io.u_branch := branch
    io.u_type := utype
    io.u_pc := io.prev.bits.pc
    io.u_target := btarget
    io.nextPC := nextPC

    io.prev.ready := 1.B
}