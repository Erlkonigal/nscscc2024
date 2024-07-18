import chisel3._
import chisel3.util._
import bundles._

class wbu extends Module {
    val io = IO(new Bundle {
        val prev = Flipped(Decoupled(new lsu_wbu()))
        // signal to ifu
        val pc = Output(UInt(32.W))
        // writeback
        val wen = Output(UInt(1.W))
        val waddr = Output(UInt(5.W))
        val wdata = Output(UInt(32.W))
        // mult
        val P = Input(UInt(32.W))
    })

    val BranchContr = Module(new branchContr())
    BranchContr.io.pc := io.prev.bits.pc
    BranchContr.io.rj_data := io.prev.bits.rj_data
    BranchContr.io.offset := io.prev.bits.Imm
    BranchContr.io.branchOp := io.prev.bits.branch
    BranchContr.io.SLess := io.prev.bits.SLess
    BranchContr.io.ULess := io.prev.bits.ULess
    BranchContr.io.Zero := io.prev.bits.Zero

    val PC = Module(new pc())
    PC.io.nextPC := BranchContr.io.nextPC
    PC.io.PCInc :=  io.prev.valid

    io.pc := PC.io.PC

    io.wen := io.prev.bits.wbSel =/= WBSel.other && io.prev.valid
    io.waddr := MuxLookup(io.prev.bits.wbDst, 0.U) (Seq(
        WBDst.rd -> io.prev.bits.rd,
        WBDst.one -> 1.U,
    ))
    io.wdata := MuxLookup(io.prev.bits.wbSel, 0.U) (Seq(
        WBSel.alu -> io.prev.bits.ALUOut,
        WBSel.mem -> io.prev.bits.MemOut,
        WBSel.mul -> io.P,
    ))

    io.prev.ready := 1.B
}

class branchContr extends Module {
    val io = IO(new Bundle {
        val pc = Input(UInt(32.W))
        val rj_data = Input(UInt(32.W))
        val offset = Input(UInt(32.W))
        val branchOp = Input(Branch())
        val SLess = Input(UInt(1.W))
        val ULess = Input(UInt(1.W))
        val Zero = Input(UInt(1.W))
        val nextPC = Output(UInt(32.W))
    })

    val PCAsrc = MuxLookup(io.branchOp, io.pc) (Seq(
        Branch.jirl -> io.rj_data,
    ))

    val PCBsrc = MuxLookup(io.branchOp, 4.U) (Seq(
        Branch.beq -> Mux(io.Zero === 1.U, io.offset, 4.U),
        Branch.bne -> Mux(io.Zero === 0.U, io.offset, 4.U),
        Branch.blt -> Mux(io.SLess === 1.U, io.offset, 4.U),
        Branch.bge -> Mux(io.SLess === 0.U, io.offset, 4.U),
        Branch.bltu -> Mux(io.ULess === 1.U, io.offset, 4.U),
        Branch.bgeu -> Mux(io.ULess === 0.U, io.offset, 4.U),
        Branch.b -> io.offset,
        Branch.bl -> io.offset,
        Branch.jirl -> io.offset,
    ))

    io.nextPC := PCAsrc + PCBsrc
}

class pc extends Module {
    val io = IO(new Bundle {
        val nextPC = Input(UInt(32.W))
        val PCInc = Input(UInt(1.W))
        val PC = Output(UInt(32.W))
    })
    val pc = RegInit("h80000000".U)
    pc := Mux(io.PCInc === 1.U, io.nextPC, pc)

    io.PC := pc
}