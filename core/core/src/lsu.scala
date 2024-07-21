import chisel3._
import chisel3.util._
import bundles._

class lsu extends Module {
    val io = IO(new Bundle {
        // mem
        val ext_in = Flipped(Decoupled(new bus_in()))
        val ext_out = Decoupled(new bus_out())
        // pipe
        val prev = Flipped(Decoupled(new exu_lsu()))
        val next = Decoupled(new lsu_wbu())
        // pipe signal
        val stall = Input(UInt(1.W))
        val flush = Input(UInt(1.W))
        // branch
        val nextPC = Output(UInt(32.W))
    })
    val Address = io.prev.bits.ALUOut
    val Op = io.prev.bits.memOp
    val ReadData = io.ext_in.bits.data_out
    val WriteData = io.prev.bits.rd_data
// read
    val FetchByte = MuxLookup(Address(1, 0), 0.U) (Seq(
        0.U -> ReadData(7, 0),
        1.U -> ReadData(15, 8),
        2.U -> ReadData(23, 16),
        3.U -> ReadData(31, 24)
    ))
    val FetchHalf = MuxLookup(Address(1), 0.U) (Seq(
        0.B -> ReadData(15, 0),
        1.B -> ReadData(31, 16)
    ))
    val FetchWord = ReadData
    val FixLoad = MuxLookup(Op, 0.U) (Seq(
        MemOp.ldb  -> Cat(Fill(24, FetchByte(7)), FetchByte(7, 0)),
        MemOp.ldbu -> Cat(Fill(24, 0.B), FetchByte(7, 0)),
        MemOp.ldh  -> Cat(Fill(16, FetchHalf(15)), FetchHalf(15, 0)),
        MemOp.ldhu -> Cat(Fill(16, 0.B), FetchHalf(15, 0)),
        MemOp.ldw  -> FetchWord,
    ))
// mask
    val ByteMask = MuxLookup(Address(1, 0), 0.U) (Seq(
        0.U -> "b1110".U,
        1.U -> "b1101".U,
        2.U -> "b1011".U,
        3.U -> "b0111".U
    ))
    val HalfMask = MuxLookup(Address(1), 0.U) (Seq(
        0.B -> "b1100".U,
        1.B -> "b0011".U
    ))
    val WordMask = "b0000".U
    val GenMask = MuxLookup(Op, 0.U) (Seq(
        MemOp.stb -> ByteMask,
        MemOp.sth -> HalfMask,
        MemOp.stw -> WordMask,
    ))
// write
    val ByteData = MuxLookup(Address(1, 0), 0.U) (Seq(
        0.U -> Cat(Fill(24, 0.B), WriteData(7, 0)),
        1.U -> Cat(Fill(16, 0.B), WriteData(7, 0), Fill(8, 0.B)),
        2.U -> Cat(Fill(8, 0.B), WriteData(7, 0), Fill(16, 0.B)),
        3.U -> Cat(WriteData(7, 0), Fill(24, 0.B))
    ))
    val HalfData = MuxLookup(Address(1), 0.U) (Seq(
        0.B -> Cat(Fill(16, 0.B), WriteData(15, 0)),
        1.B -> Cat(WriteData(15, 0), Fill(16, 0.B))
    ))
    val WordData = WriteData
    val GenData = MuxLookup(Op, 0.U) (Seq(
        MemOp.stb -> ByteData,
        MemOp.sth -> HalfData,
        MemOp.stw -> WordData,
    ))

    io.ext_out.bits.addr := Address
    io.ext_out.bits.ce_n := MuxLookup(Op, 1.U) (Seq(
        MemOp.ldb -> 0.U,
        MemOp.ldbu -> 0.U,
        MemOp.ldh -> 0.U,
        MemOp.ldhu -> 0.U,
        MemOp.ldw -> 0.U,
        MemOp.stb -> 0.U,
        MemOp.sth -> 0.U,
        MemOp.stw -> 0.U,
    ))
    io.ext_out.bits.oe_n := MuxLookup(Op, 1.U) (Seq(
        MemOp.ldb -> 0.U,
        MemOp.ldbu -> 0.U,
        MemOp.ldh -> 0.U,
        MemOp.ldhu -> 0.U,
        MemOp.ldw -> 0.U,
    ))
    io.ext_out.bits.we_n := MuxLookup(Op, 1.U) (Seq(
        MemOp.stb -> 0.U,
        MemOp.sth -> 0.U,
        MemOp.stw -> 0.U,
    ))
    io.ext_out.bits.data_wen := MuxLookup(Op, 0.U) (Seq(
        MemOp.stb -> 1.U,
        MemOp.sth -> 1.U,
        MemOp.stw -> 1.U,
    ))
    io.ext_out.bits.data_in := MuxLookup(Op, 0.U) (Seq(
        MemOp.stb -> GenData,
        MemOp.sth -> GenData,
        MemOp.stw -> GenData,
    ))
    io.ext_out.bits.be_n := MuxLookup(Op, 0.U) (Seq(
        MemOp.stb -> GenMask,
        MemOp.sth -> GenMask,
        MemOp.stw -> GenMask,
    ))

    val branch = Module(new branchContr())
    branch.io.pc := io.prev.bits.pc
    branch.io.rj_data := io.prev.bits.rj_data
    branch.io.offset := io.prev.bits.Imm
    branch.io.branchOp := io.prev.bits.branchOp
    branch.io.SLess := io.prev.bits.SLess
    branch.io.ULess := io.prev.bits.ULess
    branch.io.Zero := io.prev.bits.Zero

    io.nextPC := branch.io.nextPC

    io.ext_out.valid := io.prev.bits.memOp =/= MemOp.other && io.prev.valid
    io.ext_in.ready := 1.B

    io.next.bits.MemOut := FixLoad
    io.next.bits.ALUOut := io.prev.bits.ALUOut
    io.next.bits.wbSel := io.prev.bits.wbSel
    io.next.bits.wbDst := io.prev.bits.wbDst
    io.next.bits.rd := io.prev.bits.rd

    io.next.valid := io.prev.valid && io.stall === 0.U && io.flush === 0.U
    io.prev.ready := io.next.ready
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