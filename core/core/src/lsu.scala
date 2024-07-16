import chisel3._
import chisel3.util._
import bundles._

class lsu extends Module {
    val io = IO(new Bundle {
        val ext_in = Flipped(Decoupled(new bus_in()))
        val ext_out = Decoupled(new bus_out())
        val prev = Flipped(Decoupled(new exu2_lsu()))
        val next = Decoupled(new lsu_wbu())
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

    when(io.prev.valid) {
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
        io.next.bits.MemOut := FixLoad
    }
    .otherwise {
        io.ext_out.bits.addr := 0.U
        io.ext_out.bits.ce_n := 1.U
        io.ext_out.bits.oe_n := 1.U
        io.ext_out.bits.we_n := 1.U
        io.ext_out.bits.data_wen := 0.U
        io.ext_out.bits.data_in := 0.U
        io.ext_out.bits.be_n := 0.U
        io.next.bits.MemOut := 0.U
    }

    io.ext_out.valid := io.prev.valid
    io.ext_in.ready := 1.B

    io.next.bits.ALUOut := io.prev.bits.ALUOut
    io.next.bits.SLess := io.prev.bits.SLess
    io.next.bits.ULess := io.prev.bits.ULess
    io.next.bits.Zero := io.prev.bits.Zero
    io.next.bits.branch := io.prev.bits.branch
    io.next.bits.wbSel := io.prev.bits.wbSel
    io.next.bits.wbDst := io.prev.bits.wbDst
    io.next.bits.Imm := io.prev.bits.Imm
    io.next.bits.rd := io.prev.bits.rd
    io.next.bits.rj_data := io.prev.bits.rj_data
    io.next.bits.pc := io.prev.bits.pc

    io.next.valid := io.prev.valid
    io.prev.ready := 1.B
}