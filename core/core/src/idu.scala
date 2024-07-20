import bundles._
import chisel3._
import chisel3.util._

class forwarding extends Module {
    val io = IO(new Bundle {
        val RJ = Input(UInt(5.W))
        val RK = Input(UInt(5.W))
        val RD = Input(UInt(5.W))
        val RDst = Input(UInt(5.W))
        val RSel = Input(WBSel())

        val flush = Input(UInt(1.W))
        val stall = Output(UInt(1.W))

        val FwID_RJ = Output(ForwardSrc())
        val FwID_RK = Output(ForwardSrc())
        val FwID_RD = Output(ForwardSrc())

        val FwEX_RJ = Output(ForwardSrc())
        val FwEX_RK = Output(ForwardSrc())
        val FwEX_RD = Output(ForwardSrc())
    })
    val RDstReg = Reg(Vec(3, UInt(5.W)))
    val RSelReg = Reg(Vec(3, WBSel()))

    val calcID = (src: UInt) => {
        Mux(src === 0.U, ForwardSrc.other,
            MuxCase(ForwardSrc.other, Seq(
                (src === RDstReg(0)) -> MuxCase(ForwardSrc.other, Seq(
                    (RSelReg(0) === WBSel.alu) -> ForwardSrc.stall,
                    (RSelReg(0) === WBSel.mem) -> ForwardSrc.stall,
                    (RSelReg(0) === WBSel.mul) -> ForwardSrc.stall
                )),
                (src === RDstReg(1)) -> MuxCase(ForwardSrc.other, Seq(
                    (RSelReg(1) === WBSel.alu) -> ForwardSrc.elALU,
                    (RSelReg(1) === WBSel.mem) -> ForwardSrc.stall,
                    (RSelReg(1) === WBSel.mul) -> ForwardSrc.stall
                )),
                (src === RDstReg(2)) -> MuxCase(ForwardSrc.other, Seq(
                    (RSelReg(2) === WBSel.alu) -> ForwardSrc.lwALU,
                    (RSelReg(2) === WBSel.mem) -> ForwardSrc.lwMem,
                    (RSelReg(2) === WBSel.mul) -> ForwardSrc.mulP
                ))
            ))
        )
    }
    val calcEX = (src: UInt) => {
        Mux(src === 0.U, ForwardSrc.other,
            MuxCase(ForwardSrc.other, Seq(
                (src === RDstReg(0)) -> MuxCase(ForwardSrc.other, Seq(
                    (RSelReg(0) === WBSel.alu) -> ForwardSrc.elALU,
                    (RSelReg(0) === WBSel.mem) -> ForwardSrc.stall,
                    (RSelReg(0) === WBSel.mul) -> ForwardSrc.stall,
                )),
                (src === RDstReg(1)) -> MuxCase(ForwardSrc.other, Seq(
                    (RSelReg(1) === WBSel.alu) -> ForwardSrc.lwALU,
                    (RSelReg(1) === WBSel.mem) -> ForwardSrc.lwMem,
                    (RSelReg(1) === WBSel.mul) -> ForwardSrc.mulP,
                )),
                (src === RDstReg(2)) -> MuxCase(ForwardSrc.other, Seq(
                    (RSelReg(2) === WBSel.alu) -> ForwardSrc.stall,
                    (RSelReg(2) === WBSel.mem) -> ForwardSrc.stall,
                    (RSelReg(2) === WBSel.mul) -> ForwardSrc.stall,
                )),
            ))
        )
    }
    val calcRetID = (srcID: ForwardSrc.Type, srcEX: ForwardSrc.Type) => {
        val ret = Wire(ForwardSrc())
        when(srcID === ForwardSrc.stall && srcEX === ForwardSrc.stall) {
            ret := ForwardSrc.stall
        }
        .elsewhen(srcID === ForwardSrc.stall) {
            ret := ForwardSrc.other
        }
        .otherwise {
            ret := srcID
        }
        ret
    }
    val calcRetEX = (srcID: ForwardSrc.Type, srcEX: ForwardSrc.Type) => {
        val ret = Wire(ForwardSrc())
        when(srcID === ForwardSrc.stall && srcEX === ForwardSrc.stall) {
            ret := ForwardSrc.stall
        }
        .elsewhen(srcID === ForwardSrc.stall) {
            ret := srcEX
        }
        .otherwise {
            ret := ForwardSrc.other
        }
        ret
    }
    val ID_RJ = calcID(io.RJ)
    val ID_RK = calcID(io.RK)
    val ID_RD = calcID(io.RD)
    val EX_RJ = calcEX(io.RJ)
    val EX_RK = calcEX(io.RK)
    val EX_RD = calcEX(io.RD)

    val RetID_RJ = calcRetID(ID_RJ, EX_RJ)
    val RetID_RK = calcRetID(ID_RK, EX_RK)
    val RetID_RD = calcRetID(ID_RD, EX_RD)
    val RetEX_RJ = calcRetEX(ID_RJ, EX_RJ)
    val RetEX_RK = calcRetEX(ID_RK, EX_RK)
    val RetEX_RD = calcRetEX(ID_RD, EX_RD)

    val StallID = RetID_RJ === ForwardSrc.stall || RetID_RK === ForwardSrc.stall || RetID_RD === ForwardSrc.stall
    val StallEX = RetEX_RJ === ForwardSrc.stall || RetEX_RK === ForwardSrc.stall || RetEX_RD === ForwardSrc.stall
    val Stall = StallID || StallEX

    when(io.flush === 0.U) {
        RDstReg(0) := Mux(Stall, 0.U, io.RDst)
        RSelReg(0) := Mux(Stall, WBSel.other, io.RSel)
        for(i <- 0 until RDstReg.length - 1) {
            RDstReg(i + 1) := RDstReg(i)
            RSelReg(i + 1) := RSelReg(i)
        }
    }
    .otherwise {
        RDstReg := VecInit(Seq.fill(3)(0.U))
        RSelReg := VecInit(Seq.fill(3)(WBSel.other))
    }

    io.stall := Stall
    io.FwID_RJ := RetID_RJ
    io.FwID_RK := RetID_RK
    io.FwID_RD := RetID_RD
    io.FwEX_RJ := RetEX_RJ
    io.FwEX_RK := RetEX_RK
    io.FwEX_RD := RetEX_RD
}

class idu extends Module {
    val io = IO(new Bundle {
        // pipe
        val prev = Flipped(Decoupled(new ifu_idu()))
        val next = Decoupled(new idu_exu())
        // pipe signal
        val stall = Input(UInt(1.W))
        val flush = Input(UInt(1.W))
        // writeback to register file
        val wen = Input(UInt(1.W))
        val waddr = Input(UInt(5.W))
        val wdata = Input(UInt(32.W))
        // forwarding control
        val RJ = Output(UInt(5.W))
        val RK = Output(UInt(5.W))
        val RD = Output(UInt(5.W))
        val RDst = Output(UInt(5.W))
        val RSel = Output(WBSel())
        val FwID_RJ = Input(ForwardSrc())
        val FwID_RK = Input(ForwardSrc())
        val FwID_RD = Input(ForwardSrc())
        val FwEX_RJ = Input(ForwardSrc())
        val FwEX_RK = Input(ForwardSrc())
        val FwEX_RD = Input(ForwardSrc())
        // forwarding signal
        val ELALU = Input(UInt(32.W))
        val LWALU = Input(UInt(32.W))
        val LWMEM = Input(UInt(32.W))
        val MULP = Input(UInt(32.W))
    })

    def ADD_W     = BitPat("b00000000000100000_?????_?????_?????")
    def SUB_W     = BitPat("b00000000000100010_?????_?????_?????")
    def SLT       = BitPat("b00000000000100100_?????_?????_?????")
    def SLTU      = BitPat("b00000000000100101_?????_?????_?????")
    def NOR       = BitPat("b00000000000101000_?????_?????_?????")
    def AND       = BitPat("b00000000000101001_?????_?????_?????")
    def OR        = BitPat("b00000000000101010_?????_?????_?????")
    def XOR       = BitPat("b00000000000101011_?????_?????_?????")
    def SLL_W     = BitPat("b00000000000101110_?????_?????_?????")
    def SRL_W     = BitPat("b00000000000101111_?????_?????_?????")
    def SRA_W     = BitPat("b00000000000110000_?????_?????_?????")
    def MUL_W     = BitPat("b00000000000111000_?????_?????_?????")

    def SLLI_W    = BitPat("b00000000010000001_?????_?????_?????")
    def SRLI_W    = BitPat("b00000000010001001_?????_?????_?????")
    def SRAI_W    = BitPat("b00000000010010001_?????_?????_?????")

    def SLTI      = BitPat("b0000001000_????????????_?????_?????")
    def SLTUI     = BitPat("b0000001001_????????????_?????_?????")
    def ADDI_W    = BitPat("b0000001010_????????????_?????_?????")
    def ANDI      = BitPat("b0000001101_????????????_?????_?????")
    def ORI       = BitPat("b0000001110_????????????_?????_?????")
    def XORI      = BitPat("b0000001111_????????????_?????_?????")

    def LU12I_W   = BitPat("b0001010__????????????????????_?????")
    def PCADDU12I = BitPat("b0001110__????????????????????_?????")

    def LD_B      = BitPat("b0010100000_????????????_?????_?????")
    def LD_H      = BitPat("b0010100001_????????????_?????_?????")
    def LD_W      = BitPat("b0010100010_????????????_?????_?????")
    def ST_B      = BitPat("b0010100100_????????????_?????_?????")
    def ST_H      = BitPat("b0010100101_????????????_?????_?????")
    def ST_W      = BitPat("b0010100110_????????????_?????_?????")
    def LD_BU     = BitPat("b0010101000_????????????_?????_?????")
    def LD_HU     = BitPat("b0010101001_????????????_?????_?????")

    def JIRL      = BitPat("b010011_????????????????_?????_?????")
    def B         = BitPat("b010100_????????????????__??????????")
    def BL        = BitPat("b010101_????????????????__??????????")
    def BEQ       = BitPat("b010110_????????????????_?????_?????")
    def BNE       = BitPat("b010111_????????????????_?????_?????")
    def BLT       = BitPat("b011000_????????????????_?????_?????")
    def BGE       = BitPat("b011001_????????????????_?????_?????")
    def BLTU      = BitPat("b011010_????????????????_?????_?????")
    def BGEU      = BitPat("b011011_????????????????_?????_?????")

    val DecodeTable = Array(
        ADD_W     -> List(ALUOp.add  , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        SUB_W     -> List(ALUOp.sub  , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        SLT       -> List(ALUOp.slt  , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        SLTU      -> List(ALUOp.sltu , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        NOR       -> List(ALUOp.nor  , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        AND       -> List(ALUOp.and  , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        OR        -> List(ALUOp.or   , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        XOR       -> List(ALUOp.xor  , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        SLL_W     -> List(ALUOp.sll  , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        SRL_W     -> List(ALUOp.srl  , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        SRA_W     -> List(ALUOp.sra  , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        MUL_W     -> List(ALUOp.mul  , ALUAsrc.rj   , ALUBsrc.rk   , ImmType.other     , MemOp.other, Branch.other, WBSel.mul  , WBDst.rd),

        SLLI_W    -> List(ALUOp.sll  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI5U , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        SRLI_W    -> List(ALUOp.srl  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI5U , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        SRAI_W    -> List(ALUOp.sra  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI5U , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),

        SLTI      -> List(ALUOp.slt  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12 , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        SLTUI     -> List(ALUOp.sltu , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12 , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        ADDI_W    -> List(ALUOp.add  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12 , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        ANDI      -> List(ALUOp.and  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12U, MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        ORI       -> List(ALUOp.or   , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12U, MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        XORI      -> List(ALUOp.xor  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12U, MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),

        LU12I_W   -> List(ALUOp.add  , ALUAsrc.other, ALUBsrc.imm  , ImmType.type1RI20 , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),
        PCADDU12I -> List(ALUOp.add  , ALUAsrc.pc   , ALUBsrc.imm  , ImmType.type1RI20 , MemOp.other, Branch.other, WBSel.alu  , WBDst.rd),

        LD_B      -> List(ALUOp.add  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12 , MemOp.ldb  , Branch.other, WBSel.mem  , WBDst.rd),
        LD_H      -> List(ALUOp.add  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12 , MemOp.ldh  , Branch.other, WBSel.mem  , WBDst.rd),
        LD_W      -> List(ALUOp.add  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12 , MemOp.ldw  , Branch.other, WBSel.mem  , WBDst.rd),
        ST_B      -> List(ALUOp.add  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12 , MemOp.stb  , Branch.other, WBSel.other, WBDst.other),
        ST_H      -> List(ALUOp.add  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12 , MemOp.sth  , Branch.other, WBSel.other, WBDst.other),
        ST_W      -> List(ALUOp.add  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12 , MemOp.stw  , Branch.other, WBSel.other, WBDst.other),
        LD_BU     -> List(ALUOp.add  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12 , MemOp.ldbu , Branch.other, WBSel.mem  , WBDst.rd),
        LD_HU     -> List(ALUOp.add  , ALUAsrc.rj   , ALUBsrc.imm  , ImmType.type2RI12 , MemOp.ldhu , Branch.other, WBSel.mem  , WBDst.rd),

        JIRL      -> List(ALUOp.add  , ALUAsrc.pc   , ALUBsrc.four , ImmType.type2RI16 , MemOp.other, Branch.jirl , WBSel.alu  , WBDst.rd),
        B         -> List(ALUOp.other, ALUAsrc.other, ALUBsrc.other, ImmType.typeI26   , MemOp.other, Branch.b    , WBSel.other, WBDst.other),
        BL        -> List(ALUOp.add  , ALUAsrc.pc   , ALUBsrc.four , ImmType.typeI26   , MemOp.other, Branch.bl   , WBSel.alu  , WBDst.one),
        BEQ       -> List(ALUOp.sub  , ALUAsrc.rj   , ALUBsrc.rd   , ImmType.type2RI16 , MemOp.other, Branch.beq  , WBSel.other, WBDst.other),
        BNE       -> List(ALUOp.sub  , ALUAsrc.rj   , ALUBsrc.rd   , ImmType.type2RI16 , MemOp.other, Branch.bne  , WBSel.other, WBDst.other),
        BLT       -> List(ALUOp.sub  , ALUAsrc.rj   , ALUBsrc.rd   , ImmType.type2RI16 , MemOp.other, Branch.blt  , WBSel.other, WBDst.other),
        BGE       -> List(ALUOp.sub  , ALUAsrc.rj   , ALUBsrc.rd   , ImmType.type2RI16 , MemOp.other, Branch.bge  , WBSel.other, WBDst.other),
        BLTU      -> List(ALUOp.sub  , ALUAsrc.rj   , ALUBsrc.rd   , ImmType.type2RI16 , MemOp.other, Branch.bltu , WBSel.other, WBDst.other),
        BGEU      -> List(ALUOp.sub  , ALUAsrc.rj   , ALUBsrc.rd   , ImmType.type2RI16 , MemOp.other, Branch.bgeu , WBSel.other, WBDst.other)
    )
    val default = List(ALUOp.other, ALUAsrc.other, ALUBsrc.other, ImmType.other, MemOp.other, Branch.other, WBSel.other, WBDst.other)
    val Decode = ListLookup(io.prev.bits.inst, default, DecodeTable)

    val immGen = Module(new ImmGen())
    immGen.io.inst26 := io.prev.bits.inst(25, 0)
    immGen.io.immType := Decode(3)

    val regFile = Module(new RegFile())
    regFile.io.rd := io.prev.bits.inst(4, 0)
    regFile.io.rj := io.prev.bits.inst(9, 5)
    regFile.io.rk := io.prev.bits.inst(14, 10)
    regFile.io.wen := io.wen
    regFile.io.waddr := io.waddr
    regFile.io.wdata := io.wdata

    io.RD := io.prev.bits.inst(4, 0)
    io.RJ := io.prev.bits.inst(9, 5)
    io.RK := io.prev.bits.inst(14, 10)
    io.RSel := Decode(6)
    io.RDst := MuxLookup(Decode(7), 0.U) (Seq(
        WBDst.rd -> io.prev.bits.inst(4, 0),
        WBDst.one -> 1.U
    ))

    val FwSrcSeq = Seq(
        ForwardSrc.elALU -> io.ELALU,
        ForwardSrc.lwALU -> io.LWALU,
        ForwardSrc.lwMem -> io.LWMEM,
        ForwardSrc.mulP -> io.MULP
    )
    val ForwardRD = MuxLookup(io.FwID_RD, regFile.io.rd_data) (FwSrcSeq)
    val ForwardRJ = MuxLookup(io.FwID_RJ, regFile.io.rj_data) (FwSrcSeq)
    val ForwardRK = MuxLookup(io.FwID_RK, regFile.io.rk_data) (FwSrcSeq)

    io.next.bits.aluOp := Decode(0)
    io.next.bits.aluAsrc := Decode(1)
    io.next.bits.aluBsrc := Decode(2)
    io.next.bits.memOp := Decode(4)
    io.next.bits.branchOp := Decode(5)
    io.next.bits.wbSel := Decode(6)
    io.next.bits.wbDst := Decode(7)

    io.next.bits.Imm := immGen.io.Imm

    io.next.bits.rd := io.prev.bits.inst(4, 0)
    io.next.bits.rj := io.prev.bits.inst(9, 5)
    io.next.bits.rk := io.prev.bits.inst(14, 10)
    io.next.bits.rd_data := ForwardRD
    io.next.bits.rj_data := ForwardRJ
    io.next.bits.rk_data := ForwardRK
    io.next.bits.FwEX_RD := io.FwEX_RD
    io.next.bits.FwEX_RJ := io.FwEX_RJ
    io.next.bits.FwEX_RK := io.FwEX_RK

    io.next.bits.pc := io.prev.bits.pc

    io.next.valid := io.prev.valid && io.stall === 0.U && io.flush === 0.U
    io.prev.ready := io.next.ready
}

class RegFile extends Module {
    val io = IO(new Bundle {
        val rd = Input(UInt(5.W)) // r
        val rj = Input(UInt(5.W)) // r
        val rk = Input(UInt(5.W)) // r
        val rd_data = Output(UInt(32.W))
        val rj_data = Output(UInt(32.W))
        val rk_data = Output(UInt(32.W))

        val wen = Input(UInt(1.W))
        val waddr = Input(UInt(5.W))
        val wdata = Input(UInt(32.W))
    })
    val reg = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

    io.rd_data := Mux(io.rd === 0.U, 0.U, reg(io.rd))
    io.rj_data := Mux(io.rj === 0.U, 0.U, reg(io.rj))
    io.rk_data := Mux(io.rk === 0.U, 0.U, reg(io.rk))

    reg(io.waddr) := Mux(io.wen === 1.U, io.wdata, reg(io.waddr))
}

class ImmGen extends Module {
    val io = IO(new Bundle {
        val inst26 = Input(UInt(26.W))
        val immType = Input(ImmType())
        val Imm = Output(UInt(32.W))
    })

    io.Imm := MuxLookup(io.immType, 0.U) (
        Seq(
            ImmType.type2RI5U -> Cat(Fill(27, 0.B), io.inst26(14, 10)),
            ImmType.type2RI8 -> Cat(Fill(24, io.inst26(17)), io.inst26(17, 10)),
            ImmType.type2RI12 -> Cat(Fill(20, io.inst26(21)), io.inst26(21, 10)),
            ImmType.type2RI12U -> Cat(Fill(20, 0.B), io.inst26(21, 10)),
            ImmType.type2RI14 -> Cat(Fill(16, io.inst26(23)), io.inst26(23, 10), Fill(2, 0.B)),
            ImmType.type2RI16 -> Cat(Fill(14, io.inst26(25)), io.inst26(25, 10), Fill(2, 0.B)),
            ImmType.type1RI20 -> Cat(io.inst26(24, 5), Fill(12, 0.B)),
            ImmType.type1RI21 -> Cat(Fill(9, io.inst26(4)), io.inst26(4, 0), io.inst26(25, 10), Fill(2, 0.B)),
            ImmType.typeI26 -> Cat(Fill(4, io.inst26(9)), io.inst26(9, 0), io.inst26(25, 10), Fill(2, 0.B))
        )
    )
}

