import chisel3._
import chisel3.util._
import bundles._

class exu extends Module {
    val io = IO(new Bundle {
        val prev = Flipped(Decoupled(new idu_exu()))
        val next = Decoupled(new exu_lsu())
        val P = Output(UInt(32.W)) // multiplier
        // forward control
        val RA = Output(UInt(5.W))
        val RB = Output(UInt(5.W))
        val RDst = Output(UInt(5.W))
        val RStore = Output(UInt(5.W))
        val wbSel = Output(WBSel())
        val FwASrc = Input(ForwardSrc())
        val FwBSrc = Input(ForwardSrc())
        val FwStore = Input(ForwardSrc())
        // forward data
        val ELALU = Input(UInt(32.W)) // EXU/LSU ALUOut
        val LWALU = Input(UInt(32.W)) // LSU/WBU ALUOut
        val LWMEM = Input(UInt(32.W)) // LSU/WBU MemOut
        val MULP = Input(UInt(32.W)) // MUL P
        val WBData = Input(UInt(32.W)) // WBU->Reg Data(Last write reg data)
    })
    val FwSrcSeq = Seq(
        ForwardSrc.elALU -> io.ELALU,
        ForwardSrc.lwALU -> io.LWALU,
        ForwardSrc.lwMem -> io.LWMEM,
        ForwardSrc.mulP -> io.MULP,
        ForwardSrc.lastWB -> io.WBData,
    )
    io.RA := MuxLookup(io.prev.bits.aluAsrc, 0.U) (Seq(
        ALUAsrc.rj -> io.prev.bits.rj
    ))
    io.RB := MuxLookup(io.prev.bits.aluBsrc, 0.U) (Seq(
        ALUBsrc.rk -> io.prev.bits.rk,
        ALUBsrc.rd -> io.prev.bits.rd
    ))
    io.RDst := MuxLookup(io.prev.bits.wbDst, 0.U) (Seq(
        WBDst.one -> 1.U,
        WBDst.rd -> io.prev.bits.rd
    ))
    io.RStore := io.prev.bits.rd
    io.wbSel := io.prev.bits.wbSel

    val ALU = Module(new alu())
    val ALUA = MuxLookup(io.prev.bits.aluAsrc, 0.U) (Seq(
        ALUAsrc.rj -> MuxLookup(io.FwASrc, io.prev.bits.rj_data) (FwSrcSeq),
        ALUAsrc.pc -> io.prev.bits.pc,
    ))
    val ALUB = MuxLookup(io.prev.bits.aluBsrc, 0.U) (Seq(
        ALUBsrc.rk -> MuxLookup(io.FwBSrc, io.prev.bits.rk_data) (FwSrcSeq),
        ALUBsrc.rd -> MuxLookup(io.FwBSrc, io.prev.bits.rd_data) (FwSrcSeq),
        ALUBsrc.imm -> io.prev.bits.Imm,
        ALUBsrc.four -> 4.U,
    ))

    // store data forwarding
    io.next.bits.rd_data := MuxLookup(io.FwStore, io.prev.bits.rd_data) (FwSrcSeq)

    ALU.io.A := ALUA
    ALU.io.B := ALUB
    ALU.io.Op := io.prev.bits.aluOp

    val mult = Module(new mult_gen_0())
    mult.io.CLK := clock
    mult.io.A := ALUA
    mult.io.B := ALUB
    io.P := mult.io.P

    io.next.bits.ALUOut := ALU.io.Out
    io.next.bits.SLess := ALU.io.SLess
    io.next.bits.ULess := ALU.io.ULess
    io.next.bits.Zero := ALU.io.Zero
    io.next.bits.memOp := io.prev.bits.memOp
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

class alu extends Module {
    val io = IO(new Bundle{
        val A = Input(UInt(32.W))
        val B = Input(UInt(32.W))
        val Op = Input(ALUOp())
        val Out = Output(UInt(32.W))
        val SLess = Output(Bool())
        val ULess = Output(Bool())
        val Zero = Output(Bool())
    })

    val Sub = MuxLookup(io.Op, 0.B) (
        Seq(
            ALUOp.sub -> 1.B,
            ALUOp.slt -> 1.B,
            ALUOp.sltu -> 1.B,
        )
    )
    val FixA = io.A
    val FixB = io.B ^ Fill(32, Sub)
    val FixSum = FixA +& FixB +& Sub
    val OF = (FixA(31) === FixB(31)) && (FixSum(31) =/= FixA(31))
    val ZF = ~FixSum(31, 0).orR
    val SLess = (FixSum(31) ^ OF)
    val ULess = ~FixSum(32)

    val Shifter = Module(new bshifter())
    Shifter.io.A := io.A
    Shifter.io.B := io.B(4, 0)
    Shifter.io.Op := io.Op
    
    io.Out := MuxLookup(io.Op, 0.U) (Seq(
        ALUOp.add -> FixSum,
        ALUOp.sub -> FixSum,
        ALUOp.slt -> Cat(0.U(31.W), SLess),
        ALUOp.sltu -> Cat(0.U(31.W), ULess),
        ALUOp.and -> (io.A & io.B),
        ALUOp.or -> (io.A | io.B),
        ALUOp.xor -> (io.A ^ io.B),
        ALUOp.nor -> ~(io.A | io.B),
        ALUOp.sll -> Shifter.io.Out,
        ALUOp.srl -> Shifter.io.Out,
        ALUOp.sra -> Shifter.io.Out,
    ))

    io.SLess := SLess
    io.ULess := ULess
    io.Zero := ZF
}

class bshifter extends Module {
    val io = IO(new Bundle {
        val A = Input(UInt(32.W))
        val B = Input(UInt(5.W))
        val Op = Input(ALUOp())
        val Out = Output(UInt(32.W))
    })

    val Shifter = Wire(Vec(6, Vec(32, Bool())))
    Shifter(0) := io.A.asBools
    for(i <- 1 to 5) {
        for(j <- 0 to 31) {
            if(j < (1 << (i - 1))) {
                Shifter(i)(j) := Mux(~io.B(i - 1), Shifter(i - 1)(j), 
                MuxLookup(io.Op, Shifter(i - 1)(j)) (Seq(
                    ALUOp.sll -> 0.B,
                    ALUOp.srl -> Shifter(i - 1)(j + (1 << (i - 1))),
                    ALUOp.sra -> Shifter(i - 1)(j + (1 << (i - 1))),
                )))
            }
            else if (j >= 32 - (1 << (i - 1))) {
                Shifter(i)(j) := Mux(~io.B(i - 1), Shifter(i - 1)(j), 
                MuxLookup(io.Op, Shifter(i - 1)(j)) (Seq(
                    ALUOp.sll -> Shifter(i - 1)(j - (1 << (i - 1))),
                    ALUOp.srl -> 0.B,
                    ALUOp.sra -> io.A(31),
                )))
            }
            else {
                Shifter(i)(j) := Mux(~io.B(i - 1), Shifter(i - 1)(j), 
                MuxLookup(io.Op, Shifter(i - 1)(j)) (Seq(
                    ALUOp.sll -> Shifter(i - 1)(j - (1 << (i - 1))),
                    ALUOp.srl -> Shifter(i - 1)(j + (1 << (i - 1))),
                    ALUOp.sra -> Shifter(i - 1)(j + (1 << (i - 1))),
                )))
            }
        }
    }
    io.Out := Shifter(5).asUInt
}