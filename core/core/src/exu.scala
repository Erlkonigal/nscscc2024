import chisel3._
import chisel3.util._
import bundles._

class exu extends Module {
    val io = IO(new Bundle {
        // pipe
        val prev = Flipped(Decoupled(new idu_exu()))
        val next = Decoupled(new exu_lsu())
        val bru = Decoupled(new exu_bru())
        // pipe signal
        val stall = Input(Bool())
        // val flush = Input(Bool())
        // multiplier
        val MulOut = Output(UInt(32.W))
        // forwarding signals
        val ex_Dst = Output(UInt(5.W))
        val ex_Sel = Output(WBSel())
        // forwarding data
        val ls_ALU = Input(UInt(32.W))
        val wb_ALU = Input(UInt(32.W))
        val wb_Mem = Input(UInt(32.W))
    })
    val mult = Module(new wallace(32))
    val ALU = Module(new alu())

    val ForwardSeq = Seq(
        ForwardSrc.lsALU -> io.ls_ALU,
        ForwardSrc.wbALU -> io.wb_ALU,
        ForwardSrc.wbMem -> io.wb_Mem,
        ForwardSrc.wbMul -> mult.io.S
    )
    val ForwardRJ = MuxLookup(io.prev.bits.FwEX_RJ, io.prev.bits.rj_data) (ForwardSeq)
    val ForwardRK = MuxLookup(io.prev.bits.FwEX_RK, io.prev.bits.rk_data) (ForwardSeq)
    val ForwardRD = MuxLookup(io.prev.bits.FwEX_RD, io.prev.bits.rd_data) (ForwardSeq)

    val ALUA = MuxLookup(io.prev.bits.aluAsrc, 0.U) (Seq(
        ALUAsrc.rj -> ForwardRJ,
        ALUAsrc.pc -> io.prev.bits.pc,
    ))
    val ALUB = MuxLookup(io.prev.bits.aluBsrc, 0.U) (Seq(
        ALUBsrc.rk -> ForwardRK,
        ALUBsrc.rd -> ForwardRD,
        ALUBsrc.imm -> io.prev.bits.Imm,
        ALUBsrc.four -> 4.U,
    ))
    ALU.io.A := ALUA
    ALU.io.B := ALUB
    ALU.io.Op := io.prev.bits.aluOp

    mult.io.A := ALUA
    mult.io.B := ALUB
    mult.io.stall := io.stall
    io.MulOut := mult.io.S

    io.ex_Dst := Mux(io.prev.valid, MuxLookup(io.prev.bits.wbDst, 0.U) (Seq(
        WBDst.rd -> io.prev.bits.rd,
        WBDst.one -> 1.U
    )), 0.U)
    io.ex_Sel := io.prev.bits.wbSel

    io.next.bits.ALUOut := ALU.io.Out
    io.next.bits.memOp := io.prev.bits.memOp
    io.next.bits.wbSel := io.prev.bits.wbSel
    io.next.bits.wbDst := io.prev.bits.wbDst
    io.next.bits.Imm := io.prev.bits.Imm
    io.next.bits.rd := io.prev.bits.rd
    io.next.bits.rd_data := ForwardRD

    io.bru.bits.Zero := Compare.equals(ForwardRJ, ForwardRD)
    io.bru.bits.SLess := ForwardRJ.asSInt < ForwardRD.asSInt
    io.bru.bits.ULess := ForwardRJ < ForwardRD
    io.bru.bits.branchOp := io.prev.bits.branchOp
    io.bru.bits.pcadd4 := io.prev.bits.pc + 4.U
    io.bru.bits.pcoff := io.prev.bits.pc + io.prev.bits.Imm
    io.bru.bits.jirlpc := ForwardRJ + io.prev.bits.Imm
    io.bru.bits.pc := io.prev.bits.pc
    io.bru.bits.npc := io.prev.bits.npc

    io.bru.valid := io.prev.valid && io.next.ready // branch after handshake
    io.next.valid := io.prev.valid
    io.prev.ready := io.next.ready
}

class alu extends Module {
    val io = IO(new Bundle{
        val A = Input(UInt(32.W))
        val B = Input(UInt(32.W))
        val Op = Input(ALUOp())
        val Out = Output(UInt(32.W))
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
    val OF = ~(FixA(31) ^ FixB(31)) && (FixSum(31) ^ FixA(31))
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