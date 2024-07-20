import chisel3._
import chisel3.util._
import bundles._

class exu extends Module {
    val io = IO(new Bundle {
        // pipe
        val prev = Flipped(Decoupled(new idu_exu()))
        val next = Decoupled(new exu_lsu())
        // pipe signal
        val stall = Input(UInt(1.W))
        val flush = Input(UInt(1.W))
        // multiplier
        val P = Output(UInt(32.W))
        // forward data
        val ELALU = Input(UInt(32.W)) // EXU/LSU ALUOut
        val LWALU = Input(UInt(32.W)) // LSU/WBU ALUOut
        val LWMEM = Input(UInt(32.W)) // LSU/WBU MemOut
        // branch control
        val nextPC = Output(UInt(32.W))
    })
    val mult = Module(new mult_gen_0())
    val ALU = Module(new alu())
    val brCtrl = Module(new branchContr())

    val FwSrcSeq = Seq(
        ForwardSrc.elALU -> io.ELALU,
        ForwardSrc.lwALU -> io.LWALU,
        ForwardSrc.lwMem -> io.LWMEM,
        ForwardSrc.mulP -> mult.io.P,
    )

    val ForwardRD = MuxLookup(io.prev.bits.FwEX_RD, io.prev.bits.rd_data) (FwSrcSeq)
    val ForwardRJ = MuxLookup(io.prev.bits.FwEX_RJ, io.prev.bits.rj_data) (FwSrcSeq)
    val ForwardRK = MuxLookup(io.prev.bits.FwEX_RK, io.prev.bits.rk_data) (FwSrcSeq)

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

    mult.io.CLK := clock
    mult.io.A := ALUA
    mult.io.B := ALUB
    io.P := mult.io.P

    brCtrl.io.pc := io.prev.bits.pc
    brCtrl.io.rj_data := ForwardRJ
    brCtrl.io.offset := io.prev.bits.Imm
    brCtrl.io.branchOp := io.prev.bits.branchOp
    brCtrl.io.SLess := ALU.io.SLess
    brCtrl.io.ULess := ALU.io.ULess
    brCtrl.io.Zero := ALU.io.Zero
    io.nextPC := brCtrl.io.nextPC

    io.next.bits.ALUOut := ALU.io.Out
    io.next.bits.memOp := io.prev.bits.memOp
    io.next.bits.wbSel := io.prev.bits.wbSel
    io.next.bits.wbDst := io.prev.bits.wbDst
    io.next.bits.Imm := io.prev.bits.Imm
    io.next.bits.rd := io.prev.bits.rd
    io.next.bits.rd_data := ForwardRD

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

class alu extends Module {
    val io = IO(new Bundle{
        val A = Input(UInt(32.W))
        val B = Input(UInt(32.W))
        val Op = Input(ALUOp())
        val Out = Output(UInt(32.W))
        val ULess = Output(UInt(1.W))
        val SLess = Output(UInt(1.W))
        val Zero = Output(UInt(1.W))
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
    io.ULess := ULess
    io.SLess := SLess
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