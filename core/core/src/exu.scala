import chisel3._
import chisel3.util._
import bundles._

class exu extends Module {
    val io = IO(new Bundle {
        val prev = Flipped(Decoupled(new idu_exu()))
        val next = Decoupled(new exu_lsu())
    })
    val ALU = Module(new alu())
    val ALUA = MuxLookup(io.prev.bits.aluAsrc, 0.U) (Seq(
        ALUAsrc.rj -> io.prev.bits.rj_data,
        ALUAsrc.pc -> io.prev.bits.pc,
    ))
    val ALUB = MuxLookup(io.prev.bits.aluBsrc, 0.U) (Seq(
        ALUBsrc.rk -> io.prev.bits.rk_data,
        ALUBsrc.rd -> io.prev.bits.rd_data,
        ALUBsrc.imm -> io.prev.bits.Imm,
        ALUBsrc.four -> 4.U,
    ))
    ALU.io.A := ALUA
    ALU.io.B := ALUB
    ALU.io.Op := io.prev.bits.aluOp

    val pstage = 1
    val buf = Reg(Vec(pstage, new exu_inner()))
    buf(0).ALUOut := ALU.io.Out
    buf(0).SLess := ALU.io.SLess
    buf(0).ULess := ALU.io.ULess
    buf(0).Zero := ALU.io.Zero
    buf(0).aluOp := io.prev.bits.aluOp
    buf(0).memOp := io.prev.bits.memOp
    buf(0).branch := io.prev.bits.branch
    buf(0).wbSel := io.prev.bits.wbSel
    buf(0).wbDst := io.prev.bits.wbDst
    buf(0).Imm := io.prev.bits.Imm
    buf(0).rd := io.prev.bits.rd
    buf(0).rd_data := io.prev.bits.rd_data
    buf(0).rj_data := io.prev.bits.rj_data
    buf(0).pc := io.prev.bits.pc
    buf(0).valid := io.prev.valid

    for(i <- 1 to pstage - 1) {
        buf(i) := buf(i - 1)
    }

    val mult = Module(new mult_gen_0())
    mult.io.CLK := clock.asBool
    mult.io.A := ALUA
    mult.io.B := ALUB

    io.next.bits.ALUOut := MuxLookup(buf(pstage - 1).aluOp, buf(pstage - 1).ALUOut) (Seq(
        ALUOp.mul -> mult.io.P,
    ))
    io.next.bits.SLess := buf(pstage - 1).SLess
    io.next.bits.ULess := buf(pstage - 1).ULess
    io.next.bits.Zero := buf(pstage - 1).Zero
    io.next.bits.memOp := buf(pstage - 1).memOp
    io.next.bits.branch := buf(pstage - 1).branch
    io.next.bits.wbSel := buf(pstage - 1).wbSel
    io.next.bits.wbDst := buf(pstage - 1).wbDst
    io.next.bits.Imm := buf(pstage - 1).Imm
    io.next.bits.rd := buf(pstage - 1).rd
    io.next.bits.rd_data := buf(pstage - 1).rd_data
    io.next.bits.rj_data := buf(pstage - 1).rj_data
    io.next.bits.pc := buf(pstage - 1).pc

    io.next.valid := buf(pstage - 1).valid
    io.prev.ready := 1.B
}

class mult_gen_0 extends BlackBox {
    val io = IO(new Bundle {
        val CLK = Input(Bool())
        val A = Input(UInt(32.W))
        val B = Input(UInt(32.W))
        val P = Output(UInt(32.W))
    })
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