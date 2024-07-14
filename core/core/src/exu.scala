import chisel3._
import chisel3.util._
import bundles._

class exu extends Module {
    val io = IO(new Bundle {
        val prev = Flipped(Decoupled(new idu_exu()))
        val next = Decoupled(new exu_lsu())
    })
    val ALU = Module(new alu())
    ALU.io.A := MuxLookup(io.prev.bits.aluAsrc, 0.U) (Seq(
        ALUAsrc.rj -> io.prev.bits.rj_data,
        ALUAsrc.pc -> io.prev.bits.pc,
    ))
    ALU.io.B := MuxLookup(io.prev.bits.aluBsrc, 0.U) (Seq(
        ALUBsrc.rk -> io.prev.bits.rk_data,
        ALUBsrc.rd -> io.prev.bits.rd_data,
        ALUBsrc.imm -> io.prev.bits.Imm,
        ALUBsrc.four -> 4.U,
    ))
    ALU.io.Op := io.prev.bits.aluOp

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
    io.next.bits.rd_data := io.prev.bits.rd_data
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