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

    val SMult = Module(new wallace(32))
    val UMult = Module(new wallace(33))
    SMult.io.A := io.A
    SMult.io.B := io.B
    UMult.io.A := 0.B ## io.A
    UMult.io.B := 0.B ## io.B
    
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
        ALUOp.mul -> SMult.io.S(31, 0),
        ALUOp.mulh -> SMult.io.S(63, 32),
        ALUOp.mulhu -> UMult.io.S(63, 32),
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

import chisel3.simulator.EphemeralSimulator._
import org.scalatest.flatspec.AnyFlatSpec

class WallaceSpec extends AnyFlatSpec {
    behavior of "Wallace"
    it should "work" in {
        simulate(new wallace(8)) { dut =>
            dut.io.A.poke(255)
            dut.io.B.poke(255)
            dut.clock.step(1)
            dut.io.S.expect(1)
        }
    }
}

object WallaceMain extends App {
    org.scalatest.nocolor.run(new WallaceSpec)
}

class wallace(n: Int) extends Module {
    val io = IO(new Bundle {
        val A = Input(UInt(n.W))
        val B = Input(UInt(n.W))
        val S = Output(UInt((2 * n).W))
    })
    val odd = n % 2
    val booths = Seq.fill(n / 2)(Module(new booth2(n)))
    
    for(i <- (n - 1) to (3 - odd) by -2) {
        booths(i / 2 - odd).io.A := io.A
        booths(i / 2 - odd).io.B := io.B(i, i - 2)
    }
    if(odd == 0) {
        booths(0).io.A := io.A
        booths(0).io.B := Cat(io.B(1, 0), 0.B) // B1, B0, (B-1)
    }

    val output = Wire(Vec(n / 2 + odd, UInt((n * 2).W)))
    for(i <- 0 until n / 2) {
        output(i) := booths(i).io.S << (i * 2 + odd).U
    } // calculate the partial products
    if(odd == 1) {
        output(n / 2) := Mux(io.B(0), -Cat(Fill(n, io.A(n - 1)), io.A), 0.U)
    }

    var length = output.length
    var cur = output
    while(length > 2) {
        val tmp = Wire(Vec(n / 2 + odd, UInt((n * 2).W)))
        for(i <- 0 until n / 2 + odd) tmp(i) := 0.U

        val csas = Seq.fill(length / 3)(Module(new csa(n * 2)))
        var i = 0
        var next_length = 0
        while(i < length / 3) {
            csas(i).io.A := cur(i * 3)
            csas(i).io.B := cur(i * 3 + 1)
            csas(i).io.Cin := cur(i * 3 + 2)
            tmp(next_length) := csas(i).io.S
            tmp(next_length + 1) := csas(i).io.Cout << 1.U
            i += 1
            next_length += 2
        }
        var cnt = i * 3
        while(cnt < length) {
            tmp(next_length) := cur(cnt)
            next_length += 1
            cnt += 1
        }
        length = next_length
        cur = tmp
    }
    io.S := cur(0) + cur(1)

}

class booth2(n: Int) extends Module {
    val io = IO(new Bundle {
        val A = Input(UInt(n.W))
        val B = Input(UInt(3.W)) // Bi+1, Bi, Bi-1
        val S = Output(UInt((n * 2).W))
    })
    val SignExtA = Cat(Fill(n, io.A(n - 1)), io.A)
    io.S := MuxLookup(io.B, 0.U) (Seq(
        "b000".U -> 0.U,
        "b001".U -> SignExtA,
        "b010".U -> SignExtA,
        "b011".U -> (SignExtA << 1.U),
        "b100".U -> -(SignExtA << 1.U),
        "b101".U -> -(SignExtA),
        "b110".U -> -(SignExtA),
        "b111".U -> 0.U
    ))
}

class csa(n: Int) extends Module {
    val io = IO(new Bundle {
        val A = Input(UInt(n.W))
        val B = Input(UInt(n.W))
        val Cin = Input(UInt(n.W))
        val S = Output(UInt(n.W))
        val Cout = Output(UInt(n.W))
    })
    val S = Wire(Vec(n, Bool()))
    val Cout = Wire(Vec(n, Bool()))
    for(i <- 0 until n) {
        S(i) := io.A(i) ^ io.B(i) ^ io.Cin(i)
        Cout(i) := (io.A(i) & io.B(i)) | (io.A(i) & io.Cin(i)) | (io.B(i) & io.Cin(i))
    }
    io.S := S.asUInt
    io.Cout := Cout.asUInt
}