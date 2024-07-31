import chisel3._
import chisel3.util._
import bundles._

class mult_gen_0 extends BlackBox {
    val io = IO(new Bundle {
        val CLK = Input(Clock())
        val A = Input(UInt(32.W))
        val B = Input(UInt(32.W))
        val P = Output(UInt(32.W))
    })
}

class wallace(n: Int) extends Module {
    val io = IO(new Bundle {
        val A = Input(UInt(n.W))
        val B = Input(UInt(n.W))
        val S = Output(UInt((2 * n).W))
    })
    val odd = n % 2
// stage 1 : booth
    val output = Reg(Vec(n / 2 + odd, UInt((n * 2).W)))
    val booths = Seq.fill(n / 2)(Module(new booth2(n)))
    for(i <- (n - 1) to (3 - odd) by -2) {
        booths(i / 2 - odd).io.A := io.A
        booths(i / 2 - odd).io.B := io.B(i, i - 2)
    }
    if(odd == 0) {
        booths(0).io.A := io.A
        booths(0).io.B := Cat(io.B(1, 0), 0.B) // B1, B0, (B-1)
    }
    for(i <- 0 until n / 2) {
        output(i) := booths(i).io.S << (i * 2 + odd).U
    } // calculate the partial products
    if(odd == 1) {
        output(n / 2) := Mux(io.B(0), (~Cat(Fill(n, io.A(n - 1)), io.A) + 1.U), 0.U)
    }
// stage 2 : wallace tree
    val ma = Reg(UInt((n * 2).W))
    val mb = Reg(UInt((n * 2).W))
    var length = output.length
    var cur = output
    while(length > 2) {
        val tmp = Wire(Vec(length, UInt((n * 2).W)))
        for(i <- 0 until length) tmp(i) := 0.U

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
    ma := cur(0)
    mb := cur(1)
// stage 3 : final adder
    val S = Reg(UInt((n * 2).W))
    S := ma + mb
// output
    io.S := S
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
        "b100".U -> (~(SignExtA << 1.U) + 1.U),
        "b101".U -> (~SignExtA + 1.U),
        "b110".U -> (~SignExtA + 1.U),
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
