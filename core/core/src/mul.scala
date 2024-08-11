import chisel3._
import chisel3.util._
import bundles._

class mult_gen_0 extends BlackBox {
    val io = IO(new Bundle {
        val A = Input(UInt(32.W))
        val B = Input(UInt(32.W))
        val P = Output(UInt(32.W))
    })
}

class wallace(n: Int) extends Module {
    val io = IO(new Bundle {
        val stall = Input(Bool())
        val A = Input(UInt(n.W))
        val B = Input(UInt(n.W))
        val S = Output(UInt((2 * n).W))
    })
    val odd = n % 2
// stage 1 : booth
    val output = Reg(Vec(n / 2 + odd, UInt((n * 2).W)))
    val output_next = Wire(Vec(n / 2 + odd, UInt((n * 2).W)))
    val booths = Seq.fill(n / 2)(Module(new booth2(n)))
    val A = io.A
    val A_ = -io.A
    val SignExtA = Cat(Fill(n, A(n - 1)), A)
    val SignExtA_ = Cat(Fill(n, A_(n - 1)), A_)
    for(i <- (n - 1) to (3 - odd) by -2) {
        booths(i / 2 - odd).io.SignExtA := SignExtA
        booths(i / 2 - odd).io.SignExtA_ := SignExtA_
        booths(i / 2 - odd).io.B := io.B(i, i - 2)
    }
    if(odd == 0) {
        booths(0).io.SignExtA := SignExtA
        booths(0).io.SignExtA_ := SignExtA_
        booths(0).io.B := Cat(io.B(1, 0), 0.B) // B1, B0, (B-1)
    }
    for(i <- 0 until n / 2) {
        output_next(i) := booths(i).io.S << (i * 2 + odd).U
    } // calculate the partial products
    if(odd == 1) {
        output_next(n / 2) := Mux(io.B(0), SignExtA_, 0.U)
    }
    output := Mux(io.stall, output, output_next)
// stage 2 : wallace tree
    val S = Reg(UInt((n * 2).W))
    val S_next = Wire(UInt((n * 2).W))
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
    S_next := cur(0) +& cur(1)
    S := Mux(io.stall, S, S_next)
// output
    io.S := S
}

class booth2(n: Int) extends Module {
    val io = IO(new Bundle {
        val SignExtA = Input(UInt((n * 2).W))
        val SignExtA_ = Input(UInt((n * 2).W))
        val B = Input(UInt(3.W)) // Bi+1, Bi, Bi-1
        val S = Output(UInt((n * 2).W))
    })
    io.S := MuxLookup(io.B, 0.U) (Seq(
        "b000".U -> 0.U,
        "b001".U -> io.SignExtA,
        "b010".U -> io.SignExtA,
        "b011".U -> (io.SignExtA << 1.U),
        "b100".U -> (io.SignExtA_ << 1.U),
        "b101".U -> io.SignExtA_,
        "b110".U -> io.SignExtA_,
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
