import chisel3._
import chisel3.util._
import bundles._

class abt(n: Int) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val DataIn = Flipped(Vec(n, Decoupled(new bus_out)))
            val BusIn = Flipped(Decoupled(new bus_in))
        }
        val out = new Bundle {
            val DataOut = Decoupled(new bus_out)
            val BusOut = Vec(n, Decoupled(new bus_in))
        }
        val chosen = Output(UInt(log2Ceil(n).W))
    })
    val arbiter = Module(new RRArbiter(new bus_out, n))
    arbiter.io.in <> io.in.DataIn
    io.out.DataOut <> arbiter.io.out
    io.chosen := arbiter.io.chosen

    for (i <- 0 until n) {
        when(io.chosen === i.U) {
            io.out.BusOut(i) <> io.in.BusIn
            io.in.BusIn.ready := io.out.BusOut(i).ready
        } .otherwise {
            io.out.BusOut(i).valid := 0.U
            io.out.BusOut(i).bits := DontCare
            io.in.BusIn.ready := 0.U
        }
    }
}