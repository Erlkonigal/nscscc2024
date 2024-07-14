import chisel3._
import chisel3.util._
import bundles._

class xbar(n: Int, addr: Vector[AddrRange]) extends Module {
    val io = IO(new Bundle {
        val in = new Bundle {
            val DataIn = Flipped(Decoupled(new bus_out))
            val BusIn = Flipped(Vec(n, Decoupled(new bus_in)))
        }
        val out = new Bundle {
            val DataOut = Vec(n, Decoupled(new bus_out))
            val BusOut = Decoupled(new bus_in)
        }
    })
    for(i <- 0 until n) {
        io.out.DataOut(i).valid := 0.U
        io.out.DataOut(i).bits := DontCare
        io.in.DataIn.ready := 0.U

        io.out.BusOut.valid := 0.U
        io.out.BusOut.bits := DontCare
        io.in.BusIn(i).ready := 0.U
    }
    for(i <- 0 until n) {
        when(addr(i).contains(io.in.DataIn.bits.addr)) {
            io.out.DataOut(i) <> io.in.DataIn
            io.out.DataOut(i).bits.addr := io.in.DataIn.bits.addr - addr(i).start // offset
            io.out.BusOut <> io.in.BusIn(i)
        }
    }
}