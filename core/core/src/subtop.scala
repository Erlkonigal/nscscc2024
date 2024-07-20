import chisel3._
import chisel3.util._
import bundles._

class subtop extends Module {
    val io = IO(new Bundle {
        val base = new sram()
        val ext = new sram()
        val uart = new uart()
    })
val core = Module(new core())
    val xbar = Module(new xbar(3, Vector(
        new AddrRange("h80000000".U, "h80400000".U),
        new AddrRange("h80400000".U, "h80800000".U),
        new AddrRange("hbfd003f8".U, "hbfd003fd".U),
    )))

    val base_ram_ctrl = Module(new sram_ctrl())
    val ext_ram_ctrl = Module(new sram_ctrl())
    val uart_ctrl = Module(new uart_ctrl())

    xbar.io.in.DataIn <> core.io.bus_out
    base_ram_ctrl.io.in <> xbar.io.out.DataOut(0)
    ext_ram_ctrl.io.in <> xbar.io.out.DataOut(1)
    uart_ctrl.io.in <> xbar.io.out.DataOut(2)

    xbar.io.in.BusIn(0) <> base_ram_ctrl.io.out
    xbar.io.in.BusIn(1) <> ext_ram_ctrl.io.out
    xbar.io.in.BusIn(2) <> uart_ctrl.io.out
    core.io.bus_in <> xbar.io.out.BusOut
    
    base_ram_ctrl.io.sram <> io.base
    ext_ram_ctrl.io.sram <> io.ext
    uart_ctrl.io.uart <> io.uart

}