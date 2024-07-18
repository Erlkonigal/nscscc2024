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
    val xbar_base = Module(new xbar(1, Vector(
        new AddrRange("h80000000".U, "h80400000".U),
    )))
    val xbar_ext = Module(new xbar(2, Vector(
        new AddrRange("h80400000".U, "h80800000".U),
        new AddrRange("hbfd003f8".U, "hbfd003fd".U),
    )))

    val base_ram_ctrl = Module(new sram_ctrl())
    val ext_ram_ctrl = Module(new sram_ctrl())
    val uart_ctrl = Module(new uart_ctrl())

    xbar_base.io.in.DataIn <> core.io.base_out
    base_ram_ctrl.io.in <> xbar_base.io.out.DataOut(0)

    xbar_ext.io.in.DataIn <> core.io.ext_out
    ext_ram_ctrl.io.in <> xbar_ext.io.out.DataOut(0)
    uart_ctrl.io.in <> xbar_ext.io.out.DataOut(1)

    xbar_base.io.in.BusIn(0) <> base_ram_ctrl.io.out
    xbar_ext.io.in.BusIn(0) <> ext_ram_ctrl.io.out
    xbar_ext.io.in.BusIn(1) <> uart_ctrl.io.out
    core.io.base_in <> xbar_base.io.out.BusOut
    core.io.ext_in <> xbar_ext.io.out.BusOut
    
    base_ram_ctrl.io.sram <> io.base
    ext_ram_ctrl.io.sram <> io.ext
    uart_ctrl.io.uart <> io.uart
}