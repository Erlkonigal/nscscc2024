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

    val base_addr = new AddrRange("h80000000".U, "h80400000".U)
    val ext_addr = new AddrRange("h80400000".U, "h80800000".U)
    val uart_addr = new AddrRange("hbfd003f8".U, "hbfd003fd".U)

    val base_ram_ctrl = Module(new sram_ctrl())
    val ext_ram_ctrl = Module(new sram_ctrl())
    val uart_ctrl = Module(new uart_ctrl())

    //base_ram & ifu
    when(core.io.lsu_out.valid && 
        base_addr.contains(core.io.lsu_out.bits.addr)) {
        base_ram_ctrl.io.in.bits := core.io.lsu_out.bits
        base_ram_ctrl.io.in.valid := core.io.lsu_out.valid
        base_ram_ctrl.io.out.ready := 1.B

        core.io.ifu_in.bits := DontCare
        core.io.ifu_in.valid := 0.B
        core.io.ifu_out.ready := 0.B
    }
    .otherwise {
        base_ram_ctrl.io.in.bits := core.io.ifu_out.bits
        base_ram_ctrl.io.in.valid := core.io.ifu_out.valid
        base_ram_ctrl.io.out.ready := 1.B

        core.io.ifu_in.bits := base_ram_ctrl.io.out.bits
        core.io.ifu_in.valid := base_ram_ctrl.io.out.valid
        core.io.ifu_out.ready := 1.B
    }

    // ext_ram
    when(core.io.lsu_out.valid &&
        ext_addr.contains(core.io.lsu_out.bits.addr)) {
        ext_ram_ctrl.io.in.bits := core.io.lsu_out.bits
        ext_ram_ctrl.io.in.valid := core.io.lsu_out.valid
        ext_ram_ctrl.io.out.ready := 1.B
    }
    .otherwise {
        ext_ram_ctrl.io.in.bits := DontCare
        ext_ram_ctrl.io.in.valid := 0.B
        ext_ram_ctrl.io.out.ready := 0.B
    }

    // uart
    when(core.io.lsu_out.valid &&
        uart_addr.contains(core.io.lsu_out.bits.addr)) {
        uart_ctrl.io.in.bits := core.io.lsu_out.bits
        uart_ctrl.io.in.valid := core.io.lsu_out.valid
        uart_ctrl.io.out.ready := 1.B
    }
    .otherwise {
        uart_ctrl.io.in.bits := DontCare
        uart_ctrl.io.in.valid := 0.B
        uart_ctrl.io.out.ready := 0.B
    }

    // lsu
    when(core.io.lsu_out.valid &&
        base_addr.contains(core.io.lsu_out.bits.addr)) {
        core.io.lsu_in.bits := base_ram_ctrl.io.out.bits
        core.io.lsu_in.valid := base_ram_ctrl.io.out.valid
        core.io.lsu_out.ready := 1.B
    }
    .elsewhen(core.io.lsu_out.valid &&
        ext_addr.contains(core.io.lsu_out.bits.addr)) {
        core.io.lsu_in.bits := ext_ram_ctrl.io.out.bits
        core.io.lsu_in.valid := ext_ram_ctrl.io.out.valid
        core.io.lsu_out.ready := 1.B
    }
    .elsewhen(core.io.lsu_out.valid &&
        uart_addr.contains(core.io.lsu_out.bits.addr)) {
        core.io.lsu_in.bits := uart_ctrl.io.out.bits
        core.io.lsu_in.valid := uart_ctrl.io.out.valid
        core.io.lsu_out.ready := 1.B
    }
    .otherwise {
        core.io.lsu_in.bits := DontCare
        core.io.lsu_in.valid := 0.B
        core.io.lsu_out.ready := 0.B
    }
    
    base_ram_ctrl.io.sram <> io.base
    ext_ram_ctrl.io.sram <> io.ext
    uart_ctrl.io.uart <> io.uart

}