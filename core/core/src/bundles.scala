import chisel3._
import chisel3.util._

package bundles{
    
class AddrRange(val start: UInt, val end: UInt) {
    def contains(addr: UInt): Bool = {
        addr >= start && addr < end
    }
}

class bus_in extends Bundle {
    val data_out = Output(UInt(32.W))
}

class bus_out extends Bundle {
    val data_wen = Output(UInt(1.W))
    val data_in = Output(UInt(32.W))
    val addr = Output(UInt(32.W))
    val be_n = Output(UInt(4.W))
    val ce_n = Output(UInt(1.W))
    val oe_n = Output(UInt(1.W))
    val we_n = Output(UInt(1.W))
}

class gpio extends Bundle {
    val touch_btn = Input(UInt(4.W))
    val dip_sw = Input(UInt(32.W))
    val leds = Output(UInt(16.W))
    val dpy0 = Output(UInt(8.W))
    val dpy1 = Output(UInt(8.W))
}

class sram extends Bundle {
    val data_wen = Output(UInt(1.W))
    val data_in = Output(UInt(32.W))
    val data_out = Input(UInt(32.W)) // inout

    val addr = Output(UInt(20.W))
    val be_n = Output(UInt(4.W))
    val ce_n = Output(UInt(1.W))
    val oe_n = Output(UInt(1.W))
    val we_n = Output(UInt(1.W))
}

class uart extends Bundle {
    val rx = Input(UInt(1.W))
    val tx = Output(UInt(1.W))
}

class flash extends Bundle {
    val a = Output(UInt(23.W))

    val d_wen = Output(UInt(1.W))
    val d_in = Output(UInt(16.W))
    val d_out = Input(UInt(16.W)) // inout

    val rp_n = Output(UInt(1.W))
    val vpen = Output(UInt(1.W))
    val ce_n = Output(UInt(1.W))
    val oe_n = Output(UInt(1.W))
    val we_n = Output(UInt(1.W))
    val byte_n = Output(UInt(1.W))
}

class vga extends Bundle {
    val red = Output(UInt(3.W))
    val green = Output(UInt(3.W))
    val blue = Output(UInt(2.W))
    val hsync = Output(UInt(1.W))
    val vsync = Output(UInt(1.W))
    val clk = Output(UInt(1.W))
    val de = Output(UInt(1.W))
}

class ifu_idu extends Bundle {
    val pc = Output(UInt(32.W))
    val inst = Output(UInt(32.W))
}

class idu_exu extends Bundle {
// Controller
    val aluOp = Output(ALUOp())
    val aluAsrc = Output(ALUAsrc())
    val aluBsrc = Output(ALUBsrc())
    val memOp = Output(MemOp())
    val branch = Output(Branch())
    val wbSel = Output(WBSel())
    val wbDst = Output(WBDst())
// ImmGen
    val Imm = Output(UInt(32.W))
// RegFile
    val rd = Output(UInt(5.W))
    val rd_data = Output(UInt(32.W))
    val rj_data = Output(UInt(32.W))
    val rk_data = Output(UInt(32.W))
// PC
    val pc = Output(UInt(32.W))
}

object ALUOp extends ChiselEnum {
    val add, sub,
        slt, sltu,
        and, or, 
        xor, nor,
        sll, srl, 
        sra, other = Value
}
object ALUAsrc extends ChiselEnum {
    val rj, pc, other = Value
}
object ALUBsrc extends ChiselEnum {
    val rk, rd, imm, four, other = Value
}
object ImmType extends ChiselEnum {
    val type2RI5U, type2RI8, 
        type2RI12, type2RI12U,
        type2RI14, type2RI16,
        type1RI20, type1RI21, 
        typeI26, other = Value
}
object MemOp extends ChiselEnum {
    val ldb, ldbu, ldh, ldhu, ldw, 
        stb, sth, stw, other = Value
}
object Branch extends ChiselEnum {
    val beq, bne,
        blt, bge,
        bltu, bgeu,
        b, bl, 
        jirl, other = Value
}
object WBSel extends ChiselEnum {
    val alu, mem, other = Value
}
object WBDst extends ChiselEnum {
    val rd, one, other = Value
}
object PCAsrc extends ChiselEnum {
    val pc, rj, other = Value
}
object PCBsrc extends ChiselEnum {
    val four, imm, other = Value
}

class exu_lsu extends Bundle {
// ALU
    val ALUOut = Output(UInt(32.W))
    val SLess = Output(UInt(1.W))
    val ULess = Output(UInt(1.W))
    val Zero = Output(UInt(1.W))
// Controller
    val memOp = Output(MemOp())
    val branch = Output(Branch())
    val wbSel = Output(WBSel())
    val wbDst = Output(WBDst())
// ImmGen
    val Imm = Output(UInt(32.W))
// RegFile
    val rd = Output(UInt(5.W))
    val rd_data = Output(UInt(32.W))
    val rj_data = Output(UInt(32.W))
// PC
    val pc = Output(UInt(32.W))
}

class lsu_wbu extends Bundle {
// Mem
    val MemOut = Output(UInt(32.W))
// ALU
    val ALUOut = Output(UInt(32.W))
    val SLess = Output(UInt(1.W))
    val ULess = Output(UInt(1.W))
    val Zero = Output(UInt(1.W))
// Controller
    val branch = Output(Branch())
    val wbSel = Output(WBSel())
    val wbDst = Output(WBDst())
// ImmGen
    val Imm = Output(UInt(32.W))
// RegFile
    val rd = Output(UInt(5.W))
    val rj_data = Output(UInt(32.W))
// PC
    val pc = Output(UInt(32.W))
}

}