import chisel3._
import chisel3.util._
import bundles._

class wbu extends Module {
    val io = IO(new Bundle {
        val prev = Flipped(Decoupled(new lsu_wbu()))
        // writeback
        val wen = Output(UInt(1.W))
        val waddr = Output(UInt(5.W))
        val wdata = Output(UInt(32.W))
        // pipe signal
        val stall = Input(Bool())
        // multiplier
        val MulOut = Input(UInt(32.W))
        // forwarding
        val wb_Dst = Output(UInt(5.W))
        val wb_Sel = Output(WBSel())
        val wb_ALU = Output(UInt(32.W))
        val wb_Mem = Output(UInt(32.W))
        val wb_Mul = Output(UInt(32.W))
    })
    io.wen := io.prev.bits.wbSel =/= WBSel.other && io.prev.valid && ~io.stall
    io.waddr := MuxLookup(io.prev.bits.wbDst, 0.U) (Seq(
        WBDst.rd -> io.prev.bits.rd,
        WBDst.one -> 1.U,
    ))
    io.wdata := MuxLookup(io.prev.bits.wbSel, 0.U) (Seq(
        WBSel.alu -> io.prev.bits.ALUOut,
        WBSel.mem -> io.prev.bits.MemOut,
        WBSel.mul -> io.MulOut,
    ))

    io.wb_Dst := Mux(io.prev.valid, MuxLookup(io.prev.bits.wbDst, 0.U) (Seq(
        WBDst.rd -> io.prev.bits.rd,
        WBDst.one -> 1.U,
    )), 0.U)
    io.wb_Sel := io.prev.bits.wbSel
    io.wb_ALU := io.prev.bits.ALUOut
    io.wb_Mem := io.prev.bits.MemOut
    io.wb_Mul := io.MulOut

    io.prev.ready := 1.B
}