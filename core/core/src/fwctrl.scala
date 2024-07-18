import chisel3._
import chisel3.util._
import bundles._

class fwctrl extends Module {
    val io = IO(new Bundle {
        val RA = Input(UInt(5.W))
        val RB = Input(UInt(5.W))
        val RDst = Input(UInt(5.W))
        val RStore = Input(UInt(5.W)) // lsu forwarding

        val wbSel = Input(WBSel())

        val stall = Output(UInt(4.W)) // 3:ifu_idu, 2:idu_exu, 1:exu_lsu, 0:lsu_wbu
        val flush = Output(UInt(4.W)) // 4bit
        val FwALUA = Output(ForwardSrc())
        val FwALUB = Output(ForwardSrc())
        val FwStore = Output(ForwardSrc())
    })
    val RdReg = Reg(Vec(3, UInt(5.W)))
    val SelReg = Reg(Vec(3, WBSel()))
    RdReg(0) := io.RDst
    SelReg(0) := io.wbSel

    for(i <- 0 until RdReg.length - 1) {
        RdReg(i + 1) := RdReg(i)
        SelReg(i + 1) := SelReg(i)
    }

    val calcMux = (src: UInt) => {
        Mux(src === 0.U, ForwardSrc.other, 
            MuxCase(ForwardSrc.other, Seq(
                (src === RdReg(0)) -> MuxCase(ForwardSrc.other, Seq(
                    (SelReg(0) === WBSel.alu) -> ForwardSrc.elALU,
                    (SelReg(0) === WBSel.mem) -> ForwardSrc.stall, // stall
                    (SelReg(0) === WBSel.mul) -> ForwardSrc.stall, // stall
                )),
                (src === RdReg(1)) -> MuxCase(ForwardSrc.other, Seq(
                    (SelReg(1) === WBSel.alu) -> ForwardSrc.lwALU,
                    (SelReg(1) === WBSel.mem) -> ForwardSrc.lwMem,
                    (SelReg(1) === WBSel.mul) -> ForwardSrc.mulP,
                )),
                (src === RdReg(2)) -> MuxCase(ForwardSrc.other, Seq(
                    (SelReg(2) === WBSel.alu) -> ForwardSrc.lastWB,
                    (SelReg(2) === WBSel.mem) -> ForwardSrc.lastWB,
                    (SelReg(2) === WBSel.mul) -> ForwardSrc.lastWB,
                )),
            ))
        )
    }
    
    val FwALUA = calcMux(io.RA)
    val FwALUB = calcMux(io.RB)
    val FwStore = calcMux(io.RStore)

    val ALUAStall = FwALUA === ForwardSrc.stall
    val ALUBStall = FwALUB === ForwardSrc.stall
    val StoreStall = FwStore === ForwardSrc.stall

    val Stall = ALUAStall || ALUBStall || StoreStall

    io.stall := Cat(Stall, Stall, 0.B, 0.B)
    io.flush := 0.B
    io.FwALUA := FwALUA
    io.FwALUB := FwALUB
    io.FwStore := FwStore
}