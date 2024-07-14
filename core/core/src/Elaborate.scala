import circt.stage._
import chisel3._
import bundles.AddrRange

object Elaborate extends App {
    def top = new subtop()
    //def top = new abt(2)
    // def top = new xbar(3, Vector(
    //     new AddrRange("h80000000".U, "h80400000".U),
    //     new AddrRange("h80400000".U, "h80800000".U),
    //     new AddrRange("hbfd003f8".U, "hbfd003fd".U),
    // ))
    val generator = Seq(
        chisel3.stage.ChiselGeneratorAnnotation(() => top),
    )
    (new ChiselStage).execute(args, generator :+ CIRCTTargetAnnotation(CIRCTTarget.Verilog))
}