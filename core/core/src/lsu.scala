import chisel3._
import chisel3.util._
import bundles._

class lsu extends Module {
    val io = IO(new Bundle {
        // mem
        val ext_in = Flipped(Decoupled(new bus_in()))
        val ext_out = Decoupled(new bus_out())
        // pipe
        val prev = Flipped(Decoupled(new exu_lsu()))
        val next = Decoupled(new lsu_wbu())
        // pipe signal
        val stall = Output(Bool())
        val flush = Input(Bool())
        // forwarding
        val l1_Dst = Output(UInt(5.W))
        val l1_Sel = Output(WBSel())
        val l1_ALU = Output(UInt(32.W))
        val l1_Mem = Output(UInt(32.W))

        val l2_Dst = Output(UInt(5.W))
        val l2_Sel = Output(WBSel())
        val l2_ALU = Output(UInt(32.W))
        val l2_Mem = Output(UInt(32.W))
    })
// mem signals
    val Address = io.prev.bits.ALUOut
    val Op = io.prev.bits.memOp
    val WriteData = io.prev.bits.rd_data
    val Writevalid = io.prev.valid && ~io.flush && MuxLookup(Op, 0.B) (Seq(
        MemOp.stb -> 1.B,
        MemOp.sth -> 1.B,
        MemOp.stw -> 1.B,
    ))
    val Readvalid = io.prev.valid && ~io.flush && MuxLookup(Op, 0.B) (Seq(
        MemOp.ldb -> 1.B,
        MemOp.ldbu -> 1.B,
        MemOp.ldh -> 1.B,
        MemOp.ldhu -> 1.B,
        MemOp.ldw -> 1.B,
    ))
    val Memvalid = (Writevalid || Readvalid)
    val uMemvalid = Op === MemOp.other && io.prev.valid
// mask
    val ByteMask = MuxLookup(Address(1, 0), 0.U) (Seq(
        0.U -> "b1110".U,
        1.U -> "b1101".U,
        2.U -> "b1011".U,
        3.U -> "b0111".U
    ))
    val HalfMask = MuxLookup(Address(1), 0.U) (Seq(
        0.B -> "b1100".U,
        1.B -> "b0011".U
    ))
    val WordMask = "b0000".U
    val GenMask = MuxLookup(Op, 0.U) (Seq(
        MemOp.stb -> ByteMask,
        MemOp.sth -> HalfMask,
        MemOp.stw -> WordMask,
    ))
// write
    val ByteData = MuxLookup(Address(1, 0), 0.U) (Seq(
        0.U -> Cat(Fill(24, 0.B), WriteData(7, 0)),
        1.U -> Cat(Fill(16, 0.B), WriteData(7, 0), Fill(8, 0.B)),
        2.U -> Cat(Fill(8, 0.B), WriteData(7, 0), Fill(16, 0.B)),
        3.U -> Cat(WriteData(7, 0), Fill(24, 0.B))
    ))
    val HalfData = MuxLookup(Address(1), 0.U) (Seq(
        0.B -> Cat(Fill(16, 0.B), WriteData(15, 0)),
        1.B -> Cat(WriteData(15, 0), Fill(16, 0.B))
    ))
    val WordData = WriteData
    val GenData = MuxLookup(Op, 0.U) (Seq(
        MemOp.stb -> ByteData,
        MemOp.sth -> HalfData,
        MemOp.stw -> WordData,
    ))
// write queue
    def qsize = 4 // queue size >= 2
    val wqbundle = new Bundle {
        val valid = Bool()
        val addr = UInt(32.W)
        val data = UInt(32.W)
        val be_n = UInt(4.W)
        val op = UInt(2.W) // 0.stb, 1.sth, 2.stw
    }
    val wq = RegInit(0.U.asTypeOf(Vec(qsize, wqbundle)))
    val wq_state = RegInit(1.U(2.W))
    val wq_ptr = RegInit(0.U(log2Ceil(qsize).W))
    val wq_empty = ~wq_ptr.orR && ~wq(0).valid
    val wq_full = ~(wq_ptr ^ (qsize - 1).U).orR && wq(wq_ptr).valid

    val wq_stall = Wire(Bool())
    wq_stall := 0.B

    val newdata = Wire(wqbundle)
    newdata.valid := 1.B
    newdata.addr := Address
    newdata.data := GenData
    newdata.be_n := GenMask
    newdata.op := MuxLookup(Op, 0.U) (Seq(
        MemOp.stb -> 0.U,
        MemOp.sth -> 1.U,
        MemOp.stw -> 2.U
    ))

    val hit = Wire(Vec(qsize, Bool()))
    val hit_index = Wire(UInt(log2Ceil(qsize).W))
    for(i <- 0 until qsize) {
        hit(i) := wq(i).valid && ~(wq(i).addr ^ Address).orR && MuxLookup(Op, 0.B) (Seq(
            MemOp.ldb -> ~(wq(i).op ^ 0.U).orR,
            MemOp.ldbu -> ~(wq(i).op ^ 0.U).orR,
            MemOp.ldh -> ~(wq(i).op ^ 1.U).orR,
            MemOp.ldhu -> ~(wq(i).op ^ 1.U).orR,
            MemOp.ldw -> ~(wq(i).op ^ 2.U).orR,
        ))
    }
    hit_index := ~PriorityEncoder(hit.reverse) // leftmost(newer) has higher priority

    switch(wq_state) {
        is(1.U) {
            when(Writevalid) {
                wq_state := 2.U
                when(~wq_full) {
                    wq(wq_ptr) := newdata
                    wq_ptr := Mux(~(wq_ptr ^ (qsize - 1).U).orR, wq_ptr, wq_ptr + 1.U)
                }
                .otherwise {
                    wq_stall := 1.B
                }
            }
            .elsewhen(Readvalid) {
                wq_state := 1.U
                wq_stall := Mux(hit(hit_index), 0.B, 1.B)
            }
            .otherwise {
                when(~wq_empty) {
                    wq_state := 2.U
                }
                .otherwise {
                    wq_state := 1.U
                }
            }
        }
        is(2.U) {
            when(Writevalid) {
                wq_state := 3.U
                when(~wq_full) {
                    wq(wq_ptr) := newdata
                    wq_ptr := Mux(~(wq_ptr ^ (qsize - 1).U).orR, wq_ptr, wq_ptr + 1.U)
                }
                .otherwise {
                    wq_stall := 1.B
                }
            }
            .elsewhen(Readvalid) {
                wq_state := 1.U
                wq_stall := Mux(hit(hit_index), 0.B, 1.B)
            }
            .otherwise {
                wq_state := 3.U
            }
        }
        is(3.U) {
            when(Writevalid) {
                wq_state := 2.U
                when(~wq_full) {
                    for(i <- 0 until qsize - 1) {
                        wq(i) := Mux(~(wq_ptr ^ (i + 1).U).orR, newdata, wq(i + 1))
                    }
                    wq((qsize - 1).U) := 0.U.asTypeOf(wqbundle)
                }
                .otherwise {
                    for(i <- 0 until qsize - 1) {
                        wq(i) := wq(i + 1)
                    }
                    wq((qsize - 1).U) := newdata
                }
            }
            .elsewhen(Readvalid) {
                wq_state := 1.U
                wq_stall := Mux(hit(hit_index), 0.B, 1.B)
            }
            .otherwise {
                when((wq_ptr ^ 1.U).orR) {
                    wq_state := 2.U
                }
                .otherwise {
                    wq_state := 1.U
                }
                for(i <- 0 until qsize - 1) {
                    wq(i) := wq(i + 1)
                }
                wq((qsize - 1).U) := 0.U.asTypeOf(wqbundle)
                wq_ptr := wq_ptr - 1.U
            }
        }
    }
// state
    val state = RegInit(0.B)
    state := Mux(io.flush, 0.B ,MuxLookup(state, 0.B) (Seq(
        0.B -> Mux(wq_stall, 1.B, state),
        1.B -> 0.B
    )))
// read
    val ReadData = Mux(state, io.ext_in.bits.data_out, wq(hit_index).data)
    val FetchByte = MuxLookup(Address(1, 0), 0.U) (Seq(
        0.U -> ReadData(7, 0),
        1.U -> ReadData(15, 8),
        2.U -> ReadData(23, 16),
        3.U -> ReadData(31, 24)
    ))
    val FetchHalf = MuxLookup(Address(1), 0.U) (Seq(
        0.B -> ReadData(15, 0),
        1.B -> ReadData(31, 16)
    ))
    val FetchWord = ReadData
    val FixLoad = MuxLookup(Op, 0.U) (Seq(
        MemOp.ldb  -> Cat(Fill(24, FetchByte(7)), FetchByte(7, 0)),
        MemOp.ldbu -> Cat(Fill(24, 0.B), FetchByte(7, 0)),
        MemOp.ldh  -> Cat(Fill(16, FetchHalf(15)), FetchHalf(15, 0)),
        MemOp.ldhu -> Cat(Fill(16, 0.B), FetchHalf(15, 0)),
        MemOp.ldw  -> FetchWord,
    ))

// mem pipline
    val regs = Reg(new Bundle {
        val bits = new lsu_wbu()
        val valid = Bool()
    })
    when(io.flush) {
        regs.valid := 0.B
    }
    .elsewhen(io.prev.valid && ~wq_stall && ~state) {
        regs.bits.MemOut := FixLoad
        regs.bits.ALUOut := io.prev.bits.ALUOut
        regs.bits.rd := io.prev.bits.rd
        regs.bits.wbSel := io.prev.bits.wbSel
        regs.bits.wbDst := io.prev.bits.wbDst
        regs.bits.rd := io.prev.bits.rd
        regs.valid := 1.B
    }
    .otherwise {
        regs.valid := 0.B
    }
// io
    when(Readvalid) {
        io.ext_out.bits.addr := Address
        io.ext_out.bits.ce_n := 0.B
        io.ext_out.bits.oe_n := 0.B
        io.ext_out.bits.we_n := 1.B
        io.ext_out.bits.data_wen := 0.B
        io.ext_out.bits.data_in := 0.U
        io.ext_out.bits.be_n := 0.U
    }
    .otherwise {
        io.ext_out.bits.addr := wq(0).addr
        io.ext_out.bits.ce_n := 0.B
        io.ext_out.bits.oe_n := 1.B
        io.ext_out.bits.we_n := 0.B
        io.ext_out.bits.data_wen := 1.B
        io.ext_out.bits.data_in := wq(0).data
        io.ext_out.bits.be_n := wq(0).be_n
    }

    io.stall := wq_stall && ~state

    io.ext_out.valid := wq_state(1) || Readvalid
    io.ext_in.ready := 1.B

    val next = Wire(new lsu_wbu())  
    when(state) {
        next.MemOut := FixLoad
        next.ALUOut := io.prev.bits.ALUOut
        next.wbSel := io.prev.bits.wbSel
        next.wbDst := io.prev.bits.wbDst
        next.rd := io.prev.bits.rd
    }
    .otherwise {
        next := regs.bits
    }
    io.next.bits := next

    when(io.prev.valid && ~state && ~io.flush) {
        io.l1_Dst := MuxLookup(io.prev.bits.wbDst, 0.U) (Seq(
            WBDst.rd -> io.prev.bits.rd,
            WBDst.one -> 1.U,
        ))
        io.l1_Sel := io.prev.bits.wbSel
    }
    .otherwise {
        io.l1_Dst := 0.U
        io.l1_Sel := WBSel.other
    }
    when(state || regs.valid) {
        io.l2_Dst := MuxLookup(next.wbDst, 0.U) (Seq(
            WBDst.rd -> next.rd,
            WBDst.one -> 1.U,
        ))
        io.l2_Sel := next.wbSel
    }
    .otherwise {
        io.l2_Dst := 0.U
        io.l2_Sel := WBSel.other
    }
    io.l1_ALU := io.prev.bits.ALUOut
    io.l1_Mem := FixLoad
    io.l2_ALU := next.ALUOut
    io.l2_Mem := next.MemOut

    io.next.valid := state || regs.valid
    io.prev.ready := io.next.ready
}