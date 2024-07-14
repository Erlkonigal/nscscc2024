import "DPI-C" function void sram_read(input byte sel, input int addr, output int data);
import "DPI-C" function void sram_write(input byte sel, input int addr, input int data, input byte mask);

module sram(
    input wire [7:0] sel,
    inout wire [31:0] data,
    input wire [19:0] addr,
    input wire [3:0] be_n,
    input wire ce_n,
    input wire oe_n,
    input wire we_n
);
/* verilator lint_off UNOPTFLAT */
reg [31:0] buffer;
assign data = (ce_n == 0 && oe_n == 0 && we_n == 1) ? buffer : 32'bz;
/* verilator lint_on UNOPTFLAT */
always @(*) begin
    if (ce_n == 0) begin
        if(oe_n == 0 && we_n == 1) begin
            sram_read(sel, {12'd0, addr}, buffer);
        end
        else if(oe_n == 1 && we_n == 0) begin
            sram_write(sel, {12'd0, addr}, data, {4'd0, ~be_n});
            buffer = 0;
        end
        else begin
            buffer = 0;
        end
    end
    else begin
        buffer = 0;
    end
end

endmodule