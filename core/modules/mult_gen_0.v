module mult_gen_0 (
    input CLK,
    input [31:0] A,
    input [31:0] B,
    output [31:0] P
);

reg [31:0] buffer [0:4];

wire [63:0] mult_out = (A * B);

always @(posedge CLK) begin
    buffer[0] <= mult_out[31:0];
    buffer[1] <= buffer[0];
    buffer[2] <= buffer[1];
    buffer[3] <= buffer[2];
    buffer[4] <= buffer[3];
end
assign P = buffer[4];

endmodule
