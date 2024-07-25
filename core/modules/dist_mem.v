module dist_mem(
    input clk,
    input [5:0] a,
    input [47:0] d,
    input we,
    input [5:0] dpra,
    output [47:0] spo,
    output [47:0] dpo
);

initial begin
    integer i;
    for (i = 0; i < 64; i = i + 1) begin
        mem[i] = 48'h0;
    end
end

reg [47:0] mem [0:63];

always @(posedge clk) begin
    if (we) begin
        mem[a] <= d;
    end
end

assign spo = mem[a];
assign dpo = mem[dpra];
endmodule
