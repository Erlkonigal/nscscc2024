module inout_wrapper #(parameter WIDTH = 32)(
    input wen,
    input [WIDTH - 1:0] in,
    output [WIDTH - 1:0] out,
    
    inout [WIDTH - 1:0] data
);

assign data = wen ? in : {WIDTH{1'bz}};
assign out = data;

endmodule