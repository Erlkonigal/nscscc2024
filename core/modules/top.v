module top(
    input wire clk,
    input wire rst,
    input wire rx,
    output wire tx
);
wire base_ram_data_wen;
wire [31:0] base_ram_data_in;
wire [31:0] base_ram_data_out;
wire [31:0] base_ram_data;
wire [19:0] base_ram_addr;
wire [3:0] base_ram_be_n; 
wire base_ram_ce_n;
wire base_ram_oe_n;
wire base_ram_we_n;

wire ext_ram_data_wen;
wire [31:0] ext_ram_data_in;
wire [31:0] ext_ram_data_out;
wire [31:0] ext_ram_data;
wire [19:0] ext_ram_addr;
wire [3:0] ext_ram_be_n;
wire ext_ram_ce_n;
wire ext_ram_oe_n;
wire ext_ram_we_n;

inout_wrapper base_wrapper(
    .wen(base_ram_data_wen),
    .in(base_ram_data_in),
    .out(base_ram_data_out),
    .data(base_ram_data)
);

inout_wrapper ext_wrapper(
    .wen(ext_ram_data_wen),
    .in(ext_ram_data_in),
    .out(ext_ram_data_out),
    .data(ext_ram_data)
);

subtop subtop(
    .clock(clk),
    .reset(rst),
    .io_base_data_wen(base_ram_data_wen),
    .io_base_data_in(base_ram_data_in),
    .io_base_data_out(base_ram_data_out),
    .io_base_addr(base_ram_addr),
    .io_base_be_n(base_ram_be_n),
    .io_base_ce_n(base_ram_ce_n),
    .io_base_oe_n(base_ram_oe_n),
    .io_base_we_n(base_ram_we_n),
    .io_ext_data_wen(ext_ram_data_wen),
    .io_ext_data_in(ext_ram_data_in),
    .io_ext_data_out(ext_ram_data_out),
    .io_ext_addr(ext_ram_addr),
    .io_ext_be_n(ext_ram_be_n),
    .io_ext_ce_n(ext_ram_ce_n),
    .io_ext_oe_n(ext_ram_oe_n),
    .io_ext_we_n(ext_ram_we_n),
    .io_uart_rx(rx),
    .io_uart_tx(tx)
);

sram base(
    .clk(clk),
    .rst(rst),
    .sel(0),
    .data(base_ram_data),
    .addr(base_ram_addr),
    .be_n(base_ram_be_n),
    .ce_n(base_ram_ce_n),
    .oe_n(base_ram_oe_n),
    .we_n(base_ram_we_n)
);

sram ext(
    .clk(clk),
    .rst(rst),
    .sel(1),
    .data(ext_ram_data),
    .addr(ext_ram_addr),
    .be_n(ext_ram_be_n),
    .ce_n(ext_ram_ce_n),
    .oe_n(ext_ram_oe_n),
    .we_n(ext_ram_we_n)
);

endmodule