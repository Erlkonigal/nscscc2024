#include "verilated.h"
#include "verilated_vcd_c.h"
#include "svdpi.h"

#include "../include/common.h"
#include "../include/sram.h"

#include "Vtop.h"

Vtop *top;
VerilatedContext *contextp;

#ifdef CONFIG_WAVE
VerilatedVcdC *tfp;
#endif

char *img_bin = NULL;
uint64_t img_size = 0;

void parse_args(int argc, char *argv[]) {
    if(argc < 2) {
        Log("Usage: %s <img.bin>", argv[0]);
        exit(1);
    }
    img_bin = argv[1];
}

void read_img() {
    if(img_bin == NULL) {
        Log("No image file specified");
        exit(1);
    }
    FILE *img = fopen(img_bin, "rb");
    if(img == NULL) {
        Log("Failed to open image file");
        exit(1);
    }
    fread(sram, 1, SRAM_SIZE << 1, img);
    fseek(img, 0, SEEK_END);
    img_size = ftell(img);
    Log("Image size: %ld", img_size);
    fclose(img);
}

void init(int argc, char *argv[]) {
    sram_init();
    parse_args(argc, argv);
    read_img();

	contextp = new VerilatedContext;
	contextp->commandArgs(argc, argv);
	top = new Vtop{contextp};

#ifdef CONFIG_WAVE
	contextp->traceEverOn(true);
	tfp = new VerilatedVcdC;
	top->trace(tfp, 0);
	tfp->open("build/top.vcd");
#endif
}

void singled_cycle() {
	top->clk = !top->clk;
	top->eval();

#ifdef CONFIG_WAVE
	tfp->dump(contextp->time());
	contextp->timeInc(1);
#endif

	top->clk = !top->clk;
	top->eval();

#ifdef CONFIG_WAVE
	tfp->dump(contextp->time());
	contextp->timeInc(1);
#endif
}

void clean() {
#ifdef CONFIG_WAVE
	tfp->close();
	delete tfp;
#endif
	top->final();
	delete top;
	delete contextp;
}

void reset() {
    top->rx = 1;
    top->rst = 1;
    for(int i = 0; i < 10; i++) {
        singled_cycle();
    }
    top->rst = 0;
}

void put_char(char c) {
    top->rx = 0;
    singled_cycle();
    for(int i = 0; i < 8; i++) {
        top->rx = (c >> i) & 1;
        singled_cycle();
    }
    top->rx = 1;
    singled_cycle();
}

void put_word(int w) {
    put_char(w & 0xff);
    put_char((w >> 8) & 0xff);
    put_char((w >> 16) & 0xff);
    put_char((w >> 24) & 0xff);
    for(int i = 0; i < 20; i++) {
        singled_cycle();
    }
}

void Op_D(int addr, int num) {
    put_char('D');
    put_word(addr);
    put_word(num);
}

void Op_A(int addr, const int *data, int num) {
    put_char('A');
    put_word(addr);
    put_word(num);
    for(int i = 0; i < num; i++) {
        put_word(data[i]);
    }
}

void Op_G(int addr) {
    put_char('G');
    put_word(addr);
}

void kernel_test() {
    for(int i = 0; i < 2000; i++) {
        singled_cycle();
    }
    
    Op_G(0x80003000);

    for(int i = 0; i < 10000; i++) {
        singled_cycle();
    }
}

int main(int argc, char *argv[]) {
    init(argc, argv);
    reset();
    // for(int i = 0; i < 400000; i++) {
    //     singled_cycle();
    // }
    kernel_test();
    clean();
    return 0;
}