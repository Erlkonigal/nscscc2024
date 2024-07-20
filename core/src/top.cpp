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

int main(int argc, char *argv[]) {
    init(argc, argv);
    reset();
    for(int i = 0; i < 100000; i++) {
        singled_cycle();
    }
    clean();
    return 0;
}