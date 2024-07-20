#include "../include/sram.h"
#include <cstdint>

uint8_t sram[SRAM_SIZE << 1];

void sram_init() {
    memset(sram, 0, SRAM_SIZE << 1);
}

extern "C" void sram_read(char sel, int addr, int *data) {
    uint32_t offset = addr << 2;
    uint8_t *src;
    if(sel == 0) {
        src = sram;
    }
    else {
        src = sram + SRAM_SIZE;
    }
    *data = *(int *)(src + offset);
}

extern "C" void sram_write(char sel, int addr, int data, char mask) {
    uint32_t offset = addr << 2;
    uint8_t *dst;
    if(sel == 0) {
        dst = sram;
    } else {
        dst = sram + SRAM_SIZE;
    }
    for(int i = 0; i < 4; i++) {
        if(mask & (1 << i)) {
            memcpy(dst + offset + i, (char *)&data + i, 1);
        }
    }
}