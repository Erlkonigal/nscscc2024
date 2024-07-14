#include "../include/sram.h"
#include <cstdint>

uint8_t base_sram[SRAM_SIZE];
uint8_t ext_sram[SRAM_SIZE];

void sram_init() {
    memset(base_sram, 0, SRAM_SIZE);
    memset(ext_sram, 0, SRAM_SIZE);
}

extern "C" void sram_read(char sel, int addr, int *data) {
    uint32_t offset = addr << 2;
    uint8_t *src;
    if(sel == 0) {
        src = base_sram;
    }
    else {
        src = ext_sram;
    }
    *data = *(int *)(src + offset);
}

extern "C" void sram_write(char sel, int addr, int data, char mask) {
    uint32_t offset = addr << 2;
    uint8_t *dst;
    if(sel == 0) {
        dst = base_sram;
    } else {
        dst = ext_sram;
    }
    for(int i = 0; i < 4; i++) {
        if(mask & (1 << i)) {
            memcpy(dst + offset + i, (char *)&data + i, 1);
        }
    }
}