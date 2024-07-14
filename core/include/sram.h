#pragma once

#include "../include/common.h"

#define SRAM_SIZE 0x400000
#define BASE 0x80000000
#define EXT 0x80400000

extern uint8_t base_sram[SRAM_SIZE];
extern uint8_t ext_sram[SRAM_SIZE];

void sram_init();
extern "C" void sram_read(char sel, int addr, int *data);
extern "C" void sram_write(char sel, int addr, int data, char mask);
