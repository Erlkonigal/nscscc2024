#include "../include/uart.h"

void uart_putc(char c) {
    volatile char *uart_base = (char *) UART_BASE;
    while ((*(uart_base + UART_STATUS_OFFSET) & 0x1) != 1) {
        // do nothing
    }
    *uart_base = c;
}

void uart_getc(char *c) {
    volatile char *uart_base = (char *) UART_BASE;
    while ((*(uart_base + UART_STATUS_OFFSET) & 0x2) != 2) {
        // do nothing
    }
    *c = *uart_base;
}