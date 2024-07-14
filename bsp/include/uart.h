#pragma once

#define UART_BASE 0xBFD003F8
#define UART_DATA_OFFSET 0x0
#define UART_STATUS_OFFSET 0x4

void uart_putc(char c);
void uart_getc(char *c);