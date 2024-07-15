#include "../include/program.h"

char *hello = "Hello, World!\n";

int main() {
    for(int i = 0; hello[i] != '\0'; i++) {
        uart_putc(hello[i]);
    }
    char c;
    while(1) {
        uart_getc(&c);
        uart_putc(c);
    }
    return 0;
}