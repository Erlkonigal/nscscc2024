#include "../include/bsp.h"

int main();

void assert(int cond) {
    if (!cond) {
        halt();
    }
}

void halt() {
    while (1) {
        // do nothing
    }
}

void _init() {
    main();
    while(1) {
        // do nothing
    }
}