ENTRY(_start)

SECTIONS {
    . = 0x80000000;
    .text : {
        *(.text*)
    }
    .rodata : {
        *(.rodata*)
    }
    .data : {
        *(.data*)
    }
    .bss : {
        *(.bss*)
        *(.sbss*)
        *(.scommon)
    }
    _stack_base = ALIGN(0x1000);
    . = _stack_base + 0x10000;
    _stack = .;
    _heap = ALIGN(0x1000);
}