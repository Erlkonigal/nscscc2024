MEMORY {
    BASE_RAM (rx) : ORIGIN = 0x80000000, LENGTH = 4M
    EXT_RAM (rwx) : ORIGIN = 0x80400000, LENGTH = 4M
}

ENTRY(_start)

SECTIONS {
    .text : {
        *(entry)
        *(.text*)
    } >BASE_RAM
    .rodata : {
        *(.rodata*)
    } >EXT_RAM
    .data : {
        *(.data*)
    } >EXT_RAM
    .bss : {
        *(.bss*)
        *(.sbss*)
        *(.scommon)
    } >EXT_RAM
    _stack_base = ALIGN(0x1000);
    . = _stack_base + 0x10000;
    _stack = .;
    _heap = ALIGN(0x1000);
}