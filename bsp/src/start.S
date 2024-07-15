.text
.global _start
.section entry

_start:
    la.local $sp, _stack
    b _init
