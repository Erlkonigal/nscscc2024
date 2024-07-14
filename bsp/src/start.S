.text
.global _start

_start:
    la.local $sp, _stack
    b _init
