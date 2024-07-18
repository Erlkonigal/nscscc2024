.text
.global _start

_start:
    li.w $r10, 0x80400000
    addi.w $r1, $r0, 1
    addi.w $r2, $r1, 1
    addi.w $r3, $r2, 1
    addi.w $r4, $r3, 1
    add.w $r5, $r2, $r3
    add.w $r6, $r2, $r4
    add.w $r7, $r2, $r5
    add.w $r8, $r6, $r7
    st.w $r8, $r10, 0
    ld.w $r2, $r10, 0
    st.w $r2, $r10, 4
    ld.w $r2, $r10, 4


