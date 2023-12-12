layout asm
layout regs
focus cmd

display (long[100])inputs
display (char[100])modified
display *((long*)&inputs + $rbx)
display *((long*)&inputs + $rbx + 1)
