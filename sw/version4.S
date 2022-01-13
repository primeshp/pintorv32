/*
* LED Blinker
* Assuming that GPIO_BASE is mapped to a GPIO core, which in turn is
* connected to LEDs, this will light the LEDs one at a time.
* Useful as smoke test to see that serv is running correctly
*/
.globl _start
_start:

lb x1, 0(x0)
lb x2, 4(x0)
lb x3, 8(x0)
lb x4, 12(x0)
lh x5, 0(x0)
lh x6, 4(x0)
lh x7, 8(x0)
lh x8, 12(x0)
lw x9, 0(x0)
lw x10,4(x0)
lw x11,8(x0)
lw x12,12(x0)
lbu x13, 0(x0)
lbu x14, 4(x0)
lbu x15, 8(x0)
lbu x16, 12(x0)
lhu x17, 0(x0)
lhu x18, 4(x0)
lhu x19, 8(x0)
lhu x20, 12(x0)

