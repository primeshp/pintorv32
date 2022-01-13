/*
* LED Blinker
* Assuming that GPIO_BASE is mapped to a GPIO core, which in turn is
* connected to LEDs, this will light the LEDs one at a time.
* Useful as smoke test to see that serv is running correctly
*/
.globl _start
_start:
lui x1,0x12345
addi x1,x1,0x678
sw x1, 0(x0)
sh x1, 4(x0)
sb x1, 8(x0)
lw x2, 0(x0)
lh x3, 0(x0)
lb x4, 0(x0)
lh x5, 4(x0)
lb x6, 8(x0)
