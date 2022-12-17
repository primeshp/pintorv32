/*
* LED Blinker
* Assuming that GPIO_BASE is mapped to a GPIO core, which in turn is
* connected to LEDs, this will light the LEDs one at a time.
* Useful as smoke test to see that serv is running correctly
*/
.globl _start
_start:
lui x1,%hi(0x12345678)
addi x1,x1,%lo(0x12345678)
sw x1, 0(x0)
sh x1, 4(x0)
sb x1, 8(x0)
lw x2, 0(x0)
lh x3, 0(x0)
lb x4, 0(x0)
lui x1,%hi(0xABCDEF12)
addi x1,x1,%lo(0xABCDEF12)
sw x1, 0(x0)
sh x1, 4(x0)
sb x1, 8(x0)
lw x2, 0(x0)
lh x3, 0(x0)
lb x4, 0(x0)
lui x7,0x12345
auipc x8,0x34567
jalr x9,x0,32
