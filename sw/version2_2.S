/*
* LED Blinker
* Assuming that GPIO_BASE is mapped to a GPIO core, which in turn is
* connected to LEDs, this will light the LEDs one at a time.
* Useful as smoke test to see that serv is running correctly
*/
.globl _start
_start:
  addi x1,x0,30
  addi x2,x0,10
loop:
  addi x2,x2,-1
  addi x3,x3,5
  bne x0,x2, loop
