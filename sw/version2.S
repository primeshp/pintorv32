/*
* LED Blinker
* Assuming that GPIO_BASE is mapped to a GPIO core, which in turn is
* connected to LEDs, this will light the LEDs one at a time.
* Useful as smoke test to see that serv is running correctly
*/

.globl _start
_start:
 addi x1,x0,30
  add  x2,x0,x0
  add  x3,x0,x0
  addi x4,x0,10
loop:
  addi x2,x2,1
  addi x3,x3,5
  bne x2,x4, loop
