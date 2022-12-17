/*
* LED Blinker
* Assuming that GPIO_BASE is mapped to a GPIO core, which in turn is
* connected to LEDs, this will light the LEDs one at a time.
* Useful as smoke test to see that serv is running correctly
*/

.globl _start
_start:

	lui t1,0 #delay value
        addi t1,t1,3
        addi t0,zero,0xAA
        add x31, t0, x0

bl1:

        xori x31, x31, 0xFF
	and t2, zero, zero

time1:
	addi t2, t2, 1
	bne t1, t2, time1
	jal x0,bl1
