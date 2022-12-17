/*
* LED Blinker
* Assuming that GPIO_BASE is mapped to a GPIO core, which in turn is
* connected to LEDs, this will light the LEDs one at a time.
* Useful as smoke test to see that serv is running correctly
*/

.globl _start

_start:

	lui t1,%hi(100000000) #delay value
        addi t1,t1,%lo(100000000)
        addi t0,zero,0xAA
        add x31, t0, x0

bl1:
        
       #call blink
        jal x10,blink
        and t2, zero, zero
       #call delay 
        jal x10, delay
        jal x0, bl1

delay:
	addi t2, t2, 1
	bne t1, t2, delay
	jalr x0,x10,0
	
	
blink:
        xori x31, x31, 0xFF
        jalr x0,x10,0
	
