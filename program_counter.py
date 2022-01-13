from migen import *


######################  Parameters #####################
RESET_ADDR     = 0x00000000
INTERRUPT_ADDR = 0x00002DE4

########################################################




class program_counter(Module):
    def __init__(self):
        self.pc_o            = Signal(32,reset=RESET_ADDR)  #Current Program Counter value
        self.pc_plus_4_o     = Signal(32)  #Next increment of PC
        self.alu_output_i = Signal(32)  #Input from the output of the ALU to program counter calulations
        self.interupt_vector_i = Signal(reset=INTERRUPT_ADDR)  #Interrupt vector fixed to the specified address
        self.pc_next_value_selector_i = Signal(2) # This is based on ALU branch evaluation output and Control unit output

        self.comb += self.pc_plus_4_o.eq(self.pc_o + 4) 

       



