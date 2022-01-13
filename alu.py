from migen import *


######################  Parameters #####################
RESET_ADDR     = 0x00000000
INTERRUPT_ADDR = 0x00002DE4

########################################################

class alu(Module):
    '''
    
    ALU is responsible for calculations for Register to Register, Register to Immidiate and Branching related instructions
    operand1  : first operand for the caluclation
    operand2  : second operand for the calculation
    alu_out   : result of the ALU calculations
    operation : One hot encoded operation to perform

    '''
    def __init__(self):
        self.operand1 = Signal(32)
        self.operand2 = Signal(32)
        self.alu_out  = Signal(32)
        self.operation = Signal(16)
       

        self.comb += Case(self.operation,{
            0b0000000000000001: self.alu_out.eq(self.operand1 + self.operand2), # ADD 0x1
            0b0000000000000010: self.alu_out.eq(self.operand1 - self.operand2), # SUB 0x2
            0b0000000000000100: self.alu_out.eq(self.operand1 ^ self.operand2), # XOR 0x4
            0b0000000000001000: self.alu_out.eq(self.operand1 | self.operand2), # OR  0x8
            0b0000000000010000: self.alu_out.eq(self.operand1 & self.operand2), # AND 0x10
            0b0000000000100000: self.alu_out.eq(self.operand1 << self.operand2), # SLL 
            0b0000000001000000: self.alu_out.eq(self.operand1 >> self.operand2), # SRL
            0b0000000010000000: self.alu_out.eq(self.operand1 >> self.operand2), # SRA  <--Fixme for the operation
            0b0000000100000000: If(self.operand1 < self.operand2, self.alu_out.eq(1)).Else(self.alu_out.eq(1) ), # SLT
            0b0000001000000000: If(self.operand1 < self.operand2,  self.alu_out.eq(1)).Else(self.alu_out.eq(1) ), # Fixme SLTU

            0b0000010000000000: If(self.operand1 == self.operand2,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BEQ
            0b0000100000000000: If(self.operand1 != self.operand2,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BNE
            0b0001000000000000: If(self.operand1 < self.operand2,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BLT <-- Fixme failed 235< -234 and -234 < 235
            0b0010000000000000: If(self.operand1 >= self.operand2,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BGE -- Fixme faied -234>= 235, 235>= -234
            0b0100000000000000: If(self.operand1 < self.operand2,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BLTU  
            0b1000000000000000: If(self.operand1 >= self.operand2,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BGEU 
            "default"   : self.alu_out.eq(0)
        })


dut = alu()


def check_case(op1, op2, operation, result):
  yield dut.operand1.eq(op1)
  print(op1)
  yield dut.operand2.eq(op2)
  print(op2)
  yield dut.operation.eq(operation)
  print(operation)
  yield

  #assert (yield dut.alu_out) == result

def testbench():


  yield from check_case(234, 235,0b1000000000000000,62077)
  yield from check_case(235, 233,0b1000000000000000 ,222)
  yield from check_case(-234, -235,0b1000000000000000,62077)
  yield from check_case(-235, -234,0b1000000000000000 ,222)
  yield from check_case(-234, 235,0b1000000000000000,62077)
  yield from check_case(235, -234,0b1000000000000000 ,222)
  yield from check_case(0, 0,0b1000000000000000 ,222)
  #yield from check_case(456, 8,32 ,222)
  #yield from check_case(-234, 3,32 ,0)
  #yield from check_case(0xFF02FF10, 2,32 ,0x2e261f11)

run_simulation(dut, testbench(), vcd_name="alu.vcd")
