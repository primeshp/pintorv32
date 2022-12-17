from migen import *


######################  Parameters #####################
RESET_ADDR     = 0x00000000
INTERRUPT_ADDR = 0x00002DE4

########################################################




class alu_module(Module):
    ''' 
    ALU is responsible for calculations for Register to Register, Register to Immidiate and Branching related instructions
    operand1  : first operand for the caluclation
    operand2  : second operand for the calculation
    alu_out   : result of the ALU calculations
    operation : One hot encoded operation to perform
    '''
    def __init__(self):
        self.operand1  = Signal((32,True)) 
        self.operand2  = Signal((32,True))
        self.alu_out   = Signal((32,True))
        #self.operand1  = Signal(32) 
        #self.operand2  = Signal(32)
        #self.alu_out   = Signal(32)
        self.operation = Signal(16)
        op1_unsigned  = Signal(33) #Make Op1 unsigned by having MSB 0
        op2_unsigned  = Signal(33) #Make Op1 unsigned by having MSB 0

        self.comb +=[op1_unsigned.eq(Cat(self.operand1[0:32],0b0)),op2_unsigned.eq(Cat(self.operand2[0:32],0b0))]
       

        self.comb += Case(self.operation,{
            0b0000000000000001: self.alu_out.eq(self.operand1 + self.operand2), # ADD 0x1
            0b0000000000000010: self.alu_out.eq(self.operand1 - self.operand2), # SUB 0x2
            0b0000000000000100: self.alu_out.eq(self.operand1 ^ self.operand2), # XOR 0x4
            0b0000000000001000: self.alu_out.eq(self.operand1 | self.operand2), # OR  0x8
            0b0000000000010000: self.alu_out.eq(self.operand1 & self.operand2), # AND 0x10
            0b0000000000100000: self.alu_out.eq(self.operand1 << self.operand2), # SLL 
            0b0000000001000000: self.alu_out.eq((op1_unsigned  >> self.operand2)[:32]), # SRL
            0b0000000010000000: self.alu_out.eq(self.operand1 >> self.operand2), # SRA  <--Fixme for the operation
            0b0000000100000000: If(self.operand1 < self.operand2, self.alu_out.eq(1)).Else(self.alu_out.eq(1) ), # SLT
            0b0000001000000000: If(op1_unsigned   < op2_unsigned  ,  self.alu_out.eq(1)).Else(self.alu_out.eq(1) ), # Fixme SLTU
            0b0000010000000000: If(self.operand1 == self.operand2,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BEQ
            0b0000100000000000: If(self.operand1 != self.operand2,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BNE
            0b0001000000000000: If(self.operand1 < self.operand2,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BLT <-- Fixme failed 235< -234 and -234 < 235
            0b0010000000000000: If(self.operand1 >= self.operand2,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BGE -- Fixme faied -234>= 235, 235>= -234
            0b0100000000000000: If(op1_unsigned  < op2_unsigned ,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BLTU  
            0b1000000000000000: If(op1_unsigned >= op2_unsigned ,  self.alu_out.eq(1)).Else(self.alu_out.eq(0) ), # BGEU 
            "default"         : self.alu_out.eq(0)
        })


dut = alu_module()
ALU_ADD  = 0b0000000000000001
ALU_SUB  = 0b0000000000000010
ALU_XOR  = 0b0000000000000100
ALU_OR   = 0b0000000000001000
ALU_AND  = 0b0000000000010000
ALU_SLL  = 0b0000000000100000
ALU_SRL  = 0b0000000001000000
ALU_SRA  = 0b0000000010000000
ALU_SLT  = 0b0000000100000000
ALU_SLTU = 0b0000001000000000




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


  yield from check_case(234, 235,0b0000000000000001,62077)
  yield from check_case(235, 233,0b0000000000000001 ,222)
  yield from check_case(-234, -235,0b0000000000000001,62077)
  yield from check_case(-235, -234,0b0000000000000001 ,222)
  yield from check_case(-234, 235,0b0000000000000001,62077)
  yield from check_case(235, -234,0b0000000000000001 ,222)
  yield from check_case(0, 0,0b0000000000000001 ,222)
  #yield from check_case(456, 8,32 ,222)
  #yield from check_case(-234, 3,32 ,0)
  #yield from check_case(0xFF02FF10, 2,32 ,0x2e261f11)



  #Testing ALU from Vivonomicon.com
  def alu_test( alu ):
  # Let signals settle after reset.
  yield Settle()
  # Print a test header.
  print( "--- ALU Tests ---" )
  # Test the bitwise 'AND' operation.
  print( "AND (&) tests:" )
  yield from alu_ut( alu, 0xCCCCCCCC, 0xCCCC0000, ALU_AND, 0xCCCC0000 )
  yield from alu_ut( alu, 0x00000000, 0x00000000, ALU_AND, 0x00000000 )
  yield from alu_ut( alu, 0xFFFFFFFF, 0xFFFFFFFF, ALU_AND, 0xFFFFFFFF )
  yield from alu_ut( alu, 0x00000000, 0xFFFFFFFF, ALU_AND, 0x00000000 )
  yield from alu_ut( alu, 0xFFFFFFFF, 0x00000000, ALU_AND, 0x00000000 )
  # Test the bitwise 'OR' operation.
  print( "OR  (|) tests:" )
  yield from alu_ut( alu, 0xCCCCCCCC, 0xCCCC0000, ALU_OR, 0xCCCCCCCC )
  yield from alu_ut( alu, 0x00000000, 0x00000000, ALU_OR, 0x00000000 )
  yield from alu_ut( alu, 0xFFFFFFFF, 0xFFFFFFFF, ALU_OR, 0xFFFFFFFF )
  yield from alu_ut( alu, 0x00000000, 0xFFFFFFFF, ALU_OR, 0xFFFFFFFF )
  yield from alu_ut( alu, 0xFFFFFFFF, 0x00000000, ALU_OR, 0xFFFFFFFF )
  # Test the bitwise 'XOR' operation.
  print( "XOR (^) tests:" )
  yield from alu_ut( alu, 0xCCCCCCCC, 0xCCCC0000, ALU_XOR, 0x0000CCCC )
  yield from alu_ut( alu, 0x00000000, 0x00000000, ALU_XOR, 0x00000000 )
  yield from alu_ut( alu, 0xFFFFFFFF, 0xFFFFFFFF, ALU_XOR, 0x00000000 )
  yield from alu_ut( alu, 0x00000000, 0xFFFFFFFF, ALU_XOR, 0xFFFFFFFF )
  yield from alu_ut( alu, 0xFFFFFFFF, 0x00000000, ALU_XOR, 0xFFFFFFFF )
  # Test the addition operation.
  print( "ADD (+) tests:" )
  yield from alu_ut( alu, 0, 0, ALU_ADD, 0 )
  yield from alu_ut( alu, 0, 1, ALU_ADD, 1 )
  yield from alu_ut( alu, 1, 0, ALU_ADD, 1 )
  yield from alu_ut( alu, 0xFFFFFFFF, 1, ALU_ADD, 0 )
  yield from alu_ut( alu, 29, 71, ALU_ADD, 100 )
  yield from alu_ut( alu, 0x80000000, 0x80000000, ALU_ADD, 0 )
  yield from alu_ut( alu, 0x7FFFFFFF, 0x7FFFFFFF, ALU_ADD, 0xFFFFFFFE )
  # Test the subtraction operation.
  print( "SUB (-) tests:" )
  yield from alu_ut( alu, 0, 0, ALU_SUB, 0 )
  yield from alu_ut( alu, 0, 1, ALU_SUB, -1 )
  yield from alu_ut( alu, 1, 0, ALU_SUB, 1 )
  yield from alu_ut( alu, -1, 1, ALU_SUB, -2 )
  yield from alu_ut( alu, 1, -1, ALU_SUB, 2 )
  yield from alu_ut( alu, 29, 71, ALU_SUB, -42 )
  yield from alu_ut( alu, 0x80000000, 1, ALU_SUB, 0x7FFFFFFF )
  yield from alu_ut( alu, 0x7FFFFFFF, -1, ALU_SUB, 0x80000000 )
  # Test the signed '<' comparison operation.
  print( "SLT (signed <) tests:" )
  yield from alu_ut( alu, 0, 0, ALU_SLT, 0 )
  yield from alu_ut( alu, 1, 0, ALU_SLT, 0 )
  yield from alu_ut( alu, 0, 1, ALU_SLT, 1 )
  yield from alu_ut( alu, -1, 0, ALU_SLT, 1 )
  yield from alu_ut( alu, -42, -10, ALU_SLT, 1 )
  yield from alu_ut( alu, -10, -42, ALU_SLT, 0 )
  # Test the unsigned '<' comparison operation.
  print( "SLTU (unsigned <) tests:" )
  yield from alu_ut( alu, 0, 0, ALU_SLTU, 0 )
  yield from alu_ut( alu, 1, 0, ALU_SLTU, 0 )
  yield from alu_ut( alu, 0, 1, ALU_SLTU, 1 )
  yield from alu_ut( alu, -1, 0, ALU_SLTU, 0 )
  yield from alu_ut( alu, -42, -10, ALU_SLTU, 1 )
  yield from alu_ut( alu, -10, -42, ALU_SLTU, 0 )
  yield from alu_ut( alu, -42, 42, ALU_SLTU, 0 )
  # Test the shift right operation.
  print ( "SRL (>>) tests:" )
  yield from alu_ut( alu, 0x00000001, 0, ALU_SRL, 0x00000001 )
  yield from alu_ut( alu, 0x00000001, 1, ALU_SRL, 0x00000000 )
  yield from alu_ut( alu, 0x00000011, 1, ALU_SRL, 0x00000008 )
  yield from alu_ut( alu, 0x00000010, 1, ALU_SRL, 0x00000008 )
  yield from alu_ut( alu, 0x80000000, 1, ALU_SRL, 0x40000000 )
  yield from alu_ut( alu, 0x80000000, 4, ALU_SRL, 0x08000000 )
  # Test the shift right with sign extension operation.
  print ( "SRA (>> + sign extend) tests:" )
  yield from alu_ut( alu, 0x00000001, 0, ALU_SRA, 0x00000001 )
  yield from alu_ut( alu, 0x00000001, 1, ALU_SRA, 0x00000000 )
  yield from alu_ut( alu, 0x00000011, 1, ALU_SRA, 0x00000008 )
  yield from alu_ut( alu, 0x00000010, 1, ALU_SRA, 0x00000008 )
  yield from alu_ut( alu, 0x80000000, 1, ALU_SRA, 0xC0000000 )
  yield from alu_ut( alu, 0x80000000, 4, ALU_SRA, 0xF8000000 )
  # Done.
  yield Tick()
  print( "ALU Tests: %d Passed, %d Failed"%( p, f ) )

run_simulation(dut, testbench(), vcd_name="alu.vcd")
