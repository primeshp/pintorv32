#This add Branch instruction handling in additon to R/R and R/I alu ops


from migen import *
from migen.fhdl.tools import is_variable


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




class register_file(Module):
    '''
    This create the CPU register file. This consist of 2 sets of memory of 32bit X 32 to enable simultenious reading of 2 addressing
    when writing the value to scachpad: din is written to both scratchpads using address in rd
    when reading reach scrachpad uses rs1 and rs2 as the address and output to reg_data1 and reg_data2 respectively
    '''
    def __init__(self):
        self.rs1_in        = Signal(5)
        self.rs2_in        = Signal(5)
        self.rd_in         = Signal(5)
        self.din        = Signal(32)
        self.reg_data1_out  = Signal(32)
        self.reg_data2_out  = Signal(32)
        self.write      = Signal()

        scratchpad1 = Memory(32,32)
        scratchpad2 = Memory(32,32)

        sp1_wrport = scratchpad1.get_port(write_capable=True) #scratchpad1 write port
        sp1_rdport = scratchpad1.get_port(async_read=True) #scratchpad1 read port
        sp2_wrport = scratchpad2.get_port(write_capable=True) #scratchpad2 write port
        sp2_rdport = scratchpad2.get_port(async_read=True) #scratchpad2 read port
        self.specials += [scratchpad1, scratchpad2,sp1_rdport,sp1_wrport,sp2_rdport,sp2_wrport]
        

        #Write to both scrachpads at the same time synchronously
        self.comb += [
            If((self.rd_in!=0),
               sp1_wrport.adr.eq(self.rd_in), #Use rd_in as address to write to 
               sp1_wrport.dat_w.eq(self.din), #Use rd_in as data to write 
            
               sp2_wrport.adr.eq(self.rd_in), #Use rd_in as address to write to 
               sp2_wrport.dat_w.eq(self.din), #Use rd_in as data to write 

               sp1_wrport.we.eq(self.write),  #Only write if write signal is asserted
               sp2_wrport.we.eq(self.write )  #Only write if write signal is asserted
            )
        ]

        #read from scratchpads done asynchronously using rs1 and rs2 as each scrachpad address
        self.comb += [
            sp1_rdport.adr.eq(self.rs1_in),
            self.reg_data1_out.eq(sp1_rdport.dat_r),
            sp2_rdport.adr.eq(self.rs2_in),
            self.reg_data2_out.eq(sp2_rdport.dat_r)
        ]







class cpuModule(Module):
    def __init__(self):

        serial_run = [#0x00100013, 
        0x00100093,0x00100133,0x002081b3,0x00310233,0x004182b3,0x00520333,0x006283b3,0x00730433,
        0x008384b3,0x00940533,0x00a485b3,0x00b50633,0x00c586b3,0x00d60733,0x00e687b3,0x00f70833,0x010788b3,0x01180933,0x012889b3,
        0x01390a33,0x01498ab3,0x015a0b33,0x016a8bb3,0x017b0c33,0x018b8cb3,0x019c0d33,0x01ac8db3,0x01bd0e33,0x01cd8eb3,0x01de0f33,0x01ee8fb3
        ]
        branch_run_v1 = [0x01e00093,0x00000133,0x000001b3,0x00a00213,0x00110113,0x00518193,0xfe411ce3]
        branch_run_v2 = [0x01e00093,0x00a00113,0xfff10113,0x00518193,0xfe201ce3]


        
        instruction_init  = branch_run_v1

        alu1 = alu()
        registers = register_file()
        program_counter = Signal(32)
        instrct_mem = Memory(32,64,init=instruction_init) #Load the assembly program into the memory
        instrcut_mem_port = instrct_mem.get_port(async_read=True)
        instruction = Signal(32) #next Intruction
        mux1_select = Signal() # Multiplexer control signal to ALU operand2
        is_branch = Signal()  #Check if it a branch Instruction
        self.submodules +=[alu1, registers]
        self.specials+=[instrct_mem, instrcut_mem_port]

        self.operand1 = Signal(32)
        self.operand2 = Signal(32)
        self.alu_out  = Signal(32)
        self.operation = Signal(16)


        #Instruction Memory read 
        self.comb += [
            instrcut_mem_port.adr.eq(program_counter[0:9]),
            instruction.eq(instrcut_mem_port.dat_r)

        ]
        

        #Immidiate Value calculations
        imm_i = Signal(32)
        imm_b = Signal(32)
        imm_s = Signal(32)
        imm_u = Signal(32)
        imm_j = Signal(32)
       
        
        self.comb+=[
           imm_i.eq(Cat(instruction[20:32],Replicate(instruction[31],20))),
           imm_b.eq(Cat(0,instruction[8:12],instruction[25:31],instruction[7],instruction[31])),
           imm_s.eq(Cat(instruction[7:12],instruction[25:32],Replicate(instruction[31],20))),
           imm_u.eq(Cat(Replicate(0,12),instruction[12:32])),
           imm_j.eq(Cat(0,instruction[21:31],instruction[20],instruction[12:20],instruction[31]))

        ]






        


        
        #Register pad addressing
        self.comb += [
            registers.rs1_in.eq(instruction[15:20]),
            registers.rs2_in.eq(instruction[20:25]),
            registers.rd_in.eq(instruction[7:12]),   
        ]



        #Fee to ALU
        self.comb += [
            If(mux1_select, alu1.operand2.eq(imm_i)).Else(alu1.operand2.eq(registers.reg_data2_out)),
            alu1.operand1.eq(registers.reg_data1_out)
        ] 

        self.comb += registers.din.eq(alu1.alu_out)       # ALU output goes to register file update

        #Instruction Decoder

        opcode =Signal(7)
        funct3 = Signal(3)
        funct7 = Signal(7)

        self.comb += [opcode.eq(instruction[0:7]),funct3.eq(instruction[12:15]),funct7.eq(instruction[25:32])]


        alu_operations = {
            0x0 : If(funct7==0,alu1.operation.eq(0b0000000000000001)).Else(alu1.operation.eq(0b0000000000000010)),
            0x4 : alu1.operation.eq(0b0000000000000100),
            0x6 : alu1.operation.eq(0b0000000000001000),
            0x7 : alu1.operation.eq(0b0000000000010000),
            0x1 : alu1.operation.eq(0b0000000000100000),
            0x5 : If(funct7==0,alu1.operation.eq(0b0000000001000000)).Else(alu1.operation.eq(0b0000000010000000)),
            0x2 : alu1.operation.eq(0b0000000100000000),
            0x3 : alu1.operation.eq(0b0000001000000000),

        }


        branch_operations = {
            0x0 : alu1.operation.eq(0b0000010000000000),
            0x1 : alu1.operation.eq(0b0000100000000000),
            0x4 : alu1.operation.eq(0b0001000000000000),
            0x5 : alu1.operation.eq(0b0010000000000000),
            0x6 : alu1.operation.eq(0b0100000000000000),
            0x7 : alu1.operation.eq(0b1000000000000000),
        }



        #Instruction Decoding/Control
        self.comb += [If(opcode==0b0110011,mux1_select.eq(0), Case(funct3,alu_operations),    registers.write.eq(1),is_branch.eq(0)).          # Resiter/Register ALU operations
                    Elif(opcode==0b0010011,mux1_select.eq(1), Case(funct3,alu_operations),    registers.write.eq(1),is_branch.eq(0)).          # Register/immidiate ALU operation
                    Elif(opcode==0b1100011,mux1_select.eq(0), Case(funct3,branch_operations), registers.write.eq(0),is_branch.eq(1))           # Branch, No saving back to Registers, Mul is selected for R/R op
                    ]

        #PC Increase 
        self.sync += If(is_branch, If(alu1.alu_out[0],program_counter.eq(program_counter+Cat(imm_b[2:32])))).Else(program_counter.eq(program_counter + 1)) # Increase the program counter or Branch



if __name__ == '__main__':
    dut = cpuModule()


    def dut_tb(dut):
        for i in range(40):
            yield  #Clock once


    run_simulation(dut, dut_tb(dut), vcd_name="test.vcd")
    
