 #This add lui/luipc instructions handling in additon to R/R and R/I alu ops and branch


from migen import *


######################  Parameters #####################

RESET_ADDR     = 0x00000000
INTERRUPT_ADDR = 0x00002DE4
INSTR_MEM_SIZE = 256
DATA_MEM_SIZE  = 256

########################################################

##################### Program to Run ###################
load_test_data =[0x1234567,0x7654321,0xAAAABBBB,0xABCDEFA8 ]
serial_run = [#0x00100013, 
        0x00100093,0x00100133,0x002081b3,0x00310233,0x004182b3,0x00520333,0x006283b3,0x00730433,
        0x008384b3,0x00940533,0x00a485b3,0x00b50633,0x00c586b3,0x00d60733,0x00e687b3,0x00f70833,0x010788b3,0x01180933,0x012889b3,
        0x01390a33,0x01498ab3,0x015a0b33,0x016a8bb3,0x017b0c33,0x018b8cb3,0x019c0d33,0x01ac8db3,0x01bd0e33,0x01cd8eb3,0x01de0f33,0x01ee8fb3
        ]
branch_run = [0x01e00093,0x00000133,0x000001b3,0x00a00213,0x00110113,0x00518193,0xfe411ce3]
led_run = [0x00000337,0x00a30313,0x0aa00293,0x00028fb3,0x0fffcf93,0x000073b3,0x00138393,0xfe731ee3,0xff1ff06f]
load_run = [0x00000083,0x00400103,0x00800183,0x00c00203,0x00001283,0x00401303,0x00801383,0x00c01403,0x00002483,0x00402503,0x00802583,0x00c02603,0x00004683,0x00404703,0x00804783,0x00c04803,0x00005883,0x00405903,0x00805983,0x00c05a03]
load_store_run = [0x123450b7,0x67808093,0x00102023,0x00101223,0x00100423,0x00002103,0x00001183,0x00000203,0x00401283,0x00800303]


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
        self.operand1  = Signal(32)
        self.operand2  = Signal(32)
        self.alu_out   = Signal(32)
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


class led_module(Module):
    ''' 
    Read any data written to the scratch pad regiter 31 and write first 8 bits to an outbound port
    '''
    def __init__(self):
        self.input  = Signal(31)
        self.output = Signal(8)
        self.addr   = Signal(5)
        self.wen    = Signal()
        self.sync  += If ((self.addr==31), If(self.wen, self.output.eq(self.input[0:8])))


class register_file(Module):
    '''
    This create the CPU register file. This consist of 2 sets of memory of 32bit X 32 to enable simultenious reading of 2 addressing
    when writing the value to scachpad: din is written to both scratchpads using address in rd
    when reading reach scrachpad uses rs1 and rs2 as the address and output to reg_data1 and reg_data2 respectively
    '''
    def __init__(self):
        self.rs1_in         = Signal(5)
        self.rs2_in         = Signal(5)
        self.rd_in          = Signal(5)
        self.din            = Signal(32)
        self.reg_data1_out  = Signal(32)
        self.reg_data2_out  = Signal(32)
        self.write          = Signal()

        scratchpad1         = Memory(32,32)
        scratchpad2         = Memory(32,32)

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


class mux_4to1(Module):
    '''This is a multiplexer which takes 2 bit input and select the ourput'''
    def __init__(self):
        self.input0 = Signal(32)     #In case of PC output mux - Input for PC+4
        self.input1 = Signal(32)     #In case of PC output mux - Input for Branch
        self.input2 = Signal(32)     #In case of PC output mux - Input for Interrupt vector
        self.input3 = Signal(32)     #In case of PC output mux - Input for LUI instruction 
        self.output = Signal(32)     #In case of PC output mux - Input for ALU output
        self.mux_control = Signal(2) 
        self.is_branch = Signal()    #This should be tied to 0 for all Muxes except PC output mux
        
        cases = {
            0     : self.output.eq(self.input0),
            1     : If(self.is_branch,self.output.eq(self.input1)).Else(self.output.eq(self.input0)),
            2     : self.output.eq(self.input2),
            3     : self.output.eq(self.input3)
        }
        self.comb += Case(self.mux_control,cases)



class mux_5to1(Module):
    '''This is a multiplexer which takes 2 bit input and select the ourput'''
    def __init__(self):
        self.input0 = Signal(32)     
        self.input1 = Signal(32)    
        self.input2 = Signal(32)   
        self.input3 = Signal(32)   
        self.input4 = Signal(32)
        self.output = Signal(32) 
        self.mux_control = Signal(3) 
        self.is_branch = Signal()    #This should be tied to 0 for all Muxes except PC output mux
        
        cases = {
            0     : self.output.eq(self.input0),
            1     : If(self.is_branch,self.output.eq(self.input1)).Else(self.output.eq(self.input0)),
            2     : self.output.eq(self.input2),
            3     : self.output.eq(self.input3),
            4     : self.output.eq(self.input3)
        }
        self.comb += Case(self.mux_control,cases)

class trim_and_extend_module(Module):
    def __init__(self):
        self.operation = Signal(3)
        self.input     = Signal(32)
        self.output    = Signal(32)
        cases = {
            0     : self.output.eq(Cat(self.input[0:8],Replicate(self.input[7],24))), #Load Byte. Load the first byte and sign extend to rest of the 24 MSBs
            1     : self.output.eq(Cat(self.input[0:16],Replicate(self.input[7],16))), #load half
            2     : self.output.eq(self.input), #load word
            4     : self.output.eq(Cat(self.input[0:8],Replicate(0,24))),#Load Byte Unnumbered
            5     : self.output.eq(Cat(self.input[0:16],Replicate(0,16))),#Load Word Unnumbered
        }
        self.comb += Case(self.operation,cases)



#################################################################################################
# Main code start here
#################################################################################################

class cpuModule(Module):
    def __init__(self):
        
        instruction_init      = load_store_run  # Select the program to run

        # CPU key componenets
        alu                   = alu_module()
        registers             = register_file()
        program_counter       = Signal(32)
        pc_plus4              = Signal(32)
        
        # All the muxes needed
        pc_input_mux          = mux_4to1()
        register_file_din_mux = mux_4to1()
        alu_op1_mux           = mux_4to1()
        alu_op2_mux           = mux_5to1()

        instruction           = Signal(32) #Current Intruction
        opcode                = Signal(7)  #current instruction opcode portion      
        funct3                = Signal(3) #currrent instruction funct3 portion 
        funct7                = Signal(7) #currrent instruction funct7 portion 

        # Instruction Memory
        instrct_mem           = Memory(32,INSTR_MEM_SIZE,init=instruction_init) #Load the assembly program into the memory
        instrcut_mem_port     = instrct_mem.get_port(async_read=True)

        # Data Memory
        data_mem              = Memory(32,DATA_MEM_SIZE) #Load the assembly program into the memory
        data_mem_rd_port      = data_mem.get_port(async_read=True)
        data_mem_wr_port      = data_mem.get_port(write_capable=True,we_granularity=8)


        # Immidiate Value
        imm_i                 = Signal(32)
        imm_b                 = Signal(32)
        imm_s                 = Signal(32)
        imm_u                 = Signal(32)
        imm_j                 = Signal(32)

        trim_extend_output    = Signal(32)

        trim_and_extend = trim_and_extend_module()



        self.submodules      += [alu, registers, pc_input_mux,register_file_din_mux,alu_op1_mux,alu_op2_mux,trim_and_extend]
        self.specials        += [instrct_mem, instrcut_mem_port,data_mem,data_mem_rd_port,data_mem_wr_port]


        # Instruction Memory read 
        self.comb += [
            instrcut_mem_port.adr.eq(program_counter[0:8]),  #Fixme to expaand memory addressing and byte addressing
            instruction.eq(instrcut_mem_port.dat_r)
        ]
    


        #  Data Memory read/write  and Trim and Extend
        self.comb += [
            data_mem_rd_port.adr.eq(alu.alu_out[2:32]),  #Fixme  
            trim_and_extend.input.eq(data_mem_rd_port.dat_r),
            trim_extend_output.eq(trim_and_extend.output),
            trim_and_extend.operation.eq(funct3),
            data_mem_wr_port.adr.eq(alu.alu_out),
            data_mem_wr_port.dat_w.eq(registers.reg_data2_out)
        ]
 

        # Immidiate Value calculations     
        self.comb+=[
           imm_i.eq(Cat(instruction[20:32],Replicate(instruction[31],20))),
           imm_b.eq(Cat(0,instruction[8:12],instruction[25:31],instruction[7],Replicate(instruction[31],20))),
           imm_s.eq(Cat(instruction[7:12],instruction[25:32],Replicate(instruction[31],20))),
           imm_u.eq(Cat(Replicate(0,12),instruction[12:32])),
           imm_j.eq(Cat(0,instruction[21:31],instruction[20],instruction[12:20],Replicate(instruction[31],12)))
        ]
  

        # Register addressing
        self.comb += [  registers.rs1_in.eq(instruction[15:20]),registers.rs2_in.eq(instruction[20:25]), registers.rd_in.eq(instruction[7:12])]

        #Instruction Decoder
        self.comb += [opcode.eq(instruction[0:7]),funct3.eq(instruction[12:15]),funct7.eq(instruction[25:32])]

        # Feed to Register file Din         
        self.comb += [registers.din.eq(register_file_din_mux.output)]       # ALU output goes to register file update

        # Feed the ALU Operand1 Mux inputs
        self.comb += [alu_op1_mux.input0.eq(registers.reg_data1_out),alu_op1_mux.input1.eq(program_counter),alu_op1_mux.input2.eq(imm_j),alu_op1_mux.input3.eq(0),alu_op1_mux.is_branch.eq(1)] # Input 3 Not used. i_branch not used. So tie to 1 for normal operation Fixme ProgramCounter input is not used?

        # Feed the ALU Operand2 Mux inputs
        self.comb += [alu_op2_mux.input0.eq(registers.reg_data2_out),alu_op2_mux.input1.eq(imm_u),alu_op2_mux.input2.eq(imm_i),alu_op2_mux.input3.eq(imm_s),alu_op2_mux.input4.eq(program_counter),alu_op2_mux.is_branch.eq(1)]          # is_branch not used. So tie to 1 for normal operation

        # Feed the register file Din mux inputs
        self.comb += [register_file_din_mux.input0.eq(imm_u),register_file_din_mux.input1.eq(alu.alu_out),register_file_din_mux.input2.eq(trim_extend_output),register_file_din_mux.input3.eq(pc_plus4),register_file_din_mux.is_branch.eq(1)] #Fixme input 2  is_branch not used. So tie to 1 for normal operation

        # Feed the PC mux inputs
        self.comb += [pc_input_mux.input0.eq(pc_plus4),pc_input_mux.input1.eq(program_counter+imm_b[2:32]),pc_input_mux.input2.eq(INTERRUPT_ADDR),pc_input_mux.input3.eq(alu.alu_out[2:32])]  #Fixme....Here removed PC increase by 4s. Sould be program_counter+imm_b[0:32]...Correct back to pc_input_mux.input3.eq(Cat(0,alu.alu_out[1:32])

        # Feed to ALU
        self.comb +=[alu.operand1.eq(alu_op1_mux.output), alu.operand2.eq(alu_op2_mux.output)] # Connect Mux output to ALI operands inputs

        #LED Blinker
        self.led1 = led_module()
        self.submodules += [self.led1]
        self.comb += [self.led1.input.eq(register_file_din_mux.output),self.led1.addr.eq(instruction[7:12]),self.led1.wen.eq(registers.write)]  #RD value is the address

        math_operations = {
            0x0 : If(funct7==0,alu.operation.eq(0b0000000000000001)).Else(alu.operation.eq(0b0000000000000010)),
            0x4 : alu.operation.eq(0b0000000000000100),
            0x6 : alu.operation.eq(0b0000000000001000),
            0x7 : alu.operation.eq(0b0000000000010000),
            0x1 : alu.operation.eq(0b0000000000100000),
            0x5 : If(funct7==0,alu.operation.eq(0b0000000001000000)).Else(alu.operation.eq(0b0000000010000000)),
            0x2 : alu.operation.eq(0b0000000100000000),
            0x3 : alu.operation.eq(0b0000001000000000),

        }

        branch_operations = {
            0x0 : alu.operation.eq(0b0000010000000000),
            0x1 : alu.operation.eq(0b0000100000000000),
            0x4 : alu.operation.eq(0b0001000000000000),
            0x5 : alu.operation.eq(0b0010000000000000),
            0x6 : alu.operation.eq(0b0100000000000000),
            0x7 : alu.operation.eq(0b1000000000000000),
        }

        #This will control the memory writes based on funct3
        mem_write_ops = {
            0x0 : data_mem_wr_port.we.eq(0b0001),
            0x1 : data_mem_wr_port.we.eq(0b0011),
            0x2 : data_mem_wr_port.we.eq(0b1111),
        }

        #Control the ALU Operation, alu operad1 mux, alu operand2 mux, pc_input mux, din mux, registers write control
        self.comb += [
                    If(opcode==0b0110011,
                               Case(funct3,math_operations), alu_op1_mux.mux_control.eq(0), alu_op2_mux.mux_control.eq(0),
                               pc_input_mux.is_branch.eq(0), pc_input_mux.mux_control.eq(0), register_file_din_mux.mux_control.eq(1),
                               registers.write.eq(1), # Resiter/Register ALU operations, PC = PC+4,Write back enabled to reiterfile (10 Instructions)
                               data_mem_wr_port.we.eq(0b0000)).
                    Elif(opcode==0b0010011,
                               Case(funct3,math_operations), alu_op1_mux.mux_control.eq(0), alu_op2_mux.mux_control.eq(2),
                               pc_input_mux.is_branch.eq(0), pc_input_mux.mux_control.eq(0), register_file_din_mux.mux_control.eq(1),
                               registers.write.eq(1), # Register/immidiate ALU operation (9 Instruction)
                               data_mem_wr_port.we.eq(0b0000)).
                    Elif(opcode==0b0000011,
                               Case(0,math_operations), alu_op1_mux.mux_control.eq(0), alu_op2_mux.mux_control.eq(2),
                               pc_input_mux.is_branch.eq(0), pc_input_mux.mux_control.eq(0), register_file_din_mux.mux_control.eq(2),
                               registers.write.eq(1), # Load Instructions
                               data_mem_wr_port.we.eq(0b0000)).
                    Elif(opcode==0b0100011,
                               Case(0,math_operations), alu_op1_mux.mux_control.eq(0), alu_op2_mux.mux_control.eq(2), 
                               pc_input_mux.is_branch.eq(0), pc_input_mux.mux_control.eq(0), register_file_din_mux.mux_control.eq(0),
                               registers.write.eq(1), # Store Instructions
                               Case(funct3,mem_write_ops)).
                    Elif(opcode==0b1100011,
                               Case(funct3,branch_operations), alu_op1_mux.mux_control.eq(0), alu_op2_mux.mux_control.eq(0),
                               pc_input_mux.is_branch.eq(alu.alu_out[0]), pc_input_mux.mux_control.eq(1), register_file_din_mux.mux_control.eq(0),
                               registers.write.eq(0), # Branch Instructions (6 Instructions)
                               data_mem_wr_port.we.eq(0b0000)).
                    Elif(opcode==0b1101111,
                               Case(0,math_operations)  , alu_op1_mux.mux_control.eq(2), alu_op2_mux.mux_control.eq(4),
                               pc_input_mux.is_branch.eq(0), pc_input_mux.mux_control.eq(3), register_file_din_mux.mux_control.eq(3),
                               registers.write.eq(1),
                               data_mem_wr_port.we.eq(0b0000)). # Jump and Link Instruction
                    Elif(opcode==0b1100111,
                               Case(funct3,math_operations), alu_op1_mux.mux_control.eq(0), alu_op2_mux.mux_control.eq(2),
                               pc_input_mux.is_branch.eq(0), pc_input_mux.mux_control.eq(3), register_file_din_mux.mux_control.eq(3),
                               registers.write.eq(1),
                               data_mem_wr_port.we.eq(0b0000)). # Jump and Link Register Instruction
                    Elif(opcode==0b0110111,
                               Case(funct3,math_operations), alu_op1_mux.mux_control.eq(0), alu_op2_mux.mux_control.eq(0),
                               pc_input_mux.is_branch.eq(0), pc_input_mux.mux_control.eq(0), register_file_din_mux.mux_control.eq(0),
                               registers.write.eq(1),
                               data_mem_wr_port.we.eq(0b0000)). # Load Upper Immidiate instruction
                    Elif(opcode==0b0010111,
                               Case(0,math_operations)  , alu_op1_mux.mux_control.eq(1), alu_op2_mux.mux_control.eq(1),
                               pc_input_mux.is_branch.eq(0), pc_input_mux.mux_control.eq(0), register_file_din_mux.mux_control.eq(1),
                               registers.write.eq(1),
                               data_mem_wr_port.we.eq(0b0000))  # Load Upper Immidiate to PC Instruction
                    ]


        #PC Increase
        self.comb += pc_plus4.eq(program_counter+1)    #Fix me to increase by 4
        self.sync += program_counter.eq(pc_input_mux.output)  #Program counter increase based on different instruction types
                 


if __name__ == '__main__':
    dut = cpuModule()


    def dut_tb(dut):
        for i in range(200):
            yield  #Clock once


    run_simulation(dut, dut_tb(dut), vcd_name="test.vcd")
    
