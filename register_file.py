from migen import *
from migen.fhdl import verilog




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




dut = register_file()


def check_case(rs1,rs2,rd,data_in,write_en):
  yield dut.rs1_in.eq(rs1)
  yield dut.rs2_in.eq(rs2)
  yield dut.din.eq(data_in)
  yield dut.write.eq(write_en)
  yield dut.rd_in.eq(rd)
  yield
  yield


  #assert (yield dut.alu_out) == result

def testbench():

  yield from check_case(1,2,1,100,1)
  yield from check_case(1,2,2,200,1)
  yield from check_case(1,2,1,300,1)


if __name__ == "__main__":

  run_simulation(dut, testbench(), vcd_name="register_file.vcd")
  #rf = register_file()
  #print(verilog.convert(rf))

