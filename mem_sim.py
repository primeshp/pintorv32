#This add lui/luipc instructions handling in additon to R/R and R/I alu ops and branch


from re import M
from migen import *




#################################################################################################
# Main code start here
#################################################################################################

class memModule(Module):
    def __init__(self):
        # Data Memory
        self.read_addr =Signal(8)
        self.write_addr =Signal(8)
        self.read_data =Signal(32)
        self.write_data = Signal(32)
        self.wr_en = Signal(4)

        data_mem              = Memory(32,64) #Load the assembly program into the memory
        data_mem_rd_port      = data_mem.get_port(async_read=True)
        data_mem_wr_port      = data_mem.get_port(write_capable=True,we_granularity =8)
        self.comb += [data_mem_wr_port.dat_w.eq(self.write_data),
                      data_mem_wr_port.adr.eq(self.write_addr),
                      data_mem_wr_port.we.eq(self.wr_en),
                      data_mem_rd_port.adr.eq(self.read_addr),
                      self.read_data.eq(data_mem_rd_port.dat_r),
                      ]
        self.specials += [data_mem,data_mem_rd_port,data_mem_wr_port]

        




if __name__ == '__main__':
    dut = memModule()


    def dut_tb(dut):
        yield dut.write_data.eq(0x12345678)
        yield dut.write_addr.eq(0x0)
        yield dut.wr_en.eq(0b0001)
        yield
        yield dut.read_addr.eq(0)
        yield
        yield dut.wr_en.eq(0b0011)
        yield
        yield dut.read_addr.eq(0)
        yield
        yield dut.wr_en.eq(0b0111)
        yield
        yield dut.read_addr.eq(0)
        yield
        yield dut.wr_en.eq(0b1111)
        yield
        yield dut.read_addr.eq(0)
        yield
        yield dut.wr_en.eq(0b0000)
        yield
        yield dut.read_addr.eq(0)
        yield


    




    run_simulation(dut, dut_tb(dut), vcd_name="test.vcd")
    
