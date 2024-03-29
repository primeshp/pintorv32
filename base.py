#!/usr/bin/env python3

from migen import *
from migen.genlib.cdc import MultiReg

from litex.build.generic_platform import *
from litex.build.xilinx import XilinxPlatform
from cpu_v4 import cpuModule




_io = [
    ("user_led",  0, Pins("H5"), IOStandard("LVCMOS33")),
    ("user_led",  1, Pins("J5"), IOStandard("LVCMOS33")),

    ("user_sw",  0, Pins("A8"), IOStandard("LVCMOS33")),

    ("user_btn_r", 0, Pins("D9"), IOStandard("LVCMOS33")),
    ("user_btn_l", 0, Pins("B8"), IOStandard("LVCMOS33")),


    ("clk100", 0, Pins("E3"), IOStandard("LVCMOS33")),

    ("cpu_reset", 0, Pins("C2"), IOStandard("LVCMOS33")),

]




# Platform -----------------------------------------------------------------------------------------

class Platform(XilinxPlatform):
    default_clk_name   = "clk100"
    default_clk_period = 1e9/100e6

    def __init__(self):
        XilinxPlatform.__init__(self, "xc7a35t-csg324-1", _io, toolchain="vivado")


# IOs ----------------------------------------------------------------------------------------------


# Design -------------------------------------------------------------------------------------------



# Create our platform (fpga interface)
platform = Platform()

# Create our main module (fpga description)
class riscv(Module):
    sys_clk_freq = int(100e6)
    def __init__(self):
         
        led1 = platform.request("user_led",0)
        led2 = platform.request("user_led",1)

        address_counter = Signal(8)
        data_read = Signal(32)

        riscv_cpu = cpuModule()
        self.submodules += riscv_cpu
        self.sync += [led1.eq(riscv_cpu.led1.output[0]),led2.eq(riscv_cpu.led1.output[1])]


 
module = riscv()

# Build --------------------------------------------------------------------------------------------

platform.build(module)
