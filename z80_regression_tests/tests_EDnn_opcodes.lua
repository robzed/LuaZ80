-- Z80 Regression Tests
-- (c) Copyright 2015-2017 Rob Probin.
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
--
-- http://robprobin.com
-- https://github.com/robzed/LuaZ80
--
-- LICENSE NOTES
-- =============
-- Since this is Lua code (that generates more Lua code, so is pretty dependant
-- on a Lua interpreter), the requirement in the GPL to share does not seem
-- overly onerous/burdensome/difficult because you'll be distributing the Lua code
-- along with any product anyway. I considered MIT, ZLib, BSD, Apache licenses as
-- well but the GPL appeared to 'encourage' sharing. 
--
-- I'm quite willing to consider a different license if you have a specific 
-- use-case that would benefit - even if part of the (Lua or other) source 
-- would be closed or licensed under a non-open source license.

--
-- NOTES:
-- =====
-- 
-- Part of "z80_regression_test.lua"
--

local ED_instruction_tests = {

--[[
In the IN A and OUT n, A instructions, the I/O device’s n address appears in the 
lower half of the address bus (A7–A0), while the Accumulator content is 
transferred in the upper half of the address bus. In all Register Indirect 
input output instructions, including block I/O transfers, the contents of the 
C Register are transferred to the lower half of the address bus (device 
address) while the contents of Register B are transferred to the upper half of 
the address bus. 
--]]
--0xED 0x40
{ "IN   B,(C)", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "B", "(C)")
    end,
    { B = 0x33, C = 0x34, F={"-S", "-Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
},
{ "IN   B,(C) zero", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "B", "(C)")
    end,
    { B = 0, C = 0x34, F={"-S", "Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0
            end,
            "INPUT DATA")
    end
},
{ "IN   B,(C) no input", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "B", "(C)")
    end,
    { B = 0xFF, C = 0x34, F={"S", "-Z", "-H", "V", "-N", "-C"} },
    function(CPU, JIT)
        CPU:register_input(0xff, 0x22, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
    },
{ "IN   B,(C) single bit", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x9921)
        z:assemble("IN", "B", "(C)")
    end,
    { B = 0x01, C = 0x21, F={"-S", "-Z", "-H", "-V", "-N", "-C"}}, 
    function(CPU, JIT)
        CPU:register_input(0x02, 0x00, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x99 or l ~= 0x21 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x01
            end,
            "INPUT DATA")
    end
},


--0xED 0x48
{ "IN   C,(C)", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "C", "(C)")
    end,
    { C = 0x33, B=0x12, F={"-S", "-Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
},
{ "IN   C,(C) zero", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "C", "(C)")
    end,
    { C = 0, B=0x12, F={"-S", "Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0
            end,
            "INPUT DATA")
    end
},
{ "IN   C,(C) no input", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "C", "(C)")
    end,
    { C = 0xFF, B=0x12, F={"S", "-Z", "-H", "V", "-N", "-C"} },
    function(CPU, JIT)
        CPU:register_input(0xff, 0x22, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
    },
{ "IN   C,(C) single bit", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x9921)
        z:assemble("IN", "C", "(C)")
    end,
    { C = 0x01, B=0x99, F={"-S", "-Z", "-H", "-V", "-N", "-C"}}, 
    function(CPU, JIT)
        CPU:register_input(0x02, 0x00, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x99 or l ~= 0x21 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x01
            end,
            "INPUT DATA")
    end
},


--0xED 0x50
{ "IN   D,(C)", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "D", "(C)")
    end,
    { D = 0x33, B=0x12, C = 0x34, F={"-S", "-Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
},
{ "IN   D,(C) zero", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "D", "(C)")
    end,
    { D = 0, B=0x12, C = 0x34, F={"-S", "Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0
            end,
            "INPUT DATA")
    end
},
{ "IN   D,(C) no input", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "D", "(C)")
    end,
    { D = 0xFF, B=0x12, C = 0x34, F={"S", "-Z", "-H", "V", "-N", "-C"} },
    function(CPU, JIT)
        CPU:register_input(0xff, 0x22, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
    },
{ "IN   D,(C) single bit", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x9921)
        z:assemble("IN", "D", "(C)")
    end,
    { D = 0x01, B=0x99, C = 0x21, F={"-S", "-Z", "-H", "-V", "-N", "-C"}}, 
    function(CPU, JIT)
        CPU:register_input(0x02, 0x00, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x99 or l ~= 0x21 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x01
            end,
            "INPUT DATA")
    end
},


--0xED 0x58
{ "IN   E,(C)", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "E", "(C)")
    end,
    { E = 0x33, B=0x12, C = 0x34, F={"-S", "-Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
},
{ "IN   E,(C) zero", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "E", "(C)")
    end,
    { E = 0, B=0x12, C = 0x34, F={"-S", "Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0
            end,
            "INPUT DATA")
    end
},
{ "IN   E,(C) no input", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "E", "(C)")
    end,
    { E = 0xFF, B=0x12, C = 0x34, F={"S", "-Z", "-H", "V", "-N", "-C"} },
    function(CPU, JIT)
        CPU:register_input(0xff, 0x22, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
    },
{ "IN   E,(C) single bit", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x9921)
        z:assemble("IN", "E", "(C)")
    end,
    { E = 0x01, B=0x99, C = 0x21, F={"-S", "-Z", "-H", "-V", "-N", "-C"}}, 
    function(CPU, JIT)
        CPU:register_input(0x02, 0x00, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x99 or l ~= 0x21 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x01
            end,
            "INPUT DATA")
    end
},


--0xED 0x60
{ "IN   H,(C)", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "H", "(C)")
    end,
    { H = 0x33, B=0x12, C = 0x34, F={"-S", "-Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
},
{ "IN   H,(C) zero", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "H", "(C)")
    end,
    { H = 0, B=0x12, C = 0x34, F={"-S", "Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0
            end,
            "INPUT DATA")
    end
},
{ "IN   H,(C) no input", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "H", "(C)")
    end,
    { H = 0xFF, B=0x12, C = 0x34, F={"S", "-Z", "-H", "V", "-N", "-C"} },
    function(CPU, JIT)
        CPU:register_input(0xff, 0x22, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
    },
{ "IN   H,(C) single bit", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x9921)
        z:assemble("IN", "H", "(C)")
    end,
    { H = 0x01, B=0x99, C = 0x21, F={"-S", "-Z", "-H", "-V", "-N", "-C"}}, 
    function(CPU, JIT)
        CPU:register_input(0x02, 0x00, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x99 or l ~= 0x21 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x01
            end,
            "INPUT DATA")
    end
},


--0xED 0x68
{ "IN   L,(C)", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "L", "(C)")
    end,
    { L = 0x33, B=0x12, C = 0x34, F={"-S", "-Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
},
{ "IN   L,(C) zero", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "L", "(C)")
    end,
    { L = 0, B=0x12, C = 0x34, F={"-S", "Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0
            end,
            "INPUT DATA")
    end
},
{ "IN   L,(C) no input", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "L", "(C)")
    end,
    { L = 0xFF, B=0x12, C = 0x34, F={"S", "-Z", "-H", "V", "-N", "-C"} },
    function(CPU, JIT)
        CPU:register_input(0xff, 0x22, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
    },
{ "IN   L,(C) single bit", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x9921)
        z:assemble("IN", "L", "(C)")
    end,
    { L = 0x01, B =0x99, C = 0x21, F={"-S", "-Z", "-H", "-V", "-N", "-C"}}, 
    function(CPU, JIT)
        CPU:register_input(0x02, 0x00, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x99 or l ~= 0x21 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x01
            end,
            "INPUT DATA")
    end
},


--0xED 0x70
{ "IN   F,(C)", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "F", "(C)")
    end,
    { B = 0x12, C = 0x34, F={"-S", "-Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
},
{ "IN   F,(C) zero", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "F", "(C)")
    end,
    { B = 0x12, C = 0x34, F={"-S", "Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0
            end,
            "INPUT DATA")
    end
},
{ "IN   F,(C) no input", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "F", "(C)")
    end,
    { B = 0x12, C = 0x34, F={"S", "-Z", "-H", "V", "-N", "-C"} },
    function(CPU, JIT)
        CPU:register_input(0xff, 0x22, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
    },
{ "IN   F,(C) single bit", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x9921)
        z:assemble("IN", "F", "(C)")
    end,
    { B = 0x99, C = 0x21, F={"-S", "-Z", "-H", "-V", "-N", "-C"}}, 
    function(CPU, JIT)
        CPU:register_input(0x02, 0x00, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x99 or l ~= 0x21 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x01
            end,
            "INPUT DATA")
    end
},


--0xED 0x78
{ "IN   A,(C)", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "A", "(C)")
    end,
    { A = 0x33, B=0x12, C = 0x34, F={"-S", "-Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
},
{ "IN   A,(C) zero", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "A", "(C)")
    end,
    { A = 0, B=0x12, C = 0x34, F={"-S", "Z", "-H", "V", "-N", "C"} }, 
    function(CPU, JIT)
        CPU:register_input(0xff, 0x34, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0
            end,
            "INPUT DATA")
    end
},
{ "IN   A,(C) no input", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x1234)
        z:assemble("IN", "A", "(C)")
    end,
    { A = 0xFF, B=0x12, C = 0x34, F={"S", "-Z", "-H", "V", "-N", "-C"} },
    function(CPU, JIT)
        CPU:register_input(0xff, 0x22, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x12 or l ~= 0x34 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x33
            end,
            "INPUT DATA")
    end
    },
{ "IN   A,(C) single bit", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x9921)
        z:assemble("IN", "A", "(C)")
    end,
    { A = 0x01, B=0x99, C = 0x21, F={"-S", "-Z", "-H", "-V", "-N", "-C"}}, 
    function(CPU, JIT)
        CPU:register_input(0x02, 0x00, 
            function(ud, h, l) 
                if ud ~= "INPUT DATA" or h ~= 0x99 or l ~= 0x21 then
                    print("IN TEST FAILED: ", ud, h, l) 
                    os.exit(1)
                end
                return 0x01
            end,
            "INPUT DATA")
    end
},


-- 0x41
{ "OUT (C), B no outputs", function(z, CPU)
        z:LD("BC", 0x2212)
        z:assemble("OUT","(C)", "B")
        end, { B = 0x22, C = 0x12 } },

{ "OUT (C), B checked", function(z)
        z:LD("BC", 0x22FE)
        z:assemble("OUT","(C)", "B")
        end, 
        { B = 0x22, C=0xFE, IX=0x22 },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x22 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                "OUTPUT DATA")
        end
    },
    
{ "OUT (C), B checked, not output", function(z)
        z:LD("BC", 0x2222)
        z:assemble("OUT","(C)", "B")
        end, 
        { B = 0x22, C=0x22 },
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x22 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        print("Test OUTPUTTED unexpectedly")
                        os.exit(1)
                    end
                end,
                "OUTPUT DATA")
        end
    },
    

{ "OUT (C), B single bit checked", function(z)
        z:LD("BC", 0x7773)
        z:assemble("OUT","(C)", "B")
        end, 
        { B = 0x77, C=0x73, IX=0x77 },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0x80, 0x00, 
                function(ud, h, l ,d) 
                    if ud ~= 1234 or h ~= 0x77 or l ~= 0x73 or d ~= 0x77 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                1234)
        end
    },

-- 0x49
{ "OUT (C), C no outputs", function(z, CPU)
        z:LD("BC", 0x2212)
        z:assemble("OUT","(C)", "C")
        end, { B = 0x22, C = 0x12 } },

{ "OUT (C), C checked", function(z)
        z:LD("BC", 0x22FE)
        z:assemble("OUT","(C)", "C")
        end, 
        { B = 0x22, C=0xFE, IX=0xFE },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0xFE then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                "OUTPUT DATA")
        end
    },
    
{ "OUT (C), C checked, not output", function(z)
        z:LD("BC", 0x2222)
        z:assemble("OUT","(C)", "C")
        end, 
        { B = 0x22, C=0x22 },
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x22 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        print("Test OUTPUTTED unexpectedly")
                        os.exit(1)
                    end
                end,
                "OUTPUT DATA")
        end
    },
    

{ "OUT (C), C single bit checked", function(z)
        z:LD("BC", 0x7773)
        z:assemble("OUT","(C)", "C")
        end, 
        { B = 0x77, C=0x73, IX=0x73 },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0x80, 0x00, 
                function(ud, h, l ,d) 
                    if ud ~= 1234 or h ~= 0x77 or l ~= 0x73 or d ~= 0x73 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                1234)
        end
    },


-- 0x51
{ "OUT (C), D no outputs", function(z, CPU)
        z:LD("BC", 0x2212)
        z:LD("D", 0x36)
        z:assemble("OUT","(C)", "D")
        end, { B = 0x22, C = 0x12, D=0x36 } },

{ "OUT (C), D checked", function(z)
        z:LD("BC", 0x22FE)
        z:LD("D", 0x37)
        z:assemble("OUT","(C)", "D")
        end, 
        { B = 0x22, C=0xFE, IX=0x37, D=0x37 },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x37 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                "OUTPUT DATA")
        end
    },
    
{ "OUT (C), D checked, not output", function(z)
        z:LD("BC", 0x2222)
        z:LD("D", 0x38)
        z:assemble("OUT","(C)", "D")
        end, 
        { B = 0x22, C=0x22, D=0x38 },
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x38 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        print("Test OUTPUTTED unexpectedly")
                        os.exit(1)
                    end
                end,
                "OUTPUT DATA")
        end
    },
    

{ "OUT (C), D single bit checked", function(z)
        z:LD("BC", 0x7773)
        z:LD("D", 0x3A)
        z:assemble("OUT","(C)", "D")
        end, 
        { B = 0x77, C=0x73, IX=0x3A, D=0x3A },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0x80, 0x00, 
                function(ud, h, l ,d) 
                    if ud ~= 1234 or h ~= 0x77 or l ~= 0x73 or d ~= 0x3A then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                1234)
        end
    },



-- 0x59
{ "OUT (C), E no outputs", function(z, CPU)
        z:LD("BC", 0x2212)
        z:LD("E", 0x36)
        z:assemble("OUT","(C)", "E")
        end, { B = 0x22, C = 0x12, E=0x36 } },

{ "OUT (C), E checked", function(z)
        z:LD("BC", 0x22FE)
        z:LD("E", 0x37)
        z:assemble("OUT","(C)", "E")
        end, 
        { B = 0x22, C=0xFE, IX=0x37, E=0x37 },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x37 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                "OUTPUT DATA")
        end
    },
    
{ "OUT (C), E checked, not output", function(z)
        z:LD("BC", 0x2222)
        z:LD("E", 0x38)
        z:assemble("OUT","(C)", "E")
        end, 
        { B = 0x22, C=0x22, E=0x38 },
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x38 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        print("Test OUTPUTTED unexpectedly")
                        os.exit(1)
                    end
                end,
                "OUTPUT DATA")
        end
    },
    

{ "OUT (C), E single bit checked", function(z)
        z:LD("BC", 0x7773)
        z:LD("E", 0x3A)
        z:assemble("OUT","(C)", "E")
        end, 
        { B = 0x77, C=0x73, IX=0x3A, E=0x3A },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0x80, 0x00, 
                function(ud, h, l ,d) 
                    if ud ~= 1234 or h ~= 0x77 or l ~= 0x73 or d ~= 0x3A then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                1234)
        end
    },


-- 0x61
{ "OUT (C), H no outputs", function(z, CPU)
        z:LD("BC", 0x2212)
        z:LD("H", 0x36)
        z:assemble("OUT","(C)", "H")
        end, { B = 0x22, C = 0x12, H=0x36 } },

{ "OUT (C), H checked", function(z)
        z:LD("BC", 0x22FE)
        z:LD("H", 0x37)
        z:assemble("OUT","(C)", "H")
        end, 
        { B = 0x22, C=0xFE, IX=0x37, H=0x37 },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x37 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                "OUTPUT DATA")
        end
    },
    
{ "OUT (C), H checked, not output", function(z)
        z:LD("BC", 0x2222)
        z:LD("H", 0x38)
        z:assemble("OUT","(C)", "H")
        end, 
        { B = 0x22, C=0x22, H=0x38 },
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x38 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        print("Test OUTPUTTED unexpectedly")
                        os.exit(1)
                    end
                end,
                "OUTPUT DATA")
        end
    },
    

{ "OUT (C), H single bit checked", function(z)
        z:LD("BC", 0x7773)
        z:LD("H", 0x3A)
        z:assemble("OUT","(C)", "H")
        end, 
        { B = 0x77, C=0x73, IX=0x3A, H=0x3A },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0x80, 0x00, 
                function(ud, h, l ,d) 
                    if ud ~= 1234 or h ~= 0x77 or l ~= 0x73 or d ~= 0x3A then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                1234)
        end
    },



-- 0x69
{ "OUT (C), L no outputs", function(z, CPU)
        z:LD("BC", 0x2212)
        z:LD("L", 0x36)
        z:assemble("OUT","(C)", "L")
        end, { B = 0x22, C = 0x12, L=0x36 } },

{ "OUT (C), L checked", function(z)
        z:LD("BC", 0x22FE)
        z:LD("L", 0x37)
        z:assemble("OUT","(C)", "L")
        end, 
        { B = 0x22, C=0xFE, IX=0x37, L=0x37 },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x37 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                "OUTPUT DATA")
        end
    },
    
{ "OUT (C), L checked, not output", function(z)
        z:LD("BC", 0x2222)
        z:LD("L", 0x38)
        z:assemble("OUT","(C)", "L")
        end, 
        { B = 0x22, C=0x22, L=0x38 },
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x38 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        print("Test OUTPUTTED unexpectedly")
                        os.exit(1)
                    end
                end,
                "OUTPUT DATA")
        end
    },
    

{ "OUT (C), L single bit checked", function(z)
        z:LD("BC", 0x7773)
        z:LD("L", 0x3A)
        z:assemble("OUT","(C)", "L")
        end, 
        { B = 0x77, C=0x73, IX=0x3A, L=0x3A },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0x80, 0x00, 
                function(ud, h, l ,d) 
                    if ud ~= 1234 or h ~= 0x77 or l ~= 0x73 or d ~= 0x3A then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                1234)
        end
    },


-- 0x79
{ "OUT (C), A no outputs", function(z, CPU)
        z:LD("BC", 0x2212)
        z:LD("A", 0x36)
        z:assemble("OUT","(C)", "A")
        end, { B = 0x22, C = 0x12, A=0x36 } },

{ "OUT (C), A checked", function(z)
        z:LD("BC", 0x22FE)
        z:LD("A", 0x37)
        z:assemble("OUT","(C)", "A")
        end, 
        { B = 0x22, C=0xFE, IX=0x37, A=0x37 },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x37 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                "OUTPUT DATA")
        end
    },
    
{ "OUT (C), A checked, not output", function(z)
        z:LD("BC", 0x2222)
        z:LD("A", 0x38)
        z:assemble("OUT","(C)", "A")
        end, 
        { B = 0x22, C=0x22, A=0x38 },
        function (CPU, JIT)
            CPU:register_output(0xff, 254, 
                function(ud, h, l ,d) 
                    if ud ~= "OUTPUT DATA" or h ~= 0x22 or l ~= 0xFE or d ~= 0x38 then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        print("Test OUTPUTTED unexpectedly")
                        os.exit(1)
                    end
                end,
                "OUTPUT DATA")
        end
    },
    

{ "OUT (C), A single bit checked", function(z)
        z:LD("BC", 0x7773)
        z:LD("A", 0x3A)
        z:assemble("OUT","(C)", "A")
        end, 
        { B = 0x77, C=0x73, IX=0x3A, A=0x3A },  -- we hack the CPU state to automatically test the output worked
        function (CPU, JIT)
            CPU:register_output(0x80, 0x00, 
                function(ud, h, l ,d) 
                    if ud ~= 1234 or h ~= 0x77 or l ~= 0x73 or d ~= 0x3A then
                        print("OUT TEST FAILED: ", ud, h, l, d) 
                        os.exit(1)
                    else
                        CPU.IX = d
                    end
                end,
                1234)
        end
    },

-- 0xED 0x42
{ "SBC  HL,BC", function(z)
        z:assemble("SCF")
        z:LD("BC", 0x1234)
        z:LD("HL", 0x3478)
        z:assemble("SBC", "HL", "BC")
        end, { H = 0x22, L = 0x43, B=0x12, C=0x34, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },

{ "SBC  HL,BC  BC=FFFF", function(z)
        z:assemble("SCF")
        z:LD("BC", 0xFFFF)
        z:LD("HL", 0x0002)
        z:assemble("SBC", "HL", "BC")
        end, { H = 0x00, L = 0x02, B=0xFF, C=0xFF, F={ "-S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,BC  BC=FFFE", function(z)
        z:assemble("SCF")
        z:LD("BC", 0xFFFE)
        z:LD("HL", 0x0002)
        z:assemble("SBC", "HL", "BC")
        end, { H = 0x00, L = 0x03, B=0xFF, C=0xFE, F={ "-S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,BC  BC=0000", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x0000)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "BC")
        end, { H = 0x00, L = 0x00, B=0x00, C=0x00, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },

{ "SBC  HL,BC  BC=0001", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x0001)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "BC")
        end, { H = 0xFF, L = 0xFF, B=0x00, C=0x01, F={ "S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,BC  BC=4000", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x4000)
        z:LD("HL", 0x8000)
        z:assemble("SBC", "HL", "BC")
        end, { H = 0x40, L = 0x00, B=0x40, C=0x00, F={ "-S", "-Z", "-H", "V", "N", "-C" } } },

{ "SBC  HL,BC  BC=7FFF", function(z)
        z:assemble("SCF")
        z:assemble("CCF")

        z:LD("BC", 0x7FFF)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "BC")
        end, { H = 0x80, L = 0x01, B=0x7F, C=0xFF, F={ "S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,BC  BC=8000", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("BC", 0x8000)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "BC")   -- 0 - (-0x8000) = (0 + 0x8000) = overflow (since we can't represent 0x8000 as a *signed* number)
        end, { H = 0x80, L = 0x00, B=0x80, C=0x00, F={ "S", "-Z", "-H", "V", "N", "C" } } },


-- 0xED 0x52
{ "SBC  HL,DE", function(z)
        z:assemble("SCF")
        z:LD("DE", 0x1234)
        z:LD("HL", 0x3478)
        z:assemble("SBC", "HL", "DE")
        end, { H = 0x22, L = 0x43, D=0x12, E=0x34, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },

{ "SBC  HL,DE  DE=FFFF", function(z)
        z:assemble("SCF")
        z:LD("DE", 0xFFFF)
        z:LD("HL", 0x0002)
        z:assemble("SBC", "HL", "DE")
        end, { H = 0x00, L = 0x02, D=0xFF, E=0xFF, F={ "-S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,DE  DE=FFFE", function(z)
        z:assemble("SCF")
        z:LD("DE", 0xFFFE)
        z:LD("HL", 0x0002)
        z:assemble("SBC", "HL", "DE")
        end, { H = 0x00, L = 0x03, D=0xFF, E=0xFE, F={ "-S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,DE  DE=0000", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("DE", 0x0000)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "DE")
        end, { H = 0x00, L = 0x00, D=0x00, E=0x00, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },

{ "SBC  HL,DE  DE=0001", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("DE", 0x0001)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "DE")
        end, { H = 0xFF, L = 0xFF, D=0x00, E=0x01, F={ "S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,DE  DE=4000", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("DE", 0x4000)
        z:LD("HL", 0x8000)
        z:assemble("SBC", "HL", "DE")
        end, { H = 0x40, L = 0x00, D=0x40, E=0x00, F={ "-S", "-Z", "-H", "V", "N", "-C" } } },

{ "SBC  HL,DE  DE=7FFF", function(z)
        z:assemble("SCF")
        z:assemble("CCF")

        z:LD("DE", 0x7FFF)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "DE")
        end, { H = 0x80, L = 0x01, D=0x7F, E=0xFF, F={ "S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,DE  DE=8000", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("DE", 0x8000)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "DE")   -- 0 - (-0x8000) = (0 + 0x8000) = overflow (since we can't represent 0x8000 as a *signed* number)
        end, { H = 0x80, L = 0x00, D=0x80, E=0x00, F={ "S", "-Z", "-H", "V", "N", "C" } } },


-- 0xED 0x62
{ "SBC  HL,HL RCF", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("HL", 0x1234)
        z:assemble("SBC", "HL", "HL")
    end, { H = 0x00, L = 0x00, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },

-- 0xED 0x62
{ "SBC  HL,HL SCF", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x1234)
        z:assemble("SBC", "HL", "HL")
    end, { H = 0xFF, L = 0xFF, F={ "S", "-Z", "H", "-V", "N", "C" } } },

-- 0xED 0x62
{ "SBC  HL,HL 01", function(z)
        z:assemble("SCF")
        z:LD("HL", 0xFFFF)
        z:assemble("SBC", "HL", "HL")
    end, { H = 0xFF, L = 0xFF, F={ "S", "-Z", "H", "-V", "N", "C" } } },



-- 0xED 0x72
{ "SBC  HL,SP", function(z)
        z:assemble("SCF")
        z:LD("SP", 0x1234)
        z:LD("HL", 0x3478)
        z:assemble("SBC", "HL", "SP")
        end, { H = 0x22, L = 0x43, SP=0x1234, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },

{ "SBC  HL,SP  SP=FFFF", function(z)
        z:assemble("SCF")
        z:LD("SP", 0xFFFF)
        z:LD("HL", 0x0002)
        z:assemble("SBC", "HL", "SP")
        end, { H = 0x00, L = 0x02, SP=0xFFFF, F={ "-S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,SP  SP=FFFE", function(z)
        z:assemble("SCF")
        z:LD("SP", 0xFFFE)
        z:LD("HL", 0x0002)
        z:assemble("SBC", "HL", "SP")
        end, { H = 0x00, L = 0x03, SP=0xFFFE, F={ "-S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,SP  SP=0000", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("SP", 0x0000)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "SP")
        end, { H = 0x00, L = 0x00, SP=0x0000, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },

{ "SBC  HL,SP  SP=0001", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("SP", 0x0001)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "SP")
        end, { H = 0xFF, L = 0xFF, SP=0x0001, F={ "S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,SP  SP=4000", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("SP", 0x4000)
        z:LD("HL", 0x8000)
        z:assemble("SBC", "HL", "SP")
        end, { H = 0x40, L = 0x00, SP=0x4000, F={ "-S", "-Z", "-H", "V", "N", "-C" } } },

{ "SBC  HL,SP  SP=7FFF", function(z)
        z:assemble("SCF")
        z:assemble("CCF")

        z:LD("SP", 0x7FFF)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "SP")
        end, { H = 0x80, L = 0x01, SP=0x7FFF, F={ "S", "-Z", "H", "-V", "N", "C" } } },

{ "SBC  HL,SP  SP=8000", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("SP", 0x8000)
        z:LD("HL", 0x0000)
        z:assemble("SBC", "HL", "SP")   -- 0 - (-0x8000) = (0 + 0x8000) = overflow (since we can't represent 0x8000 as a *signed* number)
        end, { H = 0x80, L = 0x00, SP=0x8000, F={ "S", "-Z", "-H", "V", "N", "C" } } },



-- 0xED 0x43
{ "LD   (xxxx),BC", function(z)
        z:LD("BC", 0x1234)
        z:LD("(0x6000)", "BC")
    end, { B = 0x12, C = 0x34, [0x6000]=0x34, [0x6001]=0x12 } },
    
-- 0xED 0x44
{ "NEG", function(z)
        z:LD("A", 0x01)
        z:assemble("NEG")
    end,
    { A = 0xFF, F={ "S", "-Z", "H", "-V", "N", "C" } } },
{ "NEG", function(z)
        z:LD("A", 0x00)
        z:assemble("NEG")
    end,
    { A = 0x00, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "NEG", function(z)
        z:LD("A", 0x80)
        z:assemble("NEG")
    end,
    { A = 0x80, F={ "S", "-Z", "-H", "V", "N", "C" } } },
{ "NEG", function(z)
        z:LD("A", 0xFE)
        z:assemble("NEG")
    end,
    { A = 0x02, F={ "-S", "-Z", "H", "-V", "N", "C" } } },


-- 0xED 0x4A
{  "ADC HL, BC", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x1234)
        z:LD("BC", 0x1122)
        z:assemble("ADC", "HL", "BC")
    end, { B=0x11, C=0x22, H=0x23, L=0x56, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, BC carry test", function(z)
        z:assemble("SCF");
        z:LD("HL", 0x1234)
        z:LD("BC", 0x1122)
        z:assemble("ADC", "HL", "BC")
    end, { B=0x11, C=0x22, H=0x23, L=0x57, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, BC zero test carry half", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0xFFFF)
        z:LD("BC", 0x0001)
        z:assemble("ADC", "HL", "BC")
    end, { B=0x00, C=0x01, H=0x00, L=0x00, F={ "-S", "Z", "H", "-V", "-N", "C" } } },
{  "ADC HL, BC not zero test", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0FFF)
        z:LD("BC", 0x0001)
        z:assemble("ADC", "HL", "BC")
    end, { B=0x00, C=0x01, H=0x10, L=0x00, F={ "-S", "-Z", "H", "-V", "-N", "-C" } } },
{  "ADC HL, BC not zero test 2", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0000)
        z:LD("BC", 0x0100)
        z:assemble("ADC", "HL", "BC")
    end, { B=0x01, C=0x00, H=0x01, L=0x00, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, BC not zero test 3", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0000)
        z:LD("BC", 0x0001)
        z:assemble("ADC", "HL", "BC")
    end, { B=0x00, C=0x01, H=0x00, L=0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, BC sign test", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0FFF)
        z:LD("BC", 0xF000)
        z:assemble("ADC", "HL", "BC")
    end, { B=0xF0, C=0x00, H=0xFF, L=0xFF, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "ADC HL, BC overflow test", function(z)
        z:assemble("SCF");
        z:LD("HL", 0x7FFE)
        z:LD("BC", 0x0001)
        z:assemble("ADC", "HL", "BC")
    end, { B=0x00, C=0x01, H=0x80, L=0x00, F={ "S", "-Z", "H", "V", "-N", "-C" } } },

-- 0xED 0x5A
{  "ADC HL, DE", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x1234)
        z:LD("DE", 0x1122)
        z:assemble("ADC", "HL", "DE")
    end, { D=0x11, E=0x22, H=0x23, L=0x56, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, DE carry test", function(z)
        z:assemble("SCF");
        z:LD("HL", 0x1234)
        z:LD("DE", 0x1122)
        z:assemble("ADC", "HL", "DE")
    end, { D=0x11, E=0x22, H=0x23, L=0x57, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, DE zero test carry half", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0xFFFF)
        z:LD("DE", 0x0001)
        z:assemble("ADC", "HL", "DE")
    end, { D=0x00, E=0x01, H=0x00, L=0x00, F={ "-S", "Z", "H", "-V", "-N", "C" } } },
{  "ADC HL, DE not zero test", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0FFF)
        z:LD("DE", 0x0001)
        z:assemble("ADC", "HL", "DE")
    end, { D=0x00, E=0x01, H=0x10, L=0x00, F={ "-S", "-Z", "H", "-V", "-N", "-C" } } },
{  "ADC HL, DE not zero test 2", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0000)
        z:LD("DE", 0x0100)
        z:assemble("ADC", "HL", "DE")
    end, { D=0x01, E=0x00, H=0x01, L=0x00, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, DE not zero test 3", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0000)
        z:LD("DE", 0x0001)
        z:assemble("ADC", "HL", "DE")
    end, { D=0x00, E=0x01, H=0x00, L=0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, DE sign test", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0FFF)
        z:LD("DE", 0xF000)
        z:assemble("ADC", "HL", "DE")
    end, { D=0xF0, E=0x00, H=0xFF, L=0xFF, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "ADC HL, DE overflow test", function(z)
        z:assemble("SCF");
        z:LD("HL", 0x7FFE)
        z:LD("DE", 0x0001)
        z:assemble("ADC", "HL", "DE")
    end, { D=0x00, E=0x01, H=0x80, L=0x00, F={ "S", "-Z", "H", "V", "-N", "-C" } } },

-- 0xED 0x6A
{  "ADC HL, HL", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x1234)
        z:assemble("ADC", "HL", "HL")
    end, { H=0x24, L=0x68, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, HL carry test", function(z)
        z:assemble("SCF");
        z:LD("HL", 0x1234)
        z:assemble("ADC", "HL", "HL")
    end, { H=0x24, L=0x69, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, HL zero test", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x8000)
        z:assemble("ADC", "HL", "HL")
    end, { H=0x00, L=0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{  "ADC HL, HL not zero test", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0800)
        z:assemble("ADC", "HL", "HL")
    end, { H=0x10, L=0x00, F={ "-S", "-Z", "H", "-V", "-N", "-C" } } },
{  "ADC HL, HL not zero test 2", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0080)
        z:assemble("ADC", "HL", "HL")
    end, { H=0x01, L=0x00, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, HL not zero test 3", function(z)
        z:assemble("SCF");
        z:LD("HL", 0x0000)
        z:assemble("ADC", "HL", "HL")
    end, { H=0x00, L=0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, HL sign test", function(z)
        z:assemble("SCF");
        z:LD("HL", 0xFFFF)
        z:assemble("ADC", "HL", "HL")
    end, { H=0xFF, L=0xFF, F={ "S", "-Z", "H", "-V", "-N", "C" } } },
{ "ADC HL, HL overflow test", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x4000)
        z:assemble("ADC", "HL", "HL")
    end, { H=0x80, L=0x00, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0xED 0x7A
{  "ADC HL, SP", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x1234)
        z:LD("SP", 0x1122)
        z:assemble("ADC", "HL", "SP")
    end, { SP=0x1122, H=0x23, L=0x56, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, SP carry test", function(z)
        z:assemble("SCF");
        z:LD("HL", 0x1234)
        z:LD("SP", 0x1122)
        z:assemble("ADC", "HL", "SP")
    end, { SP=0x1122, H=0x23, L=0x57, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, SP zero test carry half", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0xFFFF)
        z:LD("SP", 0x0001)
        z:assemble("ADC", "HL", "SP")
    end, { SP=0x0001, H=0x00, L=0x00, F={ "-S", "Z", "H", "-V", "-N", "C" } } },
{  "ADC HL, SP not zero test", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0FFF)
        z:LD("SP", 0x0001)
        z:assemble("ADC", "HL", "SP")
    end, { SP=0x0001, H=0x10, L=0x00, F={ "-S", "-Z", "H", "-V", "-N", "-C" } } },
{  "ADC HL, SP not zero test 2", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0000)
        z:LD("SP", 0x0100)
        z:assemble("ADC", "HL", "SP")
    end, { SP=0x0100, H=0x01, L=0x00, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, SP not zero test 3", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0000)
        z:LD("SP", 0x0001)
        z:assemble("ADC", "HL", "SP")
    end, { SP=0x0001, H=0x00, L=0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{  "ADC HL, SP sign test", function(z)
        z:assemble("SCF");
        z:assemble("CCF");
        z:LD("HL", 0x0FFF)
        z:LD("SP", 0xF000)
        z:assemble("ADC", "HL", "SP")
    end, { SP=0xF000, H=0xFF, L=0xFF, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "ADC HL, SP overflow test", function(z)
        z:assemble("SCF");
        z:LD("HL", 0x7FFE)
        z:LD("SP", 0x0001)
        z:assemble("ADC", "HL", "SP")
    end, { SP=0x0001, H=0x80, L=0x00, F={ "S", "-Z", "H", "V", "-N", "-C" } } },



-- 0xED 0x4B
{   "LD   BC,(xxxx)", function(z)
        z:LD("A", 0x34)
        z:LD("(0x6000)", "A")
        z:LD("A", 0x12)
        z:LD("(0x6001)", "A")
        z:LD("BC", 0x6000)
        z:LD("A", 0xFF)
        z:LD("BC","(0x6000)")
    end, { B=0x12, C=0x34, A=0xFF, [0x6000]=0x34, [0x6001]=0x12 } },

-- 0xED 53
{ "LD (xxxx),DE", function(z)
        z:LD("DE", 0x1234)
        z:LD("(0x6000)", "DE")
    end, { D = 0x12, E = 0x34, [0x6000]=0x34, [0x6001]=0x12 } },
        
-- 0xED 0x5B
{   "LD   DE,(xxxx)", function(z)
        z:LD("A", 0x34)
        z:LD("(0x6000)", "A")
        z:LD("A", 0x12)
        z:LD("(0x6001)", "A")
        z:LD("DE", 0x6000)
        z:LD("A", 0xFF)
        z:LD("DE","(0x6000)")
    end, { D=0x12, E=0x34, A=0xFF, [0x6000]=0x34, [0x6001]=0x12 } },

-- 0xED 63
{ "LD (xxxx),HL", function(z)
        z:LD("HL", 0x1234)
        z:DB(0xED, 0x63, 0x00, 0x60)
    end, { H = 0x12, L = 0x34, [0x6000]=0x34, [0x6001]=0x12 } },

-- 0xED 0x6B
{   "LD   HL,(xxxx) ... ED 6B", function(z)
        z:LD("A", 0x34)
        z:LD("(0x6000)", "A")
        z:LD("A", 0x12)
        z:LD("(0x6001)", "A")
        z:LD("HL", 0x6000)
        z:LD("A", 0xFF)
        z:DB(0xED, 0x6B, 0x00, 0x60) --z:LD("HL","(0x6000)")
    end, { H=0x12, L=0x34, A=0xFF, [0x6000]=0x34, [0x6001]=0x12 } },

-- 0xED 73
{ "LD (xxxx),SP", function(z)
        z:LD("SP", 0x1234)
        z:LD("(0x6000)", "SP")
    end, { SP = 0x1234, [0x6000]=0x34, [0x6001]=0x12 } },

-- 0xED 0x7B
{   "LD   SP,(xxxx)", function(z)
        z:LD("A", 0x34)
        z:LD("(0x6000)", "A")
        z:LD("A", 0x12)
        z:LD("(0x6001)", "A")
        z:LD("SP", 0x6000)
        z:LD("A", 0xFF)
        z:LD("SP","(0x6000)")
    end, { SP=0x1234, A=0xFF, [0x6000]=0x34, [0x6001]=0x12 } },
}

return ED_instruction_tests
