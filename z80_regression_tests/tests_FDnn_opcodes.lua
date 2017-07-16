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

local FD_instruction_tests = {
    -- 0x09
    -- ADD IY, ss ... doesn't affect Z or S or V
    { "ADD IY, BC", function(z) z:LD("IY", 0x1234)
                                z:LD("BC", 0x4320)
                                z:ADD("IY", "BC") end,
                                { IY = 0x5554, B = 0x43, C = 0x20, F = { "-N", "-H", "-C" } } },
    
    { "ADD IY, BC no half-carry", function(z) z:LD("IY", 0x003F)
                                z:LD("BC", 0x003F)
                                    z:ADD("IY", "BC") end,
                                { IY = 0x007E, B = 0x00, C = 0x3F, F = { "-N", "-H", "-C" } } },

    { "ADD IY, BC half-carry", function(z) z:LD("IY", 0x3F00)
                                z:LD("BC", 0x0100)
                                z:ADD("IY", "BC") end,
                                { IY = 0x4000, B = 0x01, C = 0x00, F = { "-N", "H", "-C" } } },
    
    { "ADD IY, BC overflow", function(z) z:LD("IY", 0x8000)
                                z:LD("BC", 0x8000)
                                z:ADD("IY", "BC") end,
                                { IY = 0x0000, B = 0x80, C = 0x00, F = { "-N", "-H", "C" } } },

    { "ADD IY, BC overflow2", function(z) z:LD("IY", 0x1000)
                                z:LD("BC", 0x7000)
                                z:ADD("IY", "BC") end,
                                { IY = 0x8000, B = 0x70, C = 0x00, F = { "-N", "-H", "-C" } } },

    { "ADD IY, BC half and overflow", function(z) z:LD("IY", 0x0001)
                                z:LD("BC", 0xFFFF)
                                z:ADD("IY", "BC") end,
                                { IY = 0x0000, B = 0xFF, C = 0xFF, F = { "-N", "H", "C" } } },
    
    { "ADD IY, BC check no S Z flags", function(z)
                                z:LD("SP", 0x6000)
                                z:LD("IY", 0x0001)
                                z:PUSH("IY")
                                z:POP("AF")
                                z:LD("BC", 0xFFFF)
                                z:ADD("IY", "BC") end,
                                { IY = 0x0000, B = 0xFF, C = 0xFF, A = 0x00, [0x5FFE]=1, [0x5FFF]=0, SP=0x6000, F = { "-S", "-Z", "-V", "-N", "H", "C" } } },

    -- 0x19
    -- ADD IY, ss ... doesn't affect Z or S or V
    { "ADD IY, DE", function(z) z:LD("IY", 0x1234)
                                z:LD("DE", 0x4320)
                                z:ADD("IY", "DE") end,
                                { IY = 0x5554, D = 0x43, E = 0x20, F = { "-N", "-H", "-C" } } },
    
    { "ADD IY, DE no half-carry", function(z) z:LD("IY", 0x003F)
                                z:LD("DE", 0x003F)
                                    z:ADD("IY", "DE") end,
                                { IY = 0x007E, D = 0x00, E = 0x3F, F = { "-N", "-H", "-C" } } },

    { "ADD IY, DE half-carry", function(z) z:LD("IY", 0x3F00)
                                z:LD("DE", 0x0100)
                                z:ADD("IY", "DE") end,
                                { IY = 0x4000, D = 0x01, E = 0x00, F = { "-N", "H", "-C" } } },
    
    { "ADD IY, DE overflow", function(z) z:LD("IY", 0x8000)
                                z:LD("DE", 0x8000)
                                z:ADD("IY", "DE") end,
                                { IY = 0x0000, D = 0x80, E = 0x00, F = { "-N", "-H", "C" } } },

    { "ADD IY, DE overflow2", function(z) z:LD("IY", 0x1000)
                                z:LD("DE", 0x7000)
                                z:ADD("IY", "DE") end,
                                { IY = 0x8000, D = 0x70, E = 0x00, F = { "-N", "-H", "-C" } } },

    { "ADD IY, DE half and overflow", function(z) z:LD("IY", 0x0001)
                                z:LD("DE", 0xFFFF)
                                z:ADD("IY", "DE") end,
                                { IY = 0x0000, D = 0xFF, E = 0xFF, F = { "-N", "H", "C" } } },
    
    { "ADD IY, DE check no S Z flags", function(z)
                                z:LD("SP", 0x6000)
                                z:LD("IY", 0x0001)
                                z:PUSH("IY")
                                z:POP("AF")
                                z:LD("DE", 0xFFFF)
                                z:ADD("IY", "DE") end,
                                { IY = 0x0000, D = 0xFF, E = 0xFF, A = 0x00, [0x5FFE]=1, [0x5FFF]=0, SP=0x6000, F = { "-S", "-Z", "-V", "-N", "H", "C" } } },

    -- 0x21
    { "LD IY, nn", function(z)
            z:LD("IY", 0x1234)
        end, { IY = 0x1234 } },
    
    -- 0x22
    {"LD   (nn),IY", function(z) z:assemble("LD", "IY", 0x4321) 
            z:assemble("LD", "(0x5555)", "IY") 
            end, { IY=0x4321, [0x5555]=0x21, [0x5556]=0x43 }},
    {"LD   (nn),IY wrap", function(z) z:assemble("LD", "IY", 0x4321) 
            z:assemble("LD", "(0xFFFF)", "IY") 
            end, { IY=0x4321, [0xFFFF]=0x21, [0x0000]=0x43 }},

    -- 0x23
    { "INC IY", function(z)
            z:LD("IY", 0x1234)
            z:assemble("INC", "IY")
        end, { IY = 0x1235 } },
        
    { "INC IY rollover", function(z)
            z:LD("IY", 0xFFFF)
            z:assemble("INC", "IY")
        end, { IY = 0 } },


    -- 0x24
     { "INC  IYH", function(z) z:assemble("LD", "IY", 0x1155)  
            z:assemble("INC", "IYH") end, { IY=0x1255, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

     { "INC  IYH rollover", function(z) z:assemble("LD", "IY", 0xFF66)  
             -- S is set if result is negative; reset otherwise
            -- Z is set if result is zero; reset otherwise
            -- H is set if carry from bit 3; reset otherwise
            -- P/V is set if r was 7FH before operation; reset otherwise
            -- N is reset
            -- C is not affected
            z:assemble("INC", "IYH") end, { IY=0x0066, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

     { "INC  IYH half carry", function(z) z:assemble("LD", "IY", 0x0F77)  
            z:assemble("INC", "IYH") end, { IY=0x1077, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

     { "INC  IYH Flags P/V Sign", function(z) z:assemble("LD", "IY", 0x7F88)  
            z:assemble("INC", "IYH") end, { IY=0x8088, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  

    -- 0x25
    { "DEC  IYH", function(z) z:LD("IY", 0x1133)  
            z:assemble("DEC", "IYH") end, { IY=0x1033, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"} } },  

    { "DEC  IYH to zero", function(z) z:LD("IY", 0x0144)  
            z:assemble("DEC", "IYH") end, { IY=0x0044, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IYH rollover", function(z) z:assemble("LD", "IY", 0x0055)  
             -- S is set if result is negative; reset otherwise
            -- Z is set if result is zero; reset otherwise
            -- H is set if borrow from bit 4, reset otherwise
            -- P/V is set if m was 80H before operation; reset otherwise
            -- N is set
            -- C is not affected
            z:assemble("DEC", "IYH") end, { IY=0xFF55, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IYH half carry", function(z) z:assemble("LD", "IY", 0x1066)  
            z:assemble("DEC", "IYH") end, { IY=0x0F66, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IYH Flags P/V Sign", function(z) z:assemble("LD", "IY", 0x8077)  
            z:assemble("DEC", "IYH") end, { IY=0x7F77, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"} } },  

    -- 0x26
    { "LD IYH, n", function(z)
        z:LD("IY", 0x1234)
        z:LD("IYH", 0x56)
        end, { IY = 0x5634 } },

    -- 0x29
    -- ADD IY, ss ... doesn't affect Z or S or V
    { "ADD IY, IY", function(z) z:LD("IY", 0x1234)
                                z:ADD("IY", "IY") end,
                                { IY = 0x2468, F = { "-N", "-H", "-C" } } },
    
    { "ADD IY, IY no half-carry", function(z) z:LD("IY", 0x003F)
                                    z:ADD("IY", "IY") end,
                                { IY = 0x007E, F = { "-N", "-H", "-C" } } },

    { "ADD IY, IY half-carry", function(z) z:LD("IY", 0x3F00)
                                z:ADD("IY", "IY") end,
                                { IY = 0x7E00, F = { "-N", "H", "-C" } } },
    
    { "ADD IY, IY overflow", function(z) z:LD("IY", 0x8000)
                                z:ADD("IY", "IY") end,
                                { IY = 0x0000, F = { "-N", "-H", "C" } } },

    { "ADD IY, IY overflow2", function(z) z:LD("IY", 0x4000)
                                z:ADD("IY", "IY") end,
                                { IY = 0x8000, F = { "-N", "-H", "-C" } } },

    { "ADD IY, IY half and overflow", function(z) z:LD("IY", 0x8888)
                                z:ADD("IY", "IY") end,
                                { IY = 0x1110, F = { "-N", "H", "C" } } },
    
    { "ADD IY, IY check no S Z flags", function(z)
                                z:LD("SP", 0x6000)
                                z:LD("IY", 0x0001)
                                z:PUSH("IY")
                                z:POP("AF")
                                z:LD("IY", 0x8888)
                                z:ADD("IY", "IY") end,
                                { IY = 0x1110, A = 0x00, [0x5FFE]=1, [0x5FFF]=0, SP=0x6000, F = { "-S", "-Z", "-V", "-N", "H", "C" } } },


    -- 0x2A
    { "LD IY,(nn)", function(z)
            z:LD("A", 0x22) 
            z:LD("(0x6000)", "A") 
            z:LD("A", 0x11) 
            z:LD("(0x6001)", "A")
            z:LD("IY", "(0x6000)")
            end,
        { [0x6000] = 0x22, [0x6001] = 0x11, A = 0x11, IY = 0x1122 } },
    { "LD IY,(nn) rollover", function(z)
            z:LD("A", 0x44) 
            z:LD("(0xFFFF)", "A") 
            z:LD("A", 0x33) 
            z:LD("(0x0000)", "A")
            z:LD("IY", "(0xFFFF)") 
            z:LD("A", 99)
            end,
        { [0xFFFF] = 0x44, [0x0000] = 0x33, A = 99, IY = 0x3344 } },



    -- 0x2B
    { "DEC IY", function(z)
            z:LD("IY", 0x1234)
            z:assemble("DEC", "IY")
        end, { IY = 0x1233 } },
        
    { "DEC IY rollover", function(z)
            z:LD("IY", 0)
            z:assemble("DEC", "IY")
        end, { IY = 0xFFFF } },
    
    
        -- 0x2C
     { "INC  IYL", function(z) z:assemble("LD", "IY", 0xAA11)  
            z:assemble("INC", "IYL") end, { IY=0xAA12, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

     { "INC  IYL rollover", function(z) z:assemble("LD", "IY", 0xBBFF)  
             -- S is set if result is negative; reset otherwise
            -- Z is set if result is zero; reset otherwise
            -- H is set if carry from bit 3; reset otherwise
            -- P/V is set if r was 7FH before operation; reset otherwise
            -- N is reset
            -- C is not affected
            z:assemble("INC", "IYL") end, { IY=0xBB00, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

     { "INC  IYL half carry", function(z) z:assemble("LD", "IY", 0xCC0F)  
            z:assemble("INC", "IYL") end, { IY=0xCC10, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

     { "INC  IYL Flags P/V Sign", function(z) z:assemble("LD", "IY", 0xDD7F)  
            z:assemble("INC", "IYL") end, { IY=0xDD80, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  

    -- 0x2D
    { "DEC  IYL", function(z) z:LD("IY", 0x9911)  
            z:assemble("DEC", "IYL") end, { IY=0x9910, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"} } },  

    { "DEC  IYL to zero", function(z) z:LD("IY", 0x9901)  
            z:assemble("DEC", "IYL") end, { IY=0x9900, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IYL rollover", function(z) z:assemble("LD", "IY", 0x9900)  
             -- S is set if result is negative; reset otherwise
            -- Z is set if result is zero; reset otherwise
            -- H is set if borrow from bit 4, reset otherwise
            -- P/V is set if m was 80H before operation; reset otherwise
            -- N is set
            -- C is not affected
            z:assemble("DEC", "IYL") end, { IY=0x99FF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IYL half carry", function(z) z:assemble("LD", "IY", 0x9910)  
            z:assemble("DEC", "IYL") end, { IY=0x990F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IYL Flags P/V Sign", function(z) z:assemble("LD", "IY", 0x9980)  
            z:assemble("DEC", "IYL") end, { IY=0x997F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"} } },  
    
    -- 0x2E
    { "LD IYL, n", function(z)
            z:LD("IY", 0x1111)
            z:LD("IYL", 0x22)
            end, { IY = 0x1122 } },

    -- 0x34
     { "INC  (IY+0)", function(z) z:assemble("LD", "IY", 0x6000)
        z:LD("A", 0x11)
        z:LD("(0x6000)", "A")
        z:assemble("INC", "(IY+0)") end, { A=0x11, [0x6000]=0x12, IY=0x6000, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

     { "INC  (IY+0) rollover", function(z) z:assemble("LD", "IY", 0x6000)  
        z:LD("A", 0xFF)
        z:LD("(0x6000)", "A")
             -- S is set if result is negative; reset otherwise
            -- Z is set if result is zero; reset otherwise
            -- H is set if carry from bit 3; reset otherwise
            -- P/V is set if r was 7FH before operation; reset otherwise
            -- N is reset
            -- C is not affected
        z:assemble("INC", "(IY+0)") end, { A=0xFF, [0x6000]=0x00, IY=0x6000, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

     { "INC  (IY+0) half carry", function(z) z:assemble("LD", "IY", 0x6000)  
        z:LD("A", 0x0F)
        z:LD("(0x6000)", "A")
        z:assemble("INC", "(IY+0)") end, { A=0x0F, [0x6000]=0x10, IY=0x6000, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

     { "INC  (IY+0) Flags P/V Sign", function(z) z:assemble("LD", "IY", 0x6000)  
        z:LD("A", 0x7F)
        z:LD("(0x6000)", "A")
        z:assemble("INC", "(IY+0)") end, { A=0x7F, [0x6000]=0x80, IY=0x6000, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  

     { "INC  (IY-1)", function(z) z:assemble("LD", "IY", 0x6000)
        z:LD("A", 0x11)
        z:LD("(0x5FFF)", "A")
        z:assemble("INC", "(IY-1)") end, { A=0x11, [0x5FFF]=0x12, IY=0x6000, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  
     { "INC  (IY+2) addr rollover", function(z) z:assemble("LD", "IY", 0xFFFE)
        z:LD("A", 0x11)
        z:LD("(0)", "A")
        z:assemble("INC", "(IY+2)") end, { A=0x11, [0x0000]=0x12, IY=0xFFFE, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  
     { "INC  (IY+127) addr rollover", function(z) z:assemble("LD", "IY", 0x6000)
        z:LD("A", 0x11)
        z:LD("(0x607F)", "A")
        z:assemble("INC", "(IY+127)") end, { A=0x11, [0x607F]=0x12, IY=0x6000, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  
     { "INC  (IY-128) addr rollover", function(z) z:assemble("LD", "IY", 0x6000)
        z:LD("A", 0x11)
        z:LD("(0x5F80)", "A")
        z:assemble("INC", "(IY-128)") end, { A=0x11, [0x5F80]=0x12, IY=0x6000, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

    -- 0x35
    { "DEC  (IY+0)", function(z) z:LD("IY", 0x6000) 
            z:LD("A", 0x11)
            z:LD("(0x6000)", "A")
            z:assemble("DEC", "(IY+0)") end, { A=0x11, [0x6000]=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"}, IY=0x6000 } },  

    { "DEC  (IY+0) to zero", function(z) z:LD("IY", 0x6000)
            z:LD("A", 0x01)
            z:LD("(0x6000)", "A")
            z:assemble("DEC", "(IY+0)") end, { A=0x01, [0x6000]=0x00, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"}, IY=0x6000 } },  

     { "DEC  (IY+0) rollover", function(z) z:LD("IY", 0x6000)
            z:LD("A", 0x00)
            z:LD("(0x6000)", "A")
             -- S is set if result is negative; reset otherwise
            -- Z is set if result is zero; reset otherwise
            -- H is set if borrow from bit 4, reset otherwise
            -- P/V is set if m was 80H before operation; reset otherwise
            -- N is set
            -- C is not affected
            z:assemble("DEC", "(IY+0)") end, { A=0x00, [0x6000]=0xFF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"}, IY=0x6000 } },  

     { "DEC  (IY+0) half carry", function(z) z:LD("IY", 0x6000)
            z:LD("A", 0x10)
            z:LD("(0x6000)", "A")
            z:assemble("DEC", "(IY+0)") end, { A=0x10, [0x6000]=0x0F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"}, IY=0x6000 } },  

     { "DEC  (IY+0) Flags P/V Sign", function(z) z:LD("IY", 0x6000)
            z:LD("A", 0x80)
            z:LD("(0x6000)", "A")
            z:assemble("DEC", "(IY+0)") end, { A=0x80, [0x6000]=0x7F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"}, IY=0x6000 } },  

    { "DEC  (IY-1)", function(z) z:LD("IY", 0x6000) 
            z:LD("A", 0x11)
            z:LD("(0x5FFF)", "A")
            z:assemble("DEC", "(IY-1)") end, { A=0x11, [0x5FFF]=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"}, IY=0x6000 } },  

    { "DEC  (IY+2)", function(z) z:LD("IY", 0x6000) 
            z:LD("A", 0x11)
            z:LD("(0x6002)", "A")
            z:assemble("DEC", "(IY+2)") end, { A=0x11, [0x6002]=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"}, IY=0x6000 } },  

    { "DEC  (IY+127)", function(z) z:LD("IY", 0x6000) 
            z:LD("A", 0x11)
            z:LD("(0x607F)", "A")
            z:assemble("DEC", "(IY+127)") end, { A=0x11, [0x607F]=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"}, IY=0x6000 } },  

    { "DEC  (IY-128)", function(z) z:LD("IY", 0x6000) 
            z:LD("A", 0x11)
            z:LD("(0x5F80)", "A")
            z:assemble("DEC", "(IY-128)") end, { A=0x11, [0x5F80]=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"}, IY=0x6000 } },  

    -- 0x36
    { "LD (IY+0), n", function(z)
         z:LD("IY", 0x5DEF)
         z:LD("(IY+0)", 0x11)
     end, { IY=0x5DEF, [0x5DEF] = 0x11 } },
    { "LD (IY+127), n", function(z)
         z:LD("IY", 0x6000)
         z:LD("(IY+127)", 0x11)
     end, { IY=0x6000, [0x607F] = 0x11 } },
    { "LD (IY-128), n", function(z)
         z:LD("IY", 0x6000)
         z:LD("(IY-128)", 0x11)
     end, { IY=0x6000, [0x5F80] = 0x11 } },

    -- 0x39
    -- ADD IY, ss ... doesn't affect Z or S or V
    { "ADD IY, SP", function(z) z:LD("IY", 0x1234)
                                z:LD("SP", 0x4320)
                                z:ADD("IY", "SP") end,
                                { IY = 0x5554, SP = 0x4320, F = { "-N", "-H", "-C" } } },
    
    { "ADD IY, SP no half-carry", function(z) z:LD("IY", 0x003F)
                                z:LD("SP", 0x003F)
                                z:ADD("IY", "SP") end,
                                { IY = 0x007E, SP = 0x003F, F = { "-N", "-H", "-C" } } },

    { "ADD IY, SP half-carry", function(z) z:LD("IY", 0x3F00)
                                z:LD("SP", 0x0100)
                                z:ADD("IY", "SP") end,
                                { IY = 0x4000, SP = 0x0100, F = { "-N", "H", "-C" } } },
    
    { "ADD IY, SP overflow", function(z) z:LD("IY", 0x8000)
                                z:LD("SP", 0x8000)
                                z:ADD("IY", "SP") end,
                                { IY = 0x0000, SP = 0x8000, F = { "-N", "-H", "C" } } },

    { "ADD IY, SP overflow2", function(z) z:LD("IY", 0x1000)
                                z:LD("SP", 0x7000)
                                z:ADD("IY", "SP") end,
                                { IY = 0x8000, SP = 0x7000, F = { "-N", "-H", "-C" } } },

    { "ADD IY, SP half and overflow", function(z) z:LD("IY", 0x0001)
                                z:LD("SP", 0xFFFF)
                                z:ADD("IY", "SP") end,
                                { IY = 0x0000, SP = 0xFFFF, F = { "-N", "H", "C" } } },
    
    { "ADD IY, SP check no S Z flags", function(z)
                                z:LD("SP", 0x6000)
                                z:LD("IY", 0x0001)
                                z:PUSH("IY")
                                z:POP("AF")
                                z:LD("SP", 0xFFFF)
                                z:ADD("IY", "SP") end,
                                { IY = 0x0000, SP = 0xFFFF, A = 0x00, [0x5FFE]=1, [0x5FFF]=0, F = { "-S", "-Z", "-V", "-N", "H", "C" } } },

    -- 0x44
    { "LD B, IYH", function(z)
            z:LD("IY", 0x1234)
            z:LD("B", "IYH")
        end, { B = 0x12, IY=0x1234 } },
    -- 0x45
    { "LD B, IYL", function(z)
            z:LD("IY", 0x1234)
            z:LD("B", "IYL")
        end, { B = 0x34, IY=0x1234 } },
    -- 0x46
    { "LD B,(IY+0)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7001)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("B", "(IY+0)")  
            end, { A=0x7E, IY=0x7001, B=0x7E, [0x7001]=0x7E } },
    { "LD B,(IY-1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7000)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("B", "(IY-1)")  
            end, { A=0x7E, IY=0x7001, B=0x7E, [0x7000]=0x7E } },
    { "LD B,(IY+1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7002)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("B", "(IY+1)")  
            end, { A=0x7E, IY=0x7001, B=0x7E, [0x7002]=0x7E } },
    { "LD B,(IY+127)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7080)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("B", "(IY+127)")  
            end, { A=0x7E, IY=0x7001, B=0x7E, [0x7080]=0x7E } },
    { "LD B,(IY-128)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x6F81)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("B", "(IY-128)")  
            end, { A=0x7E, IY=0x7001, B=0x7E, [0x6F81]=0x7E } },
    -- 0x4C
    { "LD C, IYH", function(z)
            z:LD("IY", 0x1234)
            z:LD("C", "IYH")
        end, { C = 0x12, IY=0x1234 } },
    -- 0x4D
    { "LD C, IYL", function(z)
            z:LD("IY", 0x1234)
            z:LD("C", "IYL")
        end, { C = 0x34, IY=0x1234 } },
    
   -- 0x4E
    { "LD C,(IY+0)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7001)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("C", "(IY+0)")  
            end, { A=0x7E, IY=0x7001, C=0x7E, [0x7001]=0x7E } },
    { "LD C,(IY-1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7000)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("C", "(IY-1)")  
            end, { A=0x7E, IY=0x7001, C=0x7E, [0x7000]=0x7E } },
    { "LD C,(IY+1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7002)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("C", "(IY+1)")  
            end, { A=0x7E, IY=0x7001, C=0x7E, [0x7002]=0x7E } },
    { "LD C,(IY+127)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7080)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("C", "(IY+127)")  
            end, { A=0x7E, IY=0x7001, C=0x7E, [0x7080]=0x7E } },
    { "LD C,(IY-128)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x6F81)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("C", "(IY-128)")  
            end, { A=0x7E, IY=0x7001, C=0x7E, [0x6F81]=0x7E } },
    
    -- 0x54
    { "LD D, IYH", function(z)
            z:LD("IY", 0x1234)
            z:LD("D", "IYH")
        end, { D = 0x12, IY=0x1234 } },
    
    -- 0x55
    { "LD D, IYL", function(z)
            z:LD("IY", 0x1234)
            z:LD("D", "IYL")
        end, { D = 0x34, IY=0x1234 } },
    
    -- 0x56
    { "LD D,(IY+0)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7001)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("D", "(IY+0)")  
            end, { A=0x7E, IY=0x7001, D=0x7E, [0x7001]=0x7E } },
    { "LD D,(IY-1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7000)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("D", "(IY-1)")  
            end, { A=0x7E, IY=0x7001, D=0x7E, [0x7000]=0x7E } },
    { "LD D,(IY+1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7002)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("D", "(IY+1)")  
            end, { A=0x7E, IY=0x7001, D=0x7E, [0x7002]=0x7E } },
    { "LD D,(IY+127)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7080)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("D", "(IY+127)")  
            end, { A=0x7E, IY=0x7001, D=0x7E, [0x7080]=0x7E } },
    { "LD D,(IY-128)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x6F81)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("D", "(IY-128)")  
            end, { A=0x7E, IY=0x7001, D=0x7E, [0x6F81]=0x7E } },

    -- 0x5C
    { "LD E, IYH", function(z)
            z:LD("IY", 0x1234)
            z:LD("E", "IYH")
        end, { E = 0x12, IY=0x1234 } },
    
    -- 0x5D
    { "LD E, IYL", function(z)
            z:LD("IY", 0x1234)
            z:LD("E", "IYL")
        end, { E = 0x34, IY=0x1234 } },
    
    
    -- 0x5E
    { "LD E,(IY+0)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7001)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("E", "(IY+0)")  
            end, { A=0x7E, IY=0x7001, E=0x7E, [0x7001]=0x7E } },
    { "LD E,(IY-1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7000)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("E", "(IY-1)")  
            end, { A=0x7E, IY=0x7001, E=0x7E, [0x7000]=0x7E } },
    { "LD E,(IY+1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7002)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("E", "(IY+1)")  
            end, { A=0x7E, IY=0x7001, E=0x7E, [0x7002]=0x7E } },
    { "LD E,(IY+127)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7080)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("E", "(IY+127)")  
            end, { A=0x7E, IY=0x7001, E=0x7E, [0x7080]=0x7E } },
    { "LD E,(IY-128)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x6F81)", "A") 
            z:assemble("LD", "IY", 0x7001) 
            z:LD("E", "(IY-128)")  
            end, { A=0x7E, IY=0x7001, E=0x7E, [0x6F81]=0x7E } },

        -- 0x60
    { "LD IYH, B", function(z)
            z:LD("IY", 0x1234)
            z:LD("B", 0x99)
            z:LD("IYH", "B")
        end, { B=0x99, IY=0x9934 } },
    -- 0x61
    { "LD IYH, C", function(z)
            z:LD("IY", 0x1234)
            z:LD("C", 0x99)
            z:LD("IYH", "C")
        end, { C=0x99, IY=0x9934 } },
    
    -- 0x62
    { "LD IYH, D", function(z)
            z:LD("IY", 0x1234)
            z:LD("D", 0x99)
            z:LD("IYH", "D")
        end, { D=0x99, IY=0x9934 } },
    -- 0x63
    { "LD IYH, E", function(z)
            z:LD("IY", 0x1234)
            z:LD("E", 0x99)
            z:LD("IYH", "E")
        end, { E=0x99, IY=0x9934 } },

    -- 0x64
    { "LD IYH, IYH", function(z)
            z:LD("IY", 0x1234)
            z:LD("IYH", "IYH")
        end, { IY=0x1234 } },
    
    -- 0x65
    { "LD IYH, IYL", function(z)
            z:LD("IY", 0x1234)
            z:LD("IYH", "IYL")
        end, { IY=0x3434 } },

    -- 0x67
    { "LD IYH, A", function(z)
            z:LD("IY", 0x1234)
            z:LD("A", 0x99)
            z:LD("IYH", "A")
        end, { A=0x99, IY=0x9934 } },

    -- 0x6C
    { "LD IYL, IYH", function(z)
            z:LD("IY", 0x1234)
            z:LD("IYL", "IYH")
        end, { IY=0x1212 } },

    -- 0x6D
    { "LD IYL, IYL", function(z)
            z:LD("IY", 0x1234)
            z:LD("IYL", "IYL")
        end, { IY=0x1234 } },
    
    -- 0x6F
    { "LD IYL, A", function(z)
            z:LD("IY", 0x1234)
            z:LD("A", 0x99)
            z:LD("IYL", "A")
        end, { A=0x99, IY=0x1299 } },

    -- 0x7C
    { "LD A, IYH", function(z)
            z:LD("IY", 0x1234)
            z:LD("A", "IYH")
        end, { A = 0x12, IY=0x1234 } },
    
    -- 0x7D
    { "LD A, IYL", function(z)
            z:LD("IY", 0x1234)
            z:LD("A", "IYL")
        end, { A = 0x34, IY=0x1234 } },

    -- 0x84
    { "ADD A,IYH", function(z)
            z:LD("IY", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADD", "A", "IYH")
            end, { A = 0x12, IY = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

     { "ADD A, IYH", function(z) z:LD("A", 1) z:LD("IY", 0x200) z:assemble("ADD", "A", "IYH") end,
         { A = 3, IY = 0x200, F={"-S", "-Z", "-H", "-V", "-N", "-C"}} },
     { "ADD A, IYH", function(z) z:LD("A", 0x0F) z:LD("IY", 0x100) z:assemble("ADD", "A", "IYH") end,
         { A = 0x10, IY = 0x100, F={"-S", "-Z", "H", "-V", "-N", "-C"}} },
     { "ADD A, IYH", function(z) z:LD("A", 0x80) z:LD("IY", 0x8000) z:assemble("ADD", "A", "IYH") end,
         { A = 0, IY = 0x8000, F={"-S", "Z", "-H", "V", "-N", "C"}} },
     { "ADD A, IYH", function(z) z:LD("A", 0x7F) z:LD("IY", 0x100) z:assemble("ADD", "A", "IYH") end,
         { A = 0x80, IY = 0x100, F={"S", "-Z", "H", "V", "-N", "-C"}} },
     { "ADD A, IYH", function(z) z:LD("A", 0xFF) z:LD("IY", 0x200) z:assemble("ADD", "A", "IYH") end,
         { A = 1, IY = 0x200, F={"-S", "-Z", "H", "-V", "-N", "C"}} },


    -- 0x85
    { "ADD A,IYL", function(z)
            z:LD("IY", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADD", "A", "IYL")
            end, { A = 0x34, IY = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

     { "ADD A, IYL", function(z) z:LD("A", 1) z:LD("IY", 0x2) z:assemble("ADD", "A", "IYL") end,
         { A = 3, IY = 0x2, F={"-S", "-Z", "-H", "-V", "-N", "-C"}} },
     { "ADD A, IYL", function(z) z:LD("A", 0x0F) z:LD("IY", 0x1) z:assemble("ADD", "A", "IYL") end,
         { A = 0x10, IY = 0x1, F={"-S", "-Z", "H", "-V", "-N", "-C"}} },
     { "ADD A, IYL", function(z) z:LD("A", 0x80) z:LD("IY", 0x80) z:assemble("ADD", "A", "IYL") end,
         { A = 0, IY = 0x80, F={"-S", "Z", "-H", "V", "-N", "C"}} },
     { "ADD A, IYL", function(z) z:LD("A", 0x7F) z:LD("IY", 0x1) z:assemble("ADD", "A", "IYL") end,
         { A = 0x80, IY = 0x1, F={"S", "-Z", "H", "V", "-N", "-C"}} },
     { "ADD A, IYL", function(z) z:LD("A", 0xFF) z:LD("IY", 0x2) z:assemble("ADD", "A", "IYL") end,
         { A = 1, IY = 0x2, F={"-S", "-Z", "H", "-V", "-N", "C"}} },

     -- 0x8C
    { "ADC A,IYH", function(z)
            z:assemble("SCF")
            z:assemble("CCF")
            z:LD("IY", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADC", "A", "IYH")
            end, { A = 0x12, IY = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

     -- 0x8C
    { "ADC A,IYH", function(z)
            z:assemble("SCF")
            z:LD("IY", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADC", "A", "IYH")
            end, { A = 0x13, IY = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

     { "ADC A, IYH", function(z)
            z:assemble("SCF")
            z:assemble("CCF")
            z:LD("A", 1) z:LD("IY", 0x200) z:assemble("ADC", "A", "IYH") end,
         { A = 3, IY = 0x200, F={"-S", "-Z", "-H", "-V", "-N", "-C"}} },
     { "ADC A, IYH", function(z)
            z:assemble("SCF")
            z:assemble("CCF")
            z:LD("A", 0x0F) z:LD("IY", 0x100) z:assemble("ADC", "A", "IYH") end,
         { A = 0x10, IY = 0x100, F={"-S", "-Z", "H", "-V", "-N", "-C"}} },
     { "ADC A, IYH", function(z)
            z:assemble("SCF")
            z:assemble("CCF")
            z:LD("A", 0x80) z:LD("IY", 0x8000) z:assemble("ADC", "A", "IYH") end,
         { A = 0, IY = 0x8000, F={"-S", "Z", "-H", "V", "-N", "C"}} },
     { "ADC A, IYH", function(z)
            z:assemble("SCF")
            z:assemble("CCF")
            z:LD("A", 0x7F) z:LD("IY", 0x100) z:assemble("ADC", "A", "IYH") end,
         { A = 0x80, IY = 0x100, F={"S", "-Z", "H", "V", "-N", "-C"}} },
     { "ADC A, IYH", function(z)
            z:assemble("SCF")
            z:assemble("CCF")
            z:LD("A", 0xFF) z:LD("IY", 0x200) z:assemble("ADC", "A", "IYH") end,
         { A = 1, IY = 0x200, F={"-S", "-Z", "H", "-V", "-N", "C"}} },

    -- 0x8D
    { "ADC A,IYL", function(z)
            z:assemble("SCF")
            z:assemble("CCF")
            z:LD("IY", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADC", "A", "IYL")
            end, { A = 0x34, IY = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

    { "ADC A,IYL", function(z)
            z:assemble("SCF")
            z:assemble("CCF")
            z:LD("IY", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADC", "A", "IYL")
            end, { A = 0x34, IY = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

 { "ADC A, IYL", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 1) z:LD("IY", 0x2) z:assemble("ADC", "A", "IYL") end,
     { A = 3, IY = 0x2, F={"-S", "-Z", "-H", "-V", "-N", "-C"}} },
 { "ADC A, IYL", function(z) z:LD("A", 0x0F)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("IY", 0x1) z:assemble("ADC", "A", "IYL") end,
     { A = 0x10, IY = 0x1, F={"-S", "-Z", "H", "-V", "-N", "-C"}} },
 { "ADC A, IYL", function(z) z:LD("A", 0x80)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("IY", 0x80) z:assemble("ADC", "A", "IYL") end,
     { A = 0, IY = 0x80, F={"-S", "Z", "-H", "V", "-N", "C"}} },
 { "ADC A, IYL", function(z) z:LD("A", 0x7F)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("IY", 0x1) z:assemble("ADC", "A", "IYL") end,
     { A = 0x80, IY = 0x1, F={"S", "-Z", "H", "V", "-N", "-C"}} },
 { "ADC A, IYL", function(z) z:LD("A", 0xFF)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("IY", 0x2) z:assemble("ADC", "A", "IYL") end,
     { A = 1, IY = 0x2, F={"-S", "-Z", "H", "-V", "-N", "C"}} },


-- 0x90
{ "SUB A,IYH", function(z)
        z:LD("A", 0x26)
        z:LD("IY", 0x0201)
        z:assemble("SUB", "A", "IYH")
    end,
    { A = 0x024, IY=0x201, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
{ "SUB A,IYH zero", function(z)
        z:LD("A", 0x26)
        z:LD("IY", 0x2602)
        z:assemble("SUB", "A", "IYH")
    end,
    { A = 0x00, IY=0x2602, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "SUB A,IYH half", function(z)
        z:LD("A", 0x10)
        z:LD("IY", 0x0203)
        z:assemble("SUB", "A", "IYH")
    end,
    { A = 0x0E, IY=0x203, F={ "-S", "-Z", "H", "-V", "N", "-C" } } },
{ "SUB A,IYH carry", function(z)
        z:LD("A", 0x00)
        z:LD("IY", 0x0204)
        z:assemble("SUB", "A", "IYH")
    end,
    { A = 0xFE, IY=0x204, F={ "S", "-Z", "H", "-V", "N", "C" } } },
{ "SUB A,IYH overflow", function(z)
        z:LD("A", 0x80)
        z:LD("IY", 0x0205)
        z:assemble("SUB", "A", "IYH")
    end,
    { A = 0x7E, IY=0x205, F={ "-S", "-Z", "H", "V", "N", "-C" } } },

-- 0x95
{ "SUB A,IYL", function(z)
        z:LD("A", 0x26)
        z:LD("IY", 0x9902)
        z:assemble("SUB", "A", "IYL")
    end,
    { A = 0x024, IY=0x9902, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
{ "SUB A,IYL zero", function(z)
        z:LD("A", 0x26)
        z:LD("IY", 0x8826)
        z:assemble("SUB", "A", "IYL")
    end,
    { A = 0x00, IY=0x8826, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "SUB A,IYL half", function(z)
        z:LD("A", 0x10)
        z:LD("IY", 0x7702)
        z:assemble("SUB", "A", "IYL")
    end,
    { A = 0x0E, IY=0x7702, F={ "-S", "-Z", "H", "-V", "N", "-C" } } },
{ "SUB A,IYL carry", function(z)
        z:LD("A", 0x00)
        z:LD("IY", 0x6602)
        z:assemble("SUB", "A", "IYL")
    end,
    { A = 0xFE, IY=0x6602, F={ "S", "-Z", "H", "-V", "N", "C" } } },
{ "SUB A,IYL overflow", function(z)
        z:LD("A", 0x80)
        z:LD("IY", 0x5502)
        z:assemble("SUB", "A", "IYL")
    end,
    { A = 0x7E, IY=0x5502, F={ "-S", "-Z", "H", "V", "N", "-C" } } },


-- 0x9C
{ "SBC A,IYH zero", function(z)
        z:LD("A", 0x00) z:LD("IY", 0x0111) z:assemble("SUB", "A", "IYH")
        z:LD("A", 0x02)
        z:LD("IYH", 0x01)
        z:assemble("SBC", "A", "IYH")
    end,
    { A = 0x00, IY = 0x0111, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "SBC A,IYH not zero", function(z)
        z:LD("A", 0x01) z:LD("IY", 0x0022) z:assemble("SUB", "A", "IYH")
        z:LD("A", 0x02)
        z:LD("IYH", 0x01)
        z:assemble("SBC", "A", "IYH")
    end,
    { A = 0x01, IY = 0x0122, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
 { "SBC A,IYH underflow", function(z)
        z:LD("A", 0x00) z:LD("IY", 0x0133) z:assemble("SUB", "A", "IYH")
        z:LD("A", 0x02)
        z:LD("IYH", 0x01)
        z:assemble("SBC", "A", "IYH")
    end,
    { A = 0x00, IY = 0x0133, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },   

-- 0x9D
{ "SBC A,IYL zero", function(z)
        z:LD("A", 0x00) z:LD("IY", 0x1101) z:assemble("SUB", "A", "IYL")
        z:LD("A", 0x02)
        z:LD("IYL", 0x01)
        z:assemble("SBC", "A", "IYL")
    end,
    { A = 0x00, IY = 0x1101, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "SBC A,IYL not zero", function(z)
        z:LD("A", 0x01) z:LD("IY", 0x2200) z:assemble("SUB", "A", "IYL")
        z:LD("A", 0x02)
        z:LD("IYL", 0x01)
        z:assemble("SBC", "A", "IYL")
    end,
    { A = 0x01, IY = 0x2201, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
 { "SBC A,IYL underflow", function(z)
        z:LD("A", 0x00) z:LD("IY", 0x3301) z:assemble("SUB", "A", "IYL")
        z:LD("A", 0x02)
        z:LD("IYL", 0x01)
        z:assemble("SBC", "A", "IYL")
    end,
    { A = 0x00, IY = 0x3301, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },   

    -- 0xA4
    { "AND IYH", function(z) z:LD("A", 0x8F) z:LD("IY", 0x01EE) z:AND("IYH") end, { A=0x01, IY=0x01EE, F={"-Z", "-N", "H", "-P", "-S", "-C"} } },   -- odd number of bits = Parity clear
    { "AND IYH zero", function(z) z:LD("A", 0x80) z:LD("IY", 0x01EE) z:AND("IYH") end, { A=0x00, IY=0x01EE, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set
    
    -- 0xA5
    { "AND IYL", function(z) z:LD("A", 0x8F) z:LD("IY", 0xAA01) z:AND("IYL") end, { A=0x01, IY=0xAA01, F={"-Z", "-N", "H", "-P", "-S", "-C"} } },   -- odd number of bits = Parity clear
    { "AND IYL zero", function(z) z:LD("A", 0x80) z:LD("IY", 0xAA01) z:AND("IYL") end, { A=0x00, IY=0xAA01, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set

        -- 0xAC
    { "XOR IYH", function(z) z:LD("A", 0x8F) z:LD("IY", 0x0133) z:XOR("IYH") end, { A=0x8E, IY=0x0133, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- odd number of bits = Parity clear
    { "XOR IYH zero", function(z) z:LD("A", 0x01) z:LD("IY", 0x0144) z:XOR("IYH") end, { A=0x00, IY=0x0144, F={"Z", "-N", "-H", "P", "-S", "-C"} } },   -- even number of bits = Parity set
        -- 0xAD
    { "XOR IYL", function(z) z:LD("A", 0x8F) z:LD("IY", 0x5501) z:XOR("IYL") end, { A=0x8E, IY=0x5501, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- odd number of bits = Parity clear
    { "XOR IYL zero", function(z) z:LD("A", 0x01) z:LD("IY", 0x6601) z:XOR("IYL") end, { A=0x00, IY=0x6601, F={"Z", "-N", "-H", "P", "-S", "-C"} } },   -- even number of bits = Parity set

   -- 0xB4
    { "OR IYH", function(z) z:LD("A", 0x80) z:LD("IY", 0x0155) z:OR("IYH") end, { A=0x81, IY=0x0155, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set
    -- 0xB6
    { "OR IYL", function(z) z:LD("A", 0x80) z:LD("IY", 0x6601) z:OR("IYL") end, { A=0x81, IY=0x6601, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set

-- 0xBC
{ "CP IYH", function(z)
        z:LD("A", 0x26)
        z:LD("IY", 0x0233)
        z:assemble("CP", "IYH")
    end,
    { A = 0x026, IY=0x233, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
{ "CP IYH zero", function(z)
        z:LD("A", 0x26)
        z:LD("IY", 0x2644)
        z:assemble("CP", "IYH")
    end,
    { A = 0x26, IY=0x2644, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "CP IYH half", function(z)
        z:LD("A", 0x10)
        z:LD("IY", 0x0255)
        z:assemble("CP", "IYH")
    end,
    { A = 0x10, IY=0x255, F={ "-S", "-Z", "H", "-V", "N", "-C" } } },
{ "CP IYH carry", function(z)
        z:LD("A", 0x00)
        z:LD("IY", 0x0266)
        z:assemble("CP", "IYH")
    end,
    { A = 0x00, IY=0x266, F={ "S", "-Z", "H", "-V", "N", "C" } } },
{ "CP IYH overflow", function(z)
        z:LD("A", 0x80)
        z:LD("IY", 0x0277)
        z:assemble("CP", "IYH")
    end,
    { A = 0x80, IY=0x277, F={ "-S", "-Z", "H", "V", "N", "-C" } } },

-- 0xBD
{ "CP IYL", function(z)
        z:LD("A", 0x26)
        z:LD("IY", 0x2202)
        z:assemble("CP", "IYL")
    end,
    { A = 0x026, IY=0x2202, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
{ "CP IYL zero", function(z)
        z:LD("A", 0x26)
        z:LD("IY", 0x3326)
        z:assemble("CP", "IYL")
    end,
    { A = 0x26, IY=0x3326, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "CP IYL half", function(z)
        z:LD("A", 0x10)
        z:LD("IY", 0x4402)
        z:assemble("CP", "IYL")
    end,
    { A = 0x10, IY=0x4402, F={ "-S", "-Z", "H", "-V", "N", "-C" } } },
{ "CP IYL carry", function(z)
        z:LD("A", 0x00)
        z:LD("IY", 0x5502)
        z:assemble("CP", "IYL")
    end,
    { A = 0x00, IY=0x5502, F={ "S", "-Z", "H", "-V", "N", "C" } } },
{ "CP IYL overflow", function(z)
        z:LD("A", 0x80)
        z:LD("IY", 0x6602)
        z:assemble("CP", "IYL")
    end,
    { A = 0x80, IY=0x6602, F={ "-S", "-Z", "H", "V", "N", "-C" } } },

      -- 0xE1
    { "POP IY", function(z)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("LD", "BC", 0x1234)
            z:assemble("PUSH", "BC")
            z:assemble("POP", "IY")
        end, {SP=0x6000, IY=0x1234, B=0x12, C=0x34, [0x5FFE]=0x34, [0x5FFF]=0x12 } },
    { "POP IY (wrap)", function(z)
            z:assemble("LD", "SP", 0x0001)
            z:assemble("LD", "BC", 0x1234)
            z:assemble("PUSH", "BC")
            z:assemble("POP", "IY")
        end, {SP=0x1, IY=0x1234, B=0x12, C=0x34, [0xFFFF]=0x34, [0x0000]=0x12 } },

    -- 0xE3
    { "EX (SP), IY", function(z) 
            z:LD("SP", 0x6000)
            z:LD("IY", 0x1234)
            z:LD("A", 0xCD)
            z:LD("(0x6000)", "A")
            z:LD("A", 0xAB)
            z:LD("(0x6001)", "A")
            z:assemble("EX", "(SP)", "IY")
        end, { SP=0x6000, IY=0xABCD, A=0xAB, [0x6000]=0x34, [0x6001]=0x12 } },
    { "EX (SP), IY  rollover", function(z) 
            z:LD("SP", 0xFFFF)
            z:LD("IY", 0x1234)
            z:LD("A", 0xCD)
            z:LD("(0xFFFF)", "A")
            z:LD("A", 0xAB)
            z:LD("(0x0000)", "A")
            z:assemble("EX", "(SP)", "IY")
        end, { SP=0xFFFF, IY=0xABCD, A=0xAB, [0xFFFF]=0x34, [0x0000]=0x12 } },

    -- 0xE5
    { "PUSH IY", function(z) 
            z:assemble("LD", "IY", 0x4321)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("PUSH", "IY")
            end, { IY=0x4321, SP=0x5FFE, [0x5FFE]=0x21, [0x5FFF]=0x43 } },
    { "PUSH IY wrap0", function(z) 
            z:assemble("LD", "IY", 0x4321)
            z:assemble("LD", "SP", 0x0000)
            z:assemble("PUSH", "IY")
            end, { IY=0x4321, SP=0xFFFE, [0xFFFE]=0x21, [0xFFFF]=0x43 } },
    { "PUSH IY wrap1", function(z) 
            z:assemble("NOP")       -- this will get overwritten
            z:assemble("LD", "IY", 0x4321)
            z:assemble("LD", "SP", 0x0001)
            z:assemble("PUSH", "IY")
            end, { IY=0x4321, SP=0xFFFF, [0xFFFF]=0x21, [0x0000]=0x43 } },

-- 0xE9
{ "JP (IY)", function(z)
        z:LD("IY", 12)      -- 0
        z:LD("A", 0x01)         -- 4
        z:OR("A")               -- 6
        z:assemble("JP", "(IY)")     -- 7
        z:LD("A", 0x20)         -- 9
        z:assemble("INC", "A")   -- 11
        z:assemble("INC", "A")   -- 12
        z:assemble("INC", "A")   -- 13
        z:assemble("INC", "A")   -- 14
        end,
        { A = 0x04, IY=12, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } },

}   

return FD_instruction_tests
