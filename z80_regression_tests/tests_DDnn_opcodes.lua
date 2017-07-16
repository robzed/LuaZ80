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


local DD_instruction_tests = {

    -- 0x09
    -- ADD IX, ss ... doesn't affect Z or S or V
    { "ADD IX, BC", function(z) z:LD("IX", 0x1234)
                                z:LD("BC", 0x4320)
                                z:ADD("IX", "BC") end,
                                { IX = 0x5554, B = 0x43, C = 0x20, F = { "-N", "-H", "-C" } } },
    
    { "ADD IX, BC no half-carry", function(z) z:LD("IX", 0x003F)
                                z:LD("BC", 0x003F)
                                    z:ADD("IX", "BC") end,
                                { IX = 0x007E, B = 0x00, C = 0x3F, F = { "-N", "-H", "-C" } } },

    { "ADD IX, BC half-carry", function(z) z:LD("IX", 0x3F00)
                                z:LD("BC", 0x0100)
                                z:ADD("IX", "BC") end,
                                { IX = 0x4000, B = 0x01, C = 0x00, F = { "-N", "H", "-C" } } },
    
    { "ADD IX, BC overflow", function(z) z:LD("IX", 0x8000)
                                z:LD("BC", 0x8000)
                                z:ADD("IX", "BC") end,
                                { IX = 0x0000, B = 0x80, C = 0x00, F = { "-N", "-H", "C" } } },

    { "ADD IX, BC overflow2", function(z) z:LD("IX", 0x1000)
                                z:LD("BC", 0x7000)
                                z:ADD("IX", "BC") end,
                                { IX = 0x8000, B = 0x70, C = 0x00, F = { "-N", "-H", "-C" } } },

    { "ADD IX, BC half and overflow", function(z) z:LD("IX", 0x0001)
                                z:LD("BC", 0xFFFF)
                                z:ADD("IX", "BC") end,
                                { IX = 0x0000, B = 0xFF, C = 0xFF, F = { "-N", "H", "C" } } },
    
    { "ADD IX, BC check no S Z flags", function(z)
                                z:LD("SP", 0x6000)
                                z:LD("IX", 0x0001)
                                z:PUSH("IX")
                                z:POP("AF")
                                z:LD("BC", 0xFFFF)
                                z:ADD("IX", "BC") end,
                                { IX = 0x0000, B = 0xFF, C = 0xFF, A = 0x00, [0x5FFE]=1, [0x5FFF]=0, SP=0x6000, F = { "-S", "-Z", "-V", "-N", "H", "C" } } },

    -- 0x19
    -- ADD IX, ss ... doesn't affect Z or S or V
    { "ADD IX, DE", function(z) z:LD("IX", 0x1234)
                                z:LD("DE", 0x4320)
                                z:ADD("IX", "DE") end,
                                { IX = 0x5554, D = 0x43, E = 0x20, F = { "-N", "-H", "-C" } } },
    
    { "ADD IX, DE no half-carry", function(z) z:LD("IX", 0x003F)
                                z:LD("DE", 0x003F)
                                    z:ADD("IX", "DE") end,
                                { IX = 0x007E, D = 0x00, E = 0x3F, F = { "-N", "-H", "-C" } } },

    { "ADD IX, DE half-carry", function(z) z:LD("IX", 0x3F00)
                                z:LD("DE", 0x0100)
                                z:ADD("IX", "DE") end,
                                { IX = 0x4000, D = 0x01, E = 0x00, F = { "-N", "H", "-C" } } },
    
    { "ADD IX, DE overflow", function(z) z:LD("IX", 0x8000)
                                z:LD("DE", 0x8000)
                                z:ADD("IX", "DE") end,
                                { IX = 0x0000, D = 0x80, E = 0x00, F = { "-N", "-H", "C" } } },

    { "ADD IX, DE overflow2", function(z) z:LD("IX", 0x1000)
                                z:LD("DE", 0x7000)
                                z:ADD("IX", "DE") end,
                                { IX = 0x8000, D = 0x70, E = 0x00, F = { "-N", "-H", "-C" } } },

    { "ADD IX, DE half and overflow", function(z) z:LD("IX", 0x0001)
                                z:LD("DE", 0xFFFF)
                                z:ADD("IX", "DE") end,
                                { IX = 0x0000, D = 0xFF, E = 0xFF, F = { "-N", "H", "C" } } },
    
    { "ADD IX, DE check no S Z flags", function(z)
                                z:LD("SP", 0x6000)
                                z:LD("IX", 0x0001)
                                z:PUSH("IX")
                                z:POP("AF")
                                z:LD("DE", 0xFFFF)
                                z:ADD("IX", "DE") end,
                                { IX = 0x0000, D = 0xFF, E = 0xFF, A = 0x00, [0x5FFE]=1, [0x5FFF]=0, SP=0x6000, F = { "-S", "-Z", "-V", "-N", "H", "C" } } },


    -- 0x21
    { "LD IX, nn", function(z)
            z:LD("IX", 0x1234)
        end, { IX = 0x1234 } },
    
    
    -- 0x22
    {"LD   (nn),IX", function(z) z:assemble("LD", "IX", 0x4321) 
            z:assemble("LD", "(0x5555)", "IX") 
            end, { IX=0x4321, [0x5555]=0x21, [0x5556]=0x43 }},
    {"LD   (nn),IX wrap", function(z) z:assemble("LD", "IX", 0x4321) 
            z:assemble("LD", "(0xFFFF)", "IX") 
            end, { IX=0x4321, [0xFFFF]=0x21, [0x0000]=0x43 }},

    -- 0x23
    { "INC IX", function(z)
            z:LD("IX", 0x1234)
            z:assemble("INC", "IX")
        end, { IX = 0x1235 } },
        
    { "INC IX rollover", function(z)
            z:LD("IX", 0xFFFF)
            z:assemble("INC", "IX")
        end, { IX = 0 } },

    -- 0x24
     { "INC  IXH", function(z) z:assemble("LD", "IX", 0x1155)  
            z:assemble("INC", "IXH") end, { IX=0x1255, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

     { "INC  IXH rollover", function(z) z:assemble("LD", "IX", 0xFF66)  
             -- S is set if result is negative; reset otherwise
            -- Z is set if result is zero; reset otherwise
            -- H is set if carry from bit 3; reset otherwise
            -- P/V is set if r was 7FH before operation; reset otherwise
            -- N is reset
            -- C is not affected
            z:assemble("INC", "IXH") end, { IX=0x0066, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

     { "INC  IXH half carry", function(z) z:assemble("LD", "IX", 0x0F77)  
            z:assemble("INC", "IXH") end, { IX=0x1077, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

     { "INC  IXH Flags P/V Sign", function(z) z:assemble("LD", "IX", 0x7F88)  
            z:assemble("INC", "IXH") end, { IX=0x8088, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  

    -- 0x25
    { "DEC  IXH", function(z) z:LD("IX", 0x1133)  
            z:assemble("DEC", "IXH") end, { IX=0x1033, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"} } },  

    { "DEC  IXH to zero", function(z) z:LD("IX", 0x0144)  
            z:assemble("DEC", "IXH") end, { IX=0x0044, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IXH rollover", function(z) z:assemble("LD", "IX", 0x0055)  
             -- S is set if result is negative; reset otherwise
            -- Z is set if result is zero; reset otherwise
            -- H is set if borrow from bit 4, reset otherwise
            -- P/V is set if m was 80H before operation; reset otherwise
            -- N is set
            -- C is not affected
            z:assemble("DEC", "IXH") end, { IX=0xFF55, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IXH half carry", function(z) z:assemble("LD", "IX", 0x1066)  
            z:assemble("DEC", "IXH") end, { IX=0x0F66, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IXH Flags P/V Sign", function(z) z:assemble("LD", "IX", 0x8077)  
            z:assemble("DEC", "IXH") end, { IX=0x7F77, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"} } },  


    -- 0x26
    { "LD IXH, n", function(z)
        z:LD("IX", 0x1234)
        z:LD("IXH", 0x56)
        end, { IX = 0x5634 } },
     { "LD  IXH,n", function(z) z:assemble("LD", "IX", 0xFFFF) z:assemble("LD", "IXH", 0xe1) end, { IX=0xE1FF } }, 
     { "LD  IXH,n", function(z) z:assemble("LD", "IX", 0xFFFF) z:assemble("LD", "IXH", 0x00) end, { IX=0x00FF } }, 
     { "LD  IXH,n", function(z) z:assemble("LD", "IX", 0xA5A5) z:assemble("LD", "IXH", 0xFF) end, { IX=0xFFA5 } }, 


    -- 0x29
    -- ADD IX, ss ... doesn't affect Z or S or V
    { "ADD IX, IX", function(z) z:LD("IX", 0x1234)
                                z:ADD("IX", "IX") end,
                                { IX = 0x2468, F = { "-N", "-H", "-C" } } },
    
    { "ADD IX, IX no half-carry", function(z) z:LD("IX", 0x003F)
                                    z:ADD("IX", "IX") end,
                                { IX = 0x007E, F = { "-N", "-H", "-C" } } },

    { "ADD IX, IX half-carry", function(z) z:LD("IX", 0x3F00)
                                z:ADD("IX", "IX") end,
                                { IX = 0x7E00, F = { "-N", "H", "-C" } } },
    
    { "ADD IX, IX overflow", function(z) z:LD("IX", 0x8000)
                                z:ADD("IX", "IX") end,
                                { IX = 0x0000, F = { "-N", "-H", "C" } } },

    { "ADD IX, IX overflow2", function(z) z:LD("IX", 0x4000)
                                z:ADD("IX", "IX") end,
                                { IX = 0x8000, F = { "-N", "-H", "-C" } } },

    { "ADD IX, IX half and overflow", function(z) z:LD("IX", 0x8888)
                                z:ADD("IX", "IX") end,
                                { IX = 0x1110, F = { "-N", "H", "C" } } },
    
    { "ADD IX, IX check no S Z flags", function(z)
                                z:LD("SP", 0x6000)
                                z:LD("IX", 0x0001)
                                z:PUSH("IX")
                                z:POP("AF")
                                z:LD("IX", 0x8888)
                                z:ADD("IX", "IX") end,
                                { IX = 0x1110, A = 0x00, [0x5FFE]=1, [0x5FFF]=0, SP=0x6000, F = { "-S", "-Z", "-V", "-N", "H", "C" } } },



    -- 0x2A
    { "LD IX,(nn)", function(z)
            z:LD("A", 0x22) 
            z:LD("(0x6000)", "A") 
            z:LD("A", 0x11) 
            z:LD("(0x6001)", "A")
            z:LD("IX", "(0x6000)")
            end,
        { [0x6000] = 0x22, [0x6001] = 0x11, A = 0x11, IX = 0x1122 } },
    { "LD IX,(nn) rollover", function(z)
            z:LD("A", 0x44) 
            z:LD("(0xFFFF)", "A") 
            z:LD("A", 0x33) 
            z:LD("(0x0000)", "A")
            z:LD("IX", "(0xFFFF)") 
            z:LD("A", 99)
            end,
        { [0xFFFF] = 0x44, [0x0000] = 0x33, A = 99, IX = 0x3344 } },

    -- 0x2B
    { "DEC IX", function(z)
            z:LD("IX", 0x1234)
            z:assemble("DEC", "IX")
        end, { IX = 0x1233 } },
        
    { "DEC IX rollover", function(z)
            z:LD("IX", 0)
            z:assemble("DEC", "IX")
        end, { IX = 0xFFFF } },
    
    
    -- 0x2C
     { "INC  IXL", function(z) z:assemble("LD", "IX", 0xAA11)  
            z:assemble("INC", "IXL") end, { IX=0xAA12, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

     { "INC  IXL rollover", function(z) z:assemble("LD", "IX", 0xBBFF)  
             -- S is set if result is negative; reset otherwise
            -- Z is set if result is zero; reset otherwise
            -- H is set if carry from bit 3; reset otherwise
            -- P/V is set if r was 7FH before operation; reset otherwise
            -- N is reset
            -- C is not affected
            z:assemble("INC", "IXL") end, { IX=0xBB00, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

     { "INC  IXL half carry", function(z) z:assemble("LD", "IX", 0xCC0F)  
            z:assemble("INC", "IXL") end, { IX=0xCC10, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

     { "INC  IXL Flags P/V Sign", function(z) z:assemble("LD", "IX", 0xDD7F)  
            z:assemble("INC", "IXL") end, { IX=0xDD80, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  

    -- 0x2D
    { "DEC  IXL", function(z) z:LD("IX", 0x9911)  
            z:assemble("DEC", "IXL") end, { IX=0x9910, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"} } },  

    { "DEC  IXL to zero", function(z) z:LD("IX", 0x9901)  
            z:assemble("DEC", "IXL") end, { IX=0x9900, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IXL rollover", function(z) z:assemble("LD", "IX", 0x9900)  
             -- S is set if result is negative; reset otherwise
            -- Z is set if result is zero; reset otherwise
            -- H is set if borrow from bit 4, reset otherwise
            -- P/V is set if m was 80H before operation; reset otherwise
            -- N is set
            -- C is not affected
            z:assemble("DEC", "IXL") end, { IX=0x99FF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IXL half carry", function(z) z:assemble("LD", "IX", 0x9910)  
            z:assemble("DEC", "IXL") end, { IX=0x990F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

     { "DEC  IXL Flags P/V Sign", function(z) z:assemble("LD", "IX", 0x9980)  
            z:assemble("DEC", "IXL") end, { IX=0x997F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"} } },  
    
    -- 0x2E
    { "LD IXL, n", function(z)
            z:LD("IX", 0x1111)
            z:LD("IXL", 0x22)
            end, { IX = 0x1122 } },

-- 0x34
 { "INC  (IX+0)", function(z) z:assemble("LD", "IX", 0x6000)
    z:LD("A", 0x11)
    z:LD("(0x6000)", "A")
    z:assemble("INC", "(IX+0)") end, { A=0x11, [0x6000]=0x12, IX=0x6000, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

 { "INC  (IX+0) rollover", function(z) z:assemble("LD", "IX", 0x6000)  
    z:LD("A", 0xFF)
    z:LD("(0x6000)", "A")
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if carry from bit 3; reset otherwise
        -- P/V is set if r was 7FH before operation; reset otherwise
        -- N is reset
        -- C is not affected
    z:assemble("INC", "(IX+0)") end, { A=0xFF, [0x6000]=0x00, IX=0x6000, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

 { "INC  (IX+0) half carry", function(z) z:assemble("LD", "IX", 0x6000)  
    z:LD("A", 0x0F)
    z:LD("(0x6000)", "A")
    z:assemble("INC", "(IX+0)") end, { A=0x0F, [0x6000]=0x10, IX=0x6000, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

 { "INC  (IX+0) Flags P/V Sign", function(z) z:assemble("LD", "IX", 0x6000)  
    z:LD("A", 0x7F)
    z:LD("(0x6000)", "A")
    z:assemble("INC", "(IX+0)") end, { A=0x7F, [0x6000]=0x80, IX=0x6000, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  

 { "INC  (IX-1)", function(z) z:assemble("LD", "IX", 0x6000)
    z:LD("A", 0x11)
    z:LD("(0x5FFF)", "A")
    z:assemble("INC", "(IX-1)") end, { A=0x11, [0x5FFF]=0x12, IX=0x6000, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  
 { "INC  (IX+2) addr rollover", function(z) z:assemble("LD", "IX", 0xFFFE)
    z:LD("A", 0x11)
    z:LD("(0)", "A")
    z:assemble("INC", "(IX+2)") end, { A=0x11, [0x0000]=0x12, IX=0xFFFE, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  
 { "INC  (IX+127) addr rollover", function(z) z:assemble("LD", "IX", 0x6000)
    z:LD("A", 0x11)
    z:LD("(0x607F)", "A")
    z:assemble("INC", "(IX+127)") end, { A=0x11, [0x607F]=0x12, IX=0x6000, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  
 { "INC  (IX-128) addr rollover", function(z) z:assemble("LD", "IX", 0x6000)
    z:LD("A", 0x11)
    z:LD("(0x5F80)", "A")
    z:assemble("INC", "(IX-128)") end, { A=0x11, [0x5F80]=0x12, IX=0x6000, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  


    -- 0x35
    { "DEC  (IX+0)", function(z) z:LD("IX", 0x6000) 
            z:LD("A", 0x11)
            z:LD("(0x6000)", "A")
            z:assemble("DEC", "(IX+0)") end, { A=0x11, [0x6000]=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"}, IX=0x6000 } },  

    { "DEC  (IX+0) to zero", function(z) z:LD("IX", 0x6000)
            z:LD("A", 0x01)
            z:LD("(0x6000)", "A")
            z:assemble("DEC", "(IX+0)") end, { A=0x01, [0x6000]=0x00, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"}, IX=0x6000 } },  

     { "DEC  (IX+0) rollover", function(z) z:LD("IX", 0x6000)
            z:LD("A", 0x00)
            z:LD("(0x6000)", "A")
             -- S is set if result is negative; reset otherwise
            -- Z is set if result is zero; reset otherwise
            -- H is set if borrow from bit 4, reset otherwise
            -- P/V is set if m was 80H before operation; reset otherwise
            -- N is set
            -- C is not affected
            z:assemble("DEC", "(IX+0)") end, { A=0x00, [0x6000]=0xFF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"}, IX=0x6000 } },  

     { "DEC  (IX+0) half carry", function(z) z:LD("IX", 0x6000)
            z:LD("A", 0x10)
            z:LD("(0x6000)", "A")
            z:assemble("DEC", "(IX+0)") end, { A=0x10, [0x6000]=0x0F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"}, IX=0x6000 } },  

     { "DEC  (IX+0) Flags P/V Sign", function(z) z:LD("IX", 0x6000)
            z:LD("A", 0x80)
            z:LD("(0x6000)", "A")
            z:assemble("DEC", "(IX+0)") end, { A=0x80, [0x6000]=0x7F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"}, IX=0x6000 } },  

    { "DEC  (IX-1)", function(z) z:LD("IX", 0x6000) 
            z:LD("A", 0x11)
            z:LD("(0x5FFF)", "A")
            z:assemble("DEC", "(IX-1)") end, { A=0x11, [0x5FFF]=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"}, IX=0x6000 } },  

    { "DEC  (IX+2)", function(z) z:LD("IX", 0x6000) 
            z:LD("A", 0x11)
            z:LD("(0x6002)", "A")
            z:assemble("DEC", "(IX+2)") end, { A=0x11, [0x6002]=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"}, IX=0x6000 } },  

    { "DEC  (IX+127)", function(z) z:LD("IX", 0x6000) 
            z:LD("A", 0x11)
            z:LD("(0x607F)", "A")
            z:assemble("DEC", "(IX+127)") end, { A=0x11, [0x607F]=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"}, IX=0x6000 } },  

    { "DEC  (IX-128)", function(z) z:LD("IX", 0x6000) 
            z:LD("A", 0x11)
            z:LD("(0x5F80)", "A")
            z:assemble("DEC", "(IX-128)") end, { A=0x11, [0x5F80]=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"}, IX=0x6000 } },  
    
    -- 0x36
    { "LD (IX+0), n", function(z)
         z:LD("IX", 0x5DEF)
         z:LD("(IX+0)", 0x11)
     end, { IX=0x5DEF, [0x5DEF] = 0x11 } },
    { "LD (IX+127), n", function(z)
         z:LD("IX", 0x6000)
         z:LD("(IX+127)", 0x11)
     end, { IX=0x6000, [0x607F] = 0x11 } },
    { "LD (IX-128), n", function(z)
         z:LD("IX", 0x6000)
         z:LD("(IX-128)", 0x11)
     end, { IX=0x6000, [0x5F80] = 0x11 } },
 
 
    -- 0x39
    -- ADD IX, ss ... doesn't affect Z or S or V
    { "ADD IX, SP", function(z) z:LD("IX", 0x1234)
                                z:LD("SP", 0x4320)
                                z:ADD("IX", "SP") end,
                                { IX = 0x5554, SP = 0x4320, F = { "-N", "-H", "-C" } } },
    
    { "ADD IX, SP no half-carry", function(z) z:LD("IX", 0x003F)
                                z:LD("SP", 0x003F)
                                z:ADD("IX", "SP") end,
                                { IX = 0x007E, SP = 0x003F, F = { "-N", "-H", "-C" } } },

    { "ADD IX, SP half-carry", function(z) z:LD("IX", 0x3F00)
                                z:LD("SP", 0x0100)
                                z:ADD("IX", "SP") end,
                                { IX = 0x4000, SP = 0x0100, F = { "-N", "H", "-C" } } },
    
    { "ADD IX, SP overflow", function(z) z:LD("IX", 0x8000)
                                z:LD("SP", 0x8000)
                                z:ADD("IX", "SP") end,
                                { IX = 0x0000, SP = 0x8000, F = { "-N", "-H", "C" } } },

    { "ADD IX, SP overflow2", function(z) z:LD("IX", 0x1000)
                                z:LD("SP", 0x7000)
                                z:ADD("IX", "SP") end,
                                { IX = 0x8000, SP = 0x7000, F = { "-N", "-H", "-C" } } },

    { "ADD IX, SP half and overflow", function(z) z:LD("IX", 0x0001)
                                z:LD("SP", 0xFFFF)
                                z:ADD("IX", "SP") end,
                                { IX = 0x0000, SP = 0xFFFF, F = { "-N", "H", "C" } } },
    
    { "ADD IX, SP check no S Z flags", function(z)
                                z:LD("SP", 0x6000)
                                z:LD("IX", 0x0001)
                                z:PUSH("IX")
                                z:POP("AF")
                                z:LD("SP", 0xFFFF)
                                z:ADD("IX", "SP") end,
                                { IX = 0x0000, SP = 0xFFFF, A = 0x00, [0x5FFE]=1, [0x5FFF]=0, F = { "-S", "-Z", "-V", "-N", "H", "C" } } },

    -- 0x44
    { "LD B, IXH", function(z)
            z:LD("IX", 0x1234)
            z:LD("B", "IXH")
        end, { B = 0x12, IX=0x1234 } },
    -- 0x45
    { "LD B, IXL", function(z)
            z:LD("IX", 0x1234)
            z:LD("B", "IXL")
        end, { B = 0x34, IX=0x1234 } },
    
    -- 0x46
    { "LD B,(IX+0)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7001)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("B", "(IX+0)")  
            end, { A=0x7E, IX=0x7001, B=0x7E, [0x7001]=0x7E } },
    { "LD B,(IX-1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7000)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("B", "(IX-1)")  
            end, { A=0x7E, IX=0x7001, B=0x7E, [0x7000]=0x7E } },
    { "LD B,(IX+1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7002)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("B", "(IX+1)")  
            end, { A=0x7E, IX=0x7001, B=0x7E, [0x7002]=0x7E } },
    { "LD B,(IX+127)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7080)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("B", "(IX+127)")  
            end, { A=0x7E, IX=0x7001, B=0x7E, [0x7080]=0x7E } },
    { "LD B,(IX-128)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x6F81)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("B", "(IX-128)")  
            end, { A=0x7E, IX=0x7001, B=0x7E, [0x6F81]=0x7E } },

    -- 0x4C
    { "LD C, IXH", function(z)
            z:LD("IX", 0x1234)
            z:LD("C", "IXH")
        end, { C = 0x12, IX=0x1234 } },
    
    -- 0x4D
    { "LD C, IXL", function(z)
            z:LD("IX", 0x1234)
            z:LD("C", "IXL")
        end, { C = 0x34, IX=0x1234 } },

    -- 0x4E
    { "LD C,(IX+0)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7001)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("C", "(IX+0)")  
            end, { A=0x7E, IX=0x7001, C=0x7E, [0x7001]=0x7E } },
    { "LD C,(IX-1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7000)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("C", "(IX-1)")  
            end, { A=0x7E, IX=0x7001, C=0x7E, [0x7000]=0x7E } },
    { "LD C,(IX+1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7002)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("C", "(IX+1)")  
            end, { A=0x7E, IX=0x7001, C=0x7E, [0x7002]=0x7E } },
    { "LD C,(IX+127)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7080)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("C", "(IX+127)")  
            end, { A=0x7E, IX=0x7001, C=0x7E, [0x7080]=0x7E } },
    { "LD C,(IX-128)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x6F81)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("C", "(IX-128)")  
            end, { A=0x7E, IX=0x7001, C=0x7E, [0x6F81]=0x7E } },

    -- 0x54
    { "LD D, IXH", function(z)
            z:LD("IX", 0x1234)
            z:LD("D", "IXH")
        end, { D = 0x12, IX=0x1234 } },
    
    -- 0x55
    { "LD D, IXL", function(z)
            z:LD("IX", 0x1234)
            z:LD("D", "IXL")
        end, { D = 0x34, IX=0x1234 } },

    -- 0x56
    { "LD D,(IX+0)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7001)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("D", "(IX+0)")  
            end, { A=0x7E, IX=0x7001, D=0x7E, [0x7001]=0x7E } },
    { "LD D,(IX-1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7000)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("D", "(IX-1)")  
            end, { A=0x7E, IX=0x7001, D=0x7E, [0x7000]=0x7E } },
    { "LD D,(IX+1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7002)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("D", "(IX+1)")  
            end, { A=0x7E, IX=0x7001, D=0x7E, [0x7002]=0x7E } },
    { "LD D,(IX+127)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7080)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("D", "(IX+127)")  
            end, { A=0x7E, IX=0x7001, D=0x7E, [0x7080]=0x7E } },
    { "LD D,(IX-128)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x6F81)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("D", "(IX-128)")  
            end, { A=0x7E, IX=0x7001, D=0x7E, [0x6F81]=0x7E } },

    -- 0x5C
    { "LD E, IXH", function(z)
            z:LD("IX", 0x1234)
            z:LD("E", "IXH")
        end, { E = 0x12, IX=0x1234 } },
    
    -- 0x5D
    { "LD E, IXL", function(z)
            z:LD("IX", 0x1234)
            z:LD("E", "IXL")
        end, { E = 0x34, IX=0x1234 } },

    -- 0x5E
    { "LD E,(IX+0)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7001)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("E", "(IX+0)")  
            end, { A=0x7E, IX=0x7001, E=0x7E, [0x7001]=0x7E } },
    { "LD E,(IX-1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7000)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("E", "(IX-1)")  
            end, { A=0x7E, IX=0x7001, E=0x7E, [0x7000]=0x7E } },
    { "LD E,(IX+1)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7002)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("E", "(IX+1)")  
            end, { A=0x7E, IX=0x7001, E=0x7E, [0x7002]=0x7E } },
    { "LD E,(IX+127)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x7080)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("E", "(IX+127)")  
            end, { A=0x7E, IX=0x7001, E=0x7E, [0x7080]=0x7E } },
    { "LD E,(IX-128)", function(z) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(0x6F81)", "A") 
            z:assemble("LD", "IX", 0x7001) 
            z:LD("E", "(IX-128)")  
            end, { A=0x7E, IX=0x7001, E=0x7E, [0x6F81]=0x7E } },

    -- 0x64
    { "LD IXH, IXH", function(z)
            z:LD("IX", 0x1234)
            z:LD("IXH", "IXH")
        end, { IX=0x1234 } },
    
    --0x6D
    { "LD IXL, IXL", function(z)
            z:LD("IX", 0x1234)
            z:LD("IXL", "IXL")
        end, { IX=0x1234 } },

    -- 0x7C
    { "LD A, IXH", function(z)
            z:LD("IX", 0x1234)
            z:LD("A", "IXH")
        end, { A = 0x12, IX=0x1234 } },
    
    -- 0x7D
    { "LD A, IXL", function(z)
            z:LD("IX", 0x1234)
            z:LD("A", "IXL")
        end, { A = 0x34, IX=0x1234 } },

    -- 0x65
    { "LD IXH, IXL", function(z)
            z:LD("IX", 0x1234)
            z:LD("IXH", "IXL")
        end, { IX=0x3434 } },
    
    -- 0x6C
    { "LD IXL, IXH", function(z)
            z:LD("IX", 0x1234)
            z:LD("IXL", "IXH")
        end, { IX=0x1212 } },
    
    -- 0x60
    { "LD IXH, B", function(z)
            z:LD("IX", 0x1234)
            z:LD("B", 0x99)
            z:LD("IXH", "B")
        end, { B=0x99, IX=0x9934 } },
    -- 0x61
    { "LD IXH, C", function(z)
            z:LD("IX", 0x1234)
            z:LD("C", 0x99)
            z:LD("IXH", "C")
        end, { C=0x99, IX=0x9934 } },
    -- 0x62
    { "LD IXH, D", function(z)
            z:LD("IX", 0x1234)
            z:LD("D", 0x99)
            z:LD("IXH", "D")
        end, { D=0x99, IX=0x9934 } },
    -- 0x63
    { "LD IXH, E", function(z)
            z:LD("IX", 0x1234)
            z:LD("E", 0x99)
            z:LD("IXH", "E")
        end, { E=0x99, IX=0x9934 } },

    -- 0x67
    { "LD IXH, A", function(z)
            z:LD("IX", 0x1234)
            z:LD("A", 0x99)
            z:LD("IXH", "A")
        end, { A=0x99, IX=0x9934 } },
    
    -- 0x6F
    { "LD IXL, A", function(z)
            z:LD("IX", 0x1234)
            z:LD("A", 0x99)
            z:LD("IXL", "A")
        end, { A=0x99, IX=0x1299 } },
    
    
    -- 0x84
    { "ADD A,IXH", function(z)
            z:LD("IX", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADD", "A", "IXH")
            end, { A = 0x12, IX = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

 { "ADD A, IXH", function(z) z:LD("A", 1) z:LD("IX", 0x200) z:assemble("ADD", "A", "IXH") end,
     { A = 3, IX = 0x200, F={"-S", "-Z", "-H", "-V", "-N", "-C"}} },
 { "ADD A, IXH", function(z) z:LD("A", 0x0F) z:LD("IX", 0x100) z:assemble("ADD", "A", "IXH") end,
     { A = 0x10, IX = 0x100, F={"-S", "-Z", "H", "-V", "-N", "-C"}} },
 { "ADD A, IXH", function(z) z:LD("A", 0x80) z:LD("IX", 0x8000) z:assemble("ADD", "A", "IXH") end,
     { A = 0, IX = 0x8000, F={"-S", "Z", "-H", "V", "-N", "C"}} },
 { "ADD A, IXH", function(z) z:LD("A", 0x7F) z:LD("IX", 0x100) z:assemble("ADD", "A", "IXH") end,
     { A = 0x80, IX = 0x100, F={"S", "-Z", "H", "V", "-N", "-C"}} },
 { "ADD A, IXH", function(z) z:LD("A", 0xFF) z:LD("IX", 0x200) z:assemble("ADD", "A", "IXH") end,
     { A = 1, IX = 0x200, F={"-S", "-Z", "H", "-V", "-N", "C"}} },


    -- 0x85
    { "ADD A,IXL", function(z)
            z:LD("IX", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADD", "A", "IXL")
            end, { A = 0x34, IX = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

 { "ADD A, IXL", function(z) z:LD("A", 1) z:LD("IX", 0x2) z:assemble("ADD", "A", "IXL") end,
     { A = 3, IX = 0x2, F={"-S", "-Z", "-H", "-V", "-N", "-C"}} },
 { "ADD A, IXL", function(z) z:LD("A", 0x0F) z:LD("IX", 0x1) z:assemble("ADD", "A", "IXL") end,
     { A = 0x10, IX = 0x1, F={"-S", "-Z", "H", "-V", "-N", "-C"}} },
 { "ADD A, IXL", function(z) z:LD("A", 0x80) z:LD("IX", 0x80) z:assemble("ADD", "A", "IXL") end,
     { A = 0, IX = 0x80, F={"-S", "Z", "-H", "V", "-N", "C"}} },
 { "ADD A, IXL", function(z) z:LD("A", 0x7F) z:LD("IX", 0x1) z:assemble("ADD", "A", "IXL") end,
     { A = 0x80, IX = 0x1, F={"S", "-Z", "H", "V", "-N", "-C"}} },
 { "ADD A, IXL", function(z) z:LD("A", 0xFF) z:LD("IX", 0x2) z:assemble("ADD", "A", "IXL") end,
     { A = 1, IX = 0x2, F={"-S", "-Z", "H", "-V", "-N", "C"}} },
 
 
     -- 0x8C
    { "ADC A,IXH", function(z)
            z:assemble("SCF")
            z:assemble("CCF")
            z:LD("IX", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADC", "A", "IXH")
            end, { A = 0x12, IX = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

     -- 0x8C
    { "ADC A,IXH", function(z)
            z:assemble("SCF")
            z:LD("IX", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADC", "A", "IXH")
            end, { A = 0x13, IX = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

 { "ADC A, IXH", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 1) z:LD("IX", 0x200) z:assemble("ADC", "A", "IXH") end,
     { A = 3, IX = 0x200, F={"-S", "-Z", "-H", "-V", "-N", "-C"}} },
 { "ADC A, IXH", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0x0F) z:LD("IX", 0x100) z:assemble("ADC", "A", "IXH") end,
     { A = 0x10, IX = 0x100, F={"-S", "-Z", "H", "-V", "-N", "-C"}} },
 { "ADC A, IXH", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0x80) z:LD("IX", 0x8000) z:assemble("ADC", "A", "IXH") end,
     { A = 0, IX = 0x8000, F={"-S", "Z", "-H", "V", "-N", "C"}} },
 { "ADC A, IXH", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0x7F) z:LD("IX", 0x100) z:assemble("ADC", "A", "IXH") end,
     { A = 0x80, IX = 0x100, F={"S", "-Z", "H", "V", "-N", "-C"}} },
 { "ADC A, IXH", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0xFF) z:LD("IX", 0x200) z:assemble("ADC", "A", "IXH") end,
     { A = 1, IX = 0x200, F={"-S", "-Z", "H", "-V", "-N", "C"}} },

    -- 0x8D
    { "ADC A,IXL", function(z)
            z:assemble("SCF")
            z:assemble("CCF")
            z:LD("IX", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADC", "A", "IXL")
            end, { A = 0x34, IX = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

    { "ADC A,IXL", function(z)
            z:assemble("SCF")
            z:assemble("CCF")
            z:LD("IX", 0x1234)
            z:LD("A", 0x00)
            z:assemble("ADC", "A", "IXL")
            end, { A = 0x34, IX = 0x1234, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } }, 

 { "ADC A, IXL", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 1) z:LD("IX", 0x2) z:assemble("ADC", "A", "IXL") end,
     { A = 3, IX = 0x2, F={"-S", "-Z", "-H", "-V", "-N", "-C"}} },
 { "ADC A, IXL", function(z) z:LD("A", 0x0F)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("IX", 0x1) z:assemble("ADC", "A", "IXL") end,
     { A = 0x10, IX = 0x1, F={"-S", "-Z", "H", "-V", "-N", "-C"}} },
 { "ADC A, IXL", function(z) z:LD("A", 0x80)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("IX", 0x80) z:assemble("ADC", "A", "IXL") end,
     { A = 0, IX = 0x80, F={"-S", "Z", "-H", "V", "-N", "C"}} },
 { "ADC A, IXL", function(z) z:LD("A", 0x7F)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("IX", 0x1) z:assemble("ADC", "A", "IXL") end,
     { A = 0x80, IX = 0x1, F={"S", "-Z", "H", "V", "-N", "-C"}} },
 { "ADC A, IXL", function(z) z:LD("A", 0xFF)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("IX", 0x2) z:assemble("ADC", "A", "IXL") end,
     { A = 1, IX = 0x2, F={"-S", "-Z", "H", "-V", "-N", "C"}} },


-- 0x94
{ "SUB A,IXH", function(z)
        z:LD("A", 0x26)
        z:LD("IX", 0x0201)
        z:assemble("SUB", "A", "IXH")
    end,
    { A = 0x024, IX=0x201, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
{ "SUB A,IXH zero", function(z)
        z:LD("A", 0x26)
        z:LD("IX", 0x2602)
        z:assemble("SUB", "A", "IXH")
    end,
    { A = 0x00, IX=0x2602, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "SUB A,IXH half", function(z)
        z:LD("A", 0x10)
        z:LD("IX", 0x0203)
        z:assemble("SUB", "A", "IXH")
    end,
    { A = 0x0E, IX=0x203, F={ "-S", "-Z", "H", "-V", "N", "-C" } } },
{ "SUB A,IXH carry", function(z)
        z:LD("A", 0x00)
        z:LD("IX", 0x0204)
        z:assemble("SUB", "A", "IXH")
    end,
    { A = 0xFE, IX=0x204, F={ "S", "-Z", "H", "-V", "N", "C" } } },
{ "SUB A,IXH overflow", function(z)
        z:LD("A", 0x80)
        z:LD("IX", 0x0205)
        z:assemble("SUB", "A", "IXH")
    end,
    { A = 0x7E, IX=0x205, F={ "-S", "-Z", "H", "V", "N", "-C" } } },

-- 0x95
{ "SUB A,IXL", function(z)
        z:LD("A", 0x26)
        z:LD("IX", 0x9902)
        z:assemble("SUB", "A", "IXL")
    end,
    { A = 0x024, IX=0x9902, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
{ "SUB A,IXL zero", function(z)
        z:LD("A", 0x26)
        z:LD("IX", 0x8826)
        z:assemble("SUB", "A", "IXL")
    end,
    { A = 0x00, IX=0x8826, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "SUB A,IXL half", function(z)
        z:LD("A", 0x10)
        z:LD("IX", 0x7702)
        z:assemble("SUB", "A", "IXL")
    end,
    { A = 0x0E, IX=0x7702, F={ "-S", "-Z", "H", "-V", "N", "-C" } } },
{ "SUB A,IXL carry", function(z)
        z:LD("A", 0x00)
        z:LD("IX", 0x6602)
        z:assemble("SUB", "A", "IXL")
    end,
    { A = 0xFE, IX=0x6602, F={ "S", "-Z", "H", "-V", "N", "C" } } },
{ "SUB A,IXL overflow", function(z)
        z:LD("A", 0x80)
        z:LD("IX", 0x5502)
        z:assemble("SUB", "A", "IXL")
    end,
    { A = 0x7E, IX=0x5502, F={ "-S", "-Z", "H", "V", "N", "-C" } } },

-- 0x9C
{ "SBC A,IXH zero", function(z)
        z:LD("A", 0x00) z:LD("IX", 0x0111) z:assemble("SUB", "A", "IXH")
        z:LD("A", 0x02)
        z:LD("IXH", 0x01)
        z:assemble("SBC", "A", "IXH")
    end,
    { A = 0x00, IX = 0x0111, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "SBC A,IXH not zero", function(z)
        z:LD("A", 0x01) z:LD("IX", 0x0022) z:assemble("SUB", "A", "IXH")
        z:LD("A", 0x02)
        z:LD("IXH", 0x01)
        z:assemble("SBC", "A", "IXH")
    end,
    { A = 0x01, IX = 0x0122, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
 { "SBC A,IXH underflow", function(z)
        z:LD("A", 0x00) z:LD("IX", 0x0133) z:assemble("SUB", "A", "IXH")
        z:LD("A", 0x02)
        z:LD("IXH", 0x01)
        z:assemble("SBC", "A", "IXH")
    end,
    { A = 0x00, IX = 0x0133, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },   

-- 0x9D
{ "SBC A,IXL zero", function(z)
        z:LD("A", 0x00) z:LD("IX", 0x1101) z:assemble("SUB", "A", "IXL")
        z:LD("A", 0x02)
        z:LD("IXL", 0x01)
        z:assemble("SBC", "A", "IXL")
    end,
    { A = 0x00, IX = 0x1101, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "SBC A,IXL not zero", function(z)
        z:LD("A", 0x01) z:LD("IX", 0x2200) z:assemble("SUB", "A", "IXL")
        z:LD("A", 0x02)
        z:LD("IXL", 0x01)
        z:assemble("SBC", "A", "IXL")
    end,
    { A = 0x01, IX = 0x2201, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
 { "SBC A,IXL underflow", function(z)
        z:LD("A", 0x00) z:LD("IX", 0x3301) z:assemble("SUB", "A", "IXL")
        z:LD("A", 0x02)
        z:LD("IXL", 0x01)
        z:assemble("SBC", "A", "IXL")
    end,
    { A = 0x00, IX = 0x3301, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },   

    -- 0xA4
    { "AND IXH", function(z) z:LD("A", 0x8F) z:LD("IX", 0x01EE) z:AND("IXH") end, { A=0x01, IX=0x01EE, F={"-Z", "-N", "H", "-P", "-S", "-C"} } },   -- odd number of bits = Parity clear
    { "AND IXH zero", function(z) z:LD("A", 0x80) z:LD("IX", 0x01EE) z:AND("IXH") end, { A=0x00, IX=0x01EE, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set
    
    -- 0xA5
    { "AND IXL", function(z) z:LD("A", 0x8F) z:LD("IX", 0xAA01) z:AND("IXL") end, { A=0x01, IX=0xAA01, F={"-Z", "-N", "H", "-P", "-S", "-C"} } },   -- odd number of bits = Parity clear
    { "AND IXL zero", function(z) z:LD("A", 0x80) z:LD("IX", 0xAA01) z:AND("IXL") end, { A=0x00, IX=0xAA01, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set

        -- 0xAC
    { "XOR IXH", function(z) z:LD("A", 0x8F) z:LD("IX", 0x0133) z:XOR("IXH") end, { A=0x8E, IX=0x0133, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- odd number of bits = Parity clear
    { "XOR IXH zero", function(z) z:LD("A", 0x01) z:LD("IX", 0x0144) z:XOR("IXH") end, { A=0x00, IX=0x0144, F={"Z", "-N", "-H", "P", "-S", "-C"} } },   -- even number of bits = Parity set
        -- 0xAD
    { "XOR IXL", function(z) z:LD("A", 0x8F) z:LD("IX", 0x5501) z:XOR("IXL") end, { A=0x8E, IX=0x5501, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- odd number of bits = Parity clear
    { "XOR IXL zero", function(z) z:LD("A", 0x01) z:LD("IX", 0x6601) z:XOR("IXL") end, { A=0x00, IX=0x6601, F={"Z", "-N", "-H", "P", "-S", "-C"} } },   -- even number of bits = Parity set

    -- 0xB4
    { "OR IXH", function(z) z:LD("A", 0x80) z:LD("IX", 0x0155) z:OR("IXH") end, { A=0x81, IX=0x0155, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set
    -- 0xB6
    { "OR IXL", function(z) z:LD("A", 0x80) z:LD("IX", 0x6601) z:OR("IXL") end, { A=0x81, IX=0x6601, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set

-- 0xBC
{ "CP IXH", function(z)
        z:LD("A", 0x26)
        z:LD("IX", 0x0233)
        z:assemble("CP", "IXH")
    end,
    { A = 0x026, IX=0x233, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
{ "CP IXH zero", function(z)
        z:LD("A", 0x26)
        z:LD("IX", 0x2644)
        z:assemble("CP", "IXH")
    end,
    { A = 0x26, IX=0x2644, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "CP IXH half", function(z)
        z:LD("A", 0x10)
        z:LD("IX", 0x0255)
        z:assemble("CP", "IXH")
    end,
    { A = 0x10, IX=0x255, F={ "-S", "-Z", "H", "-V", "N", "-C" } } },
{ "CP IXH carry", function(z)
        z:LD("A", 0x00)
        z:LD("IX", 0x0266)
        z:assemble("CP", "IXH")
    end,
    { A = 0x00, IX=0x266, F={ "S", "-Z", "H", "-V", "N", "C" } } },
{ "CP IXH overflow", function(z)
        z:LD("A", 0x80)
        z:LD("IX", 0x0277)
        z:assemble("CP", "IXH")
    end,
    { A = 0x80, IX=0x277, F={ "-S", "-Z", "H", "V", "N", "-C" } } },

-- 0xBD
{ "CP IXL", function(z)
        z:LD("A", 0x26)
        z:LD("IX", 0x2202)
        z:assemble("CP", "IXL")
    end,
    { A = 0x026, IX=0x2202, F={ "-S", "-Z", "-H", "-V", "N", "-C" } } },
{ "CP IXL zero", function(z)
        z:LD("A", 0x26)
        z:LD("IX", 0x3326)
        z:assemble("CP", "IXL")
    end,
    { A = 0x26, IX=0x3326, F={ "-S", "Z", "-H", "-V", "N", "-C" } } },
{ "CP IXL half", function(z)
        z:LD("A", 0x10)
        z:LD("IX", 0x4402)
        z:assemble("CP", "IXL")
    end,
    { A = 0x10, IX=0x4402, F={ "-S", "-Z", "H", "-V", "N", "-C" } } },
{ "CP IXL carry", function(z)
        z:LD("A", 0x00)
        z:LD("IX", 0x5502)
        z:assemble("CP", "IXL")
    end,
    { A = 0x00, IX=0x5502, F={ "S", "-Z", "H", "-V", "N", "C" } } },
{ "CP IXL overflow", function(z)
        z:LD("A", 0x80)
        z:LD("IX", 0x6602)
        z:assemble("CP", "IXL")
    end,
    { A = 0x80, IX=0x6602, F={ "-S", "-Z", "H", "V", "N", "-C" } } },

      -- 0xE1
    { "POP IX", function(z)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("LD", "BC", 0x1234)
            z:assemble("PUSH", "BC")
            z:assemble("POP", "IX")
        end, {SP=0x6000, IX=0x1234, B=0x12, C=0x34, [0x5FFE]=0x34, [0x5FFF]=0x12 } },
    { "POP IX (wrap)", function(z)
            z:assemble("LD", "SP", 0x0001)
            z:assemble("LD", "BC", 0x1234)
            z:assemble("PUSH", "BC")
            z:assemble("POP", "IX")
        end, {SP=0x1, IX=0x1234, B=0x12, C=0x34, [0xFFFF]=0x34, [0x0000]=0x12 } },

    -- 0xE3
    { "EX (SP), IX", function(z) 
            z:LD("SP", 0x6000)
            z:LD("IX", 0x1234)
            z:LD("A", 0xCD)
            z:LD("(0x6000)", "A")
            z:LD("A", 0xAB)
            z:LD("(0x6001)", "A")
            z:assemble("EX", "(SP)", "IX")
        end, { SP=0x6000, IX=0xABCD, A=0xAB, [0x6000]=0x34, [0x6001]=0x12 } },
    { "EX (SP), IX  rollover", function(z) 
            z:LD("SP", 0xFFFF)
            z:LD("IX", 0x1234)
            z:LD("A", 0xCD)
            z:LD("(0xFFFF)", "A")
            z:LD("A", 0xAB)
            z:LD("(0x0000)", "A")
            z:assemble("EX", "(SP)", "IX")
        end, { SP=0xFFFF, IX=0xABCD, A=0xAB, [0xFFFF]=0x34, [0x0000]=0x12 } },

    -- 0xE5
    { "PUSH IX", function(z) 
            z:assemble("LD", "IX", 0x4321)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("PUSH", "IX")
            end, { IX=0x4321, SP=0x5FFE, [0x5FFE]=0x21, [0x5FFF]=0x43 } },
    { "PUSH IX wrap0", function(z) 
            z:assemble("LD", "IX", 0x4321)
            z:assemble("LD", "SP", 0x0000)
            z:assemble("PUSH", "IX")
            end, { IX=0x4321, SP=0xFFFE, [0xFFFE]=0x21, [0xFFFF]=0x43 } },
    { "PUSH IX wrap1", function(z) 
            z:assemble("NOP")       -- this will get overwritten
            z:assemble("LD", "IX", 0x4321)
            z:assemble("LD", "SP", 0x0001)
            z:assemble("PUSH", "IX")
            end, { IX=0x4321, SP=0xFFFF, [0xFFFF]=0x21, [0x0000]=0x43 } },

-- 0xE9
{ "JP (IX)", function(z)
        z:LD("IX", 12)      -- 0
        z:LD("A", 0x01)         -- 4
        z:OR("A")               -- 6
        z:assemble("JP", "(IX)")     -- 7
        z:LD("A", 0x20)         -- 9
        z:assemble("INC", "A")   -- 11
        z:assemble("INC", "A")   -- 12
        z:assemble("INC", "A")   -- 13
        z:assemble("INC", "A")   -- 14
        end,
        { A = 0x04, IX=12, F={"-S", "-Z", "-H", "-V", "-N", "-C"} } },

}

return DD_instruction_tests
