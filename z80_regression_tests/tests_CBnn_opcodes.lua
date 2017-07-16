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

local CB_instruction_tests = { ---[[

-- 0x00
{ "RLC B (B=0)", function(z)
        z:assemble("SCF")
        z:LD("B", 0)
        z:assemble("RLC", "B")
    end, { B = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RLC B (B=1)", function(z)
        z:assemble("SCF")
        z:LD("B", 1)
        z:assemble("RLC", "B")
    end, { B = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RLC B (B=0x80)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x80)
        z:assemble("RLC", "B")
    end, { B = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RLC B (B=0xff)", function(z)
        z:assemble("SCF")
        z:LD("B", 0xff)
        z:assemble("RLC", "B")
    end, { B = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC B (B=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("B", 0xA5)
        z:assemble("RLC", "B")
    end, { B = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC B (B=0x40)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x40)
        z:assemble("RLC", "B")
    end, { B = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
-- 0x01
{ "RLC C (C=0)", function(z)
        z:assemble("SCF")
        z:LD("C", 0)
        z:assemble("RLC", "C")
    end, { C = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RLC C (C=1)", function(z)
        z:assemble("SCF")
        z:LD("C", 1)
        z:assemble("RLC", "C")
    end, { C = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RLC C (C=0x80)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x80)
        z:assemble("RLC", "C")
    end, { C = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RLC C (C=0xff)", function(z)
        z:assemble("SCF")
        z:LD("C", 0xff)
        z:assemble("RLC", "C")
    end, { C = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC C (C=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("C", 0xA5)
        z:assemble("RLC", "C")
    end, { C = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC C (C=0x40)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x40)
        z:assemble("RLC", "C")
    end, { C = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
-- 0x02
{ "RLC D (D=0)", function(z)
        z:assemble("SCF")
        z:LD("D", 0)
        z:assemble("RLC", "D")
    end, { D = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RLC D (D=1)", function(z)
        z:assemble("SCF")
        z:LD("D", 1)
        z:assemble("RLC", "D")
    end, { D = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RLC D (D=0x80)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x80)
        z:assemble("RLC", "D")
    end, { D = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RLC D (D=0xff)", function(z)
        z:assemble("SCF")
        z:LD("D", 0xff)
        z:assemble("RLC", "D")
    end, { D = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC D (D=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("D", 0xA5)
        z:assemble("RLC", "D")
    end, { D = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC D (D=0x40)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x40)
        z:assemble("RLC", "D")
    end, { D = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
-- 0x03
{ "RLC E (E=0)", function(z)
        z:assemble("SCF")
        z:LD("E", 0)
        z:assemble("RLC", "E")
    end, { E = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RLC E (E=1)", function(z)
        z:assemble("SCF")
        z:LD("E", 1)
        z:assemble("RLC", "E")
    end, { E = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RLC E (E=0x80)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x80)
        z:assemble("RLC", "E")
    end, { E = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RLC E (E=0xff)", function(z)
        z:assemble("SCF")
        z:LD("E", 0xff)
        z:assemble("RLC", "E")
    end, { E = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC E (E=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("E", 0xA5)
        z:assemble("RLC", "E")
    end, { E = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC E (E=0x40)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x40)
        z:assemble("RLC", "E")
    end, { E = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
-- 0x04
{ "RLC H (H=0)", function(z)
        z:assemble("SCF")
        z:LD("H", 0)
        z:assemble("RLC", "H")
    end, { H = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RLC H (H=1)", function(z)
        z:assemble("SCF")
        z:LD("H", 1)
        z:assemble("RLC", "H")
    end, { H = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RLC H (H=0x80)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x80)
        z:assemble("RLC", "H")
    end, { H = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RLC H (H=0xff)", function(z)
        z:assemble("SCF")
        z:LD("H", 0xff)
        z:assemble("RLC", "H")
    end, { H = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC H (H=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("H", 0xA5)
        z:assemble("RLC", "H")
    end, { H = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC H (H=0x40)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x40)
        z:assemble("RLC", "H")
    end, { H = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
-- 0x05
{ "RLC L (L=0)", function(z)
        z:assemble("SCF")
        z:LD("L", 0)
        z:assemble("RLC", "L")
    end, { L = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RLC L (L=1)", function(z)
        z:assemble("SCF")
        z:LD("L", 1)
        z:assemble("RLC", "L")
    end, { L = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RLC L (L=0x80)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x80)
        z:assemble("RLC", "L")
    end, { L = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RLC L (L=0xff)", function(z)
        z:assemble("SCF")
        z:LD("L", 0xff)
        z:assemble("RLC", "L")
    end, { L = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC L (L=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("L", 0xA5)
        z:assemble("RLC", "L")
    end, { L = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC L (L=0x40)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x40)
        z:assemble("RLC", "L")
    end, { L = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x06
{ "RLC (HL) (HL)=0xA5", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xA5)
        z:assemble("RLC", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC (HL) (HL)=0x40", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x40)
        z:assemble("RLC", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RLC (HL) (HL)=0x80", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x80)
        z:assemble("RLC", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },

-- 0x07
{ "RLC A (A=0)", function(z)
        z:assemble("SCF")
        z:LD("A", 0)
        z:assemble("RLC", "A")
    end, { A = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RLC A (A=1)", function(z)
        z:assemble("SCF")
        z:LD("A", 1)
        z:assemble("RLC", "A")
    end, { A = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RLC A (A=0x80)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x80)
        z:assemble("RLC", "A")
    end, { A = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RLC A (A=0xff)", function(z)
        z:assemble("SCF")
        z:LD("A", 0xff)
        z:assemble("RLC", "A")
    end, { A = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC A (A=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("A", 0xA5)
        z:assemble("RLC", "A")
    end, { A = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RLC A (A=0x40)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x40)
        z:assemble("RLC", "A")
    end, { A = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x08
{ "RRC B (B=0)", function(z)
        z:assemble("SCF")
        z:LD("B", 0)
        z:assemble("RRC", "B")
    end, { B = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RRC B (B=2)", function(z)
        z:assemble("SCF")
        z:LD("B", 2)
        z:assemble("RRC", "B")
    end, { B = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RRC B (B=0x01)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x01)
        z:assemble("RRC", "B")
    end, { B = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RRC B (B=0xff)", function(z)
        z:assemble("SCF")
        z:LD("B", 0xff)
        z:assemble("RRC", "B")
    end, { B = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC B (B=0xA5)", function(z) -- 0x10100101 >> 11010010
        z:assemble("SCF")
        z:LD("B", 0xA5)
        z:assemble("RRC", "B")
    end, { B = 0xd2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC B (B=0x80)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x80)
        z:assemble("RRC", "B")
    end, { B = 0x40, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x09
{ "RRC C (C=0)", function(z)
        z:assemble("SCF")
        z:LD("C", 0)
        z:assemble("RRC", "C")
    end, { C = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RRC C (C=2)", function(z)
        z:assemble("SCF")
        z:LD("C", 2)
        z:assemble("RRC", "C")
    end, { C = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RRC C (C=0x01)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x01)
        z:assemble("RRC", "C")
    end, { C = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RRC C (C=0xff)", function(z)
        z:assemble("SCF")
        z:LD("C", 0xff)
        z:assemble("RRC", "C")
    end, { C = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC C (C=0xA5)", function(z) -- 0x10100101 >> 11010010
        z:assemble("SCF")
        z:LD("C", 0xA5)
        z:assemble("RRC", "C")
    end, { C = 0xd2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC C (C=0x80)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x80)
        z:assemble("RRC", "C")
    end, { C = 0x40, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x0A
{ "RRC D (D=0)", function(z)
        z:assemble("SCF")
        z:LD("D", 0)
        z:assemble("RRC", "D")
    end, { D = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RRC D (D=2)", function(z)
        z:assemble("SCF")
        z:LD("D", 2)
        z:assemble("RRC", "D")
    end, { D = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RRC D (D=0x01)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x01)
        z:assemble("RRC", "D")
    end, { D = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RRC D (D=0xff)", function(z)
        z:assemble("SCF")
        z:LD("D", 0xff)
        z:assemble("RRC", "D")
    end, { D = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC D (D=0xA5)", function(z) -- 0x10100101 >> 11010010
        z:assemble("SCF")
        z:LD("D", 0xA5)
        z:assemble("RRC", "D")
    end, { D = 0xd2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC D (D=0x80)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x80)
        z:assemble("RRC", "D")
    end, { D = 0x40, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x0D
{ "RRC E (E=0)", function(z)
        z:assemble("SCF")
        z:LD("E", 0)
        z:assemble("RRC", "E")
    end, { E = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RRC E (E=2)", function(z)
        z:assemble("SCF")
        z:LD("E", 2)
        z:assemble("RRC", "E")
    end, { E = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RRC E (E=0x01)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x01)
        z:assemble("RRC", "E")
    end, { E = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RRC E (E=0xff)", function(z)
        z:assemble("SCF")
        z:LD("E", 0xff)
        z:assemble("RRC", "E")
    end, { E = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC E (E=0xA5)", function(z) -- 0x10100101 >> 11010010
        z:assemble("SCF")
        z:LD("E", 0xA5)
        z:assemble("RRC", "E")
    end, { E = 0xd2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC E (E=0x80)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x80)
        z:assemble("RRC", "E")
    end, { E = 0x40, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x0C
{ "RRC H (H=0)", function(z)
        z:assemble("SCF")
        z:LD("H", 0)
        z:assemble("RRC", "H")
    end, { H = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RRC H (H=2)", function(z)
        z:assemble("SCF")
        z:LD("H", 2)
        z:assemble("RRC", "H")
    end, { H = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RRC H (H=0x01)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x01)
        z:assemble("RRC", "H")
    end, { H = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RRC H (H=0xff)", function(z)
        z:assemble("SCF")
        z:LD("H", 0xff)
        z:assemble("RRC", "H")
    end, { H = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC H (H=0xA5)", function(z) -- 0x10100101 >> 11010010
        z:assemble("SCF")
        z:LD("H", 0xA5)
        z:assemble("RRC", "H")
    end, { H = 0xd2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC H (H=0x80)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x80)
        z:assemble("RRC", "H")
    end, { H = 0x40, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x0D
{ "RRC L (L=0)", function(z)
        z:assemble("SCF")
        z:LD("L", 0)
        z:assemble("RRC", "L")
    end, { L = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RRC L (L=2)", function(z)
        z:assemble("SCF")
        z:LD("L", 2)
        z:assemble("RRC", "L")
    end, { L = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RRC L (L=0x01)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x01)
        z:assemble("RRC", "L")
    end, { L = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RRC L (L=0xff)", function(z)
        z:assemble("SCF")
        z:LD("L", 0xff)
        z:assemble("RRC", "L")
    end, { L = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC L (L=0xA5)", function(z) -- 0x10100101 >> 11010010
        z:assemble("SCF")
        z:LD("L", 0xA5)
        z:assemble("RRC", "L")
    end, { L = 0xd2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC L (L=0x80)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x80)
        z:assemble("RRC", "L")
    end, { L = 0x40, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x0E
{ "RRC (HL) (HL)=0xA5", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xA5)  -- 10100101 >> 11010010+C
        z:assemble("RRC", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0xd2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC (HL) (HL)=0x40", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x40)
        z:assemble("RRC", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x20, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RRC (HL) (HL)=0x80", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x01)
        z:assemble("RRC", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },

-- 0x0F
{ "RRC A (A=0)", function(z)
        z:assemble("SCF")
        z:LD("A", 0)
        z:assemble("RRC", "A")
    end, { A = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "RRC A (A=2)", function(z)
        z:assemble("SCF")
        z:LD("A", 2)
        z:assemble("RRC", "A")
    end, { A = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RRC A (A=0x01)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x01)
        z:assemble("RRC", "A")
    end, { A = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RRC A (A=0xff)", function(z)
        z:assemble("SCF")
        z:LD("A", 0xff)
        z:assemble("RRC", "A")
    end, { A = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC A (A=0xA5)", function(z) -- 0x10100101 >> 11010010
        z:assemble("SCF")
        z:LD("A", 0xA5)
        z:assemble("RRC", "A")
    end, { A = 0xd2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RRC A (A=0x80)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x80)
        z:assemble("RRC", "A")
    end, { A = 0x40, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },


-- 0x10
{ "RL B (B=0)", function(z)
        z:assemble("SCF")
        z:LD("B", 0)
        z:assemble("RL", "B")
    end, { B = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RL B (B=1)", function(z)
        z:assemble("SCF")
        z:LD("B", 1)
        z:assemble("RL", "B")
    end, { B = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "RL B (B=0x80)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("B", 0x80)
        z:assemble("RL", "B")
    end, { B = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RL B (B=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("B", 0xff)
        z:assemble("RL", "B")
    end, { B = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RL B (B=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("B", 0xA5)
        z:assemble("RL", "B")
    end, { B = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RL B (B=0x40)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x40)
        z:assemble("RL", "B")
    end, { B = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x11
{ "RL C (C=0)", function(z)
        z:assemble("SCF")
        z:LD("C", 0)
        z:assemble("RL", "C")
    end, { C = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RL C (C=1)", function(z)
        z:assemble("SCF")
        z:LD("C", 1)
        z:assemble("RL", "C")
    end, { C = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "RL C (C=0x80)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("C", 0x80)
        z:assemble("RL", "C")
    end, { C = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RL C (C=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("C", 0xff)
        z:assemble("RL", "C")
    end, { C = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RL C (C=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("C", 0xA5)
        z:assemble("RL", "C")
    end, { C = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RL C (C=0x40)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x40)
        z:assemble("RL", "C")
    end, { C = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x12
{ "RL D (D=0)", function(z)
        z:assemble("SCF")
        z:LD("D", 0)
        z:assemble("RL", "D")
    end, { D = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RL D (D=1)", function(z)
        z:assemble("SCF")
        z:LD("D", 1)
        z:assemble("RL", "D")
    end, { D = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "RL D (D=0x80)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("D", 0x80)
        z:assemble("RL", "D")
    end, { D = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RL D (D=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("D", 0xff)
        z:assemble("RL", "D")
    end, { D = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RL D (D=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("D", 0xA5)
        z:assemble("RL", "D")
    end, { D = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RL D (D=0x40)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x40)
        z:assemble("RL", "D")
    end, { D = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x13
{ "RL E (E=0)", function(z)
        z:assemble("SCF")
        z:LD("E", 0)
        z:assemble("RL", "E")
    end, { E = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RL E (E=1)", function(z)
        z:assemble("SCF")
        z:LD("E", 1)
        z:assemble("RL", "E")
    end, { E = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "RL E (E=0x80)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("E", 0x80)
        z:assemble("RL", "E")
    end, { E = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RL E (E=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("E", 0xff)
        z:assemble("RL", "E")
    end, { E = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RL E (E=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("E", 0xA5)
        z:assemble("RL", "E")
    end, { E = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RL E (E=0x40)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x40)
        z:assemble("RL", "E")
    end, { E = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x14
{ "RL H (H=0)", function(z)
        z:assemble("SCF")
        z:LD("H", 0)
        z:assemble("RL", "H")
    end, { H = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RL H (H=1)", function(z)
        z:assemble("SCF")
        z:LD("H", 1)
        z:assemble("RL", "H")
    end, { H = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "RL H (H=0x80)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("H", 0x80)
        z:assemble("RL", "H")
    end, { H = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RL H (H=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("H", 0xff)
        z:assemble("RL", "H")
    end, { H = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RL H (H=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("H", 0xA5)
        z:assemble("RL", "H")
    end, { H = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RL H (H=0x40)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x40)
        z:assemble("RL", "H")
    end, { H = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x15
{ "RL L (L=0)", function(z)
        z:assemble("SCF")
        z:LD("L", 0)
        z:assemble("RL", "L")
    end, { L = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RL L (L=1)", function(z)
        z:assemble("SCF")
        z:LD("L", 1)
        z:assemble("RL", "L")
    end, { L = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "RL L (L=0x80)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("L", 0x80)
        z:assemble("RL", "L")
    end, { L = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RL L (L=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("L", 0xff)
        z:assemble("RL", "L")
    end, { L = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RL L (L=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("L", 0xA5)
        z:assemble("RL", "L")
    end, { L = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RL L (L=0x40)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x40)
        z:assemble("RL", "L")
    end, { L = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x16
{ "RL (HL) (HL)=0xA5", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xA5)  -- 10100101 << 0100 1011
        z:assemble("RL", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RL (HL) (HL)=0x40", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x40)
        z:assemble("RL", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "RL (HL) (HL)=0x80", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x01)
        z:assemble("RL", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "RL (HL) (HL)=0x80", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x81)
        z:assemble("RL", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },

-- 0x17
{ "RL A (A=0)", function(z)
        z:assemble("SCF")
        z:LD("A", 0)
        z:assemble("RL", "A")
    end, { A = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RL A (A=1)", function(z)
        z:assemble("SCF")
        z:LD("A", 1)
        z:assemble("RL", "A")
    end, { A = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "RL A (A=0x80)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0x80)
        z:assemble("RL", "A")
    end, { A = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RL A (A=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0xff)
        z:assemble("RL", "A")
    end, { A = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RL A (A=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("A", 0xA5)
        z:assemble("RL", "A")
    end, { A = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "RL A (A=0x40)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x40)
        z:assemble("RL", "A")
    end, { A = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },


-- 0x18
{ "RR B (B=0)", function(z)
        z:assemble("SCF")
        z:LD("B", 0)
        z:assemble("RR", "B")
    end, { B = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RR B (B=1)", function(z)
        z:assemble("SCF")
        z:LD("B", 1)
        z:assemble("RR", "B")
    end, { B = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR B (B=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("B", 0x1)
        z:assemble("RR", "B")
    end, { B = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RR B (B=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("B", 0xff)
        z:assemble("RR", "B")
    end, { B = 0x7F, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR B (B=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("B", 0xA5) -- 1010 0101
        z:assemble("RR", "B")
    end, { B = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RR B (B=0x02)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x02)
        z:assemble("RR", "B")
    end, { B = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },


-- 0x19
{ "RR C (C=0)", function(z)
        z:assemble("SCF")
        z:LD("C", 0)
        z:assemble("RR", "C")
    end, { C = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RR C (C=1)", function(z)
        z:assemble("SCF")
        z:LD("C", 1)
        z:assemble("RR", "C")
    end, { C = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR C (C=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("C", 0x1)
        z:assemble("RR", "C")
    end, { C = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RR C (C=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("C", 0xff)
        z:assemble("RR", "C")
    end, { C = 0x7F, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR C (C=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("C", 0xA5) -- 1010 0101
        z:assemble("RR", "C")
    end, { C = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RR C (C=0x02)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x02)
        z:assemble("RR", "C")
    end, { C = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },


-- 0x1A
{ "RR D (D=0)", function(z)
        z:assemble("SCF")
        z:LD("D", 0)
        z:assemble("RR", "D")
    end, { D = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RR D (D=1)", function(z)
        z:assemble("SCF")
        z:LD("D", 1)
        z:assemble("RR", "D")
    end, { D = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR D (D=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("D", 0x1)
        z:assemble("RR", "D")
    end, { D = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RR D (D=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("D", 0xff)
        z:assemble("RR", "D")
    end, { D = 0x7F, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR D (D=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("D", 0xA5) -- 1010 0101
        z:assemble("RR", "D")
    end, { D = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RR D (D=0x02)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x02)
        z:assemble("RR", "D")
    end, { D = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },


-- 0x1B
{ "RR E (E=0)", function(z)
        z:assemble("SCF")
        z:LD("E", 0)
        z:assemble("RR", "E")
    end, { E = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RR E (E=1)", function(z)
        z:assemble("SCF")
        z:LD("E", 1)
        z:assemble("RR", "E")
    end, { E = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR E (E=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("E", 0x1)
        z:assemble("RR", "E")
    end, { E = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RR E (E=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("E", 0xff)
        z:assemble("RR", "E")
    end, { E = 0x7F, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR E (E=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("E", 0xA5) -- 1010 0101
        z:assemble("RR", "E")
    end, { E = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RR E (E=0x02)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x02)
        z:assemble("RR", "E")
    end, { E = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },


-- 0x1C
{ "RR H (H=0)", function(z)
        z:assemble("SCF")
        z:LD("H", 0)
        z:assemble("RR", "H")
    end, { H = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RR H (H=1)", function(z)
        z:assemble("SCF")
        z:LD("H", 1)
        z:assemble("RR", "H")
    end, { H = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR H (H=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("H", 0x1)
        z:assemble("RR", "H")
    end, { H = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RR H (H=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("H", 0xff)
        z:assemble("RR", "H")
    end, { H = 0x7F, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR H (H=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("H", 0xA5) -- 1010 0101
        z:assemble("RR", "H")
    end, { H = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RR H (H=0x02)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x02)
        z:assemble("RR", "H")
    end, { H = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },


-- 0x1D
{ "RR L (L=0)", function(z)
        z:assemble("SCF")
        z:LD("L", 0)
        z:assemble("RR", "L")
    end, { L = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RR L (L=1)", function(z)
        z:assemble("SCF")
        z:LD("L", 1)
        z:assemble("RR", "L")
    end, { L = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR L (L=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("L", 0x1)
        z:assemble("RR", "L")
    end, { L = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RR L (L=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("L", 0xff)
        z:assemble("RR", "L")
    end, { L = 0x7F, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR L (L=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("L", 0xA5) -- 1010 0101
        z:assemble("RR", "L")
    end, { L = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RR L (L=0x02)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x02)
        z:assemble("RR", "L")
    end, { L = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x1E
{ "RR (HL) (HL)=0xA5", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xA5)  -- 10100101 >> 11010010
        z:assemble("RR", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0xd2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RR (HL) (HL)=0x01", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x01)
        z:assemble("RR", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RR (HL) (HL)=0x80", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x80)
        z:assemble("RR", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0xC0, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "RR (HL) (HL)=0x00", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x00)
        z:assemble("RR", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x1F
{ "RR A (A=0)", function(z)
        z:assemble("SCF")
        z:LD("A", 0)
        z:assemble("RR", "A")
    end, { A = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "RR A (A=1)", function(z)
        z:assemble("SCF")
        z:LD("A", 1)
        z:assemble("RR", "A")
    end, { A = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR A (A=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0x1)
        z:assemble("RR", "A")
    end, { A = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "RR A (A=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0xff)
        z:assemble("RR", "A")
    end, { A = 0x7F, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "RR A (A=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("A", 0xA5) -- 1010 0101
        z:assemble("RR", "A")
    end, { A = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "RR A (A=0x02)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x02)
        z:assemble("RR", "A")
    end, { A = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },


-- 0x20
{ "SLA B (B=0)", function(z)
        z:assemble("SCF")
        z:LD("B", 0)
        z:assemble("SLA", "B")
    end, { B = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SLA B (B=1)", function(z)
        z:assemble("SCF")
        z:LD("B", 1)
        z:assemble("SLA", "B")
    end, { B = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLA B (B=0x80)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x80)
        z:assemble("SLA", "B")
    end, { B = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SLA B (B=0xff)", function(z)
        z:assemble("SCF")
        z:LD("B", 0xff)
        z:assemble("SLA", "B")
    end, { B = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA B (B=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("B", 0xA5)
        z:assemble("SLA", "B")
    end, { B = 0x4a, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA B (B=0x40)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x40)
        z:assemble("SLA", "B")
    end, { B = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },


-- 0x21
{ "SLA C (C=0)", function(z)
        z:assemble("SCF")
        z:LD("C", 0)
        z:assemble("SLA", "C")
    end, { C = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SLA C (C=1)", function(z)
        z:assemble("SCF")
        z:LD("C", 1)
        z:assemble("SLA", "C")
    end, { C = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLA C (C=0x80)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x80)
        z:assemble("SLA", "C")
    end, { C = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SLA C (C=0xff)", function(z)
        z:assemble("SCF")
        z:LD("C", 0xff)
        z:assemble("SLA", "C")
    end, { C = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA C (C=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("C", 0xA5)
        z:assemble("SLA", "C")
    end, { C = 0x4a, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA C (C=0x40)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x40)
        z:assemble("SLA", "C")
    end, { C = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },


-- 0x22
{ "SLA D (D=0)", function(z)
        z:assemble("SCF")
        z:LD("D", 0)
        z:assemble("SLA", "D")
    end, { D = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SLA D (D=1)", function(z)
        z:assemble("SCF")
        z:LD("D", 1)
        z:assemble("SLA", "D")
    end, { D = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLA D (D=0x80)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x80)
        z:assemble("SLA", "D")
    end, { D = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SLA D (D=0xff)", function(z)
        z:assemble("SCF")
        z:LD("D", 0xff)
        z:assemble("SLA", "D")
    end, { D = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA D (D=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("D", 0xA5)
        z:assemble("SLA", "D")
    end, { D = 0x4a, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA D (D=0x40)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x40)
        z:assemble("SLA", "D")
    end, { D = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },


-- 0x23
{ "SLA E (E=0)", function(z)
        z:assemble("SCF")
        z:LD("E", 0)
        z:assemble("SLA", "E")
    end, { E = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SLA E (E=1)", function(z)
        z:assemble("SCF")
        z:LD("E", 1)
        z:assemble("SLA", "E")
    end, { E = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLA E (E=0x80)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x80)
        z:assemble("SLA", "E")
    end, { E = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SLA E (E=0xff)", function(z)
        z:assemble("SCF")
        z:LD("E", 0xff)
        z:assemble("SLA", "E")
    end, { E = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA E (E=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("E", 0xA5)
        z:assemble("SLA", "E")
    end, { E = 0x4a, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA E (E=0x40)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x40)
        z:assemble("SLA", "E")
    end, { E = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },


-- 0x24
{ "SLA H (H=0)", function(z)
        z:assemble("SCF")
        z:LD("H", 0)
        z:assemble("SLA", "H")
    end, { H = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SLA H (H=1)", function(z)
        z:assemble("SCF")
        z:LD("H", 1)
        z:assemble("SLA", "H")
    end, { H = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLA H (H=0x80)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x80)
        z:assemble("SLA", "H")
    end, { H = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SLA H (H=0xff)", function(z)
        z:assemble("SCF")
        z:LD("H", 0xff)
        z:assemble("SLA", "H")
    end, { H = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA H (H=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("H", 0xA5)
        z:assemble("SLA", "H")
    end, { H = 0x4a, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA H (H=0x40)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x40)
        z:assemble("SLA", "H")
    end, { H = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },


-- 0x25
{ "SLA L (L=0)", function(z)
        z:assemble("SCF")
        z:LD("L", 0)
        z:assemble("SLA", "L")
    end, { L = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SLA L (L=1)", function(z)
        z:assemble("SCF")
        z:LD("L", 1)
        z:assemble("SLA", "L")
    end, { L = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLA L (L=0x80)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x80)
        z:assemble("SLA", "L")
    end, { L = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SLA L (L=0xff)", function(z)
        z:assemble("SCF")
        z:LD("L", 0xff)
        z:assemble("SLA", "L")
    end, { L = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA L (L=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("L", 0xA5)
        z:assemble("SLA", "L")
    end, { L = 0x4a, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA L (L=0x40)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x40)
        z:assemble("SLA", "L")
    end, { L = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x26
{ "SLA (HL) (HL)=0xA5", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xA5)  -- C <- | 7..... 0 |    1010 0101  << C + 0100 1010
        z:assemble("SLA", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x4a, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA (HL) (HL)=0x40", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x40)
        z:assemble("SLA", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLA (HL) (HL)=0x80", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x80)
        z:assemble("SLA", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },

-- 0x27
{ "SLA A (A=0)", function(z)
        z:assemble("SCF")
        z:LD("A", 0)
        z:assemble("SLA", "A")
    end, { A = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SLA A (A=1)", function(z)
        z:assemble("SCF")
        z:LD("A", 1)
        z:assemble("SLA", "A")
    end, { A = 0x02, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLA A (A=0x80)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x80)
        z:assemble("SLA", "A")
    end, { A = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SLA A (A=0xff)", function(z)
        z:assemble("SCF")
        z:LD("A", 0xff)
        z:assemble("SLA", "A")
    end, { A = 0xFE, F={ "S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA A (A=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("A", 0xA5)
        z:assemble("SLA", "A")
    end, { A = 0x4a, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLA A (A=0x40)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x40)
        z:assemble("SLA", "A")
    end, { A = 0x80, F={ "S", "-Z", "-H", "-V", "-N", "-C" } } }, 


-- 0x28
{ "SRA B (B=0)", function(z)
        z:assemble("SCF")
        z:LD("B", 0)
        z:assemble("SRA", "B")
    end, { B = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRA B (B=1)", function(z)
        z:assemble("SCF")
        z:LD("B", 1)
        z:assemble("SRA", "B")
    end, { B = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA B (B=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("B", 0x1)
        z:assemble("SRA", "B")
    end, { B = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA B (B=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("B", 0xff)
        z:assemble("SRA", "B")
    end, { B = 0xff, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA B (B=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("B", 0xfe)
        z:assemble("SRA", "B")
    end, { B = 0xff, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SRA B (B=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("B", 0xA5) -- 1010 0101   >> 11010010 C
        z:assemble("SRA", "B")
    end, { B = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA B (B=0x02)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x02)
        z:assemble("SRA", "B")
    end, { B = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x29
{ "SRA C (C=0)", function(z)
        z:assemble("SCF")
        z:LD("C", 0)
        z:assemble("SRA", "C")
    end, { C = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRA C (C=1)", function(z)
        z:assemble("SCF")
        z:LD("C", 1)
        z:assemble("SRA", "C")
    end, { C = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA C (C=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("C", 0x1)
        z:assemble("SRA", "C")
    end, { C = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA C (C=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("C", 0xff)
        z:assemble("SRA", "C")
    end, { C = 0xff, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA C (C=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("C", 0xfe)
        z:assemble("SRA", "C")
    end, { C = 0xff, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SRA C (C=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("C", 0xA5) -- 1010 0101   >> 11010010 C
        z:assemble("SRA", "C")
    end, { C = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA C (C=0x02)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x02)
        z:assemble("SRA", "C")
    end, { C = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x2A
{ "SRA D (D=0)", function(z)
        z:assemble("SCF")
        z:LD("D", 0)
        z:assemble("SRA", "D")
    end, { D = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRA D (D=1)", function(z)
        z:assemble("SCF")
        z:LD("D", 1)
        z:assemble("SRA", "D")
    end, { D = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA D (D=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("D", 0x1)
        z:assemble("SRA", "D")
    end, { D = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA D (D=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("D", 0xff)
        z:assemble("SRA", "D")
    end, { D = 0xff, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA D (D=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("D", 0xfe)
        z:assemble("SRA", "D")
    end, { D = 0xff, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SRA D (D=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("D", 0xA5) -- 1010 0101   >> 11010010 C
        z:assemble("SRA", "D")
    end, { D = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA D (D=0x02)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x02)
        z:assemble("SRA", "D")
    end, { D = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x2B
{ "SRA E (E=0)", function(z)
        z:assemble("SCF")
        z:LD("E", 0)
        z:assemble("SRA", "E")
    end, { E = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRA E (E=1)", function(z)
        z:assemble("SCF")
        z:LD("E", 1)
        z:assemble("SRA", "E")
    end, { E = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA E (E=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("E", 0x1)
        z:assemble("SRA", "E")
    end, { E = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA E (E=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("E", 0xff)
        z:assemble("SRA", "E")
    end, { E = 0xff, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA E (E=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("E", 0xfe)
        z:assemble("SRA", "E")
    end, { E = 0xff, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SRA E (E=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("E", 0xA5) -- 1010 0101   >> 11010010 C
        z:assemble("SRA", "E")
    end, { E = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA E (E=0x02)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x02)
        z:assemble("SRA", "E")
    end, { E = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x2C
{ "SRA H (H=0)", function(z)
        z:assemble("SCF")
        z:LD("H", 0)
        z:assemble("SRA", "H")
    end, { H = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRA H (H=1)", function(z)
        z:assemble("SCF")
        z:LD("H", 1)
        z:assemble("SRA", "H")
    end, { H = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA H (H=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("H", 0x1)
        z:assemble("SRA", "H")
    end, { H = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA H (H=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("H", 0xff)
        z:assemble("SRA", "H")
    end, { H = 0xff, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA H (H=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("H", 0xfe)
        z:assemble("SRA", "H")
    end, { H = 0xff, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SRA H (H=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("H", 0xA5) -- 1010 0101   >> 11010010 C
        z:assemble("SRA", "H")
    end, { H = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA H (H=0x02)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x02)
        z:assemble("SRA", "H")
    end, { H = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x2D
{ "SRA L (L=0)", function(z)
        z:assemble("SCF")
        z:LD("L", 0)
        z:assemble("SRA", "L")
    end, { L = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRA L (L=1)", function(z)
        z:assemble("SCF")
        z:LD("L", 1)
        z:assemble("SRA", "L")
    end, { L = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA L (L=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("L", 0x1)
        z:assemble("SRA", "L")
    end, { L = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA L (L=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("L", 0xff)
        z:assemble("SRA", "L")
    end, { L = 0xff, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA L (L=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("L", 0xfe)
        z:assemble("SRA", "L")
    end, { L = 0xff, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SRA L (L=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("L", 0xA5) -- 1010 0101   >> 11010010 C
        z:assemble("SRA", "L")
    end, { L = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA L (L=0x02)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x02)
        z:assemble("SRA", "L")
    end, { L = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x2E
{ "SRA (HL) (HL)=0xA5", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xA5)  -- 10100101 >> 11010010
        z:assemble("SRA", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0xd2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA (HL) (HL)=0x01", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x01)
        z:assemble("SRA", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA (HL) (HL)=0x80 SCF", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x80)
        z:assemble("SRA", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0xC0, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SRA (HL) (HL)=0x80 RCF", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x80)
        z:assemble("SRA", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0xC0, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SRA (HL) (HL)=0x00", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x00)
        z:assemble("SRA", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },


-- 0x2F
{ "SRA A (A=0)", function(z)
        z:assemble("SCF")
        z:LD("A", 0)
        z:assemble("SRA", "A")
    end, { A = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRA A (A=1)", function(z)
        z:assemble("SCF")
        z:LD("A", 1)
        z:assemble("SRA", "A")
    end, { A = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA A (A=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0x1)
        z:assemble("SRA", "A")
    end, { A = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRA A (A=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0xff)
        z:assemble("SRA", "A")
    end, { A = 0xff, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA A (A=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0xfe)
        z:assemble("SRA", "A")
    end, { A = 0xff, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SRA A (A=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("A", 0xA5) -- 1010 0101   >> 11010010 C
        z:assemble("SRA", "A")
    end, { A = 0xD2, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SRA A (A=0x02)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x02)
        z:assemble("SRA", "A")
    end, { A = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x30
{ "SLS B (B=0)", function(z)
        z:assemble("SCF")
        z:LD("B", 0)
        z:assemble("SLS", "B")
    end, { B = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLS B (B=1)", function(z)
        z:assemble("SCF")
        z:LD("B", 1)
        z:assemble("SLS", "B")
    end, { B = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SLS B (B=0x80)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x80)
        z:assemble("SLS", "B")
    end, { B = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLS B (B=0xff)", function(z)
        z:assemble("SCF")
        z:LD("B", 0xff)
        z:assemble("SLS", "B")
    end, { B = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS B (B=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("B", 0xA5)
        z:assemble("SLS", "B")
    end, { B = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS B (B=0x40)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x40)
        z:assemble("SLS", "B")
    end, { B = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x31
{ "SLS C (C=0)", function(z)
        z:assemble("SCF")
        z:LD("C", 0)
        z:assemble("SLS", "C")
    end, { C = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLS C (C=1)", function(z)
        z:assemble("SCF")
        z:LD("C", 1)
        z:assemble("SLS", "C")
    end, { C = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SLS C (C=0x80)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x80)
        z:assemble("SLS", "C")
    end, { C = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLS C (C=0xff)", function(z)
        z:assemble("SCF")
        z:LD("C", 0xff)
        z:assemble("SLS", "C")
    end, { C = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS C (C=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("C", 0xA5)
        z:assemble("SLS", "C")
    end, { C = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS C (C=0x40)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x40)
        z:assemble("SLS", "C")
    end, { C = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x32
{ "SLS D (D=0)", function(z)
        z:assemble("SCF")
        z:LD("D", 0)
        z:assemble("SLS", "D")
    end, { D = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLS D (D=1)", function(z)
        z:assemble("SCF")
        z:LD("D", 1)
        z:assemble("SLS", "D")
    end, { D = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SLS D (D=0x80)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x80)
        z:assemble("SLS", "D")
    end, { D = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLS D (D=0xff)", function(z)
        z:assemble("SCF")
        z:LD("D", 0xff)
        z:assemble("SLS", "D")
    end, { D = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS D (D=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("D", 0xA5)
        z:assemble("SLS", "D")
    end, { D = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS D (D=0x40)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x40)
        z:assemble("SLS", "D")
    end, { D = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x33
{ "SLS E (E=0)", function(z)
        z:assemble("SCF")
        z:LD("E", 0)
        z:assemble("SLS", "E")
    end, { E = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLS E (E=1)", function(z)
        z:assemble("SCF")
        z:LD("E", 1)
        z:assemble("SLS", "E")
    end, { E = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SLS E (E=0x80)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x80)
        z:assemble("SLS", "E")
    end, { E = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLS E (E=0xff)", function(z)
        z:assemble("SCF")
        z:LD("E", 0xff)
        z:assemble("SLS", "E")
    end, { E = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS E (E=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("E", 0xA5)
        z:assemble("SLS", "E")
    end, { E = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS E (E=0x40)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x40)
        z:assemble("SLS", "E")
    end, { E = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x34
{ "SLS H (H=0)", function(z)
        z:assemble("SCF")
        z:LD("H", 0)
        z:assemble("SLS", "H")
    end, { H = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLS H (H=1)", function(z)
        z:assemble("SCF")
        z:LD("H", 1)
        z:assemble("SLS", "H")
    end, { H = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SLS H (H=0x80)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x80)
        z:assemble("SLS", "H")
    end, { H = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLS H (H=0xff)", function(z)
        z:assemble("SCF")
        z:LD("H", 0xff)
        z:assemble("SLS", "H")
    end, { H = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS H (H=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("H", 0xA5)
        z:assemble("SLS", "H")
    end, { H = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS H (H=0x40)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x40)
        z:assemble("SLS", "H")
    end, { H = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x35
{ "SLS L (L=0)", function(z)
        z:assemble("SCF")
        z:LD("L", 0)
        z:assemble("SLS", "L")
    end, { L = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLS L (L=1)", function(z)
        z:assemble("SCF")
        z:LD("L", 1)
        z:assemble("SLS", "L")
    end, { L = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SLS L (L=0x80)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x80)
        z:assemble("SLS", "L")
    end, { L = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLS L (L=0xff)", function(z)
        z:assemble("SCF")
        z:LD("L", 0xff)
        z:assemble("SLS", "L")
    end, { L = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS L (L=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("L", 0xA5)
        z:assemble("SLS", "L")
    end, { L = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS L (L=0x40)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x40)
        z:assemble("SLS", "L")
    end, { L = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },

-- 0x36
{ "SLS (HL) (HL)=0xA5", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xA5)  -- C <- | 7..... 0 |    1010 0101  << C + 0100 1010
        z:assemble("SLS", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS (HL) (HL)=0x40", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x40)
        z:assemble("SLS", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SLS (HL) (HL)=0x80", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x80)
        z:assemble("SLS", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },

-- 0x37
{ "SLS A (A=0)", function(z)
        z:assemble("SCF")
        z:LD("A", 0)
        z:assemble("SLS", "A")
    end, { A = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SLS A (A=1)", function(z)
        z:assemble("SCF")
        z:LD("A", 1)
        z:assemble("SLS", "A")
    end, { A = 0x03, F={ "-S", "-Z", "-H", "V", "-N", "-C" } } },
{ "SLS A (A=0x80)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x80)
        z:assemble("SLS", "A")
    end, { A = 0x01, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SLS A (A=0xff)", function(z)
        z:assemble("SCF")
        z:LD("A", 0xff)
        z:assemble("SLS", "A")
    end, { A = 0xFF, F={ "S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS A (A=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("A", 0xA5)
        z:assemble("SLS", "A")
    end, { A = 0x4b, F={ "-S", "-Z", "-H", "V", "-N", "C" } } },
{ "SLS A (A=0x40)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x40)
        z:assemble("SLS", "A")
    end, { A = 0x81, F={ "S", "-Z", "-H", "V", "-N", "-C" } } },


-- 0x38
{ "SRL B (B=0)", function(z)
        z:assemble("SCF")
        z:LD("B", 0)
        z:assemble("SRL", "B")
    end, { B = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRL B (B=1)", function(z)
        z:assemble("SCF")
        z:LD("B", 1)
        z:assemble("SRL", "B")
    end, { B = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL B (B=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("B", 0x1)
        z:assemble("SRL", "B")
    end, { B = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL B (B=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("B", 0xff)
        z:assemble("SRL", "B")
    end, { B = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL B (B=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("B", 0xfe)
        z:assemble("SRL", "B")
    end, { B = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SRL B (B=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("B", 0xA5) -- 1010 0101   >> 01010010 C
        z:assemble("SRL", "B")
    end, { B = 0x52, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL B (B=0x02)", function(z)
        z:assemble("SCF")
        z:LD("B", 0x02)
        z:assemble("SRL", "B")
    end, { B = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x39
{ "SRL C (C=0)", function(z)
        z:assemble("SCF")
        z:LD("C", 0)
        z:assemble("SRL", "C")
    end, { C = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRL C (C=1)", function(z)
        z:assemble("SCF")
        z:LD("C", 1)
        z:assemble("SRL", "C")
    end, { C = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL C (C=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("C", 0x1)
        z:assemble("SRL", "C")
    end, { C = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL C (C=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("C", 0xff)
        z:assemble("SRL", "C")
    end, { C = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL C (C=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("C", 0xfe)
        z:assemble("SRL", "C")
    end, { C = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SRL C (C=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("C", 0xA5) -- 1010 0101   >> 01010010 C
        z:assemble("SRL", "C")
    end, { C = 0x52, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL C (C=0x02)", function(z)
        z:assemble("SCF")
        z:LD("C", 0x02)
        z:assemble("SRL", "C")
    end, { C = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x3A
{ "SRL D (D=0)", function(z)
        z:assemble("SCF")
        z:LD("D", 0)
        z:assemble("SRL", "D")
    end, { D = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRL D (D=1)", function(z)
        z:assemble("SCF")
        z:LD("D", 1)
        z:assemble("SRL", "D")
    end, { D = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL D (D=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("D", 0x1)
        z:assemble("SRL", "D")
    end, { D = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL D (D=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("D", 0xff)
        z:assemble("SRL", "D")
    end, { D = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL D (D=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("D", 0xfe)
        z:assemble("SRL", "D")
    end, { D = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SRL D (D=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("D", 0xA5) -- 1010 0101   >> 01010010 C
        z:assemble("SRL", "D")
    end, { D = 0x52, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL D (D=0x02)", function(z)
        z:assemble("SCF")
        z:LD("D", 0x02)
        z:assemble("SRL", "D")
    end, { D = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x3B
{ "SRL E (E=0)", function(z)
        z:assemble("SCF")
        z:LD("E", 0)
        z:assemble("SRL", "E")
    end, { E = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRL E (E=1)", function(z)
        z:assemble("SCF")
        z:LD("E", 1)
        z:assemble("SRL", "E")
    end, { E = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL E (E=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("E", 0x1)
        z:assemble("SRL", "E")
    end, { E = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL E (E=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("E", 0xff)
        z:assemble("SRL", "E")
    end, { E = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL E (E=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("E", 0xfe)
        z:assemble("SRL", "E")
    end, { E = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SRL E (E=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("E", 0xA5) -- 1010 0101   >> 01010010 C
        z:assemble("SRL", "E")
    end, { E = 0x52, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL E (E=0x02)", function(z)
        z:assemble("SCF")
        z:LD("E", 0x02)
        z:assemble("SRL", "E")
    end, { E = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x3C
{ "SRL H (H=0)", function(z)
        z:assemble("SCF")
        z:LD("H", 0)
        z:assemble("SRL", "H")
    end, { H = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRL H (H=1)", function(z)
        z:assemble("SCF")
        z:LD("H", 1)
        z:assemble("SRL", "H")
    end, { H = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL H (H=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("H", 0x1)
        z:assemble("SRL", "H")
    end, { H = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL H (H=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("H", 0xff)
        z:assemble("SRL", "H")
    end, { H = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL H (H=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("H", 0xfe)
        z:assemble("SRL", "H")
    end, { H = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SRL H (H=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("H", 0xA5) -- 1010 0101   >> 01010010 C
        z:assemble("SRL", "H")
    end, { H = 0x52, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL H (H=0x02)", function(z)
        z:assemble("SCF")
        z:LD("H", 0x02)
        z:assemble("SRL", "H")
    end, { H = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x3D
{ "SRL L (L=0)", function(z)
        z:assemble("SCF")
        z:LD("L", 0)
        z:assemble("SRL", "L")
    end, { L = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRL L (L=1)", function(z)
        z:assemble("SCF")
        z:LD("L", 1)
        z:assemble("SRL", "L")
    end, { L = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL L (L=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("L", 0x1)
        z:assemble("SRL", "L")
    end, { L = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL L (L=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("L", 0xff)
        z:assemble("SRL", "L")
    end, { L = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL L (L=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("L", 0xfe)
        z:assemble("SRL", "L")
    end, { L = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SRL L (L=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("L", 0xA5) -- 1010 0101   >> 01010010 C
        z:assemble("SRL", "L")
    end, { L = 0x52, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL L (L=0x02)", function(z)
        z:assemble("SCF")
        z:LD("L", 0x02)
        z:assemble("SRL", "L")
    end, { L = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },

-- 0x3E
{ "SRL (HL) (HL)=0xA5", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xA5)  -- 10100101 >> 01010010
        z:assemble("SRL", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x52, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL (HL) (HL)=0x01", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x01)
        z:assemble("SRL", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x00, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL (HL) (HL)=0x80 SCF", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x80)
        z:assemble("SRL", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x40, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SRL (HL) (HL)=0x80 RCF", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x80)
        z:assemble("SRL", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x40, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SRL (HL) (HL)=0x00", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x00)
        z:assemble("SRL", "(HL)")
    end, { H=0x60, L=0x00, [0x6000] = 0x00, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },


-- 0x3F
{ "SRL A (A=0)", function(z)
        z:assemble("SCF")
        z:LD("A", 0)
        z:assemble("SRL", "A")
    end, { A = 0, F={ "-S", "Z", "-H", "V", "-N", "-C" } } },
{ "SRL A (A=1)", function(z)
        z:assemble("SCF")
        z:LD("A", 1)
        z:assemble("SRL", "A")
    end, { A = 0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL A (A=0x1)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0x1)
        z:assemble("SRL", "A")
    end, { A = 0x0, F={ "-S", "Z", "-H", "V", "-N", "C" } } },
{ "SRL A (A=0xff)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0xff)
        z:assemble("SRL", "A")
    end, { A = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL A (A=0xfe)", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("A", 0xfe)
        z:assemble("SRL", "A")
    end, { A = 0x7f, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },
{ "SRL A (A=0xA5)", function(z)
        z:assemble("SCF")
        z:LD("A", 0xA5) -- 1010 0101   >> 01010010 C
        z:assemble("SRL", "A")
    end, { A = 0x52, F={ "-S", "-Z", "-H", "-V", "-N", "C" } } },
{ "SRL A (A=0x02)", function(z)
        z:assemble("SCF")
        z:LD("A", 0x02)
        z:assemble("SRL", "A")
    end, { A = 1, F={ "-S", "-Z", "-H", "-V", "-N", "-C" } } },



{ "BIT 0, B", function(z)
        z:assemble("SCF")
        z:assemble("CCF")
        z:LD("B", 0x00)
        z:assemble("BIT", 0, "B")
        end, { B = 0x00, F={ "-S", "Z", "H", "V", "-N", "-C" } } },
{ "BIT 0, B carry", function(z)
        z:assemble("SCF")
        z:LD("B", 0x00)
        z:assemble("BIT", 0, "B")
        end, { B = 0x00, F={ "-S", "Z", "H", "V", "-N", "C" } } },
{ "BIT 0, B bit set", function(z)
        z:assemble("SCF")
        z:LD("B", 0x01)
        z:assemble("BIT", 0, "B")
        end, { B = 0x01, F={ "-S", "-Z", "H", "-V", "-N", "C" } } },
{ "BIT 1, B bit clear", function(z)
        z:assemble("SCF")
        z:LD("B", 0x01)
        z:assemble("BIT", 1, "B")
        end, { B = 0x01, F={ "-S", "Z", "H", "V", "-N", "C" } } },
{ "BIT 1, B bit set FF", function(z)
        z:assemble("SCF")
        z:LD("B", 0xFF)
        z:assemble("BIT", 1, "B")
        end, { B = 0xFF, F={ "-S", "-Z", "H", "-V", "-N", "C" } } },
{ "BIT 1, B single bit set", function(z)
        z:assemble("SCF")
        z:LD("B", 0x02)
        z:assemble("BIT", 1, "B")
        end, { B = 0x02, F={ "-S", "-Z", "H", "-V", "-N", "C" } } },
{ "BIT 7, B single bit set", function(z)
        z:assemble("SCF")
        z:LD("B", 0x80)
        z:assemble("BIT", 7, "B")
        end, { B = 0x80, F={ "S", "-Z", "H", "-V", "-N", "C" } } },


{ "BIT 2, C bit clear", function(z)
        z:assemble("SCF")
        z:LD("C", 0x01)
        z:assemble("BIT", 2, "C")
        end, { C = 0x01, F={ "-S", "Z", "H", "V", "-N", "C" } } },
{ "BIT 2, C single bit set", function(z)
        z:assemble("SCF")
        z:LD("C", 0x04)
        z:assemble("BIT", 2, "C")
        end, { C = 0x04, F={ "-S", "-Z", "H", "-V", "-N", "C" } } },

{ "BIT 3, D bit clear", function(z)
        z:assemble("SCF")
        z:LD("D", 0x01)
        z:assemble("BIT", 3, "D")
        end, { D = 0x01, F={ "-S", "Z", "H", "V", "-N", "C" } } },
{ "BIT 3, C single bit set", function(z)
        z:assemble("SCF")
        z:LD("D", 0x08)
        z:assemble("BIT", 3, "D")
        end, { D = 0x08, F={ "-S", "-Z", "H", "-V", "-N", "C" } } },

{ "BIT 4, E bit clear", function(z)
        z:assemble("SCF")
        z:LD("E", 0x01)
        z:assemble("BIT", 4, "E")
        end, { E = 0x01, F={ "-S", "Z", "H", "V", "-N", "C" } } },
{ "BIT 4, E single bit set", function(z)
        z:assemble("SCF")
        z:LD("E", 0x10)
        z:assemble("BIT", 4, "E")
        end, { E = 0x10, F={ "-S", "-Z", "H", "-V", "-N", "C" } } },

{ "BIT 5, H bit clear", function(z)
        z:assemble("SCF")
        z:LD("H", 0x01)
        z:assemble("BIT", 5, "H")
        end, { H = 0x01, F={ "-S", "Z", "H", "V", "-N", "C" } } },
{ "BIT 5, H single bit set", function(z)
        z:assemble("SCF")
        z:LD("H", 0x20)
        z:assemble("BIT", 5, "H")
        end, { H = 0x20, F={ "-S", "-Z", "H", "-V", "-N", "C" } } },

{ "BIT 6, L bit clear", function(z)
        z:assemble("SCF")
        z:LD("L", 0x01)
        z:assemble("BIT", 6, "L")
        end, { L = 0x01, F={ "-S", "Z", "H", "V", "-N", "C" } } },
{ "BIT 6, L single bit set", function(z)
        z:assemble("SCF")
        z:LD("L", 0x40)
        z:assemble("BIT", 6, "L")
        end, { L = 0x40, F={ "-S", "-Z", "H", "-V", "-N", "C" } } },

{ "BIT 6, A bit clear", function(z)
        z:assemble("SCF")
        z:LD("A", 0x01)
        z:assemble("BIT", 6, "A")
        end, { A = 0x01, F={ "-S", "Z", "H", "V", "-N", "C" } } },
{ "BIT 6, A single bit set", function(z)
        z:assemble("SCF")
        z:LD("A", 0x40)
        z:assemble("BIT", 6, "A")
        end, { A = 0x40, F={ "-S", "-Z", "H", "-V", "-N", "C" } } },

{ "BIT 5, (HL) bit clear", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x01)
        z:assemble("BIT", 5, "(HL)")
        end, { H = 0x60, L = 0, [0x6000]=0x01, F={ "-S", "Z", "H", "V", "-N", "C" } } },
{ "BIT 5, (HL) single bit set", function(z)
        z:assemble("SCF")
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x20)
        z:assemble("BIT", 5, "(HL)")
        end, { H = 0x60, L = 0, [0x6000]=0x20, F={ "-S", "-Z", "H", "-V", "-N", "C" } } },

        
{ "SET 0, A", function(z)
        z:LD("A", 0x00)
        z:assemble("SET", 0, "A")
    end, { A = 0x01 } },
{ "SET 0, A", function(z)
        z:LD("A", 0xFF)
        z:assemble("SET", 0, "A")
    end, { A = 0xFF } },
{ "SET 1, A", function(z)
        z:LD("A", 0x00)
        z:assemble("SET", 1, "A")
    end, { A = 0x02 } },
{ "SET 2, A", function(z)
        z:LD("A", 0x00)
        z:assemble("SET", 2, "A")
    end, { A = 0x04 } },
{ "SET 4, A", function(z)
        z:LD("A", 0x0F)
        z:assemble("SET", 4, "A")
    end, { A = 0x1F } },
{ "SET 6, A", function(z)
        z:LD("A", 0x00)
        z:assemble("SET", 6, "A")
    end, { A = 0x40 } },
{ "SET 7, A", function(z)
        z:LD("A", 0x00)
        z:assemble("SET", 7, "A")
    end, { A = 0x80 } },
-----------------------------------------
{ "SET 0, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x00)
        z:assemble("SET", 0, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0x01 } },
{ "SET 0, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xFF)
        z:assemble("SET", 0, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0xFF } },
{ "SET 1, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x00)
        z:assemble("SET", 1, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0x02 } },
{ "SET 2, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x00)
        z:assemble("SET", 2, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0x04 } },
{ "SET 4, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x0F)
        z:assemble("SET", 4, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0x1F } },
{ "SET 6, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x00)
        z:assemble("SET", 6, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0x40 } },
{ "SET 7, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x00)
        z:assemble("SET", 7, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0x80 } },
-----------------------------------------
{ "SET 2, B", function(z)
        z:LD("B", 0x00)
        z:assemble("SET", 2, "B")
    end, { B = 0x04 } },
{ "SET 2, C", function(z)
        z:LD("C", 0x00)
        z:assemble("SET", 2, "C")
    end, { C = 0x04 } },
{ "SET 2, D", function(z)
        z:LD("D", 0x00)
        z:assemble("SET", 2, "D")
    end, { D = 0x04 } },
{ "SET 2, E", function(z)
        z:LD("E", 0x00)
        z:assemble("SET", 2, "E")
    end, { E = 0x04 } },
{ "SET 2, H", function(z)
        z:LD("H", 0x00)
        z:assemble("SET", 2, "H")
    end, { H = 0x04 } },
{ "SET 2, L", function(z)
        z:LD("L", 0x00)
        z:assemble("SET", 2, "L")
    end, { L = 0x04 } },


-----------------------------------------

{ "RES 0, A", function(z)
        z:LD("A", 0xFF)
        z:assemble("RES", 0, "A")
    end, { A = 0xFE } },
{ "RES 0, A", function(z)
        z:LD("A", 0x00)
        z:assemble("RES", 0, "A")
    end, { A = 0x00 } },
{ "RES 1, A", function(z)
        z:LD("A", 0xFE)
        z:assemble("RES", 1, "A")
    end, { A = 0xFC } },
{ "RES 2, A", function(z)
        z:LD("A", 0xFF)
        z:assemble("RES", 2, "A")
    end, { A = 0xFB } },
{ "RES 4, A", function(z)
        z:LD("A", 0xF0)
        z:assemble("RES", 4, "A")
    end, { A = 0xE0 } },
{ "RES 6, A", function(z)
        z:LD("A", 0xFF)
        z:assemble("RES", 6, "A")
    end, { A = 0xBF } },
{ "RES 7, A", function(z)
        z:LD("A", 0xFF)
        z:assemble("RES", 7, "A")
    end, { A = 0x7F } },

-----------------------------------------
{ "RES 2, B", function(z)
        z:LD("B", 0xFF)
        z:assemble("RES", 2, "B")
    end, { B = 0xFB } },
{ "RES 2, C", function(z)
        z:LD("C", 0xFF)
        z:assemble("RES", 2, "C")
    end, { C = 0xFB } },
{ "RES 2, D", function(z)
        z:LD("D", 0xFF)
        z:assemble("RES", 2, "D")
    end, { D = 0xFB } },
{ "RES 2, E", function(z)
        z:LD("E", 0xFF)
        z:assemble("RES", 2, "E")
    end, { E = 0xFB } },
{ "RES 2, H", function(z)
        z:LD("H", 0xFF)
        z:assemble("RES", 2, "H")
    end, { H = 0xFB } },
{ "RES 2, L", function(z)
        z:LD("L", 0xFF)
        z:assemble("RES", 2, "L")
    end, { L = 0xFB } },

-----------------------------------------

{ "RES 0, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xFF)
        z:assemble("RES", 0, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0xFE } },
{ "RES 0, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0x00)
        z:assemble("RES", 0, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0x00 } },
{ "RES 1, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xFE)
        z:assemble("RES", 1, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0xFC } },
{ "RES 2, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xFF)
        z:assemble("RES", 2, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0xFB } },
{ "RES 4, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xF0)
        z:assemble("RES", 4, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0xE0 } },
{ "RES 6, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xFF)
        z:assemble("RES", 6, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0xBF } },
{ "RES 7, (HL)", function(z)
        z:LD("HL", 0x6000)
        z:LD("(HL)", 0xFF)
        z:assemble("RES", 7, "(HL)")
    end, { H=0x60, L=0, [0x6000] = 0x7F } },

}


return CB_instruction_tests
