-- A strange assembler for writing Z80 code in a Lua function.
-- Mostly for testing the lua_z80 compiler.
-- (c) Copyright 2013 Rob Probin.
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
--
-- REQUIREMENTS
-- ============
-- Probably requires Lua 5.2 (e.g. Hex string escape), but could be back-ported
-- to Lua 5.1 without much effort.
--
-- http://clrhome.org/table/
-- 
require("third_party/strict")
require("third_party/middleclass")

Z80_Assembler = class("Z80_Assembler")

function Z80_Assembler:initialize()
    self:reset_environment()
end

function Z80_Assembler:reset_environment()
    self._errors = false
    self._pc = 0
    self.memory = {}
    self.messages = {}
end

--getmetatable(Z80_Assembler).__tostring = nil

function Z80_Assembler:__serialize()
--[[
    return {
        messages = self.messages, 
        _errors = self._errors, 
        _pc = self._pc
    }
--]]
    return self
end

function Z80_Assembler:set_compile_address(value)
    self._pc = value
end

function Z80_Assembler:get_compile_address(value)
    return self._pc
end

function Z80_Assembler:any_errors()
    return self._errors
end

function Z80_Assembler:any_errors_or_warnings()
    return #self.messages ~= 0
end

function Z80_Assembler:get_code()
    return self.memory
end

function Z80_Assembler:get_error_and_warning_messages()
    return self.messages
end

function Z80_Assembler:set_warning(warning_string)
    table.insert(self.messages, string.format(
        "WARNING: %s at PC = 0x%x", warning_string, self._pc))
end

function Z80_Assembler:set_error(error_string)
    table.insert(self.messages, string.format(
        "ERROR: %s at PC = 0x%x", error_string, self._pc))
    self._errors = true
end

function Z80_Assembler:_byte_check(value, warning_string)
    if value < -128 then
        self:set_warning(string.format("%s from 0x%x to -128 (== 128)", warning_string, value))
        value = 128    -- signed -128
    elseif value > 255 then
        self:set_warning(string.format("%s from 0x%x to 255", warning_string, value))
        value = 255
    elseif value < 0 then
        -- make unsigned
        value = value + 256
    end
    return value
end

function Z80_Assembler:_byte_check_signed_only(value, warning_string)
    if value < -128 or value > 127 then
        self:set_error(string.format("%s: Value 0x%x is out of signed byte bounds", warning_string, value))
        value = 0
    elseif value < 0 then
        -- make unsigned
        value = value + 256
    end
    return value
end

function Z80_Assembler:DS(...)
    for _, a_string in ipairs{...} do
        if type(a_string) ~= "string"then
            self:set_warning("DS ignoring non-string")
        else
            for i = 1, #a_string do
                self.memory[self._pc] = a_string:byte(i)
                self._pc = (self._pc + 1) % 65536
            end
        end
    end
end

function Z80_Assembler:DB(...)
    for _, byte in ipairs{...} do
        if type(byte) ~= "number" then
            self:set_warning("DB ignoring non-number")
        else
            byte = self:_byte_check(byte, "LD A value truncated")
            
            self.memory[self._pc] = byte
            self._pc = (self._pc + 1) % 65536
        end
    end
end


function Z80_Assembler:_save_opcode(opcode, ...)
    if opcode < 256 then
        self:DB(opcode)
    elseif opcode < 65536 then
        self:DB(math.floor(opcode/256), opcode%256)
    elseif opcode <= 0xFFFFFF then
        self:DB(math.floor(opcode/65536), (math.floor(opcode/256))%256, opcode%256)
    else
        self:set_error("opcode too long")
    end
    self:DB(...)
end


function Z80_Assembler:LD_A(value)
    value = self:_byte_check(value, "LD A byte truncated")
    self:_save_opcode(0x3e,value)
end

function Z80_Assembler:HALT()
    self:_save_opcode(0x76)
end

function Z80_Assembler:INC_indirect_HL()
    self:_save_opcode(0x34)
end

function Z80_Assembler:LD_HL(value)
    local high = math.floor(value / 256)
    local low = value % 256
    high = self:_byte_check(high, "LD HL high byte truncated")
    low = self:_byte_check(low, "LD HL low byte truncated")
    self:_save_opcode(0x21, low, high)
end

function Z80_Assembler:write_int16(address, value)
    if address >= 0 and address <= 65535 then
        value = math.max(math.min(value, 255), 0)
        local high = math.floor(value / 256)
        local low = value % 256
        self.memory[address] = low
        self.memory[(address+1)%65536] = high
    end
end

function Z80_Assembler:write_int8(address, value)
    if address >= 0 and address <= 65535 then
        value = math.floor(math.max(math.min(value, 255), 0))
        self.memory[address] = value
    end
end


local basic_Z80_table = {
    ["NOP"] =            0x00,
    ["LD   BC,!nn!"] =   0x01,
    ["LD   (BC),A"] =    0x02,
    ["INC  BC"] =        0x03,
    ["INC  B"] =         0x04,
    ["DEC  B"] =         0x05,
    ["LD   B,!n!"] =     0x06,
    ["RLCA"] =           0x07,
    ["EX   AF,AF'"] =    0x08,
    ["ADD  HL,BC"] =     0x09,
    ["LD   A,(BC)"] =    0x0A,
    ["DEC  BC"] =        0x0b,
    ["INC  C"] =         0x0c,
    ["DEC  C"] =         0x0d,
    ["LD   C,!n!"] =     0x0e,
    ["RRCA"] =           0x0f,
    ["DJNZ !r!"] =       0x10,
    ["LD   DE,!nn!"] =   0x11,
    ["LD   (DE),A"] =    0x12,
    ["INC  DE"] =        0x13,
    ["INC  D"] =         0x14,
    ["DEC  D"] =         0x15,
    ["LD   D,!n!"] =     0x16,
    ["RLA"] =            0x17,
    ["JR   !r!"] =       0x18,
    ["ADD  HL,DE"] =     0x19,
    ["LD   A,(DE)"] =    0x1A,
    ["DEC  DE"] =        0x1B,
    ["INC  E"] =         0x1C,
    ["DEC  E"] =         0x1D,
    ["LD   E,!n!"] =     0x1E,
    ["RRA"] =            0x1F,
    ["JR   NZ,!r!"] =    0x20,
    ["LD   HL,!nn!"] =   0x21,
    ["LD   (!nn!),HL"] = 0x22,
    ["INC  HL"] =        0x23,
    ["INC  H"] =         0x24,
    ["DEC  H"] =         0x25,
    ["LD   H,!n!"] =     0x26,
    ["DAA"] =            0x27,
    ["JR   Z,!r!"] =     0x28,
    ["ADD  HL,HL"] =     0x29,
    ["LD   HL,(!nn!)"] = 0x2A,
    ["DEC  HL"] =        0x2B,
    ["INC  L"] =         0x2C,
    ["DEC  L"] =         0x2D,
    ["LD   L,!n!"] =     0x2E,
    ["CPL"] =            0x2F,
    ["JR   NC,!r!"] =    0x30,
    ["LD   SP,!nn!"] =   0x31,
    ["LD   (!nn!),A"] =  0x32,
    ["INC  SP"] =        0x33,
    ["INC  (HL)"] =      0x34,
    ["DEC  (HL)"] =      0x35,
    ["LD   (HL),!n!"] =  0x36,
    ["SCF"] =            0x37,
    ["JR   C,!r!"] =     0x38,
    ["ADD  HL,SP"] =     0x39,
    ["LD   A,(!nn!)"] =  0x3A,
    ["DEC  SP"] =        0x3B,
    ["INC  A"] =         0x3C,
    ["DEC  A"] =         0x3D,
    ["LD   A,!n!"] =     0x3E,
    ["CCF"] =            0x3F,
    ["LD   B,B"] =       0x40,
    ["LD   B,C"] =       0x41,
    ["LD   B,D"] =       0x42,
    ["LD   B,E"] =       0x43,
    ["LD   B,H"] =       0x44,
    ["LD   B,L"] =       0x45,
    ["LD   B,(HL)"] =    0x46,
    ["LD   B,A"] =       0x47,
    ["LD   C,B"] =       0x48,
    ["LD   C,C"] =       0x49,
    ["LD   C,D"] =       0x4A,
    ["LD   C,E"] =       0x4B,
    ["LD   C,H"] =       0x4C,
    ["LD   C,L"] =       0x4D,
    ["LD   C,(HL)"] =    0x4E,
    ["LD   C,A"] =       0x4F,
    ["LD   D,B"] =       0x50,
    ["LD   D,C"] =       0x51,
    ["LD   D,D"] =       0x52,
    ["LD   D,E"] =       0x53,
    ["LD   D,H"] =       0x54,
    ["LD   D,L"] =       0x55,
    ["LD   D,(HL)"] =    0x56,
    ["LD   D,A"] =       0x57,
    ["LD   E,B"] =       0x58,
    ["LD   E,C"] =       0x59,
    ["LD   E,D"] =       0x5A,
    ["LD   E,E"] =       0x5B,
    ["LD   E,H"] =       0x5C,
    ["LD   E,L"] =       0x5D,
    ["LD   E,(HL)"] =    0x5E,
    ["LD   E,A"] =       0x5F,
    ["LD   H,B"] =       0x60,
    ["LD   H,C"] =       0x61,
    ["LD   H,D"] =       0x62,
    ["LD   H,E"] =       0x63,
    ["LD   H,H"] =       0x64,
    ["LD   H,L"] =       0x65,
    ["LD   H,(HL)"] =    0x66,
    ["LD   H,A"] =       0x67,
    ["LD   L,B"] =       0x68,
    ["LD   L,C"] =       0x69,
    ["LD   L,D"] =       0x6A,
    ["LD   L,E"] =       0x6B,
    ["LD   L,H"] =       0x6C,
    ["LD   L,L"] =       0x6D,
    ["LD   L,(HL)"] =    0x6E,
    ["LD   L,A"] =       0x6F,
    ["LD   (HL),B"] =    0x70,
    ["LD   (HL),C"] =    0x71,
    ["LD   (HL),D"] =    0x72,
    ["LD   (HL),E"] =    0x73,
    ["LD   (HL),H"] =    0x74,
    ["LD   (HL),L"] =    0x75,
    ["HALT"] =           0x76,
    ["LD   (HL),A"] =    0x77,
    ["LD   A,B"] =       0x78,
    ["LD   A,C"] =       0x79,
    ["LD   A,D"] =       0x7A,
    ["LD   A,E"] =       0x7B,
    ["LD   A,H"] =       0x7C,
    ["LD   A,L"] =       0x7D,
    ["LD   A,(HL)"] =    0x7E,
    ["LD   A,A"] =       0x7F,
    ["ADD  A,B"] =       0x80,
    ["ADD  A,C"] =       0x81,
    ["ADD  A,D"] =       0x82,
    ["ADD  A,E"] =       0x83,
    ["ADD  A,H"] =       0x84,
    ["ADD  A,L"] =       0x85,
    ["ADD  A,(HL)"] =    0x86,
    ["ADD  A,A"] =       0x87,
    ["ADC  A,B"] =       0x88,
    ["ADC  A,C"] =       0x89,
    ["ADC  A,D"] =       0x8A,
    ["ADC  A,E"] =       0x8B,
    ["ADC  A,H"] =       0x8C,
    ["ADC  A,L"] =       0x8D,
    ["ADC  A,(HL)"] =    0x8E,
    ["ADC  A,A"] =       0x8F,
    ["SUB  A,B"] =       0x90,
    ["SUB  A,C"] =       0x91,
    ["SUB  A,D"] =       0x92,
    ["SUB  A,E"] =       0x93,
    ["SUB  A,H"] =       0x94,
    ["SUB  A,L"] =       0x95,
    ["SUB  A,(HL)"] =    0x96,
    ["SUB  A,A"] =       0x97,
    ["SBC  A,B"] =       0x98,
    ["SBC  A,C"] =       0x99,
    ["SBC  A,D"] =       0x9A,
    ["SBC  A,E"] =       0x9B,
    ["SBC  A,H"] =       0x9C,
    ["SBC  A,L"] =       0x9D,
    ["SBC  A,(HL)"] =    0x9E,
    ["SBC  A,A"] =       0x9F,
    ["AND  B"] =         0xA0,
    ["AND  C"] =         0xA1,
    ["AND  D"] =         0xA2,
    ["AND  E"] =         0xA3,
    ["AND  H"] =         0xA4,
    ["AND  L"] =         0xA5,
    ["AND  (HL)"] =      0xA6,
    ["AND  A"] =         0xA7,
    ["XOR  B"] =         0xA8,
    ["XOR  C"] =         0xA9,
    ["XOR  D"] =         0xAA,
    ["XOR  E"] =         0xAB,
    ["XOR  H"] =         0xAC,
    ["XOR  L"] =         0xAD,
    ["XOR  (HL)"] =      0xAE,
    ["XOR  A"] =         0xAF,
    ["OR   B"] =         0xB0,
    ["OR   C"] =         0xB1,
    ["OR   D"] =         0xB2,
    ["OR   E"] =         0xB3,
    ["OR   H"] =         0xB4,
    ["OR   L"] =         0xB5,
    ["OR   (HL)"] =      0xB6,
    ["OR   A"] =         0xB7,
    ["CP   B"] =         0xB8,
    ["CP   C"] =         0xB9,
    ["CP   D"] =         0xBA,
    ["CP   E"] =         0xBB,
    ["CP   H"] =         0xBC,
    ["CP   L"] =         0xBD,
    ["CP   (HL)"] =      0xBE,
    ["CP   A"] =         0xBF,
    ["RET  NZ"] =        0xC0,
    ["POP  BC"] =        0xC1,
    ["JP   NZ,!nn!"] =   0xC2,
    ["JP   !nn!"] =      0xC3,
    ["CALL NZ,!nn!"] =   0xC4,
    ["PUSH BC"] =        0xC5,
    ["ADD  A,!n!"] =     0xC6,
    ["RST  00H"] =       0xC7,
    ["RET  Z"] =         0xC8,
    ["RET"] =            0xC9,
    ["JP   Z,!nn!"] =    0xCA,
    ["CALL Z,!nn!"] =    0xCC,
    ["CALL !nn!"] =      0xCD,
    ["ADC  A,!n!"] =     0xCE,
    ["RST  08H"] =       0xCF,
    ["RET  NC"] =        0xD0,
    ["POP  DE"] =        0xD1,
    ["JP   NC,!nn!"] =   0xD2,
    ["OUT  (!n!),A"] =   0xD3,
    ["CALL NC,!nn!"] =   0xD4,
    ["PUSH DE"] =        0xD5,
    ["SUB  A,!n!"] =     0xD6,
    ["RST  10H"] =       0xD7,
    ["RET  C"] =         0xD8,
    ["EXX"] =            0xD9,
    ["JP   C,!nn!"] =    0xDA,
    ["IN   A,(!n!)"] =   0xDB,
    ["CALL C,!nn!"] =    0xDC,
    ["SBC  A,!n!"] =     0xDE,
    ["RST  18H"] =       0xDF,
    ["RET  PO"] =        0xE0,
    ["POP  HL"] =        0xE1,
    ["JP   PO,!nn!"] =   0xE2,
    ["EX   (SP),HL"] =   0xE3,
    ["CALL PO,!nn!"] =   0xE4,
    ["PUSH HL"] =        0xE5,
    ["AND  !n!"] =       0xE6,
    ["RST  20H"] =       0xE7,
    ["RET  PE"] =        0xE8,
    ["JP   (HL)"] =      0xE9,
    ["JP   PE,!nn!"] =   0xEA,
    ["EX   DE,HL"] =     0xEB,
    ["CALL PE,!nn!"] =   0xEC,
    ["XOR  !n!"] =       0xEE,
    ["RST  28H"] =       0xEF,
    ["RET  P"] =         0xF0,
    ["POP  AF"] =        0xF1,
    ["JP   P,!nn!"] =    0xF2,
    ["DI"] =             0xF3,
    ["CALL P,!nn!"] =    0xF4,
    ["PUSH AF"] =        0xF5,
    ["OR   !n!"] =       0xF6,
    ["RST  30H"] =       0xF7,
    ["RET  M"] =         0xF8,
    ["LD   SP,HL"] =     0xF9,
    ["JP   M,!nn!"] =    0xFA,
    ["EI"] =             0xFB,
    ["CALL M,!nn!"] =    0xFC,
    ["CP   !n!"] =       0xFE,
    ["RST  38H"] =       0xFF,
}



local _CB_Z80_table = {
[0]="RLC  B",
"RLC  C",
"RLC  D",
"RLC  E",
"RLC  H",
"RLC  L",
"RLC  (HL)",
"RLC  A",
"RRC  B",
"RRC  C",
"RRC  D",
"RRC  E",
"RRC  H",
"RRC  L",
"RRC  (HL)",
"RRC  A",
"RL   B",
"RL   C",
"RL   D",
"RL   E",
"RL   H",
"RL   L",
"RL   (HL)",
"RL   A",
"RR   B",
"RR   C",
"RR   D",
"RR   E",
"RR   H",
"RR   L",
"RR   (HL)",
"RR   A",
"SLA  B",
"SLA  C",
"SLA  D",
"SLA  E",
"SLA  H",
"SLA  L",
"SLA  (HL)",
"SLA  A",
"SRA  B",
"SRA  C",
"SRA  D",
"SRA  E",
"SRA  H",
"SRA  L",
"SRA  (HL)",
"SRA  A",
"SLS  B",
"SLS  C",
"SLS  D",
"SLS  E",
"SLS  H",
"SLS  L",
"SLS  (HL)",
"SLS  A",
"SRL  B",
"SRL  C",
"SRL  D",
"SRL  E",
"SRL  H",
"SRL  L",
"SRL  (HL)",
"SRL  A",
"BIT  0,B",
"BIT  0,C",
"BIT  0,D",
"BIT  0,E",
"BIT  0,H",
"BIT  0,L",
"BIT  0,(HL)",
"BIT  0,A",
"BIT  1,B",
"BIT  1,C",
"BIT  1,D",
"BIT  1,E",
"BIT  1,H",
"BIT  1,L",
"BIT  1,(HL)",
"BIT  1,A",
"BIT  2,B",
"BIT  2,C",
"BIT  2,D",
"BIT  2,E",
"BIT  2,H",
"BIT  2,L",
"BIT  2,(HL)",
"BIT  2,A",
"BIT  3,B",
"BIT  3,C",
"BIT  3,D",
"BIT  3,E",
"BIT  3,H",
"BIT  3,L",
"BIT  3,(HL)",
"BIT  3,A",
"BIT  4,B",
"BIT  4,C",
"BIT  4,D",
"BIT  4,E",
"BIT  4,H",
"BIT  4,L",
"BIT  4,(HL)",
"BIT  4,A",
"BIT  5,B",
"BIT  5,C",
"BIT  5,D",
"BIT  5,E",
"BIT  5,H",
"BIT  5,L",
"BIT  5,(HL)",
"BIT  5,A",
"BIT  6,B",
"BIT  6,C",
"BIT  6,D",
"BIT  6,E",
"BIT  6,H",
"BIT  6,L",
"BIT  6,(HL)",
"BIT  6,A",
"BIT  7,B",
"BIT  7,C",
"BIT  7,D",
"BIT  7,E",
"BIT  7,H",
"BIT  7,L",
"BIT  7,(HL)",
"BIT  7,A",
"RES  0,B",
"RES  0,C",
"RES  0,D",
"RES  0,E",
"RES  0,H",
"RES  0,L",
"RES  0,(HL)",
"RES  0,A",
"RES  1,B",
"RES  1,C",
"RES  1,D",
"RES  1,E",
"RES  1,H",
"RES  1,L",
"RES  1,(HL)",
"RES  1,A",
"RES  2,B",
"RES  2,C",
"RES  2,D",
"RES  2,E",
"RES  2,H",
"RES  2,L",
"RES  2,(HL)",
"RES  2,A",
"RES  3,B",
"RES  3,C",
"RES  3,D",
"RES  3,E",
"RES  3,H",
"RES  3,L",
"RES  3,(HL)",
"RES  3,A",
"RES  4,B",
"RES  4,C",
"RES  4,D",
"RES  4,E",
"RES  4,H",
"RES  4,L",
"RES  4,(HL)",
"RES  4,A",
"RES  5,B",
"RES  5,C",
"RES  5,D",
"RES  5,E",
"RES  5,H",
"RES  5,L",
"RES  5,(HL)",
"RES  5,A",
"RES  6,B",
"RES  6,C",
"RES  6,D",
"RES  6,E",
"RES  6,H",
"RES  6,L",
"RES  6,(HL)",
"RES  6,A",
"RES  7,B",
"RES  7,C",
"RES  7,D",
"RES  7,E",
"RES  7,H",
"RES  7,L",
"RES  7,(HL)",
"RES  7,A",
"SET  0,B",
"SET  0,C",
"SET  0,D",
"SET  0,E",
"SET  0,H",
"SET  0,L",
"SET  0,(HL)",
"SET  0,A",
"SET  1,B",
"SET  1,C",
"SET  1,D",
"SET  1,E",
"SET  1,H",
"SET  1,L",
"SET  1,(HL)",
"SET  1,A",
"SET  2,B",
"SET  2,C",
"SET  2,D",
"SET  2,E",
"SET  2,H",
"SET  2,L",
"SET  2,(HL)",
"SET  2,A",
"SET  3,B",
"SET  3,C",
"SET  3,D",
"SET  3,E",
"SET  3,H",
"SET  3,L",
"SET  3,(HL)",
"SET  3,A",
"SET  4,B",
"SET  4,C",
"SET  4,D",
"SET  4,E",
"SET  4,H",
"SET  4,L",
"SET  4,(HL)",
"SET  4,A",
"SET  5,B",
"SET  5,C",
"SET  5,D",
"SET  5,E",
"SET  5,H",
"SET  5,L",
"SET  5,(HL)",
"SET  5,A",
"SET  6,B",
"SET  6,C",
"SET  6,D",
"SET  6,E",
"SET  6,H",
"SET  6,L",
"SET  6,(HL)",
"SET  6,A",
"SET  7,B",
"SET  7,C",
"SET  7,D",
"SET  7,E",
"SET  7,H",
"SET  7,L",
"SET  7,(HL)",
"SET  7,A",
}
-- add in the CB values to the basic_Z80_table
for k,v in pairs(_CB_Z80_table) do
    if not basic_Z80_table[v] then
        basic_Z80_table[v] = 0xCB00 + k
    end
end


local _ED_Z80_table = {
[0x40] = "IN   B,(C)",
[0x41] = "OUT  (C),B",
[0x42] = "SBC  HL,BC",
[0x43] = "LD   (!nn!),BC",
[0x44] = "NEG",
[0x45] = "RETN",
[0x46] = "IM   0",
[0x47] = "LD   I,A",
[0x48] = "IN   C,(C)",
[0x49] = "OUT  (C),C",
[0x4A] = "ADC  HL,BC",
[0x4B] = "LD   BC,(!nn!)",
[0x4D] = "RETI",
[0x4F] = "LD   R,A",
[0x50] = "IN   D,(C)",
[0x51] = "OUT  (C),D",
[0x52] = "SBC  HL,DE",
[0x53] = "LD   (!nn!),DE",
[0x56] = "IM   1",
[0x57] = "LD   A,I",
[0x58] = "IN   E,(C)",
[0x59] = "OUT  (C),E",
[0x5A] = "ADC  HL,DE",
[0x5B] = "LD   DE,(!nn!)",
[0x5E] = "IM   2",
[0x5F] = "LD   A,R",
[0x60] = "IN   H,(C)",
[0x61] = "OUT  (C),H",
[0x62] = "SBC  HL,HL",
[0x63] = "LD   (!nn!),HL",
[0x67] = "RRD",
[0x68] = "IN   L,(C)",
[0x69] = "OUT  (C),L",
[0x6A] = "ADC  HL,HL",
[0x6B] = "LD   HL,(!nn!)",
[0x6F] = "RLD",
[0x70] = "IN   F,(C)",
[0x71] = "OUT  (C),F",
[0x72] = "SBC  HL,SP",
[0x73] = "LD   (!nn!),SP",
[0x78] = "IN   A,(C)",
[0x79] = "OUT  (C),A",
[0x7A] = "ADC  HL,SP",
[0x7B] = "LD   SP,(!nn!)",
[0xA0] = "LDI",
[0xA1] = "CPI",
[0xA2] = "INI",
[0xA3] = "OTI",
[0xA8] = "LDD",
[0xA9] = "CPD",
[0xAA] = "IND",
[0xAB] = "OTD",
[0xB0] = "LDIR",
[0xB1] = "CPIR",
[0xB2] = "INIR",
[0xB3] = "OTIR",
[0xB8] = "LDDR",
[0xB9] = "CPDR",
[0xBA] = "INDR",
[0xBB] = "OTDR"
}
-- add in the ED values to the basic_Z80_table
for k,v in pairs(_ED_Z80_table) do
    if not basic_Z80_table[v] then
        basic_Z80_table[v] = 0xED00 + k
    end
end

local _DD_CB_Z80_table = {
[0x06] = "RLC  (IX!d!)",
[0x0E] = "RRC  (IX!d!)",
[0x16] = "RL   (IX!d!)",
[0x1E] = "RR   (IX!d!)",
[0x26] = "SLA  (IX!d!)",
[0x2E] = "SRA  (IX!d!)",
[0x36] = "SLS  (IX!d!)",
[0x3E] = "SRL  (IX!d!)",
[0x46] = "BIT  0,(IX!d!)",
[0x4E] = "BIT  1,(IX!d!)",
[0x56] = "BIT  2,(IX!d!)",
[0x5E] = "BIT  3,(IX!d!)",
[0x66] = "BIT  4,(IX!d!)",
[0x6E] = "BIT  5,(IX!d!)",
[0x76] = "BIT  6,(IX!d!)",
[0x7E] = "BIT  7,(IX!d!)",
[0x86] = "RES  0,(IX!d!)",
[0x8E] = "RES  1,(IX!d!)",
[0x96] = "RES  2,(IX!d!)",
[0x9E] = "RES  3,(IX!d!)",
[0xA6] = "RES  4,(IX!d!)",
[0xAE] = "RES  5,(IX!d!)",
[0xB6] = "RES  6,(IX!d!)",
[0xBE] = "RES  7,(IX!d!)",
[0xC6] = "SET  0,(IX!d!)",
[0xCE] = "SET  1,(IX!d!)",
[0xD6] = "SET  2,(IX!d!)",
[0xDE] = "SET  3,(IX!d!)",
[0xE6] = "SET  4,(IX!d!)",
[0xEE] = "SET  5,(IX!d!)",
[0xF6] = "SET  6,(IX!d!)",
[0xFE] = "SET  7,(IX!d!)",
}
-- add in the DDCB or FDCB values to the basic_Z80_table
for k,v in pairs(_DD_CB_Z80_table) do
    -- create the IX version
    if not basic_Z80_table[v] then
        basic_Z80_table[v] = 0xDDCB00 + k
    end
    
    -- do the IY version as well
    v = v:gsub("IX", "IY")
    if not basic_Z80_table[v] then
        basic_Z80_table[v] = 0xFDCB00 + k
    end
end


local _DD_Z80_table = {
[0x09] = "ADD  IX,BC",
[0x19] = "ADD  IX,DE",
[0x21] = "LD   IX,!nn!",
[0x22] = "LD   (!nn!),IX",
[0x23] = "INC  IX",
[0x24] = "INC  IXH",
[0x25] = "DEC  IXH",
[0x26] = "LD   IXH,!n!",
[0x29] = "ADD  IX,IX",
[0x2A] = "LD   IX,(!nn!)",
[0x2B] = "DEC  IX",
[0x2C] = "INC  IXL",
[0x2D] = "DEC  IXL",
[0x2E] = "LD   IXL,!n!",
[0x34] = "INC  (IX!d!)",
[0x35] = "DEC  (IX!d!)",
[0x36] = "LD   (IX!d!),!n!",
[0x39] = "ADD  IX,SP",
[0x44] = "LD   B,IXH",
[0x45] = "LD   B,IXL",
[0x46] = "LD   B,(IX!d!)",
[0x4C] = "LD   C,IXH",
[0x4D] = "LD   C,IXL",
[0x4E] = "LD   C,(IX!d!)",
[0x54] = "LD   D,IXH",
[0x55] = "LD   D,IXL",    
[0x56] = "LD   D,(IX!d!)", 
[0x5C] = "LD   E,IXH",    
[0x5D] = "LD   E,IXL",    
[0x5E] = "LD   E,(IX!d!)", 
[0x60] = "LD   IXH,B",    
[0x61] = "LD   IXH,C",    
[0x62] = "LD   IXH,D",    
[0x63] = "LD   IXH,E",    
[0x64] = "LD   IXH,IXH",  
[0x65] = "LD   IXH,IXL",  
[0x66] = "LD   H,(IX!d!)", 
[0x67] = "LD   IXH,A",    
[0x68] = "LD   IXL,B",    
[0x69] = "LD   IXL,C",    
[0x6A] = "LD   IXL,D",    
[0x6B] = "LD   IXL,E",    
[0x6C] = "LD   IXL,IXH",  
[0x6D] = "LD   IXL,IXL",  
[0x6E] = "LD   L,(IX!d!)", 
[0x6F] = "LD   IXL,A",    
[0x70] = "LD   (IX!d!),B", 
[0x71] = "LD   (IX!d!),C", 
[0x72] = "LD   (IX!d!),D", 
[0x73] = "LD   (IX!d!),E", 
[0x74] = "LD   (IX!d!),H", 
[0x75] = "LD   (IX!d!),L", 
[0x77] = "LD   (IX!d!),A", 
[0x7C] = "LD   A,IXH",    
[0x7D] = "LD   A,IXL",    
[0x7E] = "LD   A,(IX!d!)", 
[0x84] = "ADD  A,IXH",    
[0x85] = "ADD  A,IXL",    
[0x86] = "ADD  A,(IX!d!)", 
[0x8C] = "ADC  A,IXH",    
[0x8D] = "ADC  A,IXL",    
[0x8E] = "ADC  A,(IX!d!)", 
[0x94] = "SUB  A,IXH",    
[0x95] = "SUB  A,IXL",    
[0x96] = "SUB  A,(IX!d!)", 
[0x9C] = "SBC  A,IXH",    
[0x9D] = "SBC  A,IXL",    
[0x9E] = "SBC  A,(IX!d!)", 
[0xA4] = "AND  IXH",      
[0xA5] = "AND  IXL",      
[0xA6] = "AND  (IX!d!)",   
[0xAC] = "XOR  IXH",      
[0xAD] = "XOR  IXL",      
[0xAE] = "XOR  (IX!d!)",   
[0xB4] = "OR   IXH",      
[0xB5] = "OR   IXL",      
[0xB6] = "OR   (IX!d!)",   
[0xBC] = "CP   IXH",      
[0xBD] = "CP   IXL",      
[0xBE] = "CP   (IX!d!)",
[0xE1] = "POP  IX",       
[0xE3] = "EX   (SP),IX",  
[0xE5] = "PUSH IX",       
[0xE9] = "JP   (IX)",     
[0xF9] = "LD   SP,IX"
}

-- add in the DD or FD values to the basic_Z80_table
for k,v in pairs(_DD_Z80_table) do
    -- create the IX version
    if not basic_Z80_table[v] then
        basic_Z80_table[v] = 0xDD00 + k
    end
    
    -- do the IY version as well
    v = v:gsub("IX", "IY")
    if not basic_Z80_table[v] then
        basic_Z80_table[v] = 0xFD00 + k
    end
end


function Z80_Assembler:assemble(instruction, dest, source)
    instruction = instruction:upper()
    
    -- @todo: don't handle bracketed numbers at the moment
    local dest_op
    if dest == nil then
        dest_op = nil
    elseif type(dest) == "number" then
        dest_op = "!n!"
    elseif dest:sub(1,1) == "(" and dest:sub(#dest) ==")" and tonumber(dest:sub(2,#dest-1)) then
        dest_op = "(!n!)"
        dest = tonumber(dest:sub(2,#dest-1))
    elseif dest:sub(1,3) == "(IX" and dest:sub(#dest) ==")" and tonumber(dest:sub(4,#dest-1)) then
        dest_op = "(IX!d!)"
        dest = tonumber(dest:sub(4,#dest-1))
    elseif dest:sub(1,3) == "(IY" and dest:sub(#dest) ==")" and tonumber(dest:sub(4,#dest-1)) then
        dest_op = "(IY!d!)"
        dest = tonumber(dest:sub(4,#dest-1))
    else
        dest_op = dest:upper()
    end
    
    local src_op
    if source == nil then
        src_op = nil
    elseif type(source) == "number" then
        if type(dest) == "number" and dest_op ~= "(IX!d!)" and dest_op ~= "(IY!d!)" then
            self:set_error("both operands numeric in "..instruction)
            return
        end
        src_op = "!n!"
    elseif source:sub(1,1) == "(" and source:sub(#source) ==")" and tonumber(source:sub(2,#source-1)) then
        src_op = "(!n!)"
        source = tonumber(source:sub(2,#source-1))
    elseif source:sub(1,3) == "(IX" and source:sub(#source) ==")" and tonumber(source:sub(4,#source-1)) then
        src_op = "(IX!d!)"
        source = tonumber(source:sub(4,#source-1))
    elseif source:sub(1,3) == "(IY" and source:sub(#source) ==")" and tonumber(source:sub(4,#source-1)) then
        src_op = "(IY!d!)"
        source = tonumber(source:sub(4,#source-1))
    else
        src_op = source:upper()
    end
    
    
    while true do
        -- figure out what the op code will look like
        local attempt
        if dest_op == nil then    
            attempt = instruction
        elseif src_op == nil then
            attempt = string.format("%-5s%s", instruction, dest_op)
        else
            attempt = string.format("%-5s%s,%s", instruction, dest_op, src_op)
        end
        
        local opcode = basic_Z80_table[attempt]
        if opcode then
            -- we've found the op code, generate the code!
            if dest_op == "!n!" or dest_op == "(!n!)" then
                dest = self:_byte_check(dest, instruction .. " byte truncated")
                self:_save_opcode(opcode)
                self:DB(dest)
                
            elseif dest_op == "!r!" then
                dest = self:_byte_check_signed_only(dest, instruction)
                self:_save_opcode(opcode)
                self:DB(dest)
                
            elseif dest_op == "(IX!d!)" or dest_op == "(IY!d!)" then
                dest = self:_byte_check_signed_only(dest, instruction)
                self:_save_opcode(opcode)
                self:DB(dest)           -- displacement
                
                if src_op == "!n!" then
                    source = self:_byte_check(source, instruction .. " byte truncated")
                    self:DB(source)
                elseif src_op ~= nil then
                    self:set_error("Invalid source operand with destination (IX+d) or (IY+d) in "..instruction)
                    return
                end

            elseif dest_op == "!nn!" or dest_op == "(!nn!)" then
                local high = math.floor(dest / 256)
                local low = dest % 256
                high = self:_byte_check(high, instruction .. " high byte truncated")
                low = self:_byte_check(low, instruction .. " low byte truncated")
                self:_save_opcode(opcode)
                self:DB(low, high)
            
            elseif src_op == "!n!" or src_op == "(!n!)" then
                source = self:_byte_check(source, instruction .. " byte truncated")
                self:_save_opcode(opcode)
                self:DB(source)
            
            elseif src_op == "!r!" then
                source = self:_byte_check_signed_only(source, instruction)
                self:_save_opcode(opcode)
                self:DB(source)
            
            elseif src_op == "!nn!" or src_op == "(!nn!)" then
                local high = math.floor(source / 256)
                local low = source % 256
                high = self:_byte_check(high, instruction .. " high byte truncated")
                low = self:_byte_check(low, instruction .. " low byte truncated")
                self:_save_opcode(opcode)
                self:DB(low, high)

            elseif src_op == "(IX!d!)" or src_op == "(IY!d!)" then
                source = self:_byte_check_signed_only(source, instruction)
                self:_save_opcode(opcode)
                self:DB(source)           -- displacement
            else
                self:_save_opcode(opcode)
            end
            break
        else
            if dest_op == "!n!" then
                dest_op = "!nn!"
            elseif src_op == "!n!" then
                src_op = "!nn!"
            elseif dest_op == "!nn!" then
                dest_op = "!r!"
            elseif src_op == "!nn!" then
                src_op = "!r!"
            elseif dest_op == "(!n!)" then
                dest_op = "(!nn!)"
            elseif src_op == "(!n!)" then
                src_op = "(!nn!)"
            elseif dest_op == "!r!" and type(dest) == "number" then
                dest_op = tostring(dest)
            else
                self:set_error(string.format("invalid operands in %s (dest op='%s' source op='%s')",instruction,dest_op,src_op))
                break
            end
        end
    end
end

---
-- Generic load instruction
function Z80_Assembler:LD(dest, source)
    self:assemble("LD", dest, source)
end

---
-- Set a bit
function Z80_Assembler:SET(bit, register)
    if type(bit) == "number" then
        bit = tostring(bit)
    end
    self:assemble("SET", bit, register)
end

function Z80_Assembler:BIT(bit, register)
    if type(bit) == "number" then
        bit = tostring(bit)
    end
    self:assemble("BIT", bit, register)
end

function Z80_Assembler:RES(bit, register)
    if type(bit) == "number" then
        bit = tostring(bit)
    end
    self:assemble("RES", bit, register)
end

function Z80_Assembler:RLC(register)
    self:assemble("RLC", register)
end
function Z80_Assembler:RRC(register)
    self:assemble("RRC", register)
end
function Z80_Assembler:RL(register)
    self:assemble("RL", register)
end
function Z80_Assembler:RR(register)
    self:assemble("RR", register)
end
function Z80_Assembler:SLA(register)
    self:assemble("SLA", register)
end
function Z80_Assembler:SRA(register)
    self:assemble("SRA", register)
end
function Z80_Assembler:SLS(register)
    self:assemble("SLS", register)
end
function Z80_Assembler:SRL(register)
    self:assemble("SRL", register)
end
---
-- Interrupt routines
function Z80_Assembler:IM_0()
    self:assemble("IM", "0")
end
function Z80_Assembler:IM_1()
    self:assemble("IM", "1")
end
function Z80_Assembler:IM_2()
    self:assemble("IM", "1")
end
function Z80_Assembler:RETI()
    self:assemble("RETI")
end
function Z80_Assembler:RET()
    self:assemble("RET")
end

function Z80_Assembler:NOP()
    self:assemble("NOP")
end

function Z80_Assembler:POP(register_pair)
    self:assemble("POP", register_pair)
end

function Z80_Assembler:PUSH(register_pair)
    self:assemble("PUSH", register_pair)
end

function Z80_Assembler:ADD(op1, op2)
    self:assemble("ADD", op1, op2)
end

function Z80_Assembler:OR(op)
    self:assemble("OR", op)
end
function Z80_Assembler:XOR(op)
    self:assemble("XOR", op)
end
function Z80_Assembler:AND(op)
    self:assemble("AND", op)
end
function Z80_Assembler:CP(op)
    self:assemble("CP", op)
end

--[[
z = Z80_Assembler()
z:DS("fred", "hello", 0x23)
z:LD_A(255)
--]]
