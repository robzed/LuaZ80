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

require("third_party/strict")
require("third_party/middleclass")

Z80_Assembler = class("Z80_Assembler")

function Z80_Assembler:initialize()
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
	return #self.message ~= 0
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
		value = 128	-- signed -128
		self:warning(warning_string)
	elseif value > 255 then
		value = 255
		self:warning(warning_string)
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


function Z80_Assembler:LD_A(value)
	value = self:_byte_check(value, "LD A byte truncated")
	self:DB(0x3e,value)
end

function Z80_Assembler:HALT()
	self:DB(0x76)
end

function Z80_Assembler:INC_indirect_HL()
	self:DB(0x34)
end

function Z80_Assembler:LD_HL(value)
	local high = math.floor(value / 256)
	local low = value % 256
	high = self:_byte_check(high, "LD HL high byte truncated")
	low = self:_byte_check(low, "LD HL low byte truncated")
	self:DB(0x21, low, high)
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
-- @todo: we could make this table multiple op-code by making the result 16 bit

function Z80_Assembler:assemble(instruction, dest, source)
	instruction = instruction:upper()
	
	-- @todo: don't handle bracketed numbers at the moment
	local dest_op
	if dest == nil then
		dest_op = nil
	elseif type(dest) == "number" then
		dest_op = "!n!"
	else
		dest_op = dest:upper()
	end
	
	local src_op
	if source == nil then
		src_op = nil
	elseif type(source) == "number" then
		if type(dest) == "number" then
			self:set_error("both operands numeric in "..instruction)
			return
		end
		src_op = "!n!"
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
			if dest_op == "!n!" then
				dest = self:_byte_check(dest, instruction .. " byte truncated")
				self:DB(opcode, dest)
				
			elseif dest_op == "!nn!" then
				local high = math.floor(dest / 256)
				local low = dest % 256
				high = self:_byte_check(high, instruction .. " high byte truncated")
				low = self:_byte_check(low, instruction .. " low byte truncated")
				self:DB(opcode, low, high)
			
			elseif src_op == "!n!" then
				source = self:_byte_check(source, instruction .. " byte truncated")
				self:DB(opcode, source)
			
			elseif src_op == "!nn!" then
				local high = math.floor(source / 256)
				local low = source % 256
				high = self:_byte_check(high, instruction .. " high byte truncated")
				low = self:_byte_check(low, instruction .. " low byte truncated")
				self:DB(opcode, low, high)
			else
				self:DB(opcode)
			end
			break
		else
			if dest_op == "!n!" then
				dest_op = "!nn!"
			elseif src_op == "!n!" then
				src_op = "!nn!"
			else
				self:set_error("invalid operands in "..instruction)
				break
			end
		end
	end	
end

function Z80_Assembler:LD(dest, source)
	self:assemble("LD", dest, source)
end



--[[
z = Z80_Assembler()
z:DS("fred", "hello", 0x23)
z:LD_A(255)
--]]
