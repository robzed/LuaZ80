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
	--self.instr = {}
	self.messages = {}
	--self._last_instruction_index = nil
end

--getmetatable(Z80_Assembler).__tostring = nil

function Z80_Assembler:__serialize()
--[[
	return {
		messages = self.messages, 
		_errors = self._errors, 
		_pc = self._pc,
		-- really need several entires to display this... 
		--last_instruction = self.instr[#self.instr]
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

--function Z80_Assembler:get_code_string()
--	return table.concat(self.instr)
--end

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
			--table.insert(self.instr, string)
			for i = 1, #a_string do
				self.memory[self._pc] = a_string:byte(i)
				self._pc = self._pc + 1
			end
			--self._pc = self._pc + #string
		end
	end
end

function Z80_Assembler:DB(...)
	for _, byte in ipairs{...} do
		if type(byte) ~= "number" then
			self:set_warning("DB ignoring non-number")
		else
			byte = self:_byte_check(byte, "LD A value truncated")
			--table.insert(self.instr, string.char(byte))
			self.memory[self._pc] = byte
			self._pc = self._pc + 1
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
	local high = value / 256
	local low = value % 256
	high = self:_byte_check(high, "LD HL high byte truncated")
	low = self:_byte_check(low, "LD HL low byte truncated")
	self:DB(0x21, low, high)
end

--[[
z = Z80_Assembler()
z:DS("fred", "hello", 0x23)
z:LD_A(255)
--]]
