-- Z80 Single Step Debugger/Monitor
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


--[[

Plan: Display should be something like this (a bit like Devpac)

PC 0000 F3 AF 11 FF FF C3 CB 11
SP FF38 00 00 00 00 00 00 00 00
IY 5C3A 00 00 00 00 00 00 00 00
IX FD98 00 00 00 00 00 00 00 00
HL FC65 00 00 00 00 00 00 00 00
DE 0012 00 00 00 00 00 00 00 00
BC 6053 00 00 00 00 00 00 00 00
AF 0055 Z V
IR 0000

Ideally we'd show several lines of disassembly ... but that's probably more
than we want to do right now. 

--]]

require("third_party/strict")
require("third_party/middleclass")
require("Z80_disassembler")

Z80_SS_DEBUG = class("Z80_SS_DEBUG")

local CLEOL = "\x1b[K"
local CLL = "\x1b[2K"
local CLS = "\x1b[2J"
local Home = "\x1b[H"	-- home cursor vt100
local Hi = "\x1b[1m"
local NormAttr = "\x1b[m"

local debug_precode = " jit.debug_obj:_debug_step(0x%x) "

function Z80_SS_DEBUG:initialize(cpu, jit)
	self.cpu = cpu
	self.jit = jit
	self.last_status = "ok"
end

function Z80_SS_DEBUG:get_smem(addr)
	while addr > 65535 do
		addr = addr - 65536
	end
	while addr < 0 do
		addr = addr + 65536 
	end
	
	local m = self.jit._memory[addr]
	if not m then
		m = 0
	end
	if m > 255 or m < 0 then
		return string.format("%x",m)
	end
	return string.format("%02X", m)
end

function Z80_SS_DEBUG:get_bytes(addr, len)
	return string.format("%s %s %s %s %s %s %s %s",
		self:get_smem(addr),
		self:get_smem(addr+1),
		self:get_smem(addr+2),
		self:get_smem(addr+3),
		self:get_smem(addr+4),
		self:get_smem(addr+5),
		self:get_smem(addr+6),
		self:get_smem(addr+7)
		)
end

function Z80_SS_DEBUG:get_PC()
	if self.PC == nil then
		return self.cpu.PC		
	end
	return self.PC
end

function Z80_SS_DEBUG:disp_PC()
 	local reg = "PC"
	if self.PC == nil then
		self:disp16_reg(reg)
	else
		print(string.format("%s*%04X %s", reg, self.PC, self:get_bytes(self.PC), 8))
	end
end

function Z80_SS_DEBUG:disp16_reg(reg)
	print(string.format("%s %04X %s", reg, self.cpu[reg], self:get_bytes(self.cpu[reg]), 8))
end

function Z80_SS_DEBUG:disp88_reg(reg)
	local value = self.cpu[reg:sub(1,1)]*256+self.cpu[reg:sub(2,2)]
	print(string.format("%s %04X %s", reg, value, self:get_bytes(value, 8)))
end

function Z80_SS_DEBUG:flag_decode(f)
	local s = ""
	if bit32.btest(f, 0x80) then
		s = s .. "S "
	else
		s = s .. "  "
	end
	if bit32.btest(f, 0x40) then
		s = s .. "Z "
	else
		s = s .. "  "
	end
	-- bit 5 is just a mirror of A of the last instruction that altered flags
	
	if bit32.btest(f, 0x10) then
		s = s .. "H "
	else
		s = s .. "  "
	end
	-- bit 3 is just a mirror of A of the last instruction that altered flags

	if bit32.btest(f, 0x04) then
		s = s .. "P/V "
	else
		s = s .. "  "
	end
	if bit32.btest(f, 0x02) then
		s = s .. "N "
	else
		s = s .. "  "
	end
	if bit32.btest(f, 0x01) then
		s = s .. "C "
	else
		s = s .. "  "
	end
	return s
end

function Z80_SS_DEBUG:dispAF_reg()
	local raw_F = self.cpu._F
	if raw_F then
		print(string.format("AF %02X%02X %s (raw _F) CPU.Carry = %x", self.cpu.A, raw_F, self:flag_decode(raw_F), self.cpu.Carry))
	else
		print(string.format("AF %02X?? %s (raw _F) CPU.Carry = %x", self.cpu.A, self:flag_decode(0), self.cpu.Carry))
	end
	local get_F = self.cpu:get_F()
	print(string.format("AF %02X%02X %s (get_F)", self.cpu.A, get_F, self:flag_decode(get_F)))
end

function Z80_SS_DEBUG:calculate_line_address()
	local line_num = nil
	if self.PC and self.jit.current_lump and self.jit.current_lump.source then
		local to_find = string.format(debug_precode, self.PC)
		local i = self.jit.current_lump.source:find(to_find, 1, true)
		if i then
			-- we've got the index for the label, 
			-- now to calculate which line it's on...
			local cur_line = 1
			local cur_index = 0
			while cur_index and cur_index < i do
				cur_index = self.jit.current_lump.source:find('\n', cur_index+1, true)
				cur_line = cur_line + 1
			end
			line_num = cur_line - 1
		end
	end
	return line_num
end

function Z80_SS_DEBUG:display()
	io.write(Home)
	self:disp_PC() -- PC is special
	self:disp16_reg("SP")
	self:disp16_reg("IY")
	self:disp16_reg("IX")
	self:disp88_reg("HL")
	self:disp88_reg("DE")
	self:disp88_reg("BC")
	self:dispAF_reg()
	print()
	io.write(CLL) print(string.format("PC Disassembly = %s", single_Z80_disassemble(self.jit._memory, self:get_PC()) ))
	print()
	local lump = self.jit.current_lump
	io.write(CLL) print(string.format("Lump=%s Z80Start: %04X", tostring(lump), lump.start_address))
	io.write(CLL) print(string.format("Lua status = %s", self.last_status))
	local ll = self:calculate_line_address()
	if ll == nil then ll = '?' end
	io.write(CLL) print(string.format("Lua Line: %s", tostring(ll)))
	--io.write(CLL) print(string.format("Lua:%s", "???"))
end

function Z80_SS_DEBUG:do_command()
	io.write(CLL) local line = io.read("*line")
	if line == '?' or line == 'help' then
		print("s or step => step")
		print("q or quit or exit => quit")
		print("list n => show Lua source around line")
		print("mem address => show memory at address")
		
	elseif line == 's' or line == 'step' then
		return true
	elseif line:sub(1,4) == 'list' then
		local line_num = tonumber(line:sub(5))
		if line_num then
			io.write(CLS)
			self.jit:list_current(line_num-5, line_num+12)
			print()
			print("Hit enter")
			io.read()
			io.write(CLS)
		else
			print("No line supplied")
		end
	elseif line:sub(1,3) == 'mem' then
		local address = tonumber(line:sub(4))
		if address then			
			io.write(CLS)
			for addr = address, address+256, 16 do
				print(string.format("0x%04x: Not implemented yet", addr))
			end
			print()
			print("Hit enter")
			io.read()
			io.write(CLS)
		else
			print("No line supplied")
		end
	elseif line == 'q' or line == 'quit' or line == 'exit' then
		os.exit(0)
	else
		print("Unknown command",line,"Try ? for help")
	end
	return false
end

function Z80_SS_DEBUG:_debug_step(PC)
	self.PC = PC
	repeat
		self:display()
	until self:do_command()
end

function Z80_SS_DEBUG:run_z80(cpu, PC)
	io.write(CLS) 
	self.jit:invalidate_all()
	--self.jit.debug_fnc = self.debug_step
	self.jit.debug_obj = self
	self.last_status = self.jit:run_z80(cpu, PC, debug_precode)
	return self.last_status
end

--[[

There are two methods for debugging:

1. Compiling only single instructions into lumps. This has the advantage
of not affecting the compiler. But the problem is that a lot of jumps will be 
inside lump normally, and this debug method doesn't test how the system works
normally.

2. Compiling a debug call at the end of each instruction. There are several 
disadvantages:
 - The code needs to be called with this in place.
 - The inner compiler needs modifying.
 - Some instructions have multiple exit paths.

Either way, stopping inside a lump is 'interesting'.

--]]

