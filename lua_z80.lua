-- The main Z80 to Lua JIT compiling emulator.
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
-- Requires Lua 5.2 because we use bit operations. Other changes 5.1/5.2 changes 
-- might be documented as 5.2 in this file.
--
-- FUTURE ACTIONS
-- ==============
-- High Priority
-- @todo: Complete memory invalidation
-- @todo: test memory invalidation
-- @todo: rename old to LuaZ80_Support, Create new project, Push to github
-- @todo: blog post and web page
--
-- Medium Priority
-- @todo: Test by booting ZX81 or Spectrum ROM and showing basic screen and key input
-- @todo: Complete instructions (make a list of those!!)
-- @todo: test instructions (make a list and test actions & flags)
-- 
-- Low Priority
-- @todo: defer inc/dec flag operations?
-- @todo: quicker to defer all flag operations - or not??
-- @todo: is it possible to defer other operations?
-- @todo: work out other optimisations of generated code or compiler itself
-- @todo: Check jumps into middle of instruction inside instruction
-- @todo: Add proper cycle counting
-- @todo: Fix neg flag problems? (Most emulators don't do it right...)
-- @todo: Add undocumented instructions?
-- @todo: For all CALL instructions, check if push to SP that overwrites destination address will affect current call... (zxsp assumes not)
-- @todo: Is there a way to for JP (HL) and RET not to drop out of lump?
-- @todo: Add FD CB ff nn to lua_z80_test()
-- @todo: Add IX & IY instructions to disassembler
-- @todo: Can we avoid compiling local data as instructions? (and therefore get 
--        better alignement of instructions into a lump)  Can we avoid 
--        invalidating a block based on a write to data rather than instructions?
-- @todo: Implement interrupts in Z80 land (& I register)
-- @todo: Implement refresh register
-- @todo: Can we identify data in or immediately after code that will cause
--        lump reload, especially if it has a JR or JP immediately before
--        during compile (rather than execution because execution primatives
--        are costly)? Maybe even just stop lump compilation if JP/JR is 
--        backwards and the lump doesnt have a label right after.
--        Also can we post-identify data as not self-modifying code?
--
------------------------------------------------------------------------------------
-- NOTES: code_write_check and write_allowed go hand in hand. If an instruction doesn't
-- do a write_allowed  first, then code_write_check won't help

require("third_party/strict")
require("third_party/middleclass")
require("z80_ss_debug")		-- only if debug enabled below... (search for 'uncomment for debug')
require("Z80_disassembler")

local Z80_SOURCE_DEBUG = true

-- bit masks for z80 flag register:
Z80_S_FLAG = 0x80	-- sign flag (duplicates bit 7 of A) ... 1 = negative
Z80_Z_FLAG = 0x40	-- zero (or equal) flag ... 1 = Zero
Z80_FLAG_6 = 0x20	-- duplicates bit 6 of A
Z80_H_FLAG = 0x10	-- half carry flag ... 1 = Carry of lower nibble
Z80_FLAG_3 = 0x08	-- duplicates bit 3 of A
Z80_P_FLAG = 0x04	-- parity/overflow flag ... 1 = Parity Even, 0=Parity Odd
Z80_V_FLAG = 0x04	-- parity/overflow flag ... 1 = Overflow between sign/unsigned
Z80_N_FLAG = 0x02	-- negate flag ... 1 = previous instruction was a substract
Z80_C_FLAG = 0x01	-- carry flag = 1 = Carry
	
-- Basic CPU. Note, this does not contain any memory...
-- 
-- References: 
--  - https://en.wikipedia.org/wiki/Zilog_Z80
Z80CPU = class("Z80CPU")
function Z80CPU:initialize()

	-- See http://www.cpu-world.com/Arch/Z80.html
	-- The Z80 is a little endian machine
	-- 
	
	-- 16-bit registers
	self.PC = 0
	self.IX = 0	-- remember there is IXH and IXL as well
	self.IY = 0	-- remember there is IYH and IYL as well
	self.SP = 0
	self.I = 0

	-- R refresh register..
	-- 2 modes, 
	--   1. random 7-bit value
	--   2. accurate emulation, involving emulation off all R updates in all instructions
	self.R = 0
	self._interrupt_mode = 0
	-- main registers
	self.A = 0
	self._F = 0			-- might only be updated only when requested (if nil). call :get_F()
	self.H = 0
	self.L = 0
	self.B = 0
	self.C = 0
	self.D = 0
	self.E = 0
	-- Flags 
	self.Carry = 0	-- duplicate/master copy of carry flag in F register. Not a boolean, but 0 or 1.
	
	-- cycle counting not implemented yet. However, don't add a number on every 
	-- instruction, instead accumulate them up during compile and add them on
	-- before a jump instruction, where we can replace 'jit.jump_count' with
	-- instruction count limit.
	self.cycles = 0

	-- alternative 'shadow' registers
	self.A_ = 0
	self.F_ = 0		-- never deferred
	self.H_ = 0
	self.L_ = 0
	self.B_ = 0
	self.C_ = 0
	self.D_ = 0
	self.E_ = 0
	
	-- interrupt flags
	self.IFF1 = false -- this on effects maskable interrupts
	self.IFF2 = false -- this one is readible (ld a,i/ld a,r -> P/Ov)
	-- interrupt mode flip flops
	-- IMFa=0, IMFb=0 - Interrupt mode 0. This is an 8080-compatible interrupt mode.
	-- IMFa=0, IMFb=1 - Not used.
	-- IMFa=1, IMFb=0 - Interrupt mode 1. In this mode CPU jumps to location 0038h for interrupt processing.
	-- IMFa=1, IMFb=1 - Interrupt mode 2. In this mode CPU jumps to location, which high-order address is taken from I register, and low order address is supplied by peripheral device.
	self.IMFa = false
	self.IMFb = false

	-- these are used when flag setting is deferred
	self._oldA = 0
	self._otherOp = 0
	self._newA = 0
	self._subtract = false
	
	
	self.simple_flags = {}
	for A = 0,255 do
		self.simple_flags[A] = bit32.band(A,0xa8)	-- create the sign, bit6, bit3
		if A == 0 then
			self.simple_flags[A] = self.simple_flags[A] + Z80_Z_FLAG
		end
	end

end

local function calc_add_overflow(oldA,op2,newA)
--if (~(ra^R)&(wml^ra)&0x80 ~= 0 then V_FLAG end
	if bit32.band(bit32.bnot(bit32.bxor(oldA, op2)), bit32.bxor(newA,oldA), 0x80) ~= 0 then
		return Z80_V_FLAG
	end
	return 0
end

local function calc_sub_overflow(oldA,op2,newA)
--if ((ra^R)&(wml^ra)&0x80 ~= 0 then V_FLAG end
	if bit32.band(bit32.bxor(oldA, op2), bit32.bxor(newA,oldA), 0x80) ~= 0 then
		return Z80_V_FLAG
	end
	return 0
end

local function calc_half_carry(oldA,op2,newA)
-- ((ra^R^wml)&H_FLAG)
	--return "bit32.band(bit32.bxor(CPU.A, "..r..", result),Z80_H_FLAG)"
	return bit32.band(bit32.bxor(oldA, op2, newA),Z80_H_FLAG)
end

function Z80CPU:get_F()
	local F = self._F
	if not F then
		local oldA = self._oldA
		local op2 = self._otherOp
		local newA = self._newA
		F = self.simple_flags[newA] + calc_half_carry(oldA,op2,newA) + 
			  self.Carry
		if self._subtract then
			F = F + Z80_N_FLAG + calc_sub_overflow(oldA,op2,newA)
		else
			F = F + calc_add_overflow(oldA,op2,newA)
		end
		self._F = F	-- don't calculate twice
	end
	
	return F
end

-- A lump is the container for a block of code that represents a compiled Lia set
-- of instructions for a current set of memory addresses. It also contains self-write 
-- detection in order to invalidate the lump.
Z80Lump = class("Z80Lump")
function Z80Lump:initialize(start_address)
	-- other
	--self.code = nil		-- no code to start with
	--self.error = nil	-- no error to start with
	--self.write_detect_to_me = {}
	self.start_address = start_address
	--self.deleted = false
end



----------------------------------------------------------------------------

local function inc_address(address)
	-- maybe we should use return (address+1)%65536 ??
	address = address + 1
	if address >= 65536 then
		address = address - 65536
	end
	return address
end


-- CB = rotates, shifts, bit set/reset/testing
local decode_CB_instructions = {
}

-- DD = IX register
local decode_DD_instructions = {
}

-- FD = IY register
local decode_FD_instructions = {
}

-- extended instructions
local decode_ED_instructions = {

	-- Debug instruction, writes to host stdout .. ED ED
	[0xED] = "io.write(string.char(CPU.A))",	-- fake function for debugging

	-- NEG (like 0-A)  ... we actually use subtract code. Might be easier to generate flags manually.
	--
	-- From Z80 user manual:
	-- S is set if result is negative; reset otherwise
	-- Z is set if result is 0; reset otherwise
	-- H is set if borrow from bit 4; reset otherwise
	-- P/V is set if Accumulator was 80H before operation; reset otherwise N is set
	-- C is set if Accumulator was not 00H before operation; reset otherwise
	--
	[0x44] = [[result=-CPU.A
			if result < 0 then
				CPU.Carry=1 result=result+256
			else
				CPU.Carry=0
			end
			CPU._F = nil
			CPU._oldA = 0
			CPU._otherOp = CPU.A
			CPU._newA = result
			CPU._subtract = true
			CPU.A = result
			]]
}
local function call_code_string(return_addr, target)
	return string.format( [[
		CPU.SP = (CPU.SP-1)%%65536
		if jit.write_allowed[CPU.SP] then memory[CPU.SP] = 0x%x result = jit:code_write_check(CPU.SP) end
		CPU.SP = (CPU.SP-1)%%65536
		if jit.write_allowed[CPU.SP] then memory[CPU.SP] = 0x%x result = result or jit:code_write_check(CPU.SP) end
		if result then CPU.PC = 0x%x return 'invalidate' end
		if jit.jump_count==0 then CPU.PC = 0x%x; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end]], 
			math.floor(return_addr/256), return_addr%256, target, target, target)
end

local function write_2bytes_to_address_command_string(source1, source2, dest_address1, dest_address2)
	return string.format(
	-- we only do write_checks for invalid if the write was allowed. 
	-- we return after both writes have been attempted.
	[[ addr=%s if jit.write_allowed[addr] then memory[addr]=%s
	if jit:code_write_check(addr) then addr = nil end end
	addr2=%s if jit.write_allowed[addr2] then memory[addr2]=%s
	if jit:code_write_check(addr2) then addr = nil end end
	if addr == nil then return 'invalidate' end ]], 
	dest_address1, source1, dest_address2, source2)
end

local function write_to_address_command_string(source, dest_address, optional_flag_check_code)
	-- e.g. dest_address = CPU.H*256+CPU.L
	-- e.g. source = CPU.A
	optional_flag_check_code = optional_flag_check_code or ""
	return string.format(
	[[ addr=%s %sif jit.write_allowed[addr] then memory[addr]=%s  
	if jit:code_write_check(addr) then return 'invalidate' end end]], 
	dest_address, optional_flag_check_code, source)
end

local decode_instruction

local decode_first_byte = {
	-- 00 = NOP
	[0] = "",		-- this is the best instruction :-)

	-- 08 = EX AF, AF'
	-- ensure the flags are updated first!
	[0x08] = [[ CPU.get_F()
		result=CPU.A CPU.A=CPU.A_ CPU.A_=result
		result=CPU._F CPU._F=CPU.F_ CPU.F_=result ]],
	-- 10 = DJNZ xx
	[0x10] = function(memory, iaddr) 
			-- safe to pre-read because a lump write in this rejoin immediately invalidates lump
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr)
			if byte1 > 127 then byte1 = byte1-256 end
			local target = iaddr+byte1
			return string.format(
			"CPU.B = CPU.B - 1 if CPU.B ~= 0 then if jit.jump_count==0 then CPU.PC = 0x%x; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end end", target, target), iaddr, target
		end,
	-- 18 = JR xx
	[0x18] = function(memory, iaddr) 
			-- safe to pre-read because a lump write in this returns immediately invalidates lump
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr)
			if byte1 > 127 then byte1 = byte1-256 end
			local target = iaddr+byte1
			return string.format(
			"if jit.jump_count==0 then CPU.PC = 0x%x; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end", target, target), iaddr, target
		end,
	-- 20 = JR NZ, xx
	[0x20] = function(memory, iaddr) 
			-- safe to pre-read because a lump write in this returns immediately invalidates lump
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr)
			if byte1 > 127 then byte1 = byte1-256 end
			local target = iaddr+byte1
			return string.format(
			"if not bit32.btest(CPU:get_F(), Z80_Z_FLAG) then if jit.jump_count==0 then CPU.PC = 0x%x; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end end", target, target), iaddr, target
		end,
	-- 28 = JR Z, xx
	[0x28] = function(memory, iaddr) 
			-- safe to pre-read because a lump write in this returns immediately invalidates lump
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr)
			if byte1 > 127 then byte1 = byte1-256 end
			local target = iaddr+byte1
			return string.format(
			"if bit32.btest(CPU:get_F(), Z80_Z_FLAG) then if jit.jump_count==0 then CPU.PC = 0x%x; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end end", target, target), iaddr, target
		end,
	-- 30 = JR NC, xx
	[0x30] = function(memory, iaddr) 
			-- safe to pre-read because a lump write in this returns immediately invalidates lump
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr)
			if byte1 > 127 then byte1 = byte1-256 end
			local target = iaddr+byte1
			return string.format(
			"if not CPU.Carry then if jit.jump_count==0 then CPU.PC = 0x%x; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end end", target, target), iaddr, target
		end,
	-- 38 = JR C, xx
	[0x38] = function(memory, iaddr) 
			-- safe to pre-read because a lump write in this returns immediately invalidates lump
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr)
			if byte1 > 127 then byte1 = byte1-256 end
			local target = iaddr+byte1
			return string.format(
			"if CPU.Carry then if jit.jump_count==0 then CPU.PC = 0x%x; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end end", target, target), iaddr, target
		end,

	-- 01 = LD BC, xxxx
	[0x01] = function(memory, iaddr) local byte1 = memory[iaddr];iaddr = inc_address(iaddr);return string.format("CPU.B=%s;CPU.C=%s", memory[iaddr], byte1), inc_address(iaddr) end,
	-- 11 = LD DE, xxxx
	[0x11] = function(memory, iaddr) local byte1 = memory[iaddr];iaddr = inc_address(iaddr);return string.format("CPU.D=%s;CPU.E=%s", memory[iaddr], byte1), inc_address(iaddr) end,
	-- 21 = LD HL, xxxx
	[0x21] = function(memory, iaddr) local byte1 = memory[iaddr];iaddr = inc_address(iaddr);return string.format("CPU.H=%s;CPU.L=%s", memory[iaddr], byte1), inc_address(iaddr) end,
	-- 31 = LD SP, xxxx
	[0x31] = function(memory, iaddr) local byte1 = memory[iaddr];iaddr = inc_address(iaddr);return string.format("CPU.SP=%s", memory[iaddr]*256+byte1), inc_address(iaddr) end,

	-- 02 = LD (BC) , A
	[0x02] = function(memory, iaddr)
			return write_to_address_command_string("CPU.A", "CPU.B*256+CPU.C"), iaddr
		end,
	-- 12 = LD (DE), A
	[0x12] = function(memory, iaddr)
			return write_to_address_command_string("CPU.A", "CPU.D*256+CPU.E"), iaddr
		end,
	-- 22 = LD (xxxx), HL
	[0x22] = function(memory, iaddr)
			local addr = memory[iaddr]; iaddr = inc_address(iaddr);
			addr = addr+256*memory[iaddr]; iaddr = inc_address(iaddr); 
			return write_2bytes_to_address_command_string("CPU.L", "CPU.H", string.format("memory[0x%x]", addr), string.format("memory[0x%x]", addr+1)), iaddr
		end,
	-- 32 = LD (xxxx), A
	[0x32] = function(memory, iaddr)
			local addr = memory[iaddr]; iaddr = inc_address(iaddr);
			addr = addr+256*memory[iaddr]; iaddr = inc_address(iaddr); 
			return write_to_address_command_string("CPU.A", string.format("memory[0x%x]", addr)), iaddr
		end,

	-- 03 = INC BC
	[0x03] = "CPU.C = CPU.C + 1 if CPU.C == 256 then CPU.C = 0 CPU.B = (CPU.B + 1)%256 end",
	-- 13 = INC DE
	[0x13] = "CPU.E = CPU.E + 1 if CPU.E == 256 then CPU.E = 0 CPU.D = (CPU.D + 1)%256 end",
	-- 23 = INC HL
	[0x23] = "CPU.L = CPU.L + 1 if CPU.L == 256 then CPU.L = 0 CPU.H = (CPU.H + 1)%256 end",
	-- 33 = INC SP
	[0x33] = "CPU.SP = (CPU.SP + 1)%65536",
	
	-- n4 = INC r, covered below
	-- n5 = DEC r, covered below
	-- n6 = LD r, xx, covered below
	
	-- 07 = RLCA
	-- 17 = RLA
	-- 27 = DAA
	-- 37 = SCF
		
	-- 09 = ADD HL, BC
	-- 19 = ADD HL, DE
	-- 29 = ADD HL, HL
	-- 39 = ADD HL, SP
	
	-- 0A = LD A,(BC)
	-- 1A = LD A,(DE)
	-- 2A = LD HL,(xxxx)
	-- 3A = LD A,(xxxx)
	
	-- 0B = DEC BC
	[0x03] = "CPU.C = CPU.C - 1 if CPU.C == -1 then CPU.C = 255 CPU.B = (CPU.B - 1)%256 end",
	-- 1B = DEC DE
	[0x13] = "CPU.E = CPU.E - 1 if CPU.E == -1 then CPU.E = 255 CPU.D = (CPU.D - 1)%256 end",
	-- 2B = DEC HL
	[0x23] = "CPU.L = CPU.L - 1 if CPU.L == -1 then CPU.L = 255 CPU.H = (CPU.H - 1)%256 end",
	-- 3B = DEC SP
	[0x33] = "CPU.SP = (CPU.SP - 1)%65536",
	
	-- nC = INC r, covered below
	-- nD = DEC r, covered below
	-- nE = LD r, xx, covered below
	
	-- 0F = RRCA
	-- 1F = RRA
	-- 2F = CPL
	-- 3F = CCF

	-- 40 to 7F = mostly various LD, covered below
	-- 80 to BF = ADD/ADC/SUB/SBC/AND/XOR/OR/CP, covered below

	-- C0/C8/D0/D8/E0/E8/F0/F8 = return instructions with different flags, covered below
	-- C1/D1/E1/F1 = pop register pairs (DE/BC/HL/AF)
	-- C2/CA/D2/DA/E2/EA/F2/FA = JP instructions with different flags, covered below

	-- C3 = JP XXXX
	-- If it's inside this block, we jump to it. We also set a label at that target.
	-- If it's outside this block (or on a mis-aligned instruction from our 
	--  perspective), we alter PC then finish.
	-- There is a question about whether we should stop compilation at this 
	-- stage, since if it was a straight run we'd need to switch elsewhere.
	--
	[0xC3] = function(memory, iaddr) 
			-- safe to pre-read because a lump write in this returns immediately invalidates lump
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr); local byte2 = memory[iaddr]; iaddr = inc_address(iaddr)
			local target = byte1 + 256*byte2
			return string.format(
			"if jit.jump_count==0 then CPU.PC = 0x%x; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end", target, target), iaddr, target
		end,
		
	-- D3 = OUT (xx),A
	-- E3 = EX (SP), HL
	-- F3 = DI
	[0xf3] = "CPU.IFF1 = false;CPU.IFF2 = false",

	-- C4/CC/D4/DC/E4/EC/F4/FC = CALL instructions with different flags, covered below
	
	-- C5/D5/E5/F5 - push register pairs (DE/BC/HL/AF)

	-- C6/CE/D6/DE/E6/EE/F6/FE = ADD/ADC/SUB/SBC/AND/XOR/OR/CP A with immediate value, covered below.

	-- C7/CF/D7/DF/E7/EF/F7/FF = RST instructions
	[0xC7] = function(memory, iaddr) 	-- RST00
			local target = 0x00
			return call_code_string(iaddr, target), iaddr, target
		end,
	[0xCF] = function(memory, iaddr) 	-- RST08
			local target = 0x08
			return call_code_string(iaddr, target), iaddr, target
		end,
	[0xD7] = function(memory, iaddr) 	-- RST10
			local target = 0x10
			return call_code_string(iaddr, target), iaddr, target
		end,
	[0xDF] = function(memory, iaddr) 	-- RST18
			local target = 0x18
			return call_code_string(iaddr, target), iaddr, target
		end,
	[0xE7] = function(memory, iaddr) 	-- RST20
			local target = 0x20
			return call_code_string(iaddr, target), iaddr, target
		end,
	[0xEF] = function(memory, iaddr) 	-- RST28
			local target = 0x28
			return call_code_string(iaddr, target), iaddr, target
		end,
	[0xF7] = function(memory, iaddr) 	-- RST30
			local target = 0x30
			return call_code_string(iaddr, target), iaddr, target
		end,
	[0xFF] = function(memory, iaddr) 	-- RST38
			local target = 0x38
			return call_code_string(iaddr, target), iaddr, target
		end,

	
	-- C9 = RET    ...pop PC.l, pop PC.h
	-- drop out of lump to process return
	[0xC9] = [[
			do result = memory[CPU.SP]
			CPU.SP = (CPU.SP+1)%65536
			result = result + 256*memory[CPU.SP]
			CPU.SP = (CPU.SP+1)%65536
			CPU.PC = result; return 'ok' end]],

	-- D9 = EXX
	[0xD9] = [[
	result=CPU.D CPU.D=CPU.D_ CPU.D_=result
	result=CPU.E CPU.E=CPU.E_ CPU.E_=result
	result=CPU.B CPU.B=CPU.B_ CPU.B_=result
	result=CPU.C CPU.C=CPU.C_ CPU.C_=result
	result=CPU.H CPU.H=CPU.H_ CPU.H_=result
	result=CPU.L CPU.L=CPU.L_ CPU.L_=result
	]],
	-- E9 = JP (HL)
	[0xE9] = "do CPU.PC = CPU.HL; return 'ok' end",
	-- surely there must be a way of jumping internally without dropping out of 
	-- this lump?
	--[0xE9] = function(memory, iaddr)
	--		target = ???
	--		return string.format(
	--		"if jit.jump_count==0 then CPU.PC = CPU.HL; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end", target), iaddr, ???
	--	end
	
	-- F9 = LD SP, HL
	[0xF9] = "CPU.SP = CPU.H*256+CPU.L",

	-- CB = extended instructions
	[0xCB] = function(memory, iaddr)
				return decode_instruction(memory, iaddr, decode_CB_instructions)
		end,
	-- DB = IN A, (xx)
	-- EB = EX DE, HL
	[0xEB] = "result=CPU.D CPU.D=CPU.H CPU.H=result result=CPU.E CPU.E=CPU.L CPU.L=result",
	-- FB = EI
	[0xFB] = "CPU.IFF1 = true;CPU.IFF2 = true",
	
	-- CD = CALL xxxx
	-- Notes as per unconditional JP instruction
	--
	[0xCD] = function(memory, iaddr) 
			-- safe to pre-read because a lump write in this returns immediately invalidates lump
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr)
			local byte2 = memory[iaddr]; iaddr = inc_address(iaddr)
			local target = byte1 + 256*byte2
			return call_code_string(iaddr, target), iaddr, target
		end,

	-- DD = extended instructions for IX
	[0xDD] = function(memory, iaddr)
				return decode_instruction(memory, iaddr, decode_DD_instructions)
		end,
	-- ED = extended instructions
	[0xED] = function(memory, iaddr)
				return decode_instruction(memory, iaddr, decode_ED_instructions)
		end,
	-- FD = extended instructions for IY
	[0xFD] = function(memory, iaddr)
				return decode_instruction(memory, iaddr, decode_FD_instructions)
		end,


	-- write to a location
	--  * check if write allowed (i.e. it's RAM not ROM or unallocated), skip if not
	--  * do the actual write
	--  * check if it's invalidated any code and mark
	--  * abort if it's invalidated this code!
}


-- (HL) becomes lua.memory[CPU.H*256+CPU.L]
local reg_index = {[0]="CPU.B","CPU.C","CPU.D","CPU.E","CPU.H","CPU.L","memory[CPU.H*256+CPU.L]","CPU.A"}

local function _is_single_reg(reg)
	return #reg == 5		-- simple, but effective
end
--
-- populate the load instructions
--
for i = 0x40, 0x7f do
	local from_reg = reg_index[bit32.band(i,7)]			-- not speed critical
	local to_reg = reg_index[bit32.band(bit32.rshift(i,3),7)]	-- not speed critical
	if _is_single_reg(to_reg) then
		-- no memory write check for single reg write
		decode_first_byte[i] = string.format("%s=%s", to_reg, from_reg)
	else
		decode_first_byte[i] = string.format(
		[[addr = CPU.H*256+CPU.L;if jit.write_allowed[addr] then memory[addr]=%s 
		if jit:code_write_check(addr) then return 'invalidate' end end]], from_reg)
	end
end

--
-- ld (HL),(HL) is actually HALT
-- (above code might be broken, anyhow)
decode_first_byte[0x76] = function(memory, iaddr)
	return string.format("do CPU.PC = 0x%x; return 'halt' end", iaddr), iaddr
	end



local function ADD_to_A_string(what)
	return "result=".."CPU.A+" .. what .. [[
	if result > 255 then 
		CPU.Carry=1 result=result-256
	else
		CPU.Carry=0
	end 
	CPU._F = nil
	CPU._oldA = CPU.A
	CPU._otherOp = ]] .. what .. [[
	CPU._newA = result
	CPU._subtract = false
	CPU.A = result
	]]
end
local function ADC_to_A_string(what)
	return "result=".."CPU.A+" .. what .. [[+CPU.Carry
	if result > 255 then
		CPU.Carry=1 result=result-256
	else
		CPU.Carry=0
	end 
	CPU._F = nil
	CPU._oldA = CPU.A
	CPU._otherOp = ]] .. what .. [[
	CPU._newA = result
	CPU._subtract = false
	CPU.A = result
	]]
end
local function SUB_to_A_string(what)
	return "result=".."CPU.A-" .. what .. [[
	if result < 0 then
		CPU.Carry=1 result=result+256
	else
		CPU.Carry=0
	end 
	CPU._F = nil
	CPU._oldA = CPU.A
	CPU._otherOp = ]] .. what .. [[
	CPU._newA = result
	CPU._subtract = true
	CPU.A = result
	]]
end
local function SBC_to_A_string(what)
	return "CPU.A=".."CPU.A-" .. what .. [[-CPU.Carry
	if result < 0 then
		CPU.Carry=1 result=result+256
	else
		CPU.Carry=0
	end 
	CPU._F = nil
	CPU._oldA = CPU.A
	CPU._otherOp = ]] .. what .. [[
	CPU._newA = result
	CPU._subtract = true
	CPU.A = result
	]]
end
local function AND_to_A_string(what)
	return "CPU.A=".."bit32.band(CPU.A," .. what ..
		") CPU._F = zflags[CPU.A]+Z80_H_FLAG CPU.Carry=0"
end
local function XOR_to_A_string(what)
	return "CPU.A=".."bit32.bxor(CPU.A," .. what ..
	") CPU._F = zflags[CPU.A]  CPU.Carry=0"
end
local function OR_to_A_string(what)
	return "CPU.A=".."bit32.bor(CPU.A," .. what ..
	") CPU._F = zflags[CPU.A]  CPU.Carry=0"
end
local function CP_to_A_string(what)
	-- Note: FLAG_5 and FLAG_3 are copied from the operand, not the result... so 
	-- this CP is wrong in that tiny regard... 
	return "result=".."(CPU.A-" .. what .. [[)
	if result < 0 then
		CPU.Carry=1 result=result+256
	else
		CPU.Carry=0
	end 
	CPU._F = nil
	CPU._oldA = CPU.A
	CPU._otherOp = ]] .. what .. [[
	CPU._newA = result
	CPU._subtract = true
	]]	-- no CPU-A set at the end, otherwise same as SUBend
end

local inc_flag_calc = [[ CPU._F = CPU.simple_flags[%s]+CPU.Carry
		if %s %% 0x10 == 0 then CPU._F = CPU._F + Z80_H_FLAG end
		if %s == 0x80 then CPU._F = CPU._F + Z80_V_FLAG end ]]
local dec_flag_calc = [[ CPU._F = CPU.simple_flags[%s] + CPU.Carry + Z80_N_FLAG
		if %s %% 0x10 == 0x0F then CPU._F = CPU._F + Z80_H_FLAG end
		if %s == 0x7F then CPU._F = CPU._F + Z80_V_FLAG end ]]

-- inc 'r', op-code=4*(8*i)		e.g	3C = INC A	[0x3C] = "CPU.A = (CPU.A + 1)%256",	-- inc A		
-- dec 'r'	-- 3E = LD A,XX [0x3E] = function(memory, iaddr) return "CPU.A="..tostring(memory[iaddr]), inc_address(iaddr) end ,
-- generally modulo faster than bitwise-and for Lua's floating point numbers.
for i = 0, 7 do
	local reg = reg_index[i]
	if _is_single_reg(reg) then
		-- inc reg
		decode_first_byte[4+(8*i)] = reg.."=("..reg.."+1)%256" .. 
			string.format(inc_flag_calc, reg, reg, reg)
	else
		-- inc(HL)
		decode_first_byte[4+(8*i)] = 
			write_to_address_command_string("result", "CPU.H*256+CPU.L", 
			"result = (memory[addr]+1)%256 " .. 
			string.format(inc_flag_calc, "result", "result", "result"))
	end
	if _is_single_reg(reg) then
		-- dec reg
		decode_first_byte[5+(8*i)] = reg.."=("..reg.."-1)%256" ..
			string.format(dec_flag_calc, reg, reg, reg)
	else
		-- dec(HL)
		decode_first_byte[5+(8*i)] = 
			write_to_address_command_string("result", "CPU.H*256+CPU.L",
			"result = (memory[addr]-1)%256" ..
			string.format(dec_flag_calc, "result", "result", "result"))
	end
	if _is_single_reg(reg) then
		--ld reg, &00
		decode_first_byte[6+(8*i)] = function(memory, iaddr)
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr);
			return string.format("%s=%d", reg, byte1), iaddr
		end
	else
		--ld (HL), &00
		decode_first_byte[6+(8*i)] = function(memory, iaddr)
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr);
			return write_to_address_command_string(byte1, "CPU.H*256+CPU.L"), iaddr
		end
	end

	-- add a, reg  ... where reg could include (HL)
	decode_first_byte[0x80+i] = ADD_to_A_string(reg)

	-- adc a, reg  ... where reg could include (HL)
	decode_first_byte[0x88+i] = ADC_to_A_string(reg)

	-- sub a, reg  ... where reg could include (HL)
	decode_first_byte[0x90+i] = SUB_to_A_string(reg)

	-- sbc a, reg  ... where reg could include (HL)
	decode_first_byte[0x98+i] = SBC_to_A_string(reg)

	-- and reg  ... where reg could include (HL)
	decode_first_byte[0xA0+i] = AND_to_A_string(reg)
	-- xor reg, e.g. AF = XOR A
	decode_first_byte[0xA8+i] = XOR_to_A_string(reg)
	-- or reg
	decode_first_byte[0xB0+i] = OR_to_A_string(reg)

	-- cp reg    flags as if (A-s)  ... where reg could include (HL)
	-- Note: FLAG_5 and FLAG_3 are copied from the operand, not the result... so 
	-- this CP is wrong in that tiny regard... 
	decode_first_byte[0xB8+i] = CP_to_A_string(reg)
		
end



-- [c6] = ADD A,xx
decode_first_byte[0xc6] = function (memory, iaddr)
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr);
			return ADD_to_A_string(byte1), iaddr
		end
-- [ce] = ADC A,xx
decode_first_byte[0xce] = function (memory, iaddr)
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr);
			return ADC_to_A_string(byte1), iaddr
		end
-- [d6] = SUB A,xx
decode_first_byte[0xd6] = function (memory, iaddr)
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr);
			return SUB_to_A_string(byte1), iaddr
		end
-- [de] = SBC A,xx
decode_first_byte[0xde] = function (memory, iaddr)
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr);
			return SBC_to_A_string(byte1), iaddr
		end
-- [e6] = AND xx
decode_first_byte[0xe6] = function (memory, iaddr)
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr);
			return AND_to_A_string(byte1), iaddr
		end
-- [ee] = XOR xx
decode_first_byte[0xee] = function (memory, iaddr)
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr);
			return XOR_to_A_string(byte1), iaddr
		end
-- [f6] = OR xx
decode_first_byte[0xf6] = function (memory, iaddr)
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr);
			return OR_to_A_string(byte1), iaddr
		end
-- [fe] = CP xx
decode_first_byte[0xfe] = function (memory, iaddr)
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr);
			return CP_to_A_string(byte1), iaddr
		end

-- flags
local flag_index = {
	[0]="not bit32.btest(CPU:get_F(), Z80_Z_FLAG)",	-- NZ = Not Zero (Z_FLAG)
	"bit32.btest(CPU:get_F(), Z80_Z_FLAG)",		-- Z = Zero (Z_FLAG)
	"not CPU.Carry",							-- NC = Carry (C_FLAG)
	"CPU.Carry",								-- C = Carry (C_FLAG)

	"not bit32.btest(CPU:get_F(), Z80_P_FLAG)",	-- PO = Parity Odd (P/V_FLAG)
	"bit32.btest(CPU:get_F(), Z80_P_FLAG)",		-- PE = Parity Even (P/V_FLAG)
	"not bit32.btest(CPU:get_F(), Z80_S_FLAG)",	-- P = P sign positive (S_FLAG)
	"bit32.btest(CPU:get_F(), Z80_S_FLAG)",		-- M = M sign negative (S_FLAG)
}

--
-- Instructions with different flag options
--
for i = 0, 7 do
	local flag_check = flag_index[i]
	
	-- C2/CA/D2/DA/E2/EA/F2/FA = JP instructions with different flags
	-- See notes on unconditional JP
	--
	decode_first_byte[0xC2+(i*8)] = function(memory, iaddr) 
			-- safe to pre-read because a lump write in this returns immediately invalidates lump
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr); local byte2 = memory[iaddr]; iaddr = inc_address(iaddr)
			local target = byte1 + 256*byte2
			return string.format(
			"if %s then if jit.jump_count==0 then CPU.PC = 0x%x; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end end", flag_check, target, target), iaddr, target
		end

	-- C4/CC/D4/DC/E4/EC/F4/FC = CALL instructions with different flags
	-- Notes as per unconditional CALL instruction
	--
	decode_first_byte[0xC4+(i*8)] = function(memory, iaddr) 
			-- safe to pre-read because a lump write in this returns immediately invalidates lump
			local byte1 = memory[iaddr]; iaddr = inc_address(iaddr)
			local byte2 = memory[iaddr]; iaddr = inc_address(iaddr)
			local target = byte1 + 256*byte2
			return string.format([[ if %s then %s end ]], flag_check, call_code_string(iaddr, target)), iaddr, target
		end

	-- C0/C8/D0/D8/E0/E8/F0/F8 = return instructions with different flags, covered below
	--  ...pop PC.l, pop PC.h
	-- drop out of lump to process return
	decode_first_byte[0xC0+(i*8)] = [[ if ]] .. flag_check .. [[ then
			result = memory[CPU.SP]
			CPU.SP = (CPU.SP+1)%65536
			result = result + 256*memory[CPU.SP]
			CPU.SP = (CPU.SP+1)%65536
			CPU.PC = result; return 'ok' end]]

end


-- Decode a single instruction
-- 
-- @param bytes_left must be at least one
-- @param memory should be the memory data starting at the instruction
-- 
-- @return the bytes in this instruction, or nil if 
--
-- (already local)
function decode_instruction(memory, address, instruction_table)	
	instruction_table = instruction_table or decode_first_byte
	-- now decode instruction
	local data = memory[address]
	if not data then
		data = 0		-- nop instruction
	end
	--print(string.format("%x 0x%x", address, data))
	address = inc_address(address)

  	local instruction = instruction_table[data]
	local patch
	if type(instruction) == "string" then
		-- simple
	elseif type(instruction) == "function" then
		--print(memory, address)
		instruction, address, patch = instruction(memory, address)
	elseif instruction == nil then
		--print(string.format("Unknown instruction 0x%x at address 0x%x", data, address))
		-- might be trying to compile data, so this isn't as bad on compile.
		return string.format("do CPU.PC = 0x%x;return 'undefined' end",address) , address
	else
		print("Unknown type in table", type(instruction))
		return nil
	end

	--print(address)
	--io.write(instruction)
	return instruction, address, patch
end



-- z80_compile
--
-- Compiles code to Lua.
-- Needs to be relatively fast, since it's done when code is invalidated
-- and when code is loaded.
-- Produces a lump, which include the Lua block invalidation information.
-- Note: This map skips bytes which are not executed, so it's not contigous.
function z80_compile(memory, address, number_number_instructions_to_compile, pre_code)
	local newlump = Z80Lump:new(address)
	local instructions
	local to_patch
	local instruction_addresses = {}
	local to_patch_table = {}
	local codetext_table = {"local CPU, jit = ... ","; local addr, addr2, result; local memory = jit._memory; local zflags = jit._zflags"} -- define the CPU parameter
	local next_address

	while number_number_instructions_to_compile > 0 do
		instruction_addresses[address] = #codetext_table
		instructions, next_address, to_patch = decode_instruction(memory, address)
		if to_patch then
			to_patch_table[to_patch] = to_patch
			--table.insert(to_patch_table, to_patch)
		end
		if Z80_SOURCE_DEBUG then
			instructions = string.format(
			"%s\n-- MAddr: 0x%x\n-- MCode:", instructions, 
			address)
			for ad = address, next_address-1 do
				instructions = string.format("%s %02x", instructions, memory[ad] or 0)
			end
			instructions = instructions .. "\n-- Disas: " .. single_Z80_disassemble(memory, address)
			instructions = instructions .. "\n--------------------------------\n"
		end
		-- This inserts code before each line, if any.
		-- Maybe we should do this after the compile loop, so there is only
		-- one check rather than one per line?
		if pre_code then
			-- we make the instruction address available here for debugging
			local specific_pre_code = string.format(pre_code, address)
			instructions = specific_pre_code .. instructions
		end
		table.insert(codetext_table, instructions)
		number_number_instructions_to_compile = number_number_instructions_to_compile - 1
		-- next instruction will be at next address
		address = next_address
	end
  -- record address of last Z80 op-code byte
  newlump.last_address = next_address -1
	
	table.insert(codetext_table, "goto finished")
	-- instruction_addresses contains a list of line numbers against addresses
	-- to_patch_table contains a list of labels that have been called
	for _,target_addr in pairs(to_patch_table) do
		-- if not an internal address, return with PC set correctly
		local line = instruction_addresses[target_addr]
		if line then
			line = line +1
			codetext_table[line] = string.format("::l_%04x:: %s", target_addr, codetext_table[line])
		else
			table.insert(codetext_table, string.format("::l_%04x:: do CPU.PC = 0x%x;return 'ok' end", target_addr, target_addr))
		end
	end
	
	table.insert(codetext_table, "::finished:: return 'ok'")
	local source = table.concat(codetext_table, "\n")
	if Z80_SOURCE_DEBUG then
		newlump.source = source
	end	

	--lua 5.2 only
	newlump.code, newlump.error = load(source)
	--lua 5.1 version
	--newlump.code, newlump.error = loadstring(source)
	if newlump.code == nil then
		print("COMPILE ERROR", newlump.error)
		local mstart, mend = newlump.error:find(":%d+:")
		if mstart then
			newlump.error_line = tonumber(newlump.error:sub(mstart+1, mend-1))
		end
	end
	return newlump
end


-- A Z80JIT does the on the fly compilation when running a block.
-- In order to do this successfully, it also needs to represent the
-- system memory.
Z80JIT = class("Z80JIT")


function Z80JIT:initialize()
	self._compiled_code = {}
	
	-- instructions_per_block_max
	-- This determines how often we check for other events.
	-- There is also a slight change more instructions per block means more chance 
	-- self-modifying code will affect this block ... maybe!
	-- However more instructions per block means less overhead because of repeated
	-- run calls, therefore faster.
	--
	-- Conversely, two things:
	-- 1. If a small loop happens to be across a lump boundary, it will be could 
	--    call all the external code (check other events) every instruction!
	-- 2. It doesn't control how many instructions between other event/external
	--    code calls ... since a long loop count be inside a lump.
	-- @todo: Fix this, especially (2).
	self._instructions_per_block_max = 64
	self._jumps_till_pause_max = 50
	self.jump_count = self._jumps_till_pause_max
	self:clear_all()
	
	-- convert:   A -> Z80-flags with S, Z, V=parity
	--            C == 0, H == 0
	self._zflags = {}
	for A = 0,255 do
		self._zflags[A] = bit32.band(A,0xa8)	-- create the sign, bit6, bit3
		if A == 0 then
			self._zflags[A] = self._zflags[A] + Z80_Z_FLAG
		end
		-- um0080
		-- This flag is also used with logical operations and rotate instructions 
		-- to indicate the resulting parity is Even. The number of 1 bits in a 
		-- byte are counted. If the total is Odd, ODD parity is flagged (P = 0). 
		-- If the total is Even, EVEN parity is flagged (P = 1).
		local bitcount = A%2
		for i = 1, 7 do
			bitcount = bitcount + (bit32.rshift(A,i)%2)
		end
		--if (~A+(A>>1)+(A>>2)+(A>>3)+(A>>4)+(A>>5)+(A>>6)+(A>>7))%2 == 1 then
		if bitcount%2 == 0 then
			self._zflags[A] = self._zflags[A] + Z80_P_FLAG
		end
	end
	
	--[[ Print out parity values for checking
	for A = 0, 255, 16 do
		for B = 0, 15 do
			local i = A+B
			local r = bit32.band(self._zflags[i], P_FLAG)
			io.write(string.format("%d, ", r))
		end
		io.write("\n")
	end
	os.exit(0)
	--]]

	self.current_lump = nil
	
	self.codemap = {}			-- table where index is address and contents are nil or table of 'lumps'.
	--self.lumps_count = {}		-- originally table of address against number of lumps
	self.invalidated = {}		-- list of lumps invalidated
	
end


--
-- Load a section of memory
--
function Z80JIT:load_memory(data_as_string, address, is_writable)
	local end_addr = address + data_as_string:len()
	if end_addr >= self._total_memory_length then
		local length_over = end_addr - self._total_memory_length
		data_as_string = data_as_string:sub(1, -length_over)
	end
	local length = data_as_string:len()
	self:code_invalidate_block(address, length)
	self:remove_invalidated_lumps()

	self:select_writable(address, length, is_writable)

	for i = 1, length do
		self._memory[address] = data_as_string:byte(i)
		--print(string.format("load memory ~ %x %x", address, self._memory[address]))
		address = address + 1
	end
end

function Z80JIT:select_writable(address, length, is_writable)
	if type(is_writable) == "boolean" then
		for i = address,address+length do
			self.write_allowed[address] = is_writable
		end
	end
end

function Z80JIT:make_RAM(address, length)
	self:select_writable(address,length, true)
end

function Z80JIT:make_ROM(address, length)
	self:select_writable(address, length, false)
end

-- 
function Z80JIT:clear_all()
	self._total_memory_length = 65536
	
	-- should we use strings here, or just use a dictionary with numerical indexes?
	self.write_allowed = {} -- string.rep('n', self._total_memory_length)
	self._memory = {} -- string.rep('\0', self._total_memory_length)
	
	-- should the dirty flag be updated at the end of the current lump execution
	-- to delete affected blocks, rather than a object level variable?
	--
	-- what about SMC that affect the current lump??
	self._code_dirty = {} --string.rep('n', self._total_memory_length)

	self:invalidate_all()
end

function Z80JIT:fetch_memory(address, length)
	address = address or 0
	length = length or self._total_memory_length
	
	local output = {}
	for a = address, address+length do
		local ci = self._memory[a]
		if not ci then
			ci = 0
		end
		table.insert(output, string.char(ci))
	end
	return table.concat(output)
end

function Z80JIT:exists(address)
	-- @todo: finish this routine ... what is it meant to do, anyway?	
end

function Z80JIT:service_interrupts()
	-- @todo: if greater than 20ms, call service routine
	-- 2 methods: 
	--   1. accurate cycle counting of all instructions
	--   2. just call it every 20ms
end

function Z80JIT:run_z80(our_z80_cpu, address, optional_pre_code)
	local status, success
	our_z80_cpu.PC = address
	repeat
		self:service_interrupts()
		address = our_z80_cpu.PC

		-- if compiled code for this location doesn't exist, make some...
		if not self._compiled_code[address] then
			self._compiled_code[address] = z80_compile(self._memory, address, self._instructions_per_block_max, optional_pre_code)
      if self._compiled_code[address].code then
            self:mark_lump_as_code(self._compiled_code[address])
      end
		end
    
		--
		--- now run the code
		--
		-- fetch the code for this address
		self.current_lump = self._compiled_code[address]
		if self.current_lump.code then
      --
			-- execute z80 lump
			--
			-- We use pcall at the moment
			--status = self.current_lump.code(our_z80_cpu, self)
			success, status = pcall(self.current_lump.code, our_z80_cpu, self)
			if not success then
				print(status)
				self.current_lump.error = status
				local mstart, mend = status:find(":%d+:")
				if mstart then
					self.current_lump.error_line = tonumber(status:sub(mstart+1, mend-1))
				end
				status = "Execute fail"
			end
			if self.jump_count <= 0 then
				-- done enough jumps, return to caller
				self.jump_count = self._jumps_till_pause_max
				break
			end
		else
			status = "Compile fail"
		end
		
		self.current_lump = nil			-- we shouldn't have a current lump any more
		self:remove_invalidated_lumps()		-- remove any lumps that got invalidated

		-- not return what to do with undefined and other erros... 
		-- leave to the caller
	until status ~= "ok"
	return status
end

function Z80JIT:show_details()
	if self.current_lump then
		print("Lump", tostring(self.current_lump))
		print("Lump Z80 start address", self.current_lump.start_address)
	else
		print("No current lump")
	end
end

function Z80JIT:list_current(from, to)
	from = from or 0
	to = to or 987654321
	if self.current_lump then
		if self.current_lump.source then
			local linenum = 1
			for line in self.current_lump.source:gmatch("[^\n]*\n?") do
				if linenum >= from then
					if #line ~= 0 then
						if line:sub(-1) == "\n" then
							line = line:sub(1, -2)
				 		end
						print(string.format("%04d %s", linenum, line))
					end
				end
				if linenum >= to then
					break
				end
		 		linenum = linenum + 1
			end
		else
			print("No source")
		end
	else
		print("No current lump")
	end
end

function Z80JIT:list_current_error()
	if self.current_lump then
		if self.current_lump.error_line then
			self:list_current(self.current_lump.error_line-7, self.current_lump.error_line+8)
		else
			print("No current error line for list")
		end
	else
		print("No current lump for list")
	end
end


--[[ 

Memory checking routines
========================

This section mostly deals with ensuring code is invalidated if the memory is
changed that that code relates to.

We could skip known non-complied instructions (e.g. in-line data) theoretically
but we don't do that at the moment.

--]]

----------------------------------------------------------------------------
-- Called from every write in the produced code.
-- Must check if the write affects
-- any produced code and if so mark an invalidation address.
-- @function code_write_check
-- @param z80 memory address
-- @return True if the current lump is invalidated
-- (if it's this block of code, return true to show that the current block is invalid.)
--
function Z80JIT:code_write_check(address)
	if self.codemap[address] then
		local lumps = self.codemap[address]
		--local count = self.lumps_count[address]
		table.insert(self.invalidated, lumps)
		if lumps[self.current_lump] then
			return true
		end
	end
	return false
end

----------------------------------------------------------------------------
-- Invalidate a single address without checking current lump.
-- Similar to code_write_check() except doesn't check current lump. Used by internal
-- routines for external loading of code.
-- Must check if the write affects any produced code and if so mark an invalidation address.
-- @function _silent_write_invalidate
-- @param z80 memory address
-- @see code_write_check
function Z80JIT:_silent_write_invalidate(address)
	if self.codemap[address] then
		local lumps = self.codemap[address]
		--local count = self.lumps_count[address]
		table.insert(self.invalidated, lumps)
	end
end


----------------------------------------------------------------------------
-- Mark a block of address as code
-- *****not used currently*****
function Z80JIT:mark_range_as_code(first_address, last_address, lump)
	for address = first_address, last_address do
		if not self.codemap[address] then
			self.codemap[address] = { [lump] = true}
			--self.lumps_count[address] = 1
		else
			self.codemap[address][lump] = true
			--table.insert(self.codemap[address], lump)
			--self.lumps_count[address] = self.lumps_count[address] + 1
		end
	end
end

----------------------------------------------------------------------------
-- Mark a lump in the code address
function Z80JIT:mark_lump_as_code(lump)
   self:mark_range_as_code(lump.start_address, lump.last_address, lump)
end

----------------------------------------------------------------------------
-- Mark a single address as code
-- *****not used currently*****
function Z80JIT:mark_single_as_code(address, lump)
	if not self.codemap[address] then
		self.codemap[address] = { [lump] = true}
	else
		self.codemap[address][lump] = true
	end
end

----------------------------------------------------------------------------
-- Mark a single address as code
-- *****not used currently*****
function Z80JIT:mark_iterator_as_code(iterator)
	-- when the lump generates a non contigous set of addresses
	-- we *might* need a more complex method than "start to end"
	-- two methods: 
	--  1. iterator
 	--  2. table of addresses (indexed key, i.e. use ipairs)
	-- 
	-- Of course it might be fine to have a start and last address
	-- and just invalide that specific lump even if it doesn't exist
	-- in those specific codemap (write table) addresses.
	-- Also see _remove_single_lump.
	print("mark_iterator_as_code - not written yet!")
	os.exit()
end
----------------------------------------------------------------------------
-- Removes a lump from the compiled code table and write detect table
-- 
-- 
function Z80JIT:_remove_single_lump(lump)
	-- primary thing is to remove from compiled code
	--
	-- secondary thing is to remove this lump from the codemap
	-- (start address).
	-- 
	if lump.deleted then
		return
	end
	local address = lump.start_address
	self._compiled_code[address] = nil

	-- remove lump from write tables at each address
	for addr = address, lump.last_address do
		-- self.codemap[addr] is either nil or table of [xlump]=true.
		-- We are not checking for the former because we assume that
		-- lumps don't have breaks in their code-address mapping.
		-- This is true at the moment, but might not be always true.
		-- If that is not true, then the for condtions might change.
		-- See mark_iterator_as_code for more notes.
		self.codemap[addr][lump] = nil
	end

	-- mark this lump as already deleted. 
	-- Don't delete it again.
	lump.deleted = true
end

----------------------------------------------------------------------------
-- Removes lumps that are invalidated from the code stream.
-- Note: Should not be used from running Z80 code!
-- @function remove_invalidated_lumps
function Z80JIT:remove_invalidated_lumps()
	for _,lumps in ipairs(self.invalidated) do
		-- key is arbitary (numeric index), so ignore
		for lump, _ in pairs(lumps) do
			-- value is arbitary (true), so ignore
			-- primary thing is to remove from compiled code
			--
			-- secondary thing is to remove this lump from the codemap
			-- (start address). 
			self:_remove_single_lump(lump)
		end
	end
end

----------------------------------------------------------------------------
-- Invalidates a whole range of addresses from the code map. 
-- Usually done on loading a new piece of memory, externally not inside code - 
-- although could be from inside Z80 (does the self lump invalidate).
-- @function code_invalidate_block
-- @param address Z80 memory address start
-- @param len length of the Z80 memory block in bytes
-- @return True if the current lump is invalidated
function Z80JIT:code_invalidate_block(address, len)
	local invalidated_self = false
	if self.current_lump then
		for addr = address, (address+len)-1 do
			if self.code_write_check(addr) then
				invalidated_self = true
			end
			-- alternate method
			-- call code_write_check every time, but always maintain a true
			-- invalidated_self = code_write_check(address) or invalidated_self
		end
	else	-- no current lump cases
		for addr = address, (address+len)-1 do
			self:_silent_write_invalidate(addr)
		end		
	end
	return invalidated_self
end

----------------------------------------------------------------------------
-- Invalidate all cached lumps so code is reloaded.
-- Limitations:
--   - This will not properly invalidate the current lump if that is running.
-- @function invalidate_all
function Z80JIT:invalidate_all()
	-- clear the code
	self._compiled_code = {}
	self.codemap = {}			-- table where index is address and contents are nil or table of 'lumps'.
	self.invalidated = {}		-- list of lumps invalidated
end


-- Timings from 2.66GHz Intel Core i7 dual core.
-- 3.6s
--function test1()
--	local k = 0
--	for i = 0, 100000000 do
--		k = k + i%256
--	end
--end
--
-- 13.9s
--function test2()
--	local k = 0
--	for i = 0, 100000000 do
--		k = k + bit32.band(i, 255)
--	end
--end
-- 
-- 4.1s
--function test3()
--	local k = 0
--	for i = 0, 100000000 do
--		k = k + i
--		-- we could use this for things like inc or add
--		if k > 3435 then
--			k = 0
--		end
--	end
--end
--
--



return true