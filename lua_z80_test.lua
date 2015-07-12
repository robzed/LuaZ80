-- Load some test code and run through the Z80 emulator.
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

print(_VERSION)
require("lua_z80")
require("z80_ss_debug")
require("Z80_assembler")

Z80_debugger_enabled = true
function lua_z80_test()
	--run_code(lua_basic_test(), Z80_debugger_enabled, "force instruction block size")
	
	local code = lua_memory_invalidate_test()
	if code then
		run_code(code, Z80_debugger_enabled, "writable")
	end
end
 
function lua_memory_invalidate_test()
	local z = Z80_Assembler:new()
	z:set_compile_address(0)
	
	-- will be address of LD A immediate operand
	local hl_address = z:get_compile_address() + 1
	z:LD_HL(0)
	z:INC_indirect_HL()
	
	local target_address = z:get_compile_address() + 1
	z:LD_A(65)	-- the character 'A"
	z:DB(0xED, 0xED)	-- internal lua_z80 print-to-console instruction
	
	z:LD_A(string.byte("!"))
	
	-- test invalidation works for this load instruction
	z:LD("(HL)", "A")		-- test that code is generated ok for a random register
	z:LD("B", 66)
	z:LD("(HL)", "B")		-- test that code is generated ok for a random register	
	z:HALT()
	z:SET(7,"A")
	z:RES(7,"A")
	z:BIT(7,"A")
	z:RLC("A")
	z:RRC("A")
	z:RL("A")
	z:RR("A")
	z:SLA("E")
	z:SRA("E")
	z:SLS("B")
	z:SRL("B")
	z:IM_0()
	z:IM_1()
	z:IM_2()
	z:RETI()
	z:RET()
	
	--z:patch_address()
	z:DS("Hello")
	
	-- patch up the address
	z:write_int16(hl_address, target_address)
	
	local code
	if not z:any_errors() then
		--local jit = Z80JIT:new()
		code = z:get_code()

	else
		print("FAIL: didn't assemble")
		for _,errors in ipairs(z:get_error_and_warning_messages()) do
			print(errors)
		end
	end
	
	return code
end

function lua_basic_test()
	-- This test function generates all basic instructions, just to ensure they
	-- compile ok. Because of the crude nature of the generation, lots of 
	-- illegal instructions are also generated.
	-- The program terminates with a HALT
	-- \x format is lua 5.2 only
	-- LD A, 0x21, RST 10H, INC A, RST 10H, HALT, JP 2
	-- NOP NOP NOP NOP NOP NOP NOP RET
	--  addresses  0   1   2   3   4   5   6   7   8  9   a    b   c   d   e   f   10
	local code = "\x3E\x21\xED\xED\x3c\xED\xED\x76\xc3\x02\x00\x00\x00\x00\x00\x00\x00\x00\xED\xED\xC9" -- .. string.rep('\x00', 10000)
	-- instructions with 0000 afterwards to allow bytes with operands
	for i = 0x00, 0xFF do
		if i == 0xCB then
			-- CB instructions are only CB nn
			for j = 0x00, 0xFF do
				code = code .. string.char(i) .. string.char(j)
			end
		elseif i == 0xED then
			-- ED instructions can be 4 bytes
			for j = 0x00, 0xFF do
				code = code .. string.char(i) .. string.char(j) .. string.char(0) .. string.char(0)
			end
		elseif i == 0xFD or i == 0xDD then
			-- FD and DD instructions can be 4 bytes, either via instructions like FD 21 nn nn or FD CB ff nn
			-- We don't generate many of the second form currently, but these are undocumented instructions anyway.
			for j = 0x00, 0xFF do
				code = code .. string.char(i) .. string.char(j) .. string.char(0) .. string.char(0)
			end
			
		else
			code = code .. string.char(i) .. string.char(0) .. string.char(0)
		end
	end
	
	return code
end

function run_code(code, debug_on, special)
	-- lua 5.1 version
	--local code = string.char(0x3e, 0x21, 0xd7, 0x3c, 0xd7)
	--print (code)

	local jit = Z80JIT:new()
	if special == "force instruction block size" then
		jit._instructions_per_block_max = 3262	-- just enough to get to instruction FF (RST 38H)
	end
	
	local writable = false
	if special == "writable" then
		writable = true
	end

	jit:load_memory(code, 0, writable)


	local cpu = Z80CPU:new()
	cpu.PC = 0
	
	local engine = jit
	if debug_on then
		engine = Z80_SS_DEBUG(cpu, jit)
	end
	
	local status
	repeat
		--status = jit:run_z80(cpu, cpu.PC)
		status = engine:run_z80(cpu, cpu.PC)
		print()
		print("Status =",status)
		print("Next Address =", cpu.PC)
		jit:show_details()
	until status ~= "ok"
	
	if status == "Compile fail" or status=="Execute fail" then
		jit:list_current_error()
		--jit:list_current(228,248)
	end
	--jit:list_current()
end

lua_z80_test()

return lua_z80_test
