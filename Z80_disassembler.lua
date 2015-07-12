-- Crude Disassembler for Z80 instructions.
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


require("third_party/strict")

local function decode_operands(instruction, memory, start, size_so_far)
	-- Decodes operands, specifically:
	-- !nn!
	-- !n!
	-- !r!
	-- !d!
	local byte1 = memory[start] or 0
	local byte2 = memory[((start+1)%65536)] or 0

	-- 2's completement displacement for IX and IY ... is occurs before other offsets
	local mstart, mend = instruction:find("!d!")
	if mstart then
		local sign = "+"
		if byte1 >= 128 then
			byte1 = 256-byte1	-- 255 = 1, 254 = 2
			sign = "-"
		end
		instruction = string.format("%s%s%02XH%s",
			instruction:sub(1,mstart-1),
			sign,
			byte1,
			instruction:sub(mend+1))
		
		start = start + 1
		size_so_far = size_so_far + 1
		byte1 = memory[start] or 0
		byte2 = memory[((start+1)%65536)] or 0
	end
	-- 16 bit immediate operand
	local mstart, mend = instruction:find("!nn!")
	if mstart then
		instruction = string.format("%s%02X%02xH%s",
			instruction:sub(1,mstart-1),
			byte2, byte1,
			instruction:sub(mend+1))
		size_so_far = size_so_far + 2
	end
	-- 8 bit immediate operand
	mstart, mend = instruction:find("!n!")
	if mstart then
		instruction = string.format("%s%02XH%s",
			instruction:sub(1,mstart-1),
			byte1,
			instruction:sub(mend+1))
		size_so_far = size_so_far + 1
	end
	-- 8 bit relative address offset
	mstart, mend = instruction:find("!r!")
	if mstart then
		if byte1 > 127 then byte1 = byte1-256 end
		local address = ((start+1+byte1)%65536)
		instruction = string.format("%s%04XH%s",
			instruction:sub(1,mstart-1),
			address,
			instruction:sub(mend+1))
		size_so_far = size_so_far + 1
	end
	return instruction, size_so_far
end

local CB_Z80_table = {
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

local function CB_instruction_decoder(memory, start)
	return CB_Z80_table[memory[start] or 0], 2
end

local ED_Z80_table = {
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
[0x4C] = "[neg]",
[0x4D] = "RETI",
[0x4E] = "[im0]",
[0x4F] = "LD   R,A",
[0x50] = "IN   D,(C)",
[0x51] = "OUT  (C),D",
[0x52] = "SBC  HL,DE",
[0x53] = "LD   (!nn!),DE",
[0x54] = "[neg]",
[0x55] = "[retn]",
[0x56] = "IM   1",
[0x57] = "LD   A,I",
[0x58] = "IN   E,(C)",
[0x59] = "OUT  (C),E",
[0x5A] = "ADC  HL,DE",
[0x5B] = "LD   DE,(!nn!)",
[0x5C] = "[neg]",
[0x5D] = "[reti]",
[0x5E] = "IM   2",
[0x5F] = "LD   A,R",
[0x60] = "IN   H,(C)",
[0x61] = "OUT  (C),H",
[0x62] = "SBC  HL,HL",
[0x63] = "LD   (!nn!),HL",
[0x64] = "[neg]",
[0x65] = "[retn]",
[0x66] = "[im0]",
[0x67] = "RRD",
[0x68] = "IN   L,(C)",
[0x69] = "OUT  (C),L",
[0x6A] = "ADC  HL,HL",
[0x6B] = "LD   HL,(!nn!)",
[0x6C] = "[neg]",
[0x6D] = "[reti]",
[0x6E] = "[im0]",
[0x6F] = "RLD",
[0x70] = "IN   F,(C)",
[0x71] = "OUT  (C),F",
[0x72] = "SBC  HL,SP",
[0x73] = "LD   (!nn!),SP",
[0x74] = "[neg]",
[0x75] = "[retn]",
[0x76] = "[im1]",
[0x77] = "[ld i,i?]",
[0x78] = "IN   A,(C)",
[0x79] = "OUT  (C),A",
[0x7A] = "ADC  HL,SP",
[0x7B] = "LD   SP,(!nn!)",
[0x7C] = "[neg]",
[0x7D] = "[reti]",
[0x7E] = "[im2]",
[0x7F] = "[ld r,r?]",
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
[0xBB] = "OTDR",
[0xED] = "LuaZ80_Print",
}

local function ED_instruction_decoder(memory, start)
	local value = memory[start] or 0
	local instruction = ED_Z80_table[value]

	if instruction == nil then
		return string.format("[ED%02X Unknown]", value), 2
	end

	return decode_operands(instruction, memory, (start+1)%65536, 2)
end

-- Apparently the undocumented instructions here can do three things
-- 1. rotates/shifts/bit ops
-- 2. access low/high of IX/IY
-- 3. or they just affect indexed byte and nothing else
-- See Z80oplist.txt by J.G.Harston for more details
local DD_CB_Z80_table = {
[0] = "? rlc (IX!d!)->b",
"? rlc (IX!d!)->c",
"? rlc (IX!d!)->d",
"? rlc (IX!d!)->e",
"? rlc (IX!d!)->h or rlc IXH",
"? rlc (IX!d!)->l or rlc IXL",
"RLC  (IX!d!)",
"? rlc (IX!d!)->a",
"? rrc (IX!d!)->b",
"? rrc (IX!d!)->c",
"? rrc (IX!d!)->d",
"? rrc (IX!d!)->e",
"? rrc (IX!d!)->h or rrc IXH",
"? rrc (IX!d!)->l or rrc IXL",
"RRC  (IX!d!)",
"? rrc (IX!d!)->a",
"? rl  (IX!d!)->b",
"? rl  (IX!d!)->c",
"? rl  (IX!d!)->d",
"? rl  (IX!d!)->e",
"? rl  (IX!d!)->h or rl IXH",
"? rl  (IX!d!)->l or rl IXL",
"RL   (IX!d!)",
"? rl  (IX!d!)->a",
"? rr  (IX!d!)->b",
"? rr  (IX!d!)->c",
"? rr  (IX!d!)->d",
"? rr  (IX!d!)->e",
"? rr  (IX!d!)->h or rr IXH",
"? rr  (IX!d!)->l or rr IXL",
"RR   (IX!d!)",
"? rr  (IX!d!)->a",
"? sla (IX!d!)->b",
"? sla (IX!d!)->c",
"? sla (IX!d!)->d",
"? sla (IX!d!)->e",
"? sla (IX!d!)->h or sla IXH",
"? sla (IX!d!)->l or sla IXL",
"SLA  (IX!d!)",
"? sla (IX!d!)->a",
"? sra (IX!d!)->b",
"? sra (IX!d!)->c",
"? sra (IX!d!)->d",
"? sra (IX!d!)->e",
"? sra (IX!d!)->h or sra IXH",
"? sra (IX!d!)->l or sra IXL",
"SRA  (IX!d!)",
"? sra (IX!d!)->a",
"? sls (IX!d!)->b",
"? sls (IX!d!)->c",
"? sls (IX!d!)->d",
"? sls (IX!d!)->e",
"? sls (IX!d!)->h or sls IXH",
"? sls (IX!d!)->l or sls IXL",
"SLS  (IX!d!)",
"? sls (IX!d!)->a",
"? srl (IX!d!)->b",
"? srl (IX!d!)->c",
"? srl (IX!d!)->d",
"? srl (IX!d!)->e",
"? srl (IX!d!)->h or sls IXH",
"? srl (IX!d!)->l or sls IXL",
"SRL  (IX!d!)",
"? srl (IX!d!)->a",
"? bit 0,(IX!d!)->b",
"? bit 0,(IX!d!)->c",
"? bit 0,(IX!d!)->d",
"? bit 0,(IX!d!)->e",
"? bit 0,(IX!d!)->h",
"? bit 0,(IX!d!)->l",
"BIT  0,(IX!d!)",
"? bit 0,(IX!d!)->a",
"? bit 1,(IX!d!)->b",
"? bit 1,(IX!d!)->c",
"? bit 1,(IX!d!)->d",
"? bit 1,(IX!d!)->e",
"? bit 1,(IX!d!)->h",
"? bit 1,(IX!d!)->l",
"BIT  1,(IX!d!)",
"? bit 1,(IX!d!)->a",
"? bit 2,(IX!d!)->b",
"? bit 2,(IX!d!)->c",
"? bit 2,(IX!d!)->d",
"? bit 2,(IX!d!)->e",
"? bit 2,(IX!d!)->h",
"? bit 2,(IX!d!)->l",
"BIT  2,(IX!d!)",
"? bit 2,(IX!d!)->a",
"? bit 3,(IX!d!)->b",
"? bit 3,(IX!d!)->c",
"? bit 3,(IX!d!)->d",
"? bit 3,(IX!d!)->e",
"? bit 3,(IX!d!)->h",
"? bit 3,(IX!d!)->l",
"BIT  3,(IX!d!)",
"? bit 3,(IX!d!)->a",
"? bit 4,(IX!d!)->b",
"? bit 4,(IX!d!)->c",
"? bit 4,(IX!d!)->d",
"? bit 4,(IX!d!)->e",
"? bit 4,(IX!d!)->h",
"? bit 4,(IX!d!)->l",
"BIT  4,(IX!d!)",
"? bit 4,(IX!d!)->a",
"? bit 5,(IX!d!)->b",
"? bit 5,(IX!d!)->c",
"? bit 5,(IX!d!)->d",
"? bit 5,(IX!d!)->e",
"? bit 5,(IX!d!)->h",
"? bit 5,(IX!d!)->l",
"BIT  5,(IX!d!)",
"? bit 5,(IX!d!)->a",
"? bit 6,(IX!d!)->b",
"? bit 6,(IX!d!)->c",
"? bit 6,(IX!d!)->d",
"? bit 6,(IX!d!)->e",
"? bit 6,(IX!d!)->h",
"? bit 6,(IX!d!)->l",
"BIT  6,(IX!d!)",
"? bit 6,(IX!d!)->a",
"? bit 7,(IX!d!)->b",
"? bit 7,(IX!d!)->c",
"? bit 7,(IX!d!)->d",
"? bit 7,(IX!d!)->e",
"? bit 7,(IX!d!)->h",
"? bit 7,(IX!d!)->l",
"BIT  7,(IX!d!)",
"? bit 7,(IX!d!)->a",
"? res 0,(IX!d!)->b",
"? res 0,(IX!d!)->c",
"? res 0,(IX!d!)->d",
"? res 0,(IX!d!)->e",
"? res 0,(IX!d!)->h",
"? res 0,(IX!d!)->l",
"RES  0,(IX!d!)",
"? res 0,(IX!d!)->a",
"? res 1,(IX!d!)->b",
"? res 1,(IX!d!)->c",
"? res 1,(IX!d!)->d",
"? res 1,(IX!d!)->e",
"? res 1,(IX!d!)->h",
"? res 1,(IX!d!)->l",
"RES  1,(IX!d!)",
"? res 1,(IX!d!)->a",
"? res 2,(IX!d!)->b",
"? res 2,(IX!d!)->c",
"? res 2,(IX!d!)->d",
"? res 2,(IX!d!)->e",
"? res 2,(IX!d!)->h",
"? res 2,(IX!d!)->l",
"RES  2,(IX!d!)",
"? res 2,(IX!d!)->a",
"? res 3,(IX!d!)->b",
"? res 3,(IX!d!)->c",
"? res 3,(IX!d!)->d",
"? res 3,(IX!d!)->e",
"? res 3,(IX!d!)->h",
"? res 3,(IX!d!)->l",
"RES  3,(IX!d!)",
"? res 3,(IX!d!)->a",
"? res 4,(IX!d!)->b",
"? res 4,(IX!d!)->c",
"? res 4,(IX!d!)->d",
"? res 4,(IX!d!)->e",
"? res 4,(IX!d!)->h",
"? res 4,(IX!d!)->l",
"RES  4,(IX!d!)",
"? res 4,(IX!d!)->a",
"? res 5,(IX!d!)->b",
"? res 5,(IX!d!)->c",
"? res 5,(IX!d!)->d",
"? res 5,(IX!d!)->e",
"? res 5,(IX!d!)->h",
"? res 5,(IX!d!)->l",
"RES  5,(IX!d!)",
"? res 5,(IX!d!)->a",
"? res 6,(IX!d!)->b",
"? res 6,(IX!d!)->c",
"? res 6,(IX!d!)->d",
"? res 6,(IX!d!)->e",
"? res 6,(IX!d!)->h",
"? res 6,(IX!d!)->l",
"RES  6,(IX!d!)",
"? res 6,(IX!d!)->a",
"? res 7,(IX!d!)->b",
"? res 7,(IX!d!)->c",
"? res 7,(IX!d!)->d",
"? res 7,(IX!d!)->e",
"? res 7,(IX!d!)->h",
"? res 7,(IX!d!)->l",
"RES  7,(IX!d!)",
"? res 7,(IX!d!)->a",
"? set 0,(IX!d!)->b",
"? set 0,(IX!d!)->c",
"? set 0,(IX!d!)->d",
"? set 0,(IX!d!)->e",
"? set 0,(IX!d!)->h",
"? set 0,(IX!d!)->l",
"SET  0,(IX!d!)",
"? set 0,(IX!d!)->a",
"? set 1,(IX!d!)->b",
"? set 1,(IX!d!)->c",
"? set 1,(IX!d!)->d",
"? set 1,(IX!d!)->e",
"? set 1,(IX!d!)->h",
"? set 1,(IX!d!)->l",
"SET  1,(IX!d!)",
"? set 1,(IX!d!)->a",
"? set 2,(IX!d!)->b",
"? set 2,(IX!d!)->c",
"? set 2,(IX!d!)->d",
"? set 2,(IX!d!)->e",
"? set 2,(IX!d!)->h",
"? set 2,(IX!d!)->l",
"SET  2,(IX!d!)",
"? set 2,(IX!d!)->a",
"? set 3,(IX!d!)->b",
"? set 3,(IX!d!)->c",
"? set 3,(IX!d!)->d",
"? set 3,(IX!d!)->e",
"? set 3,(IX!d!)->h",
"? set 3,(IX!d!)->l",
"SET  3,(IX!d!)",
"? set 3,(IX!d!)->a",
"? set 4,(IX!d!)->b",
"? set 4,(IX!d!)->c",
"? set 4,(IX!d!)->d",
"? set 4,(IX!d!)->e",
"? set 4,(IX!d!)->h",
"? set 4,(IX!d!)->l",
"SET  4,(IX!d!)",
"? set 4,(IX!d!)->a",
"? set 5,(IX!d!)->b",
"? set 5,(IX!d!)->c",
"? set 5,(IX!d!)->d",
"? set 5,(IX!d!)->e",
"? set 5,(IX!d!)->h",
"? set 5,(IX!d!)->l",
"SET  5,(IX!d!)",
"? set 5,(IX!d!)->a",
"? set 6,(IX!d!)->b",
"? set 6,(IX!d!)->c",
"? set 6,(IX!d!)->d",
"? set 6,(IX!d!)->e",
"? set 6,(IX!d!)->h",
"? set 6,(IX!d!)->l",
"SET  6,(IX!d!)",
"? set 6,(IX!d!)->a",
"? set 7,(IX!d!)->b",
"? set 7,(IX!d!)->c",
"? set 7,(IX!d!)->d",
"? set 7,(IX!d!)->e",
"? set 7,(IX!d!)->h",
"? set 7,(IX!d!)->l",
"SET  7,(IX!d!)",
"? set 7,(IX!d!)->a",
}

-- decode FDCB or DDCD. 
-- Note, we can't use the FD_DD_instruction_decoder with a table change
-- because of the displacement is in the 3rd byte not fourth.
local function FD_DD_CB_instruction_decoder(memory, start, opcode_str, register, size_so_far)
	local opcode = (start+1)%65536
	local value = memory[opcode] or 0
	local instruction = DD_CB_Z80_table[value]
	size_so_far = size_so_far + 1
	
	if instruction == nil then
		return string.format("[%s%02X %s Unknown]", opcode_str, value, register), size_so_far
	end
	
	-- assume it's a string
	
	-- do a replacement of IX with IY, if necessary
	if register ~= "IX" then
		instruction = instruction:gsub("IX", register)
	end

	return decode_operands(instruction, memory, start, size_so_far)
end

local DD_Z80_table = {
[0x09] = "ADD  IX,BC",
[0x19] = "ADD  IX,DE",
[0x21] = "LD   IX,!nn!",
[0x22] = "LD  (!nn!),IX",
[0x23] = "INC  IX",
[0x24] = "INC  IXH",
[0x25] = "DEC  IXH",
[0x26] = "LD   IXH,!n!",
[0x29] = "ADD  IX,IX",
[0x2A] = "LD  IX,(!nn!)",
[0x2B] = "DEC  IX",
[0x2C] = "INC  IXL",
[0x2D] = "DEC  IXL",
[0x2E] = "LD   IXL,!n!",
[0x34] = "INC  (IX!d!)",
[0x35] = "DEC  (IX!d!)",
[0x36] = "LD  (IX!d!),!n!",
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
[0xCB] = FD_DD_CB_instruction_decoder,
[0xE1] = "POP  IX",       
[0xE3] = "EX   (SP),IX",  
[0xE5] = "PUSH IX",       
[0xE9] = "JP   (IX)",     
}


-- This decoder is common to DD and FD decoders
local function FD_DD_instruction_decoder(memory, start, opcode_str, register, size_so_far)
	local value = memory[start] or 0
	local instruction = DD_Z80_table[value]

	if instruction == nil then
		return string.format("[%s%02X %s Unknown]", opcode_str, value, register), size_so_far
	end
	
	if type(instruction) == "function" then
		return instruction(memory, (start+1)%65536, string.format("%s%02X", opcode_str, value), register, size_so_far)
	end
	-- assume it's a string
	
	-- do a replacement of IX with IY, if necessary
	if register ~= "IX" then
		instruction = instruction:gsub("IX", register)
	end

	return decode_operands(instruction, memory, (start+1)%65536, size_so_far)
end

-- This decoder deals with IX instructions
local function DD_instruction_decoder(memory, start)
	return FD_DD_instruction_decoder(memory, start, "DD", "IX", 2)
end

-- This decoder deals with IY instructions
local function FD_instruction_decoder(memory, start)
	return FD_DD_instruction_decoder(memory, start, "FD", "IY", 2)
end

local basic_Z80_table = {
[0x00] = "NOP",
[0x01] = "LD   BC,!nn!",
[0x02] = "LD   (BC),A",
[0x03] = "INC  BC",
[0x04] = "INC  B",
[0x05] = "DEC  B",
[0x06] = "LD   B,!n!",
[0x07] = "RLCA",
[0x08] = "EX   AF,AF'",
[0x09] = "ADD  HL,BC",
[0x0A] = "LD   A,(BC)",
[0x0b] = "DEC BC",
[0x0c] = "INC  C",
[0x0d] = "DEC  C",
[0x0e] = "LD   C,!n!",
[0x0f] = "RRCA",
[0x10] = "DJNZ !r!",
[0x11] = "LD   DE,!nn!",
[0x12] = "LD   (DE),A",
[0x13] = "INC  DE",
[0x14] = "INC  D",
[0x15] = "DEC  D",
[0x16] = "LD   D,!n!",
[0x17] = "RLA",
[0x18] = "JR   !r!",
[0x19] = "ADD  HL,DE",
[0x1A] = "LD   A,(DE)",
[0x1B] = "DEC  DE",
[0x1C] = "INC  E",
[0x1D] = "DEC  E",
[0x1E] = "LD   E,!n!",
[0x1F] = "RRA",
[0x20] = "JR   NZ,!r!",
[0x21] = "LD   HL,!nn!",
[0x22] = "LD  (!nn!),HL",
[0x23] = "INC  HL",
[0x24] = "INC  H",
[0x25] = "DEC  H",
[0x26] = "LD   H,!n!",
[0x27] = "DAA",
[0x28] = "JR   Z,!r!",
[0x29] = "ADD  HL,HL",
[0x2A] = "LD  HL,(!nn!)",
[0x2B] = "DEC  HL",
[0x2C] = "INC  L",
[0x2D] = "DEC  L",
[0x2E] = "LD   L,!n!",
[0x2F] = "CPL",
[0x30] = "JR   NC,!r!",
[0x31] = "LD   SP,!nn!",
[0x32] = "LD   (!nn!),A",
[0x33] = "INC  SP",
[0x34] = "INC  (HL)",
[0x35] = "DEC  (HL)",
[0x36] = "LD   (HL),!n!",
[0x37] = "SCF",
[0x38] = "JR   C,!r!",
[0x39] = "ADD  HL,SP",
[0x3A] = "LD   A,(!nn!)",
[0x3B] = "DEC  SP",
[0x3C] = "INC  A",
[0x3D] = "DEC  A",
[0x3E] = "LD   A,!n!",
[0x3F] = "CCF",
[0x40] = "LD   B,B",
[0x41] = "LD   B,C",
[0x42] = "LD   B,D",
[0x43] = "LD   B,E",
[0x44] = "LD   B,H",
[0x45] = "LD   B,L",
[0x46] = "LD   B,(HL)",
[0x47] = "LD   B,A",
[0x48] = "LD   C,B",
[0x49] = "LD   C,C",
[0x4A] = "LD   C,D",
[0x4B] = "LD   C,E",
[0x4C] = "LD   C,H",
[0x4D] = "LD   C,L",
[0x4E] = "LD   C,(HL)",
[0x4F] = "LD   C,A",
[0x50] = "LD   D,B",
[0x51] = "LD   D,C",
[0x52] = "LD   D,D",
[0x53] = "LD   D,E",
[0x54] = "LD   D,H",
[0x55] = "LD   D,L",
[0x56] = "LD   D,(HL)",
[0x57] = "LD   D,A",
[0x58] = "LD   E,B",
[0x59] = "LD   E,C",
[0x5A] = "LD   E,D",
[0x5B] = "LD   E,E",
[0x5C] = "LD   E,H",
[0x5D] = "LD   E,L",
[0x5E] = "LD   E,(HL)",
[0x5F] = "LD   E,A",
[0x60] = "LD   H,B",
[0x61] = "LD   H,C",
[0x62] = "LD   H,D",
[0x63] = "LD   H,E",
[0x64] = "LD   H,H",
[0x65] = "LD   H,L",
[0x66] = "LD   H,(HL)",
[0x67] = "LD   H,A",
[0x68] = "LD   L,B",
[0x69] = "LD   L,C",
[0x6A] = "LD   L,D",
[0x6B] = "LD   L,E",
[0x6C] = "LD   L,H",
[0x6D] = "LD   L,L",
[0x6E] = "LD   L,(HL)",
[0x6F] = "LD   L,A",
[0x70] = "LD   (HL),B",
[0x71] = "LD   (HL),C",
[0x72] = "LD   (HL),D",
[0x73] = "LD   (HL),E",
[0x74] = "LD   (HL),H",
[0x75] = "LD   (HL),L",
[0x76] = "HALT",
[0x77] = "LD   (HL),A",
[0x78] = "LD   A,B",
[0x79] = "LD   A,C",
[0x7A] = "LD   A,D",
[0x7B] = "LD   A,E",
[0x7C] = "LD   A,H",
[0x7D] = "LD   A,L",
[0x7E] = "LD   A,(HL)",
[0x7F] = "LD   A,A",
[0x80] = "ADD  A,B",
[0x81] = "ADD  A,C",
[0x82] = "ADD  A,D",
[0x83] = "ADD  A,E",
[0x84] = "ADD  A,H",
[0x85] = "ADD  A,L",
[0x86] = "ADD  A,(HL)",
[0x87] = "ADD  A,A",
[0x88] = "ADC  A,B",
[0x89] = "ADC  A,C",
[0x8A] = "ADC  A,D",
[0x8B] = "ADC  A,E",
[0x8C] = "ADC  A,H",
[0x8D] = "ADC  A,L",
[0x8E] = "ADC  A,(HL)",
[0x8F] = "ADC  A,A",
[0x90] = "SUB  A,B",
[0x91] = "SUB  A,C",
[0x92] = "SUB  A,D",
[0x93] = "SUB  A,E",
[0x94] = "SUB  A,H",
[0x95] = "SUB  A,L",
[0x96] = "SUB  A,(HL)",
[0x97] = "SUB  A,A",
[0x98] = "SBC  A,B",
[0x99] = "SBC  A,C",
[0x9A] = "SBC  A,D",
[0x9B] = "SBC  A,E",
[0x9C] = "SBC  A,H",
[0x9D] = "SBC  A,L",
[0x9E] = "SBC  A,(HL)",
[0x9F] = "SBC  A,A",
[0xA0] = "AND  B",
[0xA1] = "AND  C",
[0xA2] = "AND  D",
[0xA3] = "AND  E",
[0xA4] = "AND  H",
[0xA5] = "AND  L",
[0xA6] = "AND  (HL)",
[0xA7] = "AND  A",
[0xA8] = "XOR  B",
[0xA9] = "XOR  C",
[0xAA] = "XOR  D",
[0xAB] = "XOR  E",
[0xAC] = "XOR  H",
[0xAD] = "XOR  L",
[0xAE] = "XOR  (HL)",
[0xAF] = "XOR  A",
[0xB0] = "OR   B",
[0xB1] = "OR   C",
[0xB2] = "OR   D",
[0xB3] = "OR   E",
[0xB4] = "OR   H",
[0xB5] = "OR   L",
[0xB6] = "OR   (HL)",
[0xB7] = "OR   A",
[0xB8] = "CP   B",
[0xB9] = "CP   C",
[0xBA] = "CP   D",
[0xBB] = "CP   E",
[0xBC] = "CP   H",
[0xBD] = "CP   L",
[0xBE] = "CP   (HL)",
[0xBF] = "CP   A",
[0xC0] = "RET  NZ",
[0xC1] = "POP  BC",
[0xC2] = "JP   NZ,!nn!",
[0xC3] = "JP   !nn!",
[0xC4] = "CALL NZ,!nn!",
[0xC5] = "PUSH BC",
[0xC6] = "ADD  A,!n!",
[0xC7] = "RST  00H",
[0xC8] = "RET  Z",
[0xC9] = "RET",
[0xCA] = "JP   Z,!nn!",
[0xCB] = CB_instruction_decoder,
[0xCC] = "CALL Z,!nn!",
[0xCD] = "CALL !nn!",
[0xCE] = "ADC  A,!n!",
[0xCF] = "RST  08H",
[0xD0] = "RET  NC",
[0xD1] = "POP  DE",
[0xD2] = "JP   NC,!nn!",
[0xD3] = "OUT  (!n!),A",
[0xD4] = "CALL NC,!nn!",
[0xD5] = "PUSH DE",
[0xD6] = "SUB  A,!n!",
[0xD7] = "RST  10H",
[0xD8] = "RET  C",
[0xD9] = "EXX",
[0xDA] = "JP   C,!nn!",
[0xDB] = "IN   A,(!n!)",
[0xDC] = "CALL C,!nn!",
[0xDD] = DD_instruction_decoder,
[0xDE] = "SBC  A,!n!",
[0xDF] = "RST  18H",
[0xE0] = "RET  PO",
[0xE1] = "POP  HL",
[0xE2] = "JP   PO,!nn!",
[0xE3] = "EX   (SP),HL",
[0xE4] = "CALL PO,!nn!",
[0xE5] = "PUSH HL",
[0xE6] = "AND  !n!",
[0xE7] = "RST  20H",
[0xE8] = "RET  PE",
[0xE9] = "JP   (HL)",
[0xEA] = "JP   PE,!nn!",
[0xEB] = "EX   DE,HL",
[0xEC] = "CALL PE,!nn!",
[0xED] = ED_instruction_decoder,
[0xEE] = "XOR  !n!",
[0xEF] = "RST  28H",
[0xF0] = "RET  P",
[0xF1] = "POP  AF",
[0xF2] = "JP   P,!nn!",
[0xF3] = "DI",
[0xF4] = "CALL P,!nn!",
[0xF5] = "PUSH AF",
[0xF6] = "OR   !n!",
[0xF7] = "RST  30H",
[0xF8] = "RET  M",
[0xF9] = "LD   SP,HL",
[0xFA] = "JP   M,!nn!",
[0xFB] = "EI",
[0xFC] = "CALL M,!nn!",
[0xFD] = FD_instruction_decoder,
[0xFE] = "CP   !n!",
[0xFF] = "RST  38H",
}



function single_Z80_disassemble(memory, start)
	local instruction = basic_Z80_table[memory[start] or 0]

	if type(instruction) == "string" then
		return decode_operands(instruction, memory, (start+1)%65536, 1)
	elseif type(instruction) == "function" then
		return instruction(memory, (start+1)%65536)
	else
		return tostring(instruction), 1
	end
end

-- Making a different function that was a range of bytes would be trivial
-- NOTE: Bad instructions might result in misalignment of subsequent instructions.
function multiple_Z80_disassemble(memory, start, number_of_instructions)
	local instructions = {}
	for i = 1, number_of_instructions do
		local instruction, size = single_Z80_disassemble(memory, start)
		table.insert(instructions, instruction)
		start = start + size
	end
	return instructions, start
end

--[[
	print()
	print("Simple instructions")
	print(single_Z80_disassemble({ [0]=0x00 }, 0))
	print(single_Z80_disassemble({ [0]=0x3e, 0x33 }, 0))
	print(single_Z80_disassemble({ [0]=0x01, 0x34, 0x12 }, 0))
	print(single_Z80_disassemble({ [0]=0x06, 0x99 }, 0))
	print(single_Z80_disassemble({ [0]=0xDB, 0x88 }, 0))
	print()
	print("ED instructions")
	print(single_Z80_disassemble({ [0]=0xED, 0x40 }, 0))
	print(single_Z80_disassemble({ [0]=0xED, 0x44 }, 0))
	print(single_Z80_disassemble({ [0]=0xED, 0x46 }, 0))
	print(single_Z80_disassemble({ [0]=0xED, 0x4B, 0x34, 0x12 }, 0))
	print(single_Z80_disassemble({ [0]=0xED, 0x53, 0x34, 0x12 }, 0))
	print(single_Z80_disassemble({ [0]=0xED, 0x00 }, 0))
	print()
	print("CB instructions")
	print(single_Z80_disassemble({ [0]=0xCB, 0x00 }, 0))
	print(single_Z80_disassemble({ [0]=0xCB, 0x01 }, 0))
	print(single_Z80_disassemble({ [0]=0xCB, 0xFF }, 0))
	print()
	print("IX/IY instructions")
	print(single_Z80_disassemble({ [0]=0xDD, 0x36, 0xF1, 0x21 }, 0))
	print(single_Z80_disassemble({ [0]=0xDD, 0x09 }, 0))
	print(single_Z80_disassemble({ [0]=0xFD, 0x09 }, 0))
	print(single_Z80_disassemble({ [0]=0xFD, 0x22, 0x34, 0x12 }, 0))
	print(single_Z80_disassemble({ [0]=0xFD, 0x34, 0x77 }, 0))
	print(single_Z80_disassemble({ [0]=0xFD, 0x36, 0x44, 0xAB, 0xFF }, 0))
	print()
	print("IX/IY CB instructions")
	print(single_Z80_disassemble({ [0]=0xDD, 0xCB, 0x20, 0x06 }, 0))
	print(single_Z80_disassemble({ [0]=0xFD, 0xCB, 0x20, 0x06 }, 0))
	print(single_Z80_disassemble({ [0]=0xDD, 0xCB, 0x06, 0x12 }, 0))
	print(single_Z80_disassemble({ [0]=0xFD, 0xCB, 0x06, 0x12 }, 0))
	print(single_Z80_disassemble({ [0]=0xFD, 0xCB, 0x00, 0xFE }, 0))

	print(single_Z80_disassemble({ [0]=0xFD, 0xCB, 0xFF, 0x00 }, 0))
	print(single_Z80_disassemble({ [0]=0xFD, 0xCB, 0xFE, 0x00 }, 0))
	print(single_Z80_disassemble({ [0]=0xFD, 0xCB, 0x80, 0x00 }, 0))
	print(single_Z80_disassemble({ [0]=0xFD, 0xCB, 0x7F, 0x00 }, 0))
	
	print()
	local result, nextaddr = multiple_Z80_disassemble({[0]=0x3E, 0x33, 0xD7, 0xC9, 0xDD, 0xCB, 0x20, 0x06 }, 0, 4)
	print("Multiple test")
	for _,i in ipairs(result) do
		print(i)
	end
	print("Next address=", nextaddr);
--]]


return single_Z80_disassemble
