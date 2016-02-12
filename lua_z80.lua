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
-- Requires Lua 5.2 because we use bit operations. Other changes 5.1/5.2 changes 
-- might be documented as 5.2 in this file. 
--
-- Also goto is used to implement branches - that might be more 'serious' for
-- backporting to Lua 5.1.
--
-- FUTURE ACTIONS
-- ==============
-- High Priority
-- @todo: Complete memory invalidation
-- @todo: test memory invalidation
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
-- @todo: What if the whole memory is NOPs? Does the interpreter hang?
-- @todo: Fix flag bits 5 and 3
------------------------------------------------------------------------------------
-- NOTES: code_write_check and write_allowed go hand in hand. If an instruction doesn't
-- do a write_allowed  first, then code_write_check won't help

require("third_party/strict")
require("third_party/middleclass")
require("z80_ss_debug")        -- only if debug enabled below... (search for 'uncomment for debug')
require("Z80_disassembler")

local Z80_SOURCE_DEBUG = true

-- bit masks for z80 flag register:
Z80_S_FLAG = 0x80    -- sign flag (duplicates bit 7 of A) ... 1 = negative
Z80_Z_FLAG = 0x40    -- zero (or equal) flag ... 1 = Zero
Z80_FLAG_6 = 0x20    -- duplicates bit 6 of A
Z80_H_FLAG = 0x10    -- half carry flag ... 1 = Carry of lower nibble
Z80_FLAG_3 = 0x08    -- duplicates bit 3 of A
Z80_P_FLAG = 0x04    -- parity/overflow flag ... 1 = Parity Even, 0=Parity Odd
Z80_V_FLAG = 0x04    -- parity/overflow flag ... 1 = Overflow between sign/unsigned
Z80_N_FLAG = 0x02    -- negate flag ... 1 = previous instruction was a substract
Z80_C_FLAG = 0x01    -- carry flag = 1 = Carry
    
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
    self.IX = 0    -- remember there is IXH and IXL as well
    self.IY = 0    -- remember there is IYH and IYL as well
    self.SP = 0xFFFF
    self.I = 0

    -- R refresh register..
    -- 2 modes, 
    --   1. random 7-bit value
    --   2. accurate emulation, involving emulation off all R updates in all instructions
    self.R = 0
    self._interrupt_mode = 0    -- look at IMFa/IMFb
    -- main registers
    self.A = 0xFF
    self._F = 0xFF            -- might only be updated only when requested (if nil). call :get_F()
    self.H = 0
    self.L = 0
    self.B = 0
    self.C = 0
    self.D = 0
    self.E = 0
    -- Flags 
    self.Carry = self._F % 2    -- duplicate/master copy of carry flag in F register. Not a boolean, but 0 or 1.
                                -- Carry must always be maintained with F!!!
    
    -- cycle counting not implemented yet. However, don't add a number on every 
    -- instruction, instead accumulate them up during compile and add them on
    -- before a jump instruction, where we can replace 'jit.jump_count' with
    -- instruction count limit.
    self.cycles = 0

    -- alternative 'shadow' registers
    self.A_ = 0xFF
    self.F_ = 0xFF       -- never deferred
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
    
    self._inputs = {}
    self._outputs = {}
    self.simple_flags = {}
    for A = 0,255 do
        self.simple_flags[A] = bit32.band(A,0xa8)    -- create the sign, bit5, bit3
        if A == 0 then
            self.simple_flags[A] = self.simple_flags[A] + Z80_Z_FLAG
        end
    end

end

function Z80CPU:register_input(address_mask, address_state, target_function, target_userdata)
    local io = { mask=address_mask, state=address_state, f=target_function, ud=target_userdata }
    table.insert(self._inputs, io)
end

function Z80CPU:register_output(address_mask, address_state, target_function, target_userdata)
    local io = { mask=address_mask, state=address_state, f=target_function, ud=target_userdata }
    table.insert(self._outputs, io)
end

-- DAA badly documented in Z80 UM & Zaks? Yep!
-- http://stackoverflow.com/questions/8119577/z80-daa-instruction
-- http://z80-heaven.wikidot.com/instructions-set:daa
-- http://datasheets.chipdb.org/Zilog/Z80/z80-documented-0.90.pdf
function Z80CPU:DAA(zflags)
    -- @todo: look for more efficent ways of implementing this, e.g. table lookup
    local flags = self:get_F()
    local result = self.A
    local half_flag_result = 0
    local offset = 0
    local lower = bit32.band(result, 0x0F)
    if lower > 9 or bit32.btest(flags, Z80_H_FLAG) then
        offset =  6
    end
    if result > 0x99 or self.Carry == 1 then
        offset = offset + 0x60
    end
    -- subtract instructions
    if bit32.btest(flags, Z80_N_FLAG) then
        if lower < bit32.band(0xF, offset) then
            half_flag_result = Z80_H_FLAG
        end
        result = result - offset
        if result < 0 then
            self.Carry=1 result=result+256
            
        -- carry is never reset!
        --else
        --    self.Carry=0
        end
    else    -- add instructions
        if lower > 9 then
            half_flag_result = Z80_H_FLAG
        end
        result = result + offset
        if result > 255 then
            self.Carry=1 result=result-256
        -- carry is never reset!
        --else
        --    self.Carry=0
        end
    end
    self._F = zflags[result] + self.Carry + half_flag_result + bit32.band(flags, Z80_N_FLAG)
    self.A = result
end

-- Flags are complex
-- http://rk.nvg.ntnu.no/sinclair/faq/tech_z80.html#RREG
-- http://www.z80.info/z80sflag.htm
-- 
local function calc_add_overflow(oldA,op2,newA)
--if (~(ra^R)&(wml^ra)&0x80 ~= 0 then V_FLAG end
    if bit32.band(bit32.bnot(bit32.bxor(oldA, op2)), bit32.bxor(newA,oldA), 0x80) ~= 0 then
        return Z80_V_FLAG
    end
    return 0
end
Z80CPU.calc_add_overflow = calc_add_overflow

local function calc_sub_overflow(oldA,op2,newA)
--if ((ra^R)&(wml^ra)&0x80 ~= 0 then V_FLAG end
    if bit32.band(bit32.bxor(oldA, op2), bit32.bxor(newA,oldA), 0x80) ~= 0 then
        return Z80_V_FLAG
    end
    return 0
end
Z80CPU.calc_sub_overflow = calc_sub_overflow

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
        self._F = F    -- don't calculate twice
    end
    
    return F
end
local SZV_flag_mask = Z80_S_FLAG + Z80_V_FLAG + Z80_Z_FLAG
function Z80CPU:get_F_only_SZV()
    local F = self._F
    if not F then
        local oldA = self._oldA
        local op2 = self._otherOp
        local newA = self._newA
        F = self.simple_flags[newA]
        if self._subtract then
            F = F + calc_sub_overflow(oldA,op2,newA)
        else
            F = F + calc_add_overflow(oldA,op2,newA)
        end
    end
    F = bit32.band(F, SZV_flag_mask)
    self._F = F
    return F
end

function Z80CPU:__serialize()
    return self
end

-- A lump is the container for a block of code that represents a compiled Lia set
-- of instructions for a current set of memory addresses. It also contains self-write 
-- detection in order to invalidate the lump.
Z80Lump = class("Z80Lump")
function Z80Lump:initialize(start_address)
    -- other
    --self.code = nil        -- no code to start with
    --self.error = nil    -- no error to start with
    --self.write_detect_to_me = {}
    self.start_address = start_address
    --self.deleted = false
end

function Z80Lump:__serialize()
    return self
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

-- (HL) becomes lua.memory[CPU.H*256+CPU.L]
local reg_index = {[0]="CPU.B","CPU.C","CPU.D","CPU.E","CPU.H","CPU.L","memory[CPU.H*256+CPU.L]","CPU.A"}

local function _is_single_reg(reg)
    return #reg == 5        -- simple, but effective
end


-- CB = rotates, shifts, bit set/reset/testing
local decode_CB_instructions = {
}

--  BIT n,r
-- *Z513*0-  PV as Z, S set only if n=7 and b7 of r set
-- Behaves much like AND r,2^n, except Carry is unchanged.
local function BIT_string(bitmask, what)
    return string.format("CPU._F = zflags[bit32.band(%s, %s)]+Z80_H_FLAG+CPU.Carry", bitmask, what)
end

-- populate
--    SET b, r
--    RES b, r
for reg = 0, 7 do
    local reg_string = reg_index[reg]            -- not speed critical
    
    if _is_single_reg(reg_string) then
    -- no memory write check for single reg write
    
    -- RLC r ... bit 7 to carry and bit 0
    decode_CB_instructions[0x00 + reg] = string.format(
        "CPU:get_F_only_SZV() result=%s*2 if result>255 then result=result-255 CPU.Carry=1 else CPU.Carry=0 end CPU._F=zflags[result]+CPU.Carry %s=result",
        reg_string, reg_string)
    -- RRC r ... bit 0 to carry and bit 7
    decode_CB_instructions[0x08 + reg] = string.format(
        "CPU:get_F_only_SZV() result=%s if (result %% 2) == 1 then result=(result+255)/2 CPU.Carry=1 else result=result/2 CPU.Carry=0 end CPU._F=zflags[result]+CPU.Carry %s=result",
        reg_string, reg_string)
    -- RL r
    decode_CB_instructions[0x10 + reg] = string.format(
        "CPU:get_F_only_SZV() result=%s*2+CPU.Carry if result>255 then result=result-256 CPU.Carry=1 else CPU.Carry=0 end CPU._F=zflags[result]+CPU.Carry %s=result",
        reg_string, reg_string)
    -- RR r
    decode_CB_instructions[0x18 + reg] = string.format(
        "CPU:get_F_only_SZV() result=%s temp=result%%2 result=(result-temp)/2+CPU.Carry*128 CPU.Carry=temp CPU._F=zflags[result]+CPU.Carry %s=result",
        reg_string, reg_string)
    -- SLA r ... bit 7 to carry
    decode_CB_instructions[0x20 + reg] = string.format(
        "CPU:get_F_only_SZV() result=%s*2 if result>255 then result=result-256 CPU.Carry=1 else CPU.Carry=0 end CPU._F=zflags[result]+CPU.Carry %s=result",
        reg_string, reg_string)
    -- SRA r
    decode_CB_instructions[0x28 + reg] = string.format(
        "CPU:get_F_only_SZV() result=%s CPU.Carry=result%%2 result=((result-CPU.Carry)/2)+bit32.band(result,128) CPU._F=zflags[result]+CPU.Carry %s=result",
        reg_string, reg_string)
    -- SLS r ... bit 7 to carry, set bit 0
    decode_CB_instructions[0x30 + reg] = string.format(
        "CPU:get_F_only_SZV() result=(%s*2)+1 if result>255 then result=result-256 CPU.Carry=1 else CPU.Carry=0 end CPU._F=zflags[result]+CPU.Carry %s=result",
        reg_string, reg_string)
    -- SRL r
    decode_CB_instructions[0x38 + reg] = string.format(
        "CPU:get_F_only_SZV() result=%s CPU.Carry=result%%2 result=((result-CPU.Carry)/2) CPU._F=zflags[result]+CPU.Carry %s=result",
        reg_string, reg_string)

    else
    -- RLC (HL) ... bit 7 to carry and bit 0
    decode_CB_instructions[0x00 + reg] = function(memory, iaddr) return string.format(
        [[CPU:get_F_only_SZV() addr=CPU.H*256+CPU.L;result=memory[addr]*2 if result>255 then result=result-255 CPU.Carry=1 else CPU.Carry=0 end CPU._F=zflags[result]+CPU.Carry 
        if jit.write_allowed[addr] then memory[addr]=result if jit:code_write_check(addr) then CPU.PC = 0x%x; return 'invalidate' end end]], iaddr), iaddr end

    -- RRC (HL) ... bit 7 to carry and bit 0
    decode_CB_instructions[0x08 + reg] = function(memory, iaddr) return string.format(
        [[CPU:get_F_only_SZV() addr=CPU.H*256+CPU.L; result=memory[addr] if (result%%2)==1 then result=(result+255)/2 CPU.Carry=1 else result=result/2 CPU.Carry=0 end CPU._F=zflags[result]+CPU.Carry 
        if jit.write_allowed[addr] then memory[addr]=result if jit:code_write_check(addr) then CPU.PC = 0x%x; return 'invalidate' end end]], iaddr), iaddr end
    
    -- RL (HL) ... bit 7 to carry and bit 0
    decode_CB_instructions[0x10 + reg] = function(memory, iaddr) return string.format(
        [[CPU:get_F_only_SZV() addr=CPU.H*256+CPU.L; result=memory[addr]*2+CPU.Carry if result>255 then result=result-256 CPU.Carry=1 else CPU.Carry=0 end CPU._F=zflags[result]+CPU.Carry 
        if jit.write_allowed[addr] then memory[addr]=result if jit:code_write_check(addr) then CPU.PC = 0x%x; return 'invalidate' end end]], iaddr), iaddr end
    
    -- RR (HL) ... bit 0 to carry and bit 7
    decode_CB_instructions[0x18 + reg] = function(memory, iaddr) return string.format(
        [[CPU:get_F_only_SZV() addr=CPU.H*256+CPU.L; result=memory[addr] temp=result%%2 result=(result-temp)/2+CPU.Carry*128 CPU.Carry=temp CPU._F=zflags[result]+CPU.Carry
        if jit.write_allowed[addr] then memory[addr]=result if jit:code_write_check(addr) then CPU.PC = 0x%x; return 'invalidate' end end]], iaddr), iaddr end

    -- SLA (HL) ... bit 7 to carry
    decode_CB_instructions[0x20 + reg] = function(memory, iaddr) return string.format(
        [[CPU:get_F_only_SZV() addr=CPU.H*256+CPU.L;result=memory[addr]*2 if result>255 then result=result-256 CPU.Carry=1 else CPU.Carry=0 end CPU._F=zflags[result]+CPU.Carry 
        if jit.write_allowed[addr] then memory[addr]=result if jit:code_write_check(addr) then CPU.PC = 0x%x; return 'invalidate' end end]], iaddr), iaddr end
    
    -- SRA (HL) ... bit 0 to carry and bit 7
    decode_CB_instructions[0x28 + reg] = function(memory, iaddr) return string.format(
        [[CPU:get_F_only_SZV() addr=CPU.H*256+CPU.L; result=memory[addr] CPU.Carry=result%%2 result=((result-CPU.Carry)/2)+bit32.band(result,128) CPU._F=zflags[result]+CPU.Carry
        if jit.write_allowed[addr] then memory[addr]=result if jit:code_write_check(addr) then CPU.PC = 0x%x; return 'invalidate' end end]], iaddr), iaddr end

    -- SLS (HL) ... bit 7 to carry, set bit 0
    decode_CB_instructions[0x30 + reg] = function(memory, iaddr) return string.format(
        [[CPU:get_F_only_SZV() addr=CPU.H*256+CPU.L;result=memory[addr]*2+1 if result>255 then result=result-256 CPU.Carry=1 else CPU.Carry=0 end CPU._F=zflags[result]+CPU.Carry 
        if jit.write_allowed[addr] then memory[addr]=result if jit:code_write_check(addr) then CPU.PC = 0x%x; return 'invalidate' end end]], iaddr), iaddr end

    -- SRL (HL) ... bit 0 to carry and bit 7
    decode_CB_instructions[0x38 + reg] = function(memory, iaddr) return string.format(
        [[CPU:get_F_only_SZV() addr=CPU.H*256+CPU.L; result=memory[addr] CPU.Carry=result%%2 result=((result-CPU.Carry)/2) CPU._F=zflags[result]+CPU.Carry
        if jit.write_allowed[addr] then memory[addr]=result if jit:code_write_check(addr) then CPU.PC = 0x%x; return 'invalidate' end end]], iaddr), iaddr end

    end

    -- bit ops
    for bit = 0, 7 do
        local bitmask = 2 ^ bit
        local invbitmask = 255 - bitmask
        if _is_single_reg(reg_string) then
            -- no memory write check for single reg write
            -- SET b, r
            decode_CB_instructions[0xC0 + 8* bit + reg] = string.format("%s=bit32.bor(%s, %s)",reg_string, reg_string, bitmask)
            -- RES b, r
            decode_CB_instructions[0x80 + 8* bit + reg] = string.format("%s=bit32.band(%s, %s)",reg_string, reg_string, invbitmask)
            -- BIT b, r
            decode_CB_instructions[0x40 + 8* bit + reg] = BIT_string(bitmask, reg_string)
        else
            -- SET n,(HL)
            decode_CB_instructions[0xC0 + 8* bit + reg] = function(memory, iaddr)
                    return string.format([[addr = CPU.H*256+CPU.L;if jit.write_allowed[addr] then memory[addr]=bit32.bor(memory[addr], %s)
                    if jit:code_write_check(addr) then CPU.PC = 0x%x; return 'invalidate' end end]], bitmask, iaddr), iaddr
                end
            -- RES n,(HL)
            decode_CB_instructions[0x80 + 8* bit + reg] = function(memory, iaddr)
                    return string.format([[addr = CPU.H*256+CPU.L;if jit.write_allowed[addr] then memory[addr]=bit32.band(memory[addr], %s)
                    if jit:code_write_check(addr) then CPU.PC = 0x%x; return 'invalidate' end end]], invbitmask, iaddr), iaddr
                end
            -- BIT b, (HL)
            decode_CB_instructions[0x40 + 8* bit + reg] = BIT_string(bitmask, reg_string)
        end
    end
end


local function write_2bytes_to_address_command_string(source1, source2, dest_address1, dest_address2, next_pc, pre_code)
    pre_code = pre_code or ""
    return string.format(
    -- we only do write_checks for invalid if the write was allowed. 
    -- we return after both writes have been attempted.
    [[%s addr=%s if jit.write_allowed[addr] then memory[addr]=%s
    if jit:code_write_check(addr) then addr = nil end end
    addr2=%s if jit.write_allowed[addr2] then memory[addr2]=%s
    if jit:code_write_check(addr2) then addr = nil end end
    if addr == nil then CPU.PC = 0x%x return 'invalidate' end ]], 
    pre_code, dest_address1, source1, dest_address2, source2, next_pc)
end

-- DD = IX register
local decode_DD_instructions = {
    [0x21] = function(memory, iaddr) local byte1 = memory[iaddr];iaddr = inc_address(iaddr);return string.format("CPU.IX=%s", memory[iaddr]*256+byte1), inc_address(iaddr) end,
    -- "INC  IX"
    [0x23] = "CPU.IX = (CPU.IX+1) % 65536",
    [0x26] = function(memory, iaddr) local byte1 = memory[iaddr];iaddr = inc_address(iaddr);return string.format("CPU.IX=(CPU.IX%%256)+%s", 256*byte1), iaddr end,
    --"DEC  IX"
    [0x2B] = "CPU.IX = (CPU.IX-1) % 65536",
    [0x2E] = function(memory, iaddr) local byte1 = memory[iaddr];iaddr = inc_address(iaddr);return string.format("CPU.IX=bit32.band(0xFF00, CPU.IX)+%s", byte1), iaddr end,
    [0x44] = "CPU.B=bit32.band(CPU.IX, 0xFF00)/256",
    [0x45] = "CPU.B=CPU.IX%256",
    [0x4C] = "CPU.C=bit32.band(CPU.IX, 0xFF00)/256",
    [0x4D] = "CPU.C=CPU.IX%256",
    [0x54] = "CPU.D=bit32.band(CPU.IX, 0xFF00)/256",
    [0x55] = "CPU.D=CPU.IX%256",
    [0x5C] = "CPU.E=bit32.band(CPU.IX, 0xFF00)/256",
    [0x5D] = "CPU.E=CPU.IX%256",
    
    [0x60] = "CPU.IX=CPU.IX%256+CPU.B*256", -- LD IXH, B
    [0x61] = "CPU.IX=CPU.IX%256+CPU.C*256", -- LD IXH, C
    [0x62] = "CPU.IX=CPU.IX%256+CPU.D*256", -- LD IXH, D
    [0x63] = "CPU.IX=CPU.IX%256+CPU.E*256", -- LD IXH, E
    
    [0x64] = "", -- LD IXH, IXH
    [0x65] = "CPU.IX=(CPU.IX%256)*0x101",
    
    [0x67] = "CPU.IX=CPU.IX%256+CPU.A*256", -- LD IXH, A
    [0x6C] = "temp=bit32.band(CPU.IX, 0xFF00) CPU.IX=temp/256+temp",
    [0x6D] = "", -- LD IXL, IXL
    [0x6F] = "CPU.IX=bit32.band(CPU.IX, 0xFF00)+CPU.A", -- LD IXL, A
    [0x7C] = "CPU.A=bit32.band(CPU.IX, 0xFF00)/256",
    [0x7D] = "CPU.A=CPU.IX%256",
    [0xE1] = "CPU.IX = memory[CPU.SP] + 256*memory[(CPU.SP+1)%65536] CPU.SP = (CPU.SP+2)%65536", -- POP IX
    
        -- E3 = EX (SP), IX
    [0xE3] = function(memory, iaddr)
            -- seems like candidate for faster implementation! (2 extra copies, extra calculation of SP+1)
            return write_2bytes_to_address_command_string("temp", "result", "CPU.SP", "(CPU.SP+1)%65536", iaddr, 
                "result = bit32.band(CPU.IX,0xFF00) / 256  temp = CPU.IX % 256 CPU.IX = memory[CPU.SP] + 256 * memory[(CPU.SP+1)%65536]"), iaddr
        end,

    [0xE5] = function(memory, iaddr)    -- push IX
            return write_2bytes_to_address_command_string("CPU.IX%256", "bit32.band(CPU.IX, 0xFF00)/256", "CPU.SP", "(CPU.SP+1)%65536", iaddr, "CPU.SP=(CPU.SP-2)%65536"), iaddr
        end,
    -- E9 = JP (IX)   (see JP (HL) for notes)
    [0xE9] = "do CPU.PC = CPU.IX; return 'ok' end",
}

-- FD = IY register
local decode_FD_instructions = {
    [0x21] = function(memory, iaddr) local byte1 = memory[iaddr];iaddr = inc_address(iaddr);return string.format("CPU.IY=%s", memory[iaddr]*256+byte1), inc_address(iaddr) end,
    -- "INC  IY"
    [0x23] = "CPU.IY = (CPU.IY+1) % 65536",
    [0x26] = function(memory, iaddr) local byte1 = memory[iaddr];iaddr = inc_address(iaddr);return string.format("CPU.IY=(CPU.IY%%256)+%s", 256*byte1), iaddr end,
    --"DEC  IY"
    [0x2B] = "CPU.IY = (CPU.IY-1) % 65536",
    [0x2E] = function(memory, iaddr) local byte1 = memory[iaddr];iaddr = inc_address(iaddr);return string.format("CPU.IY=bit32.band(0xFF00, CPU.IY)+%s", byte1), iaddr end,
    [0x44] = "CPU.B=bit32.band(CPU.IY, 0xFF00)/256",
    [0x45] = "CPU.B=CPU.IY%256",
    [0x4C] = "CPU.C=bit32.band(CPU.IY, 0xFF00)/256",
    [0x4D] = "CPU.C=CPU.IY%256",
    [0x54] = "CPU.D=bit32.band(CPU.IY, 0xFF00)/256",
    [0x55] = "CPU.D=CPU.IY%256",
    [0x5C] = "CPU.E=bit32.band(CPU.IY, 0xFF00)/256",
    [0x5D] = "CPU.E=CPU.IY%256",
    
    [0x60] = "CPU.IY=CPU.IY%256+CPU.B*256", -- LD IYH, B
    [0x61] = "CPU.IY=CPU.IY%256+CPU.C*256", -- LD IYH, C
    [0x62] = "CPU.IY=CPU.IY%256+CPU.D*256", -- LD IYH, D
    [0x63] = "CPU.IY=CPU.IY%256+CPU.E*256", -- LD IYH, E
    
    [0x64] = "", -- LD IYH, IYH
    [0x65] = "CPU.IY=(CPU.IY%256)*0x101",
    
    [0x67] = "CPU.IY=CPU.IY%256+CPU.A*256", -- LD IYH, A
    [0x6C] = "temp=bit32.band(CPU.IY, 0xFF00) CPU.IY=temp/256+temp",
    [0x6D] = "", -- LD IYL, IYL
    [0x6F] = "CPU.IY=bit32.band(CPU.IY, 0xFF00)+CPU.A", -- LD IYL, A
    [0x7C] = "CPU.A=bit32.band(CPU.IY, 0xFF00)/256",
    [0x7D] = "CPU.A=CPU.IY%256",
    [0xE1] = "CPU.IY = memory[CPU.SP] + 256*memory[(CPU.SP+1)%65536] CPU.SP = (CPU.SP+2)%65536", -- POP IY
    
    -- E3 = EX (SP), IY
    [0xE3] = function(memory, iaddr)
            -- seems like candidate for faster implementation! (2 extra copies, extra calculation of SP+1)
            return write_2bytes_to_address_command_string("temp", "result", "CPU.SP", "(CPU.SP+1)%65536", iaddr, 
                "result = bit32.band(CPU.IY,0xFF00) / 256  temp = CPU.IY % 256 CPU.IY = memory[CPU.SP] + 256 * memory[(CPU.SP+1)%65536]"), iaddr
        end,
    
    [0xE5] = function(memory, iaddr)    -- push IY
        return write_2bytes_to_address_command_string("CPU.IY%256", "bit32.band(CPU.IY, 0xFF00)/256", "CPU.SP", "(CPU.SP+1)%65536", iaddr, "CPU.SP=(CPU.SP-2)%65536"), iaddr
    end,
    -- E9 = JP (IY)   (see JP (HL) for notes)
    [0xE9] = "do CPU.PC = CPU.IY; return 'ok' end",

}



local function port_output_string(addr_hi, addr_lo, data)
    return string.format(
        -- run all output sources, not just first
        -- @todo:could precalculate specific outputs on registration?
        [[ for _,pd in ipairs(CPU._outputs) do 
if bit32.band(pd.mask, %s*256+%s) == pd.state then 
    pd.f(pd.ud, %s, %s, %s) 
end end]], addr_hi, addr_lo, addr_hi, addr_lo, data)
end

local function port_input_string(addr_hi, addr_lo, target_register, flags_required)
    local flags_code = ""
    if flags_required then
        flags_code = "CPU._F=zflags[result]+CPU.Carry "
    end
    return string.format(
        -- run first matching input source only
        -- @todo:could precalculate specific input on registration?
        [[ result = 0xFF for _,pd in ipairs(CPU._inputs) do 
if bit32.band(pd.mask, %s*256+%s) == pd.state then 
    result = pd.f(pd.ud, %s, %s) 
    break
end end %s%s=result]], addr_hi, addr_lo, addr_hi, addr_lo, flags_code, target_register)
end


-- extended instructions
local decode_ED_instructions = {

    -- ED 40 = IN B, (C) ... see below
    -- ED 48 = IN C, (C) ... see below
    -- ED 50 = IN D, (C) ... see below
    -- ED 58 = IN E, (C) ... see below
    -- ED 60 = IN H, (C) ... see below
    -- ED 68 = IN L, (C) ... see below
    -- ED 70 = IN F, (C) ... see below
    -- ED 78 = IN A, (C) ... see below
    
    -- ED 41 = OUT (C), B ... see below
    -- ED 49 = OUT (C), C ... see below
    -- ED 51 = OUT (C), D ... see below
    -- ED 59 = OUT (C), E ... see below
    -- ED 61 = OUT (C), H ... see below
    -- ED 69 = OUT (C), L ... see below
    -- ED 71 = OUT (C), F ... see below
    -- ED 79 = OUT (C), A ... see below

    -- ED 44 = NEG (like 0-A)  ... we actually use subtract code. Might be easier to generate flags manually.
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
            ]],
            
    --LD   (xxxx),BC
    [0x43] = function(memory, iaddr)
            local addr = memory[iaddr]; iaddr = inc_address(iaddr);
            addr = addr+256*memory[iaddr]; iaddr = inc_address(iaddr); 
            return write_2bytes_to_address_command_string("CPU.C", "CPU.B", string.format("0x%x", addr), string.format("0x%x", (addr+1)%65536), iaddr), iaddr
        end,

    --LD   BC,(xxxx)
    [0x4B] = function(memory, iaddr)
            -- safe to pre-read because a lump write in this returns immediately invalidates lump
            local byte1 = memory[iaddr]; iaddr = inc_address(iaddr); local byte2 = memory[iaddr]; iaddr = inc_address(iaddr)
            local target = byte1 + 256*byte2
            return string.format(
            "CPU.C = memory[0x%x] CPU.B = memory[0x%x]", target, (target+1)%65536 ), iaddr
        end,
        
    -- LD   (xxxx),DE
    [0x53] = function(memory, iaddr)
            local addr = memory[iaddr]; iaddr = inc_address(iaddr);
            addr = addr+256*memory[iaddr]; iaddr = inc_address(iaddr); 
            return write_2bytes_to_address_command_string("CPU.E", "CPU.D", string.format("0x%x", addr), string.format("0x%x", (addr+1)%65536), iaddr), iaddr
        end,

    --LD   DE,(xxxx)
    [0x5B] = function(memory, iaddr)
            -- safe to pre-read because a lump write in this returns immediately invalidates lump
            local byte1 = memory[iaddr]; iaddr = inc_address(iaddr); local byte2 = memory[iaddr]; iaddr = inc_address(iaddr)
            local target = byte1 + 256*byte2
            return string.format(
            "CPU.E = memory[0x%x] CPU.D = memory[0x%x]", target, (target+1)%65536 ), iaddr
        end,
    
    -- LD   (xxxx), HL
    [0x63] = function(memory, iaddr)
            local addr = memory[iaddr]; iaddr = inc_address(iaddr);
            addr = addr+256*memory[iaddr]; iaddr = inc_address(iaddr); 
            return write_2bytes_to_address_command_string("CPU.L", "CPU.H", string.format("0x%x", addr), string.format("0x%x", (addr+1)%65536), iaddr), iaddr
        end,

    --LD  HL,(xxxx)
    [0x6B] = function(memory, iaddr)
            -- safe to pre-read because a lump write in this returns immediately invalidates lump
            local byte1 = memory[iaddr]; iaddr = inc_address(iaddr); local byte2 = memory[iaddr]; iaddr = inc_address(iaddr)
            local target = byte1 + 256*byte2
            return string.format(
            "CPU.L = memory[0x%x] CPU.H = memory[0x%x]", target, (target+1)%65536 ), iaddr
        end,
    
    -- LD   (xxxx),SP
    [0x73] = function(memory, iaddr)
            local addr = memory[iaddr]; iaddr = inc_address(iaddr);
            addr = addr+256*memory[iaddr]; iaddr = inc_address(iaddr); 
            return write_2bytes_to_address_command_string("(CPU.SP%256)", "bit32.rshift(CPU.SP, 8)", string.format("0x%x", addr), string.format("0x%x", (addr+1)%65536), iaddr), iaddr
        end,

    --LD   SP,(xxxx)
    [0x7B] = function(memory, iaddr)
            -- safe to pre-read because a lump write in this returns immediately invalidates lump
            local byte1 = memory[iaddr]; iaddr = inc_address(iaddr); local byte2 = memory[iaddr]; iaddr = inc_address(iaddr)
            local target = byte1 + 256*byte2
            return string.format(
            "CPU.SP = memory[0x%x] + 256*memory[0x%x]", target, (target+1)%65536 ), iaddr
        end,
        
    -- Debug instruction, writes to host stdout .. ED ED
    [0xED] = "io.write(string.char(CPU.A))",    -- fake function for debugging

}

IN_reg_list = { "CPU.B", "CPU.C", "CPU.D", "CPU.E", "CPU.H", "CPU.L", "temp", "CPU.A" }

--  "The IN (C), F instructions is only usefull if you test bits
-- which have the same number as a flag in the F-register because
-- some older Z80 lock up otherwise." -- http://www.z80.info/z80undoc.htm
for i = 0, 7 do
    local reg = IN_reg_list[i+1]
    -- ED 40 = IN B, (C), ED 48 = IN C, (C) ... etc.
    decode_ED_instructions[0x40+i*8] = function(memory, iaddr)
            return port_input_string("CPU.B", "CPU.C", reg, true), iaddr
        end
end

OUT_reg_list = { "CPU.B", "CPU.C", "CPU.D", "CPU.E", "CPU.H", "CPU.L", "CPU.F", "CPU.A" }

--  "I also have doubts about the usefullness and correctness of
-- the OUT (C),F instruction." -- http://www.z80.info/z80undoc.htm
--
-- I'm sure the OUT (C), F is broken ... it's unclear what it should do...
-- I wonder if it should be left as undefined?
for i = 0, 7 do
    local reg = OUT_reg_list[i+1]
    
    if reg ~= "CPU.F" then  -- leave OUT (C), F undefined.
        
      -- ED 41 = OUT (C), B ED 49 = OUT (C), C ... etc.
      decode_ED_instructions[0x41+i*8] = function(memory, iaddr)
              return port_output_string("CPU.B", "CPU.C", reg), iaddr
          end
          
    end
end


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



local function write_to_address_command_string(source, dest_address, next_pc, optional_flag_check_code)
    -- e.g. dest_address = CPU.H*256+CPU.L
    -- e.g. source = CPU.A
    optional_flag_check_code = optional_flag_check_code or ""
    return string.format(
    [[ addr=%s %sif jit.write_allowed[addr] then memory[addr]=%s  
    if jit:code_write_check(addr) then CPU.PC = 0x%x return 'invalidate' end end]], 
    dest_address, optional_flag_check_code, source, next_pc)
end


local decode_instruction

local decode_first_byte = {
    -- 00 = NOP
    [0] = "",        -- this is the best instruction :-)

    -- 08 = EX AF, AF'
    -- ensure the flags are updated first!
    [0x08] = [[ CPU:get_F()
        result=CPU.A CPU.A=CPU.A_ CPU.A_=result
        result=CPU._F CPU._F=CPU.F_ CPU.F_=result 
        CPU.Carry = CPU._F % 2 ]],
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
            "if CPU.Carry == 0 then if jit.jump_count==0 then CPU.PC = 0x%x; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end end", target, target), iaddr, target
        end,
    -- 38 = JR C, xx
    [0x38] = function(memory, iaddr) 
            -- safe to pre-read because a lump write in this returns immediately invalidates lump
            local byte1 = memory[iaddr]; iaddr = inc_address(iaddr)
            if byte1 > 127 then byte1 = byte1-256 end
            local target = iaddr+byte1
            return string.format(
            "if CPU.Carry == 1 then if jit.jump_count==0 then CPU.PC = 0x%x; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end end", target, target), iaddr, target
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
            return write_to_address_command_string("CPU.A", "CPU.B*256+CPU.C", iaddr), iaddr
        end,
    -- 12 = LD (DE), A
    [0x12] = function(memory, iaddr)
            return write_to_address_command_string("CPU.A", "CPU.D*256+CPU.E", iaddr), iaddr
        end,
    -- 22 = LD (xxxx), HL
    [0x22] = function(memory, iaddr)
            local addr = memory[iaddr]; iaddr = inc_address(iaddr);
            addr = addr+256*memory[iaddr]; iaddr = inc_address(iaddr); 
            return write_2bytes_to_address_command_string("CPU.L", "CPU.H", string.format("0x%x", addr), string.format("0x%x", (addr+1)%65536), iaddr), iaddr
        end,
    -- 32 = LD (xxxx), A
    [0x32] = function(memory, iaddr)
            local addr = memory[iaddr]; iaddr = inc_address(iaddr);
            addr = addr+256*memory[iaddr]; iaddr = inc_address(iaddr); 
            return write_to_address_command_string("CPU.A", string.format("0x%x", addr), iaddr), iaddr
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
    
    -- 07 = RLCA ... bit 7 to carry and bit 0
    [0x07] = "CPU:get_F_only_SZV() CPU.A = CPU.A * 2 if CPU.A > 255 then CPU.A = CPU.A - 255 CPU._F = CPU._F + 1 CPU.Carry = 1 else CPU.Carry = 0 end",
    -- 17 = RLA ... carry to bit 0, bit 7 to carry
    [0x17] = "CPU:get_F_only_SZV() CPU.A = CPU.A * 2 + CPU.Carry if CPU.A > 255 then CPU.A = CPU.A - 256 CPU._F = CPU._F + 1 CPU.Carry = 1 else CPU.Carry = 0 end",
    -- 27 = DAA
    [0x27] = "CPU:DAA(zflags)",
    
    -- 37 = SCF
    [0x37] = [[ CPU._F = bit32.band(CPU:get_F(), 0xFF-(Z80_N_FLAG + Z80_H_FLAG + Z80_C_FLAG)) + Z80_C_FLAG CPU.Carry = 1 ]],

    -- these four are covered below
    -- 09 = ADD HL, BC
    -- 19 = ADD HL, DE
    -- 29 = ADD HL, HL
    -- 39 = ADD HL, SP

    
    -- 0A = LD A,(BC)
    [0x0A] = "CPU.A = memory[CPU.B*256+CPU.C]",
    -- 1A = LD A,(DE)
    [0x1A] = "CPU.A = memory[CPU.D*256+CPU.E]",
    -- 2A = LD HL,(xxxx)
    [0x2A] = function(memory, iaddr) 
            -- safe to pre-read because a lump write in this returns immediately invalidates lump
            local byte1 = memory[iaddr]; iaddr = inc_address(iaddr); local byte2 = memory[iaddr]; iaddr = inc_address(iaddr)
            local target = byte1 + 256*byte2
            return string.format(
            "CPU.L = memory[0x%x] CPU.H = memory[0x%x]", target, (target+1)%65536 ), iaddr
        end,
    -- 3A = LD A,(xxxx)
    [0x3A] = function(memory, iaddr) 
            -- safe to pre-read because a lump write in this returns immediately invalidates lump
            local byte1 = memory[iaddr]; iaddr = inc_address(iaddr); local byte2 = memory[iaddr]; iaddr = inc_address(iaddr)
            local target = byte1 + 256*byte2
            return string.format(
            "CPU.A = memory[0x%x]", target), iaddr
        end,
    
    -- 0B = DEC BC
    [0x0B] = "CPU.C = CPU.C - 1 if CPU.C == -1 then CPU.C = 255 CPU.B = (CPU.B - 1)%256 end",
    -- 1B = DEC DE
    [0x1B] = "CPU.E = CPU.E - 1 if CPU.E == -1 then CPU.E = 255 CPU.D = (CPU.D - 1)%256 end",
    -- 2B = DEC HL
    [0x2B] = "CPU.L = CPU.L - 1 if CPU.L == -1 then CPU.L = 255 CPU.H = (CPU.H - 1)%256 end",
    -- 3B = DEC SP
    [0x3B] = "CPU.SP = (CPU.SP - 1)%65536",
    
    -- nC = INC r, covered below
    -- nD = DEC r, covered below
    -- nE = LD r, xx, covered below
    
    -- 0F = RRCA ... bit 0 to carry and bit 7
    [0x0F] = "CPU:get_F_only_SZV() if (CPU.A % 2) == 1 then CPU.A = CPU.A + 256 CPU._F = CPU._F + 1 CPU.Carry = 1 else CPU.Carry = 0 end CPU.A = bit32.rshift(CPU.A, 1)",
    -- 1F = RRA ... carry to bit 7, bit 0 to carry
    [0x1F] = "CPU:get_F_only_SZV() result = bit32.rshift(CPU.A, 1) + (128*CPU.Carry) if (CPU.A % 2) == 1 then CPU._F = CPU._F + 1 CPU.Carry = 1 else CPU.Carry = 0 end CPU.A = result",
    -- 2F = CPL
    [0x2F] = "CPU._F = bit32.bor(CPU:get_F(), Z80_N_FLAG + Z80_H_FLAG) CPU.A = 255 - CPU.A",
    
    -- 3F = CCF
    [0x3F] = [[   CPU._F = bit32.band(CPU:get_F(), 0xFF-(Z80_N_FLAG + Z80_H_FLAG + Z80_C_FLAG))
     if CPU.Carry==1 then CPU._F = CPU._F + Z80_H_FLAG CPU.Carry = 0 else CPU._F = CPU._F + Z80_C_FLAG CPU.Carry = 1 end ]],

    -- 40 to 7F = mostly various LD, covered below
    -- 80 to BF = ADD/ADC/SUB/SBC/AND/XOR/OR/CP, covered below

    -- C0/C8/D0/D8/E0/E8/F0/F8 = return instructions with different flags, covered below
    -- C1/D1/E1/F1 = pop register pairs (BC/DE/HL/AF)
        [0xC1] = "CPU.C = memory[CPU.SP] CPU.B = memory[(CPU.SP+1)%65536] CPU.SP = (CPU.SP+2)%65536",
        [0xD1] = "CPU.E = memory[CPU.SP] CPU.D = memory[(CPU.SP+1)%65536] CPU.SP = (CPU.SP+2)%65536",
        [0xE1] = "CPU.L = memory[CPU.SP] CPU.H = memory[(CPU.SP+1)%65536] CPU.SP = (CPU.SP+2)%65536",
        [0xF1] = "CPU._F = memory[CPU.SP] CPU.A = memory[(CPU.SP+1)%65536] CPU.SP = (CPU.SP+2)%65536 CPU.Carry = CPU._F % 2",

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
    [0xD3] = function(memory, iaddr)
            local addr = memory[iaddr]; iaddr = inc_address(iaddr);
            return port_output_string("CPU.A", addr, "CPU.A"), iaddr
        end,
    
    -- E3 = EX (SP), HL
    [0xE3] = function(memory, iaddr)
            -- seems like candidate for faster implementation! (2 extra copies, extra calculation of SP+1)
            return write_2bytes_to_address_command_string("temp", "result", "CPU.SP", "(CPU.SP+1)%65536", iaddr, 
                "result = CPU.H temp = CPU.L CPU.L = memory[CPU.SP] CPU.H = memory[(CPU.SP+1)%65536]"), iaddr
        end,

    -- F3 = DI
    [0xf3] = "CPU.IFF1 = false;CPU.IFF2 = false",

    -- C4/CC/D4/DC/E4/EC/F4/FC = CALL instructions with different flags, covered below
    
    -- C5/D5/E5/F5 - push register pairs (BC=00/DE=01/HL=10/AF=11)
    [0xC5] = function(memory, iaddr)    -- push BC
            return write_2bytes_to_address_command_string("CPU.C", "CPU.B", "CPU.SP", "(CPU.SP+1)%65536", iaddr, "CPU.SP=(CPU.SP-2)%65536"), iaddr
        end,
    [0xD5] = function(memory, iaddr)    -- push DE
            return write_2bytes_to_address_command_string("CPU.E", "CPU.D", "CPU.SP", "(CPU.SP+1)%65536", iaddr, "CPU.SP=(CPU.SP-2)%65536"), iaddr
        end,
    [0xE5] = function(memory, iaddr)    -- push HL
            return write_2bytes_to_address_command_string("CPU.L", "CPU.H", "CPU.SP", "(CPU.SP+1)%65536", iaddr, "CPU.SP=(CPU.SP-2)%65536"), iaddr
        end,
    [0xF5] = function(memory, iaddr)    -- push AF
            return write_2bytes_to_address_command_string("CPU._F", "CPU.A", "CPU.SP", "(CPU.SP+1)%65536", iaddr, "CPU.SP=(CPU.SP-2)%65536 CPU:get_F()"), iaddr
        end,

    -- C6/CE/D6/DE/E6/EE/F6/FE = ADD/ADC/SUB/SBC/AND/XOR/OR/CP A with immediate value, covered below.

    -- C7/CF/D7/DF/E7/EF/F7/FF = RST instructions
    [0xC7] = function(memory, iaddr)     -- RST00
            local target = 0x00
            return call_code_string(iaddr, target), iaddr, target
        end,
    [0xCF] = function(memory, iaddr)     -- RST08
            local target = 0x08
            return call_code_string(iaddr, target), iaddr, target
        end,
    [0xD7] = function(memory, iaddr)     -- RST10
            local target = 0x10
            return call_code_string(iaddr, target), iaddr, target
        end,
    [0xDF] = function(memory, iaddr)     -- RST18
            local target = 0x18
            return call_code_string(iaddr, target), iaddr, target
        end,
    [0xE7] = function(memory, iaddr)     -- RST20
            local target = 0x20
            return call_code_string(iaddr, target), iaddr, target
        end,
    [0xEF] = function(memory, iaddr)     -- RST28
            local target = 0x28
            return call_code_string(iaddr, target), iaddr, target
        end,
    [0xF7] = function(memory, iaddr)     -- RST30
            local target = 0x30
            return call_code_string(iaddr, target), iaddr, target
        end,
    [0xFF] = function(memory, iaddr)     -- RST38
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
    [0xE9] = "do CPU.PC = CPU.H*256+CPU.L; return 'ok' end",
    -- surely there must be a way of jumping internally without dropping out of 
    -- this lump?
    --[0xE9] = function(memory, iaddr)
    --        target = ???
    --        return string.format(
    --        "if jit.jump_count==0 then CPU.PC = CPU.HL; return 'ok' else jit.jump_count = jit.jump_count-1;goto l_%04x end", target), iaddr, ???
    --    end
    
    -- F9 = LD SP, HL
    [0xF9] = "CPU.SP = CPU.H*256+CPU.L",

    -- CB = extended instructions
    [0xCB] = function(memory, iaddr)
                return decode_instruction(memory, iaddr, decode_CB_instructions)
        end,
    -- DB = IN A, (xx)
    [0xDB] = function(memory, iaddr)
            local addr = memory[iaddr]; iaddr = inc_address(iaddr);
            return port_input_string("CPU.A", addr, "CPU.A"), iaddr
        end,

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


--
-- populate the load instructions
--
for i = 0x40, 0x7f do
    local from_reg = reg_index[bit32.band(i,7)]            -- not speed critical
    local to_reg = reg_index[bit32.band(bit32.rshift(i,3),7)]    -- not speed critical
    if _is_single_reg(to_reg) then
        -- no memory write check for single reg write
        decode_first_byte[i] = string.format("%s=%s", to_reg, from_reg)
    else
        -- LD (HL), r
        decode_first_byte[i] = function(memory, iaddr)
                return string.format([[addr = CPU.H*256+CPU.L;if jit.write_allowed[addr] then memory[addr]=%s 
                if jit:code_write_check(addr) then CPU.PC = 0x%x; return 'invalidate' end end]], from_reg, iaddr), iaddr            
            end
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

-- 09 = ADD HL, BC
-- 19 = ADD HL, DE
-- 29 = ADD HL, HL
-- 39 = ADD HL, SP
-- ADD HL, ss ... doesn't affect Z or S, just H F3 F5 V N C

local function ADD_to_HL_string(hi, lo)
    -- basically a simple add followed by an ADC H,x.
    -- maybe we should try a 16 bit add? (but then we have to split anyway...)
    return string.format(
[[  
    result=CPU.L+%s
    if result > 255 then 
        temp=1 result=result-256
    else
        temp=0
    end
    CPU.L=result
    
    result = CPU.H+%s+temp
    temp = CPU:get_F_only_SZV()
    
    if result > 255 then
        CPU.Carry=1 result=result-256 temp = temp + 1
    else
        CPU.Carry=0
    end
    temp = temp + bit32.band(bit32.bxor(CPU.H, %s, result),Z80_H_FLAG)
    CPU._F = temp
    CPU.H = result ]], lo, hi, hi)
end
decode_first_byte[0x09] = ADD_to_HL_string("CPU.B", "CPU.C")
decode_first_byte[0x19] = ADD_to_HL_string("CPU.D", "CPU.E")
decode_first_byte[0x29] = ADD_to_HL_string("CPU.H", "CPU.L")
decode_first_byte[0x39] = ADD_to_HL_string("bit32.rshift(CPU.SP, 8)", "(CPU.SP%256)") -- math.floor(b / 256)


-- ED 4A = ADC HL, BC
-- ED 5A = ADC HL, DE
-- ED 6A = ADC HL, HL
-- ED 7A = ADC HL, SP
-- ADC HL, ss ... unlike ADD HL, ss - does affect Z and S, as well as H F3 F5 V N C

local function ADC_to_HL_string(hi, lo)
    -- basically a simple ADC followed by an ADC H,x.
    -- maybe we should try a 16 bit add? (but then we have to split anyway...)
    return string.format(
[[  
    result=CPU.L+%s+CPU.Carry
    if result > 255 then 
        temp=1 result=result-256
    else
        temp=0
    end
    CPU.L=result
    
    result = CPU.H+%s+temp
    
    if result > 255 then
        CPU.Carry=1 result=result-256
    else
        CPU.Carry=0
    end
    temp = CPU.simple_flags[result] + CPU.calc_add_overflow(CPU.H, %s, result) + CPU.Carry
    if result == 0 and CPU.L ~= 0 then
        temp = temp - Z80_Z_FLAG
    end
    temp = temp + bit32.band(bit32.bxor(CPU.H, %s, result),Z80_H_FLAG)
    CPU._F = temp
    CPU.H = result ]], lo, hi, hi, hi)
end
decode_ED_instructions[0x4A] = ADC_to_HL_string("CPU.B", "CPU.C")
decode_ED_instructions[0x5A] = ADC_to_HL_string("CPU.D", "CPU.E")
decode_ED_instructions[0x6A] = ADC_to_HL_string("CPU.H", "CPU.L")
decode_ED_instructions[0x7A] = ADC_to_HL_string("bit32.rshift(CPU.SP, 8)", "(CPU.SP%256)") -- math.floor(b / 256)


-- ED 42 = SBC HL, BC
-- ED 52 = SBC HL, DE
-- ED 62 = SBC HL, HL
-- ED 72 = SBC HL, SP
-- SBC HL, ss ... does affect Z and S, as well as H F3 F5 V N C

local function SBC_to_HL_string(hi, lo)
    -- basically a simple SBC followed by an SBC H,x.
    -- maybe we should try a 16 bit add? (but then we have to split anyway...)
    return string.format(
[[  
    result=CPU.L-%s-CPU.Carry
    if result < 0 then 
        temp=1 result=result+256
    else
        temp=0
    end
    CPU.L=result
    
    result = CPU.H-%s-temp
    
    if result < 0 then
        CPU.Carry=1 result=result+256
    else
        CPU.Carry=0
    end
    temp = CPU.simple_flags[result] + CPU.calc_sub_overflow(CPU.H, %s, result) + CPU.Carry
    if result == 0 and CPU.L ~= 0 then
        temp = temp - Z80_Z_FLAG
    end
    temp = temp + bit32.band(bit32.bxor(CPU.H, %s, result),Z80_H_FLAG) + Z80_N_FLAG
    CPU._F = temp
    CPU.H = result ]], lo, hi, hi, hi)
end
decode_ED_instructions[0x42] = SBC_to_HL_string("CPU.B", "CPU.C")
decode_ED_instructions[0x52] = SBC_to_HL_string("CPU.D", "CPU.E")
decode_ED_instructions[0x62] = SBC_to_HL_string("CPU.H", "CPU.L")
decode_ED_instructions[0x72] = SBC_to_HL_string("bit32.rshift(CPU.SP, 8)", "(CPU.SP%256)") -- math.floor(b / 256)


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
    return "result=".."CPU.A-" .. what .. [[-CPU.Carry
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
    ]]    -- no CPU-A set at the end, otherwise same as SUBend
end

local inc_flag_calc = [[ CPU._F = CPU.simple_flags[%s]+CPU.Carry
        if %s %% 0x10 == 0 then CPU._F = CPU._F + Z80_H_FLAG end
        if %s == 0x80 then CPU._F = CPU._F + Z80_V_FLAG end ]]
local dec_flag_calc = [[ CPU._F = CPU.simple_flags[%s] + CPU.Carry + Z80_N_FLAG
        if %s %% 0x10 == 0x0F then CPU._F = CPU._F + Z80_H_FLAG end
        if %s == 0x7F then CPU._F = CPU._F + Z80_V_FLAG end ]]

-- inc 'r', op-code=4*(8*i)        e.g    3C = INC A    [0x3C] = "CPU.A = (CPU.A + 1)%256",    -- inc A        
-- dec 'r'    -- 3E = LD A,XX [0x3E] = function(memory, iaddr) return "CPU.A="..tostring(memory[iaddr]), inc_address(iaddr) end ,
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
            function(memory, iaddr)
                return write_to_address_command_string("result", "CPU.H*256+CPU.L", 
                iaddr,
                "result = (memory[addr]+1)%256 " .. 
                string.format(inc_flag_calc, "result", "result", "result")), iaddr
            end
    end
    if _is_single_reg(reg) then
        -- dec reg
        decode_first_byte[5+(8*i)] = reg.."=("..reg.."-1)%256" ..
            string.format(dec_flag_calc, reg, reg, reg)
    else
        -- dec(HL)
        decode_first_byte[5+(8*i)] = 
            function(memory, iaddr)
                return write_to_address_command_string("result", "CPU.H*256+CPU.L",
                iaddr, 
                "result = (memory[addr]-1)%256" ..
                string.format(dec_flag_calc, "result", "result", "result")), iaddr
            end
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
            return write_to_address_command_string(byte1, "CPU.H*256+CPU.L", iaddr), iaddr
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



decode_DD_instructions[0x84] = ADD_to_A_string("bit32.band(CPU.IX, 0xFF00)/256")
decode_DD_instructions[0x85] = ADD_to_A_string("CPU.IX%256")
decode_FD_instructions[0x84] = ADD_to_A_string("bit32.band(CPU.IY, 0xFF00)/256")
decode_FD_instructions[0x85] = ADD_to_A_string("CPU.IY%256")

decode_DD_instructions[0x8C] = ADC_to_A_string("bit32.band(CPU.IX, 0xFF00)/256")
decode_DD_instructions[0x8D] = ADC_to_A_string("CPU.IX%256")
decode_FD_instructions[0x8C] = ADC_to_A_string("bit32.band(CPU.IY, 0xFF00)/256")
decode_FD_instructions[0x8D] = ADC_to_A_string("CPU.IY%256")

decode_DD_instructions[0x94] = SUB_to_A_string("bit32.band(CPU.IX, 0xFF00)/256")
decode_DD_instructions[0x95] = SUB_to_A_string("CPU.IX%256")
decode_FD_instructions[0x94] = SUB_to_A_string("bit32.band(CPU.IY, 0xFF00)/256")
decode_FD_instructions[0x95] = SUB_to_A_string("CPU.IY%256")

decode_DD_instructions[0x9C] = SBC_to_A_string("bit32.band(CPU.IX, 0xFF00)/256")
decode_DD_instructions[0x9D] = SBC_to_A_string("CPU.IX%256")
decode_FD_instructions[0x9C] = SBC_to_A_string("bit32.band(CPU.IY, 0xFF00)/256")
decode_FD_instructions[0x9D] = SBC_to_A_string("CPU.IY%256")

-- IXH
decode_DD_instructions[0xA4] = AND_to_A_string("bit32.band(CPU.IX, 0xFF00)/256")
decode_DD_instructions[0xAC] = XOR_to_A_string("bit32.band(CPU.IX, 0xFF00)/256")
decode_DD_instructions[0xB4] = OR_to_A_string("bit32.band(CPU.IX, 0xFF00)/256")
decode_DD_instructions[0xBC] = CP_to_A_string("bit32.band(CPU.IX, 0xFF00)/256")

-- IXL
decode_DD_instructions[0xA5] = AND_to_A_string("CPU.IX%256")
decode_DD_instructions[0xAD] = XOR_to_A_string("CPU.IX%256")
decode_DD_instructions[0xB5] = OR_to_A_string("CPU.IX%256")
decode_DD_instructions[0xBD] = CP_to_A_string("CPU.IX%256")

-- IYH
decode_FD_instructions[0xA4] = AND_to_A_string("bit32.band(CPU.IY, 0xFF00)/256")
decode_FD_instructions[0xAC] = XOR_to_A_string("bit32.band(CPU.IY, 0xFF00)/256")
decode_FD_instructions[0xB4] = OR_to_A_string("bit32.band(CPU.IY, 0xFF00)/256")
decode_FD_instructions[0xBC] = CP_to_A_string("bit32.band(CPU.IY, 0xFF00)/256")

-- IYL
decode_FD_instructions[0xA5] = AND_to_A_string("CPU.IY%256")
decode_FD_instructions[0xAD] = XOR_to_A_string("CPU.IY%256")
decode_FD_instructions[0xB5] = OR_to_A_string("CPU.IY%256")
decode_FD_instructions[0xBD] = CP_to_A_string("CPU.IY%256")


-- flags
local flag_index = {
    [0]="not bit32.btest(CPU:get_F(), Z80_Z_FLAG)",    -- NZ = Not Zero (Z_FLAG)
    "bit32.btest(CPU:get_F(), Z80_Z_FLAG)",        -- Z = Zero (Z_FLAG)
    "CPU.Carry==0",                            -- NC = Carry (C_FLAG)
    "CPU.Carry==1",                                -- C = Carry (C_FLAG)

    "not bit32.btest(CPU:get_F(), Z80_P_FLAG)",    -- PO = Parity Odd (P/V_FLAG)
    "bit32.btest(CPU:get_F(), Z80_P_FLAG)",        -- PE = Parity Even (P/V_FLAG)
    "not bit32.btest(CPU:get_F(), Z80_S_FLAG)",    -- P = P sign positive (S_FLAG)
    "bit32.btest(CPU:get_F(), Z80_S_FLAG)",        -- M = M sign negative (S_FLAG)
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
        data = 0        -- nop instruction
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

--  We use these to calculate the timings of each instruction
local instruction_timings = {}

-- R register ... 
-- http://rk.nvg.ntnu.no/sinclair/faq/tech_z80.html#RREG
-- http://z80.info/z80info.htm
local M1_cycles = {}


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
    local codetext_table = {"local CPU, jit = ... ","; local addr, addr2, result, temp; local memory = jit._memory; local zflags = jit._zflags"} -- define the CPU parameter
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
    
    table.insert(codetext_table, string.format("::finished:: CPU.PC = 0x%x; return 'ok'", next_address))
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
        print(source)
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
        self._zflags[A] = bit32.band(A,0xa8)    -- create the sign, bit5, bit3
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
    -- insert zero flag
    self._zflags[0] = self._zflags[0] + Z80_Z_FLAG
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
    
    self.codemap = {}            -- table where index is address and contents are nil or table of 'lumps'.
    --self.lumps_count = {}        -- originally table of address against number of lumps
    self.invalidated = {}        -- list of lumps invalidated
    
end


--
-- Load a section of memory
--
function Z80JIT:load_memory(data, address, is_writable)
    local data_type = type(data)
    if data_type == "string" then
        local data_as_string    = data
        
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

    elseif data_type == "table" then
        for address, value in pairs(data) do
            if type(address) == "number" and type(value) == "number" and 
                address >= 0 and address <= 65535 then
                
                value = math.floor(value)
                value = math.max(math.min(value, 255), 0)
                
                self:code_invalidate_block(address, 1)
                self:select_writable(address, 1, is_writable)
                
                self._memory[address] = value
            end
            self:remove_invalidated_lumps()
        end
        
    end
end

function Z80JIT:select_writable(address, length, is_writable)
    if type(is_writable) == "boolean" then
        for ptr = address,address+length-1 do
            self.write_allowed[ptr] = is_writable
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

function Z80JIT:fetch_memory_table(address, length)
    address = address or 0
    length = length or self._total_memory_length
    
    local output = {}
    for a = address, address+length-1 do
        local ci = self._memory[a]
        if not ci then
            ci = 0
        end
        output[a] = string.char(ci)
    end
    return output
end

function Z80JIT:fetch_memory_string(address, length)
    local output = self:fetch_memory_table(address, length)
    return table.concat(output, nil, address, address+length-1)
end

function Z80JIT:fetch_memory_string_table(address, length)
    local output = self:fetch_memory_table(address, length)
    return table.concat(output, nil, address, address+length-1), output
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
    local status
    local success = true
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
            -- We use pcall at the moment, but alternatively we can call directly 
            -- by switching the comments of these two lines.
            status = self.current_lump.code(our_z80_cpu, self)
            --success, status = pcall(self.current_lump.code, our_z80_cpu, self)
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
        
        self.current_lump = nil            -- we shouldn't have a current lump any more
        self:remove_invalidated_lumps()        -- remove any lumps that got invalidated

        -- not return what to do with undefined and other errors... 
        -- leave to the caller
        -- invalidate we've taken care of already, so just loop and do more code :-)
    until status ~= "ok" and status ~= "invalidate"
    -- other returns are 'undefined', 'halt', 'Execute fail' and "Compile fail"
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
    else    -- no current lump cases
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
    self.codemap = {}            -- table where index is address and contents are nil or table of 'lumps'.
    self.invalidated = {}        -- list of lumps invalidated
end


function Z80JIT:__serialize()
    return self
end


-- Timings from 2.66GHz Intel Core i7 dual core.
-- 3.6s
--function test1()
--    local k = 0
--    for i = 0, 100000000 do
--        k = k + i%256
--    end
--end
--
-- 13.9s
--function test2()
--    local k = 0
--    for i = 0, 100000000 do
--        k = k + bit32.band(i, 255)
--    end
--end
-- 
-- 4.1s
--function test3()
--    local k = 0
--    for i = 0, 100000000 do
--        k = k + i
--        -- we could use this for things like inc or add
--        if k > 3435 then
--            k = 0
--        end
--    end
--end
--
--



return true
