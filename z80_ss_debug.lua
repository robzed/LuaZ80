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
local Home = "\x1b[H"    -- home cursor vt100


local Hi = "\x1b[1m"        -- also 'bright'
local NormAttr = "\x1b[m"

-- 
-- Sets multiple display attribute settings. The following lists standard attributes:
-- From http://www.termsys.demon.co.uk/vtansi.htm#colors
local ANSI_attrib = {
    Reset_all = 0,
    Bright = 1,
    Dim = 2,
    Underscore = 4,
    Blink = 5,
    Reverse = 7,
    Hidden = 8,

    -- Foreground Colours
    FG_Black = 30,
    FG_Red = 31,
    FG_Green = 32,
    FG_Yellow = 33,
    FG_Blue = 34,
    FG_Magenta = 35,
    FG_Cyan = 36,
    FG_White = 37,

    -- Background Colours
    BG_Black = 40,
    BG_Red = 41,
    BG_Green = 42,
    BG_Yellow = 43,
    BG_Blue = 44,
    BG_Magenta = 45,
    BG_Cyan = 46,
    BG_White = 47,
    }

local write_allowed_colour = ANSI_attrib.FG_White
local write_protected_colour = ANSI_attrib.FG_Magenta
local background_colour = ANSI_attrib.BG_Black
local changed_colour = ANSI_attrib.Underscore


function string_ANSI_colour(...)
    return '\x1b[' .. table.concat({...}, ';') .. 'm'
end
---
-- io_write_attr()
-- Set Attribute Mode    <ESC>[{attr1};...;{attrn}m
function set_ANSI_colour(...)
    local s = '\x1b[' .. table.concat({...}, ';') .. 'm'
    io.write(s)
end


local debug_precode = " jit.debug_obj:_debug_step(0x%x) "

function Z80_SS_DEBUG:initialize(cpu, jit)
    self.cpu = cpu
    self.jit = jit
    self.last_status = "ok"
    
    self.record_pc = 0
    self.record_sp = 0
    self.record_iy = 0
    self.record_ix = 0
    self.record_hl = 0
    self.record_de = 0
    self.record_bc = 0
    self.record_af = 0    -- special, contains two F registers
    
    self._has_changed = {}
    self._changed_addr_store = {}
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

function Z80_SS_DEBUG:_colour_for_memory_status(address, mark_change)
    local addr = (address)%65536
    local write_colour
    if self.jit.write_allowed[addr] then
        write_colour = write_allowed_colour
    else
        write_colour = write_protected_colour
    end
    local colour_str
    if mark_change and self._has_changed[addr] then
        colour_str = string_ANSI_colour(background_colour, write_colour, changed_colour)
    else
        colour_str = string_ANSI_colour(ANSI_attrib.Reset_all, background_colour, write_colour)
    end
    return colour_str
end


function Z80_SS_DEBUG:get_bytes(addr, len, mark_change)
    return string.format("%s%s %s%s %s%s %s%s %s%s %s%s %s%s %s%s%s",
        self:_colour_for_memory_status(addr, mark_change), self:get_smem(addr),
        self:_colour_for_memory_status(addr+1, mark_change), self:get_smem(addr+1),
        self:_colour_for_memory_status(addr+2, mark_change), self:get_smem(addr+2),
        self:_colour_for_memory_status(addr+3, mark_change), self:get_smem(addr+3),
        self:_colour_for_memory_status(addr+4, mark_change), self:get_smem(addr+4),
        self:_colour_for_memory_status(addr+5, mark_change), self:get_smem(addr+5),
        self:_colour_for_memory_status(addr+6, mark_change), self:get_smem(addr+6),
        self:_colour_for_memory_status(addr+7, mark_change), self:get_smem(addr+7),
        string_ANSI_colour(ANSI_attrib.Reset_all)
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
        self:disp16_reg(reg, true)
    else
        print(string.format("%s*%04X %s", reg, self.PC, self:get_bytes(self.PC, 8, true)))
    end
    return self.PC
end

function Z80_SS_DEBUG:_disp_reg_common(reg, old_value, new_value)
    local colour = ""
    if old_value ~= new_value then
        colour = string_ANSI_colour(changed_colour)
    end

    print(string.format("%s %s%04X%s %s", reg, colour, new_value, NormAttr, self:get_bytes(new_value, 8, true)))
end

function Z80_SS_DEBUG:disp16_reg(reg, old_value)
    local new_value = self.cpu[reg]
    self:_disp_reg_common(reg, old_value, new_value)
    return new_value
end

function Z80_SS_DEBUG:disp88_reg(reg, old_value)
    local new_value = self.cpu[reg:sub(1,1)]*256+self.cpu[reg:sub(2,2)]
    self:_disp_reg_common(reg, old_value, new_value)
    return new_value
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

function Z80_SS_DEBUG:dispAF_reg(old_value)
    local raw_F = self.cpu._F
    local colour_A = ""
    if math.floor(old_value/65536) ~= self.cpu.A then
        colour_A = string_ANSI_colour(changed_colour)
    end
    local colour_raw_F = ""
    if (old_value % 256) ~= raw_F then
        colour_raw_F = string_ANSI_colour(changed_colour)
    end
    
    if raw_F then
        print(string.format("AF %s%02X%s%s%02X%s %s (raw _F) CPU.Carry = %x", colour_A, self.cpu.A, NormAttr, colour_raw_F, raw_F, NormAttr, self:flag_decode(raw_F), self.cpu.Carry))
    else
        print(string.format("AF %s%02X%s?? %s (raw _F) CPU.Carry = %x", colour_A, self.cpu.A, NormAttr, self:flag_decode(0), self.cpu.Carry))
    end
    local get_F = self.cpu:get_F()
    local colour_get_F = ""
    if (math.floor(old_value/256) % 256) ~= get_F then
        colour_get_F = string_ANSI_colour(changed_colour)
    end
    
    print(string.format("AF %s%02X%s%s%02X%s %s (get_F)", colour_A, self.cpu.A, NormAttr, colour_get_F, get_F, NormAttr, self:flag_decode(get_F)))
    
    local new_value = (self.cpu.A*65536) + (get_F*256) + (raw_F or 0)
    return new_value
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
    self.record_pc = self:disp_PC() -- PC is special
    self.record_sp = self:disp16_reg("SP", self.record_sp)
    self.record_iy = self:disp16_reg("IY", self.record_sp)
    self.record_ix = self:disp16_reg("IX", self.record_ix)
    self.record_hl = self:disp88_reg("HL", self.record_hl)
    self.record_de = self:disp88_reg("DE", self.record_de)
    self.record_bc = self:disp88_reg("BC", self.record_bc)
    self.record_af = self:dispAF_reg(self.record_af)
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

function Z80_SS_DEBUG:_get_mem(address, length)
    local str = ""
    for i = 0, length-1 do
        local addr = (address+i)%65536
        local val = self.jit._memory[addr] or 0
        local colour = self:_colour_for_memory_status(addr)
        str = str .. string.format("%s%02X ", colour, val)
    end
    str = str .. string_ANSI_colour(ANSI_attrib.Reset_all)
    return str
end

function Z80_SS_DEBUG:_get_ascii(address, length)
    local str = ""
    for i = 0, length-1 do
        local addr = (address+i)%65536
        local val = self.jit._memory[addr] or 0
        local colour = self:_colour_for_memory_status(addr)
        if val >= 32 and val <= 127 then
            str = str .. string.format("%s%s", colour, string.char(val))
        else
            str = str .. string.format("%s.", colour)
        end
    end
    str = str .. string_ANSI_colour(ANSI_attrib.Reset_all)
    return str
end

function Z80_SS_DEBUG:do_command()
    io.write(CLL) local line = io.read("*line")
    if line == '?' or line == 'help' then
        print("s or step => step")
        print("q or quit or exit => quit")
        print("list n => show Lua source around line")
        print("mem address => show memory at address")
        print("dis address => disassemble Z80 address")
        print("cls => clear the screen")
        --print("dis PC => disassemble Z80 from PC")
        --print("lua text => execute text as lua")
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
            print("No line supplied   ")
        end
    elseif line:sub(1,3) == 'mem' or line:sub(1,3) == 'dis' then
        local address = tonumber(line:sub(4))
        if address then            
            io.write(CLS)
            if line:sub(1,3) == 'mem' then
                for addr = address, address+256, 16 do
                    print(string.format("0x%04x: %s  %s", addr, self:_get_mem(addr, 16), self:_get_ascii(addr, 16)))
                end
            else
                local instructions = multiple_Z80_disassemble(self.jit._memory, address, 16)
                print(string.format("Start Address = 0x%x", address))
                print()
                for _, instruction in ipairs(instructions) do
                    print(string.format("    %s", instruction))
                end
            end
            print()
            print("Hit enter")
            io.read()
            io.write(CLS)
        else
            print("No address supplied    ")
        end
    --elseif line:sub(1,3) == 'lua' then
    elseif line == 'cls' then
        io.write(CLS)
    elseif line == 'q' or line == 'quit' or line == 'exit' then
        os.exit(0)
    else
        print("Unknown command",line,"Try ? for help")
    end
    return false
end

function Z80_SS_DEBUG:_calculate_has_changed()
    self._has_changed = {}
    for addr, old_value in pairs(self._changed_addr_store) do
        local new_value = self.jit._memory[addr]
        if new_value ~= old_value then
            self._has_changed[addr] = true
        end
    end
end

function Z80_SS_DEBUG:_mark_change_monitor_addresses(start, length)
    start = start % 65536
    for addr = start, start+length do    
        self._changed_addr_store[addr] = self.jit._memory[addr]
    end
end

function Z80_SS_DEBUG:_update_change_monitor_addresses()
    self._changed_addr_store = {}
    self:_mark_change_monitor_addresses(self.record_pc - 2, 22)
    self:_mark_change_monitor_addresses(self.record_sp - 2, 22)
    self:_mark_change_monitor_addresses(self.record_iy - 2, 22)
    self:_mark_change_monitor_addresses(self.record_ix - 2, 22)
    self:_mark_change_monitor_addresses(self.record_hl - 2, 22)
    self:_mark_change_monitor_addresses(self.record_de - 2, 22)
    self:_mark_change_monitor_addresses(self.record_bc - 2, 22)
end

function Z80_SS_DEBUG:_debug_step(PC)
    self.PC = PC
    repeat
        self:_calculate_has_changed()
        self:display()
        self:_update_change_monitor_addresses()
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

There are two possible methods for debugging:

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

