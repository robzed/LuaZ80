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
-- This file is not built for speed, it is intended to be simple & fast to write
-- and run only occasionally.
-- 
-- It checks 'user visible' changes to the CPU, e.g. registers and 
-- There are interactions that it doesn't test. One big example is that
-- the Carry flag is seperate from the F register bottom bit. So there are 
-- interactions between instructions that might have bugs. 

require("lua_z80")
require("z80_ss_debug")
require("Z80_assembler")

local TIMING_ON = false
local IGNORE_BIT3_BIT5_FLAGS = true

local function get_state(jit, cpu)
    
    local str, tab = jit:fetch_memory_string_table(0, 65536)
    return {
        
        -- we only check the 'user-visibie' registers here, not various
        -- internal registers.
        reg = {
            PC = cpu.PC,
            IX = cpu.IX,
            IY = cpu.IY,
            SP = cpu.SP,
            I = cpu.I,
            R = cpu.R,
            
            -- main registers
            A = cpu.A,
            F = cpu:get_F(),    -- always calculate F
            Carry = cpu.Carry,
            H = cpu.H,
            L = cpu.L,
            B = cpu.B,
            C = cpu.C,
            D = cpu.D,
            E = cpu.E,

            -- alternative 'shadow' registers
            -- we don't have access to the CPU flip flop directly
            A_ = cpu.A_,
            F_ = cpu.F_, 
            H_ = cpu.H_, 
            L_ = cpu.L_, 
            B_ = cpu.B_, 
            C_ = cpu.C_, 
            D_ = cpu.D_, 
            E_ = cpu.E_, 
            
            -- interrupt flags
            IFF1 = cpu.IFF1, -- this on effects maskable interrupts
            IFF2 = cpu.IFF2, -- this one is readible (ld a,i/ld a,r -> P/Ov)
            -- interrupt mode flip flops
            -- IMFa=0, IMFb=0 - Interrupt mode 0. This is an 8080-compatible interrupt mode.
            -- IMFa=0, IMFb=1 - Not used.
            -- IMFa=1, IMFb=0 - Interrupt mode 1. In this mode CPU jumps to location 0038h for interrupt processing.
            -- IMFa=1, IMFb=1 - Interrupt mode 2. In this mode CPU jumps to location, which high-order address is taken from I register, and low order address is supplied by peripheral device.
            IMFa = cpu.IMFa,
            IMFb = cpu.IMFb,
        },
        
        mem = tab,
        mem_string = str
        }
end



local function block_of_halts(number)
    local t = {}
    local halt_instruction = 0x76
    for addr = 0, number-1 do
        t[addr] = halt_instruction
    end
    return t
end

local function run_code(initial_memory, code, post_setup)
    -- make the JIT compiler and memory
    local jit = Z80JIT:new()
    local time_start = os.clock()
    jit:make_RAM(0,1)               -- first byte RAM for some tests (e.g. PUSH BC wrap1)
    jit:make_ROM(1,16384)
    jit:make_RAM(16384,16384)
    -- gap of 16384
    jit:make_RAM(49152,16384)       -- use for certain tests (roll-over)
    local time_end = os.clock()
    if TIMING_ON then print("Setting memory type Took", time_end - time_start) end
    local time_start = os.clock()
    jit:load_memory(initial_memory, 0)
    local time_end = os.clock()
    if TIMING_ON then print("Loading HALTs Took", time_end - time_start) end
    local time_start = os.clock()
    jit:load_memory(code, 0)    -- load code at zero address
    local time_end = os.clock()
    if TIMING_ON then print("Loading code Took", time_end - time_start) end
    
    -- now make a CPU to run the code
    local cpu = Z80CPU:new()
    if post_setup then
        post_setup(cpu, jit)
    end
    local time_start = os.clock()
    local old_state = get_state(jit, cpu)
    local time_end = os.clock()
    if TIMING_ON then print("Get old_state Took", time_end - time_start) end

    local time_start = os.clock()
    local status
    repeat
        status = jit:run_z80(cpu, cpu.PC)
    until status ~= "ok"
    if status ~= "halt" then
        print("Failed to Halt")
        print("Status:", status)
        os.exit(1)
    end
    local time_end = os.clock()
    if TIMING_ON then print("Execute Took", time_end - time_start) end
    
    local time_start = os.clock()
    local new_state = get_state(jit, cpu)
    local time_end = os.clock()
    if TIMING_ON then print("Get new_state Took", time_end - time_start) end
    return old_state, new_state
end



local function assemble_code(assembler, code_in)
    assembler:reset_environment()
    local z = assembler
    z:set_compile_address(0)    -- compile for zero address

    code_in(z)
    
    local end_addr = z:get_compile_address()
    
    local code
    if not z:any_errors() then
        if z:any_errors_or_warnings() then
            for _,warnings in ipairs(z:get_error_and_warning_messages()) do
                print(warnings)
            end
        end
        
        code = z:get_code()
    else
        print("FAIL: didn't assemble")
        for _,errors in ipairs(z:get_error_and_warning_messages()) do
            print(errors)
        end
        -- terminate immediately
        os.exit(1)
    end
    
    return code, end_addr
end

local function flag_mask_op(altered_mask, mask, flag_string)
    if bit32.band(altered_mask, mask) ~= 0 then
        print("Flag switch " .. bad_message .. " flag mentioned twice")
        os.exit(7)
    end
    altered_mask = bit32.bor(altered_mask, mask)
    return altered_mask
end

local function flag_set(flags, mask)
    return bit32.bor(flags, mask)
end

local function flag_clear(flags, mask)
    return bit32.band(flags, 0xFF - mask)
end

local flag_lookup_table = 
{
    ["S"] = 0x80, ["-S"] = 0x80,
    ["Z"] = 0x40, ["-Z"] = 0x40,
    ["5"] = 0x20, ["-5"] = 0x20,
    ["H"] = 0x10, ["-H"] = 0x10,
    ["3"] = 0x08, ["-3"] = 0x08,
    ["P"] = 0x04, ["-P"] = 0x04,
    ["V"] = 0x04, ["-V"] = 0x04,
    ["N"] = 0x02, ["-N"] = 0x02,
    ["C"] = 0x01, ["-C"] = 0x01,
}

local function decode_flags(flag_table, old_flags)
    -- {"S", "-Z", "-H", "-P", "-N", "oldF=0x34"}
    -- http://www.z80.info/z80sflag.htm
    local flags = 0
    local altered_mask = 0x00
    local old_F = -1
    for _, flag_switch in pairs(flag_table) do
        local mask = flag_lookup_table[flag_switch]
        if mask then
            if flag_switch:sub(1,1) == "-" then
                altered_mask = flag_mask_op(altered_mask, mask, flag_switch)
                flags = flag_clear(flags, mask)
            else
                altered_mask = flag_mask_op(altered_mask, mask, flag_switch)
                flags = flag_set(flags, mask)
            end
        elseif type(flag_switch) == "string" and #flag_switch >= 6 and flag_switch:sub(1,5) == "oldF=" then
            old_F = tonumber(flag_switch:sub(6))
            if not old_F then
                print("Unable to convert flag_switch", flag_switch)
                os.exit(8)
            end
        else
            print("Unexpected Flag switch", flag_switch)
            os.exit(6)
        end
    end
    
    -- bits that are not affected get their value either from the flags
    -- before the test is run, except when we specify old F. 
    if old_F == -1 then
        old_flags = bit32.band(old_flags, 0xFF-altered_mask)
        flags = bit32.bor(flags, old_flags)
    else
        old_F = bit32.band(old_F, 0xFF-altered_mask)
        flags = bit32.bor(flags, old_F)
    end
    
    return flags
end

local function itable_to_string(t)
    return "{ " .. table.concat(t, " ") .. " }"
end

local function flags_to_str(f)
    -- "SZ5H3VNC"
    local s = ""
    if bit32.btest(f, 0x80) then
        s = s .. "S"
    else
        s = s .. " "
    end
    if bit32.btest(f, 0x40) then
        s = s .. "Z"
    else
        s = s .. " "
    end
    if bit32.btest(f, 0x20) then
        s = s .. "5"
    else
        s = s .. " "
    end    
    if bit32.btest(f, 0x10) then
        s = s .. "H"
    else
        s = s .. " "
    end
    if bit32.btest(f, 0x08) then
        s = s .. "3"
    else
        s = s .. " "
    end
    if bit32.btest(f, 0x04) then
        s = s .. "V"
    else
        s = s .. " "
    end
    if bit32.btest(f, 0x02) then
        s = s .. "N"
    else
        s = s .. " "
    end
    if bit32.btest(f, 0x01) then
        s = s .. "C"
    else
        s = s .. " "
    end
    return s
end

local function check_changes(old_state, new_state, checks)
    if new_state.reg.F % 2 ~= new_state.reg.Carry then
        print("Carry Flag and F register don't agree in new state")
        print(string.format("Register F was 0x%x now 0x%x Carry=%d", old_state.reg["F"], new_state.reg["F"], new_state.reg.Carry))
        os.exit(1)
    end
    if old_state.reg.F % 2 ~= old_state.reg.Carry then
        print("Carry Flag and F register don't agree in old state")
        print(string.format("Register F was 0x%x now 0x%x Carry=%d", old_state.reg["F"], new_state.reg["F"], new_state.reg.Carry))
        os.exit(1)
    end
    -- hide carry flag - it's checked as part of F
    new_state.reg.Carry = old_state.reg.Carry
    
    for k,v in pairs(checks) do
        if type(k) == "number" then
            -- address
            local new = (new_state.mem[k]):byte()
            local old = (old_state.mem[k]):byte()
            if new ~= v then
                print("Memory change didn't occur as expected")
                print("Address", k, "was", old, "now", new, "expected", v)
                os.exit(5)
            end
            -- revert known change
            new_state.mem[k] = old_state.mem[k]
        else
            local extra = ""
            if k == "F" then
                if type(v) == "table" then
                    extra = itable_to_string(v)
                    v = decode_flags(v, old_state.reg[k])
                end
                if IGNORE_BIT3_BIT5_FLAGS then
                    v = bit32.band(v, 0xFF-0x28)
                    new_state.reg[k] = bit32.band(new_state.reg[k], 0xFF-0x28)
                end
            end
            if new_state.reg[k] ~= v then
                print("Register change didn't occur as expected")
                print(string.format("Register %s was 0x%x now 0x%x expected 0x%x %s", k, old_state.reg[k], new_state.reg[k], v, extra))
                if k == "F" then
                    print(string.format("was %s now %s expected %s", flags_to_str(old_state.reg[k]), flags_to_str(new_state.reg[k]), flags_to_str(v)))
                end
                os.exit(4)
            end
            -- change it back so we can ignore it
            new_state.reg[k] = old_state.reg[k]
        end
    end
end

local function compare_state(old_state, new_state)
    for k,v in pairs(old_state.reg) do
        local new = new_state.reg[k]
        if new ~= v then
            print("Unexpected register change")
            print("Register", k, "was", v, "now", new)
            os.exit(2)
        end
    end
    if old_state.mem_string ~= new_state.mem_string then
        for index, v in pairs(old_state.mem) do
            local new = new_state.mem[index]
            local addr = index
            if v ~= new then
                print("Unexpected memory change")
                print("location: ", addr, "was", v:byte(),"now", new:byte())
                os.exit(3)
            end
        end
    end
end


local function test(assembler, code, checks, post_setup)
    local time_start = os.clock()
    local initial_mem = block_of_halts(2048)
    local time_end = os.clock()
    if TIMING_ON then print("Halt table Took", time_end - time_start) end
    
    local binary, end_addr = assemble_code(assembler, code)
    if not checks.PC then
        checks.PC = end_addr + 1 -- +1 for halt instruction
    end
    local old_state, new_state = run_code(initial_mem, binary, post_setup)

    check_changes(old_state, new_state, checks)
    local time_start = os.clock()
    compare_state(old_state, new_state)
    local time_end = os.clock()
    if TIMING_ON then print("Get compare_state Took", time_end - time_start) end
end


local function run_batch(assembler, test_name, tests)
    local num_tests = #tests
    for i, one_test in pairs(tests) do
        print(string.format("Running test %s - %i of %i - %s", test_name, i, num_tests, one_test[1]))
        local time_start = os.clock()
        test(assembler, one_test[2], one_test[3], one_test[4])
        local time_end = os.clock()
        if TIMING_ON then print("Took", time_end - time_start) end
        
    end
    print("Finished all tests successfully")
end

--test("LD A, 33", { "A"=33, "F"={"C", "NZ"} )
--test("LD A, 33 \n LD(100),A", { [100]=33 }
-- test(function(z) z:LD("A", 33)   end, { ["A"]=33 })
--test(function(z) z:NOP()   end, { })
--test(function(z) z:assemble("LD", "BC", 0x4321) end, { B=0x43, C=0x21 })


----------------------------------------------------------------------------
--
--
-- Specialise Z80 assembler to collect metrics

local Collecting_Assembler = class("collecting_assembler", Z80_Assembler)

function Collecting_Assembler:initialize()
  Z80_Assembler.initialize(self) -- invoking the superclass' initializer
  self.instructions_completed = {}
  
  self.tested_official = 0
  self.untested_official = 0
  self.tested_undoc = 0
  self.untested_undoc = 0    
  self.tested_unrecognised = 0

  self:_mark_expected_instructions()
end

function Collecting_Assembler:_save_opcode(opcode, ...)
    local old = self.instructions_completed[opcode]
    if old == nil then
        self.tested_unrecognised = self.tested_unrecognised + 1
        self.instructions_completed[opcode] = 0
    elseif old == 1 then
        self.tested_undoc = self.tested_undoc + 1
        self.untested_undoc = self.untested_undoc - 1
        self.instructions_completed[opcode] = 2
    elseif old == false then
        --print(string.format("%x", opcode))
        self.tested_official = self.tested_official + 1
        self.untested_official = self.untested_official - 1
        self.instructions_completed[opcode] = true
    end -- old == 2 and old == true and old == 0 don't count

    Z80_Assembler._save_opcode(self, opcode, ...)
end

function Collecting_Assembler:_mark_expected_instructions()
    
    -- deal with single byte plus CBxx instructions
    for i = 0, 255 do
        self.instructions_completed[i] = false
        self.instructions_completed[0xCB00+i] = false
    end
    
    -- these instrrctions don't appear as single bytes
    self.instructions_completed[0xCB] = nil
    self.instructions_completed[0xDD] = nil
    self.instructions_completed[0xED] = nil
    self.instructions_completed[0xFD] = nil
    
    -- these CBxx instructions are not official
    for i = 0, 7 do
        self.instructions_completed[0xCB30+i] = 1
    end
    
    -- 0xEDnn instructions
    for i = 0, 0x0f do
        self.instructions_completed[0xED40+i] = false
        self.instructions_completed[0xED50+i] = false
        self.instructions_completed[0xED60+i] = false
        self.instructions_completed[0xED70+i] = false
    end
    self.instructions_completed[0xED4C] = 1
    self.instructions_completed[0xED4E] = 1
    self.instructions_completed[0xED54] = 1
    self.instructions_completed[0xED5C] = 1
    self.instructions_completed[0xED63] = 1
    self.instructions_completed[0xED64] = 1
    self.instructions_completed[0xED6B] = 1
    self.instructions_completed[0xED6C] = 1
    self.instructions_completed[0xED6E] = 1
    self.instructions_completed[0xED70] = 1
    self.instructions_completed[0xED71] = 1
    self.instructions_completed[0xED74] = 1
    self.instructions_completed[0xED7C] = 1
    self.instructions_completed[0xED77] = nil
    self.instructions_completed[0xED7F] = nil
    
    for i = 0, 3 do
        self.instructions_completed[0xEDA0+i] = false
        self.instructions_completed[0xEDB0+i] = false
    end
    for i = 8, 0x0b do
        self.instructions_completed[0xEDA0+i] = false
        self.instructions_completed[0xEDB0+i] = false
    end
    
    -- need to check against WoS_z80undoc.htm, not just http://clrhome.org/table/
    -- DDxx and FDxx instructions
    --
    -- this ones complex so let's do a 'map'
    local map = {
    --   0123456789abcdef
        '         y      ',
        '         y      ',
        '.yyy???..yyy??? ',
        '....yyy..y      ',
        '    ??y     ??y ',
        '    ??y     ??y ',
        '??????y???????y?',
        'yyyyyy y    ??y ',
        '    ??y     ??y ',
        '    ??y     ??y ',
        '    ??y     ??y ',
        '    ??y     ??y ',
        '                ',
        '                ',
        ' y y y...y      ',
        '         y      ',
        }
    for line, str in ipairs(map) do
        local column = 0
        for c in str:gmatch"." do
            if c == 'y' then
                c = false
            elseif c == '?' then
                c = 1
            else
                c = nil
            end
            self.instructions_completed[0xFD00+(line-1)*16+column] = c
            self.instructions_completed[0xDD00+(line-1)*16+column] = c
            column = column + 1
        end
    end
    
    -- DDCBxx and FDCBxx instructions
    for i = 0, 255 do
        self.instructions_completed[0xDDCB00+i] = 1
        self.instructions_completed[0xFDCB00+i] = 1
    end
    for i = 0, 0x0f0, 0x10 do
        self.instructions_completed[0xDDCB06+i] = false
        self.instructions_completed[0xDDCB0E+i] = false
        self.instructions_completed[0xFDCB06+i] = false
        self.instructions_completed[0xFDCB0E+i] = false
    end
    self.instructions_completed[0xDDCB36] = 1
    self.instructions_completed[0xFDCB36] = 1


    -- count up number to test
    for _, v in pairs(self.instructions_completed) do
        if v == 1 then
            self.untested_undoc = self.untested_undoc + 1
        elseif v == false then
            self.untested_official = self.untested_official + 1
        else
            print("Unrecognised value")
            os.exit(1)
        end
    end

end



function Collecting_Assembler:print_stats()
    print("Tested official instructions", self.tested_official)
    print("Not tested official instructions", self.untested_official)
    print("Tested undocumented instructions", self.tested_undoc)
    print("Not tested undocumented instructions", self.untested_undoc)    
    print("Tested unrecognised instructions", self.tested_unrecognised)
end

function Collecting_Assembler:print_detailed_stats()
    self:print_stats()
    
    local lookup = { }
    lookup[true] = "Y"
    lookup[false]= "N"
    lookup[0] = "x"
    lookup[1] = "n"
    lookup[2] = "y"
    
    local print_graphic_map
    function print_graphic_map(offset)
        print("  0 1 2 3 4 5 6 7 8 9 A B C D E F")
        for line = 0, 15 do
            local line_str = ""
            for column = 0, 15 do
                local address = offset + 16 * line + column
                local c = lookup[self.instructions_completed[address]]
                if c == nil then c = "." end
                line_str = line_str .. " " .. c
            end
            print(string.format("%X%s", line, line_str))
        end
    end
    
    print("00 to FF")
    print("========")
    print_graphic_map(0)
    
    print()
    print("ED00 to EDFF")
    print("========")
    print_graphic_map(0xED00)
    
    print()
    print("CB00 to CBFF")
    print("========")
    print_graphic_map(0xCB00)

    print()
    print("DD00 to DDFF")
    print("========")
    print_graphic_map(0xDD00)

    print()
    print("FD00 to FDFF")
    print("========")
    print_graphic_map(0xFD00)

    print()
    print("DDCB00 to DDCBFF")
    print("========")
    print_graphic_map(0xDDCB00)

    print()
    print("FDCB00 to FDCBFF")
    print("========")
    print_graphic_map(0xFDCB00)

end

--function FindExpectedInstructions



----------------------------------------------------------------------------
--
--
local basic_instruction_tests = require("z80_regression_tests/tests_nn_opcodes")
local CB_instruction_tests = require("z80_regression_tests/tests_CBnn_opcodes")
local ED_instruction_tests = require("z80_regression_tests/tests_EDnn_opcodes")
local DD_instruction_tests = require("z80_regression_tests/tests_DDnn_opcodes")
local FD_instruction_tests = require("z80_regression_tests/tests_FDnn_opcodes")

----------------------------------------------------------------------------
--
-- These lines select the tests to run
--


--our_assembler = Z80_Assembler:new()
our_assembler = Collecting_Assembler:new()

run_batch(our_assembler, "0xnn", basic_instruction_tests)
--run_batch(our_assembler, "temp", temp_test)
run_batch(our_assembler, "0xCBnn", CB_instruction_tests)
run_batch(our_assembler, "0xEDnn", ED_instruction_tests)
run_batch(our_assembler, "0xDDnn", DD_instruction_tests)
run_batch(our_assembler, "0xFDnn", FD_instruction_tests)
--run_batch(our_assembler, "0xDDCBnn", DDCB_instruction_tests)
--run_batch(our_assembler, "0xFDCBnn", FDCB_instruction_tests)
--run_batch(our_assembler, "", memory_invalidation_tests)
--run_batch(our_assembler, "", more_advanced_tests)
--run_batch(our_assembler, "", interrupt_test)
--run_batch(our_assembler, "", R_reg_test)
--run_batch(our_assembler, "", Carry_Flag_seperate_from_F_flag_interactions_test)

our_assembler:print_detailed_stats()
