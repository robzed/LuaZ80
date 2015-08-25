-- Z80 Single Step Debugger/Monitor
-- (c) Copyright 2015 Rob Probin.
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
-- and run only occasionlly.
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

local function run_code(initial_memory, code)
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



local function assemble_code(code_in)
    local z = Z80_Assembler:new()
    z:set_compile_address(0)    -- compile for zero address

    code_in(z)
    
    local end_addr = z:get_compile_address()
    
    local code
    if not z:any_errors() then
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


local function test(code, checks)
    local time_start = os.clock()
    local initial_mem = block_of_halts(2048)
    local time_end = os.clock()
    if TIMING_ON then print("Halt table Took", time_end - time_start) end
    
    local binary, end_addr = assemble_code(code)
    if not checks.PC then
        checks.PC = end_addr + 1 -- +1 for halt instruction
    end
    local old_state, new_state = run_code(initial_mem, binary)

    check_changes(old_state, new_state, checks)
    local time_start = os.clock()
    compare_state(old_state, new_state)
    local time_end = os.clock()
    if TIMING_ON then print("Get compare_state Took", time_end - time_start) end
end


local function run_batch(tests)
    local num_tests = #tests
    for i, one_test in pairs(tests) do
        print(string.format("Running test %i of %i - %s", i, num_tests, one_test[1]))
        local time_start = os.clock()
        test(one_test[2], one_test[3])
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

local basic_instruction_tests = {
    

{ "NOP",     function(z) z:NOP()   end, { } },
{ "LD BC,n", function(z) z:assemble("LD", "BC", 0x4321) end, { B=0x43, C=0x21 } },
    
{ "LD   (BC),A (beyond memory)", function(z)
                    z:assemble("LD", "BC", 0x8000) 
                    z:assemble("LD", "A", 0x01)
                    z:assemble("LD", "(BC)", "A") 
                end, 
                { B=0x80, C=0x00, A=0x01 } },
{ "LD   (BC),A", function(z)
                    z:assemble("LD", "BC", 0x7001) 
                    z:assemble("LD", "A", 0x01)
                    z:assemble("LD", "(BC)", "A") 
                end, 
                { B=0x70, C=0x01, A=0x01, [0x7001]=0x01 } },
{ "LD   (BC),A (write ROM)", function(z)
                    z:assemble("LD", "BC", 0x3F00) 
                    z:assemble("LD", "A", 0x01)
                    z:assemble("LD", "(BC)", "A") 
                end, 
                { B=0x3F, C=0x00, A=0x01 } },

-- 0x03
{ "INC  BC", function(z) z:assemble("LD", "BC", 0x43FF)  
        z:assemble("INC", "BC") end, { B=0x44, C=0x00 } },  
 
 { "INC  BC rollover", function(z) z:assemble("LD", "BC", 0xFFFF)  
        z:assemble("INC", "BC") end, { B=0x00, C=0x00 } },  

-- 0x04
 { "INC  B", function(z) z:assemble("LD", "B", 0x11)  
        z:assemble("INC", "B") end, { B=0x12, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

 { "INC  B rollover", function(z) z:assemble("LD", "B", 0xFF)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if carry from bit 3; reset otherwise
        -- P/V is set if r was 7FH before operation; reset otherwise
        -- N is reset
        -- C is not affected
        z:assemble("INC", "B") end, { B=0x00, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

 { "INC  B half carry", function(z) z:assemble("LD", "B", 0x0F)  
        z:assemble("INC", "B") end, { B=0x10, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

 { "INC  B Flags P/V Sign", function(z) z:assemble("LD", "B", 0x7F)  
        z:assemble("INC", "B") end, { B=0x80, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  

-- 0x05
{ "DEC  B", function(z) z:LD("B", 0x11)  
        z:assemble("DEC", "B") end, { B=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"} } },  

{ "DEC  B to zero", function(z) z:LD("B", 0x01)  
        z:assemble("DEC", "B") end, { B=0x00, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  B rollover", function(z) z:assemble("LD", "B", 0x00)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if borrow from bit 4, reset otherwise
        -- P/V is set if m was 80H before operation; reset otherwise
        -- N is set
        -- C is not affected
        z:assemble("DEC", "B") end, { B=0xFF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  B half carry", function(z) z:assemble("LD", "B", 0x10)  
        z:assemble("DEC", "B") end, { B=0x0F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  B Flags P/V Sign", function(z) z:assemble("LD", "B", 0x80)  
        z:assemble("DEC", "B") end, { B=0x7F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"} } },  

-- 0x06
 { "LD  B,n", function(z) z:assemble("LD", "B", 0xe1) end, { B=0xe1 } }, 

--[[
    ["RLCA"] =           0x07,
    --]]
    
-- 0x08
{ "EX AF, AF'", function(z) z:LD("SP", 0x6000)
                            z:LD("DE", 0x1234)
                            z:PUSH("DE") 
                            z:POP("AF")
                            z:assemble("EX", "AF", "AF'")
                            z:LD("DE", 0xABCD)
                            z:PUSH("DE") 
                            z:POP("AF")
                            z:LD("DE", 0x9876)
                        end, 
        { D=0x98, E=0x76, A=0xAB, F=0xCD, SP=0x6000, [0x5FFE]=0xCD, [0x5FFF]=0xAB, 
            A_ = 0x12, F_=0x34 } },
    --[[
    ["ADD  HL,BC"] =     0x09,
    --]]
    -- 0x0A
    { "LD A,(BC)", function(z) 
            z:LD("BC", 0x5F12) 
            z:LD("A", 0x0A)
            z:LD("(BC)", "A")
            z:LD("A", 0xFF)
            z:LD("A", "(BC)")
            end, { B = 0x5F, C = 0x12, A = 0x0A, [0x5F12]=0x0A } }, 
    -- 0x0b
    { "DEC BC", function(z) z:LD("BC", 0x1234) z:assemble("DEC", "BC") end, { C = 0x33, B = 0x12 } },
    { "DEC BC rollover1", function(z) z:LD("BC", 0x0100) z:assemble("DEC", "BC") end, { B = 0x00, C = 0xFF } },
    { "DEC BC rollover2", function(z) z:LD("BC", 0x0000) z:assemble("DEC", "BC") end, { B = 0xFF, C = 0xFF } },
    

-- 0x0c
{ "INC  C", function(z) z:assemble("LD", "C", 0x11)  
        z:assemble("INC", "C") end, { C=0x12, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

 { "INC  C rollover", function(z) z:assemble("LD", "C", 0xFF)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if carry from bit 3; reset otherwise
        -- P/V is set if r was 7FH before operation; reset otherwise
        -- N is reset
        -- C is not affected
        z:assemble("INC", "C") end, { C=0x00, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

 { "INC  C half carry", function(z) z:assemble("LD", "C", 0x0F)  
        z:assemble("INC", "C") end, { C=0x10, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

 { "INC  C Flags P/V Sign", function(z) z:assemble("LD", "C", 0x7F)  
        z:assemble("INC", "C") end, { C=0x80, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  


-- 0x0d
{ "DEC  C", function(z) z:LD("C", 0x11)  
        z:assemble("DEC", "C") end, { C=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"} } },  

{ "DEC  C to zero", function(z) z:LD("C", 0x01)  
        z:assemble("DEC", "C") end, { C=0x00, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  C rollover", function(z) z:assemble("LD", "C", 0x00)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if borrow from bit 4, reset otherwise
        -- P/V is set if m was 80H before operation; reset otherwise
        -- N is set
        -- C is not affected
        z:assemble("DEC", "C") end, { C=0xFF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  C half carry", function(z) z:assemble("LD", "C", 0x10)  
        z:assemble("DEC", "C") end, { C=0x0F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  C Flags P/V Sign", function(z) z:assemble("LD", "C", 0x80)  
        z:assemble("DEC", "C") end, { C=0x7F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"} } },  


-- 0x0e
 { "LD  C,n", function(z) z:assemble("LD", "C", 0xe1) end, { C=0xe1 } }, 

--[[

    ["RRCA"] =           0x0f,
    ["DJNZ !r!"] =       0x10,
--]]

-- 0x11
{ "LD DE,n", function(z) z:assemble("LD", "DE", 0x4321) end, { D=0x43, E=0x21 } },

-- 0x12
{ "LD (DE), A", function(z) 
        z:LD("A", 12)
        z:LD("DE", 0x6040)
        z:LD("(DE)", "A") 
    end,
    { D = 0x60, E = 0x40, A = 12, [0x6040] = 12 } },

-- 0x13
{ "INC  DE", function(z) z:assemble("LD", "DE", 0x43FF)  
        z:assemble("INC", "DE") end, { D=0x44, E=0x00 } },  
 
 { "INC  DE rollover", function(z) z:assemble("LD", "DE", 0xFFFF)  
        z:assemble("INC", "DE") end, { D=0x00, E=0x00 } },  

-- 0x14
 { "INC  D", function(z) z:assemble("LD", "D", 0x11)  
        z:assemble("INC", "D") end, { D=0x12, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

 { "INC  D rollover", function(z) z:assemble("LD", "D", 0xFF)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if carry from bit 3; reset otherwise
        -- P/V is set if r was 7FH before operation; reset otherwise
        -- N is reset
        -- C is not affected
        z:assemble("INC", "D") end, { D=0x00, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

 { "INC  D half carry", function(z) z:assemble("LD", "D", 0x0F)  
        z:assemble("INC", "D") end, { D=0x10, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

 { "INC  D Flags P/V Sign", function(z) z:assemble("LD", "D", 0x7F)  
        z:assemble("INC", "D") end, { D=0x80, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  

-- 0x15
{ "DEC  D", function(z) z:LD("D", 0x11)  
        z:assemble("DEC", "D") end, { D=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"} } },  

{ "DEC  D to zero", function(z) z:LD("D", 0x01)  
        z:assemble("DEC", "D") end, { D=0x00, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  D rollover", function(z) z:assemble("LD", "D", 0x00)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if borrow from bit 4, reset otherwise
        -- P/V is set if m was 80H before operation; reset otherwise
        -- N is set
        -- C is not affected
        z:assemble("DEC", "D") end, { D=0xFF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  D half carry", function(z) z:assemble("LD", "D", 0x10)  
        z:assemble("DEC", "D") end, { D=0x0F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  D Flags P/V Sign", function(z) z:assemble("LD", "D", 0x80)  
        z:assemble("DEC", "D") end, { D=0x7F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"} } },  


-- 0x26
 { "LD  D,n", function(z) z:assemble("LD", "D", 0xe1) end, { D=0xe1 } }, 

--[[
    ["RLA"] =            0x17,
    ["JR   !r!"] =       0x18,
    ["ADD  HL,DE"] =     0x19,
--]]
    -- 0x1A
    { "LD A,(DE)", function(z) 
            z:LD("DE", 0x5F12) 
            z:LD("A", 0x0A)
            z:LD("(DE)", "A")
            z:LD("A", 0xFF)
            z:LD("A", "(DE)")
        end, { D = 0x5F, E = 0x12, A = 0x0A, [0x5F12]=0x0A } },
    
    -- 0x1B
    { "DEC DE", function(z) z:LD("DE", 0x1234) z:assemble("DEC", "DE") end, { E = 0x33, D = 0x12 } },
    { "DEC DE rollover1", function(z) z:LD("DE", 0x0100) z:assemble("DEC", "DE") end, { D = 0x00, E = 0xFF } },
    { "DEC DE rollover2", function(z) z:LD("DE", 0x0000) z:assemble("DEC", "DE") end, { D = 0xFF, E = 0xFF } },

-- 0x1C
 { "INC  E", function(z) z:assemble("LD", "E", 0x11)  
        z:assemble("INC", "E") end, { E=0x12, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

 { "INC  E rollover", function(z) z:assemble("LD", "E", 0xFF)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if carry from bit 3; reset otherwise
        -- P/V is set if r was 7FH before operation; reset otherwise
        -- N is reset
        -- C is not affected
        z:assemble("INC", "E") end, { E=0x00, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

 { "INC  E half carry", function(z) z:assemble("LD", "E", 0x0F)  
        z:assemble("INC", "E") end, { E=0x10, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

 { "INC  E Flags P/V Sign", function(z) z:assemble("LD", "E", 0x7F)  
        z:assemble("INC", "E") end, { E=0x80, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  
        
-- 0x1D
{ "DEC  E", function(z) z:LD("E", 0x11)  
        z:assemble("DEC", "E") end, { E=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"} } },  

{ "DEC  E to zero", function(z) z:LD("E", 0x01)  
        z:assemble("DEC", "E") end, { E=0x00, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  E rollover", function(z) z:assemble("LD", "E", 0x00)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if borrow from bit 4, reset otherwise
        -- P/V is set if m was 80H before operation; reset otherwise
        -- N is set
        -- C is not affected
        z:assemble("DEC", "E") end, { E=0xFF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  E half carry", function(z) z:assemble("LD", "E", 0x10)  
        z:assemble("DEC", "E") end, { E=0x0F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  E Flags P/V Sign", function(z) z:assemble("LD", "E", 0x80)  
        z:assemble("DEC", "E") end, { E=0x7F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"} } },  


-- 0x1E
 { "LD  E,n", function(z) z:assemble("LD", "E", 0xe1) end, { E=0xe1 } }, 

--[[
    ["RRA"] =            0x1F,
    ["JR   NZ,!r!"] =    0x20,
--]]

-- 0x21
{ "LD HL,n", function(z) z:assemble("LD", "HL", 0x4321) end, { H=0x43, L=0x21 } },

-- 0x22
{"LD   (nn),HL", function(z) z:assemble("LD", "HL", 0x4321) 
        z:assemble("LD", "(0x5555)", "HL") 
        end, { H=0x43, L=0x21, [0x5555]=0x21, [0x5556]=0x43 }},
{"LD   (nn),HL wrap", function(z) z:assemble("LD", "HL", 0x4321) 
        z:assemble("LD", "(0xFFFF)", "HL") 
        end, { H=0x43, L=0x21, [0xFFFF]=0x21, [0x0000]=0x43 }},

-- 0x23
{ "INC  HL", function(z) z:assemble("LD", "HL", 0x43FF)  
        z:assemble("INC", "HL") end, { H=0x44, L=0x00 } },  
 
 { "INC  HL rollover", function(z) z:assemble("LD", "HL", 0xFFFF)  
        z:assemble("INC", "HL") end, { H=0x00, L=0x00 } },  

-- 0x24
 { "INC  H", function(z) z:assemble("LD", "H", 0x11)  
        z:assemble("INC", "H") end, { H=0x12, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

 { "INC  H rollover", function(z) z:assemble("LD", "H", 0xFF)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if carry from bit 3; reset otherwise
        -- P/V is set if r was 7FH before operation; reset otherwise
        -- N is reset
        -- C is not affected
        z:assemble("INC", "H") end, { H=0x00, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

 { "INC  H half carry", function(z) z:assemble("LD", "H", 0x0F)  
        z:assemble("INC", "H") end, { H=0x10, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

 { "INC  H Flags P/V Sign", function(z) z:assemble("LD", "H", 0x7F)  
        z:assemble("INC", "H") end, { H=0x80, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  


-- 0x25
{ "DEC  H", function(z) z:LD("H", 0x11)  
        z:assemble("DEC", "H") end, { H=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"} } },  

{ "DEC  H to zero", function(z) z:LD("H", 0x01)  
        z:assemble("DEC", "H") end, { H=0x00, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  H rollover", function(z) z:assemble("LD", "H", 0x00)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if borrow from bit 4, reset otherwise
        -- P/V is set if m was 80H before operation; reset otherwise
        -- N is set
        -- C is not affected
        z:assemble("DEC", "H") end, { H=0xFF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  H half carry", function(z) z:assemble("LD", "H", 0x10)  
        z:assemble("DEC", "H") end, { H=0x0F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  H Flags P/V Sign", function(z) z:assemble("LD", "H", 0x80)  
        z:assemble("DEC", "H") end, { H=0x7F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"} } },  



-- 0x26
 { "LD  H,n", function(z) z:assemble("LD", "H", 0xe1) end, { H=0xe1 } }, 

--[[
    ["DAA"] =            0x27,
    ["JR   Z,!r!"] =     0x28,
    ["ADD  HL,HL"] =     0x29,
--]]
    -- 0x2A
    { "LD HL,(nn)", function(z)
            z:LD("A", 22) 
            z:LD("(0x6000)", "A") 
            z:LD("A", 11) 
            z:LD("(0x6001)", "A")
            z:LD("HL", "(0x6000)")
            end,
        { [0x6000] = 22, [0x6001] = 11, A = 11, L = 22, H = 11 } },
    { "LD HL,(nn) rollover", function(z)
            z:LD("A", 22) 
            z:LD("(0xFFFF)", "A") 
            z:LD("A", 33) 
            z:LD("(0x0000)", "A")
            z:LD("HL", "(0xFFFF)") 
            z:LD("A", 99)
            end,
        { [0xFFFF] = 22, [0x0000] = 33, A = 99, L = 22, H = 33 } },

    -- 0x2B
    { "DEC HL", function(z) z:LD("HL", 0x1234) z:assemble("DEC", "HL") end, { L = 0x33, H = 0x12 } },
    { "DEC HL rollover1", function(z) z:LD("HL", 0x0100) z:assemble("DEC", "HL") end, { H = 0x00, L = 0xFF } },
    { "DEC HL rollover2", function(z) z:LD("HL", 0x0000) z:assemble("DEC", "HL") end, { H = 0xFF, L = 0xFF } },

-- 0x2C
 { "INC  L", function(z) z:assemble("LD", "L", 0x11)  
        z:assemble("INC", "L") end, { L=0x12, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

 { "INC  L rollover", function(z) z:assemble("LD", "L", 0xFF)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if carry from bit 3; reset otherwise
        -- P/V is set if r was 7FH before operation; reset otherwise
        -- N is reset
        -- C is not affected
        z:assemble("INC", "L") end, { L=0x00, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

 { "INC  L half carry", function(z) z:assemble("LD", "L", 0x0F)  
        z:assemble("INC", "L") end, { L=0x10, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

 { "INC  L Flags P/V Sign", function(z) z:assemble("LD", "L", 0x7F)  
        z:assemble("INC", "L") end, { L=0x80, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  


-- 0x2D
{ "DEC  L", function(z) z:LD("L", 0x11)  
        z:assemble("DEC", "L") end, { L=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"} } },  

{ "DEC  L to zero", function(z) z:LD("L", 0x01)  
        z:assemble("DEC", "L") end, { L=0x00, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  L rollover", function(z) z:assemble("LD", "L", 0x00)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if borrow from bit 4, reset otherwise
        -- P/V is set if m was 80H before operation; reset otherwise
        -- N is set
        -- C is not affected
        z:assemble("DEC", "L") end, { L=0xFF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  L half carry", function(z) z:assemble("LD", "L", 0x10)  
        z:assemble("DEC", "L") end, { L=0x0F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  L Flags P/V Sign", function(z) z:assemble("LD", "L", 0x80)  
        z:assemble("DEC", "L") end, { L=0x7F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"} } },  


-- 0x2E
 { "LD  L,n", function(z) z:assemble("LD", "L", 0xe1) end, { L=0xe1 } }, 

--[[
    ["CPL"] =            0x2F,
    ["JR   NC,!r!"] =    0x30,
--]]

-- 0x31
{ "LD SP,n", function(z) z:assemble("LD", "SP", 0x4321) end, { SP=0x4321 } },

-- 0x32
{"LD   (nn),A", function(z) z:assemble("LD", "A", 0x83) 
        z:assemble("LD", "(0x5555)", "A") 
        end, { A=0x83, [0x5555]=0x83 }},

-- 0x33
{ "INC  SP", function(z) z:assemble("LD", "SP", 0x43FF)  
        z:assemble("INC", "SP") end, { SP=0x4400 } },  
 
 { "INC  SP rollover", function(z) z:assemble("LD", "SP", 0xFFFF)  
        z:assemble("INC", "SP") end, { SP=0x0000 } },  

--[[
    ["INC  (HL)"] =      0x34,
--]]

-- 0x35
{ "DEC  (HL)", function(z) z:LD("HL", 0x6000) z:LD("(HL)", 0x11)  
        z:assemble("DEC", "(HL)") end, { [0x6000]=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"}, H=0x60, L=0x00 } },  

{ "DEC  (HL) to zero", function(z) z:LD("HL", 0x6000) z:LD("(HL)", 0x01)  
        z:assemble("DEC", "(HL)") end, { [0x6000]=0x00, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"}, H=0x60, L=0x00 } },  

 { "DEC  (HL) rollover", function(z) z:LD("HL", 0x6000) z:assemble("LD", "(HL)", 0x00)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if borrow from bit 4, reset otherwise
        -- P/V is set if m was 80H before operation; reset otherwise
        -- N is set
        -- C is not affected
        z:assemble("DEC", "(HL)") end, { [0x6000]=0xFF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"}, H=0x60, L=0x00 } },  

 { "DEC  (HL) half carry", function(z) z:LD("HL", 0x6000) z:assemble("LD", "(HL)", 0x10)  
        z:assemble("DEC", "(HL)") end, { [0x6000]=0x0F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"}, H=0x60, L=0x00 } },  

 { "DEC  (HL) Flags P/V Sign", function(z) z:LD("HL", 0x6000) z:assemble("LD", "(HL)", 0x80)  
        z:assemble("DEC", "(HL)") end, { [0x6000]=0x7F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"}, H=0x60, L=0x00 } },  

 { "LD (HL), n", function(z)
         z:LD("HL", 0x5DEF)
         z:LD("(HL)", 0x11)
         end, { H=0x5D, L=0xEF, [0x5DEF] = 0x11 } },
--[[
    ["SCF"] =            0x37,
    ["JR   C,!r!"] =     0x38,
    ["ADD  HL,SP"] =     0x39,
--]]

    -- 0x3A
    { "LD A,(nn)", function(z) z:LD("A", 22) z:LD("(0x6000)", "A") z:LD("A", 0) z:LD("A", "(0x6000)") end,
        { [0x6000] = 22, A = 22 } },
    
    -- 0x3B
    { "DEC SP", function(z) z:LD("SP", 0x1234) z:assemble("DEC", "SP") end, { SP = 0x1233 } },
    { "DEC SP rollover1", function(z) z:LD("SP", 0x0100) z:assemble("DEC", "SP") end, { SP = 0x00FF } },
    { "DEC SP rollover2", function(z) z:LD("SP", 0x0000) z:assemble("DEC", "SP") end, { SP = 0xFFFF } },

-- 0x3C
 { "INC  A", function(z) z:assemble("LD", "A", 0x11)  
        z:assemble("INC", "A") end, { A=0x12, F={"-S", "-Z", "-H", "-V", "-N", "C", "oldF=0x11"} } },  

 { "INC  A rollover", function(z) z:assemble("LD", "A", 0xFF)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if carry from bit 3; reset otherwise
        -- P/V is set if r was 7FH before operation; reset otherwise
        -- N is reset
        -- C is not affected
        z:assemble("INC", "A") end, { A=0x00, F={"-S", "Z", "H", "-V", "-N", "C", "oldF=0xFF"} } },  

 { "INC  A half carry", function(z) z:assemble("LD", "A", 0x0F)  
        z:assemble("INC", "A") end, { A=0x10, F={"-S", "-Z", "H", "-V", "-N", "C", "oldF=0x0F"} } },  

 { "INC  A Flags P/V Sign", function(z) z:assemble("LD", "A", 0x7F)  
        z:assemble("INC", "A") end, { A=0x80, F={"S", "-Z", "H", "V", "-N", "C", "oldF=0x7F"} } },  
    
-- 0x3D
{ "DEC  A", function(z) z:LD("A", 0x11)  
        z:assemble("DEC", "A") end, { A=0x10, F={"-S", "-Z", "-H", "-V", "N", "oldF=0xFF"} } },  

{ "DEC  A to zero", function(z) z:LD("A", 0x01)  
        z:assemble("DEC", "A") end, { A=0x00, F={"-S", "Z", "-H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  A rollover", function(z) z:assemble("LD", "A", 0x00)  
         -- S is set if result is negative; reset otherwise
        -- Z is set if result is zero; reset otherwise
        -- H is set if borrow from bit 4, reset otherwise
        -- P/V is set if m was 80H before operation; reset otherwise
        -- N is set
        -- C is not affected
        z:assemble("DEC", "A") end, { A=0xFF, F={"S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  A half carry", function(z) z:assemble("LD", "A", 0x10)  
        z:assemble("DEC", "A") end, { A=0x0F, F={"-S", "-Z", "H", "-V", "N", "oldF=0xFF"} } },  

 { "DEC  A Flags P/V Sign", function(z) z:assemble("LD", "A", 0x80)  
        z:assemble("DEC", "A") end, { A=0x7F, F={"-S", "-Z", "H", "V", "N", "oldF=0xFF"} } },  


{ "LD A,33", function(z) z:LD("A", 33)   end, { ["A"]=33 } },

-- 0x3F
-- F flags: negative cleared, half carry is the same as old carry, carry completemented
{ "CCF", function(z) z:assemble("CCF") end, { F={"-N", "-C", "H" } } },

{ "CCF F=00", function(z) 
        z:assemble("LD", "SP", 0x6000)
        z:assemble("LD", "DE", 0x0000) 
        z:assemble("PUSH","DE") 
        z:assemble("POP", "AF")
        z:assemble("CCF") 
    end, { F={ "oldF=0", "-N", "C", "-H" }, D = 0, E = 0, A = 0, SP = 0x6000, [0x5FFF]=0, [0x5FFe]=0 } },

{ "CCF F=FF", function(z) 
        z:assemble("LD", "SP", 0x6000)
        z:assemble("LD", "DE", 0xFFFF) 
        z:assemble("PUSH","DE") 
        z:assemble("POP", "AF")
        z:assemble("CCF") 
        end, { F={"oldF=0xFF", "-N", "-C", "H" }, D = 0xFF, E = 0xFF, A = 0xFF, SP = 0x6000, [0x5FFF]=0xFF, [0x5FFe]=0xFF } },

{ "CCF carry flag set", function(z)
        z:LD("A", 0)
        z:ADD("A", "A")
        z:assemble("CCF") 
        z:LD("A", 0)
        z:assemble("ADC", "A", "A")
        end,
        { A = 1, F={"-N", "-C", "-H", "-V", "-S", "-Z"} } },
    
{ "CCF carry flag clear", function(z)
        z:LD("A", 0xFF)
        z:ADD("A", "A")
        z:assemble("CCF")
        z:LD("A", 0)
        z:assemble("ADC", "A", "A")
        end,
        { A = 0, F={"-N", "-C", "-H", "-V", "-S", "Z"} } },

    --0x40,
    { "LD B,B", function(z) z:LD("B", 0x7f) z:LD("B", "B") end, { ["B"]=0x7f } },
    --0x41,
    { "LD B,C", function(z) z:LD("C", 0x7f) z:LD("B", "C") end, { ["C"]=0x7f, ["B"]=0x7f } },
    --0x42,
    { "LD B,D", function(z) z:LD("D", 0x7f) z:LD("B", "D") end, { ["D"]=0x7f, ["B"]=0x7f } },
    --0x43,
    { "LD B,E", function(z) z:LD("E", 0x7f) z:LD("B", "E") end, { ["E"]=0x7f, ["B"]=0x7f } },
    --0x44,
    { "LD B,H", function(z) z:LD("H", 0x7f) z:LD("B", "H") end, { ["H"]=0x7f, ["B"]=0x7f } },
    --0x45,
    { "LD B,L", function(z) z:LD("L", 0x7f) z:LD("B", "L") end, { ["L"]=0x7f, ["B"]=0x7f } },
    --0x46,
    { "LD B,(HL)", function(z) 
            z:assemble("LD", "HL", 0x7001) 
            z:assemble("LD", "D", 0x7E)
            z:assemble("LD", "(HL)", "D") 
            z:LD("B", "(HL)")  
            end, { ["B"]=0x7E, H=0x70, L=0x01, D=0x7E, [0x7001]=0x7E } },
    --0x47,
    { "LD B,A", function(z) z:LD("A", 0x7f) z:LD("B", "A")  end, { ["B"]=0x7f, ["A"]=0x7f } },
    

    --0x48,
    { "LD C,B", function(z) z:LD("B", 0x7f) z:LD("C", "B") end, { ["B"]=0x7f, ["C"]=0x7f } },
    --0x49,
    { "LD C,C", function(z) z:LD("C", 0x7f) z:LD("C", "C") end, { ["C"]=0x7f } },
    --0x4A,
    { "LD C,D", function(z) z:LD("D", 0x7f) z:LD("C", "D") end, { ["D"]=0x7f, ["C"]=0x7f } },
    --0x4B,
    { "LD C,E", function(z) z:LD("E", 0x7f) z:LD("C", "E") end, { ["E"]=0x7f, ["C"]=0x7f } },
    --0x4C,
    { "LD C,H", function(z) z:LD("H", 0x7f) z:LD("C", "H") end, { ["H"]=0x7f, ["C"]=0x7f } },
    --0x4D,
    { "LD C,L", function(z) z:LD("L", 0x7f) z:LD("C", "L") end, { ["L"]=0x7f, ["C"]=0x7f } },
    --0x4E,
    { "LD C,(HL)", function(z) 
            z:assemble("LD", "HL", 0x7001) 
            z:assemble("LD", "D", 0x7E)
            z:assemble("LD", "(HL)", "D") 
            z:LD("C", "(HL)")  
            end, { ["C"]=0x7E, H=0x70, L=0x01, D=0x7E, [0x7001]=0x7E } },
    --0x4F,
    { "LD C,A", function(z) z:LD("A", 0x7f) z:LD("C", "A")  end, { ["A"]=0x7f, C=0x7f } },
    
    
    --0x50,
    { "LD D,B", function(z) z:LD("B", 0x7f) z:LD("D", "B") end, { ["B"]=0x7f, ["D"]=0x7f } },
    --0x51,
    { "LD D,C", function(z) z:LD("C", 0x7f) z:LD("D", "C") end, { ["C"]=0x7f, ["D"]=0x7f } },
    --0x52,
    { "LD D,D", function(z) z:LD("D", 0x7f) z:LD("D", "D") end, { ["D"]=0x7f } },
    --0x53,
    { "LD D,E", function(z) z:LD("E", 0x7f) z:LD("D", "E") end, { ["E"]=0x7f, ["D"]=0x7f } },
    --0x54,
    { "LD D,H", function(z) z:LD("H", 0x7f) z:LD("D", "H") end, { ["H"]=0x7f, ["D"]=0x7f } },
    --0x55,
    { "LD D,L", function(z) z:LD("L", 0x7f) z:LD("D", "L") end, { ["L"]=0x7f, ["D"]=0x7f } },
    --0x56,
    { "LD D,(HL)", function(z) 
            z:assemble("LD", "HL", 0x7001) 
            z:assemble("LD", "A", 0x7E)
            z:assemble("LD", "(HL)", "A") 
            z:LD("D", "(HL)")  
            end, { ["D"]=0x7E, H=0x70, L=0x01, A=0x7E, [0x7001]=0x7E } },
    --0x57,
    { "LD D,A", function(z) z:LD("A", 0x7f) z:LD("D", "A")  end, { ["D"]=0x7f, A=0x7f } },
    
    
    --0x58,
    { "LD E,B", function(z) z:LD("B", 0x7f) z:LD("E", "B") end, { ["B"]=0x7f, ["E"]=0x7f } },
    --0x59,
    { "LD E,C", function(z) z:LD("C", 0x7f) z:LD("E", "C") end, { ["C"]=0x7f, ["E"]=0x7f } },
    --0x5A,
    { "LD E,D", function(z) z:LD("D", 0x7f) z:LD("E", "D") end, { ["D"]=0x7f, ["E"]=0x7f } },
    --0x5B,
    { "LD E,E", function(z) z:LD("E", 0x7f) z:LD("E", "E") end, { ["E"]=0x7f } },
    --0x5C,
    { "LD E,H", function(z) z:LD("H", 0x7f) z:LD("E", "H") end, { ["H"]=0x7f, ["E"]=0x7f } },
    --0x5D,
    { "LD E,L", function(z) z:LD("L", 0x7f) z:LD("E", "L") end, { ["L"]=0x7f, ["E"]=0x7f } },
    --0x5E,
    { "LD E,(HL)", function(z) 
            z:assemble("LD", "HL", 0x7001) 
            z:assemble("LD", "D", 0x7E)
            z:assemble("LD", "(HL)", "D") 
            z:LD("E", "(HL)")  
            end, { ["E"]=0x7E, H=0x70, L=0x01, D=0x7E, [0x7001]=0x7E } },
    --0x5F,
    { "LD E,A", function(z) z:LD("A", 0x7f) z:LD("E", "A")  end, { ["A"]=0x7f, E=0x7f } },
    
    
    --0x60,
    { "LD H,B", function(z) z:LD("B", 0x7f) z:LD("H", "B") end, { ["B"]=0x7f, ["H"]=0x7f } },
    --0x61,
    { "LD H,C", function(z) z:LD("C", 0x7f) z:LD("H", "C") end, { ["C"]=0x7f, ["H"]=0x7f } },
    --0x62,
    { "LD H,D", function(z) z:LD("D", 0x7f) z:LD("H", "D") end, { ["D"]=0x7f, ["H"]=0x7f } },
    --0x63,
    { "LD H,E", function(z) z:LD("E", 0x7f) z:LD("H", "E") end, { ["E"]=0x7f, ["H"]=0x7f } },
    --0x64,
    { "LD H,H", function(z) z:LD("H", 0x7f) z:LD("H", "H") end, { ["H"]=0x7f } },
    --0x65,
    { "LD H,L", function(z) z:LD("L", 0x7f) z:LD("H", "L") end, { ["L"]=0x7f, ["H"]=0x7f } },
    --0x66,
    { "LD H,(HL)", function(z) 
            z:assemble("LD", "HL", 0x7001) 
            z:assemble("LD", "D", 0x7E)
            z:assemble("LD", "(HL)", "D") 
            z:LD("H", "(HL)")  
            end, { ["H"]=0x7E, L=0x01, D=0x7E, [0x7001]=0x7E } },
    --0x67,
    { "LD H,A", function(z) z:LD("A", 0x7f) z:LD("H", "A")  end, { ["A"]=0x7f, H=0x7f } },
    
    
    --0x68,
    { "LD L,B", function(z) z:LD("B", 0x7f) z:LD("L", "B") end, { ["B"]=0x7f, ["L"]=0x7f } },
    --0x69,
    { "LD L,C", function(z) z:LD("C", 0x7f) z:LD("L", "C") end, { ["C"]=0x7f, ["L"]=0x7f } },
    --0x6A,
    { "LD L,D", function(z) z:LD("D", 0x7f) z:LD("L", "D") end, { ["D"]=0x7f, ["L"]=0x7f } },
    --0x6B,
    { "LD L,E", function(z) z:LD("E", 0x7f) z:LD("L", "E") end, { ["E"]=0x7f, ["L"]=0x7f } },
    --0x6C,
    { "LD L,H", function(z) z:LD("H", 0x7f) z:LD("L", "H") end, { ["H"]=0x7f, ["L"]=0x7f } },
    --0x6D,
    { "LD L,L", function(z) z:LD("L", 0x7f) z:LD("L", "L") end, { ["L"]=0x7f } },
    --0x6E,
    { "LD L,(HL)", function(z) 
            z:assemble("LD", "HL", 0x7001) 
            z:assemble("LD", "D", 0x7E)
            z:assemble("LD", "(HL)", "D") 
            z:LD("L", "(HL)")  
            end, { ["L"]=0x7E, H=0x70, D=0x7E, [0x7001]=0x7E } },
    --0x6F,
    { "LD L,A", function(z) z:LD("A", 0x7f) z:LD("L", "A")  end, { ["A"]=0x7f, L=0x7F } },

    -- 0x70
    { "LD   (HL),B", function(z)
        z:assemble("LD", "HL", 0x6677) 
        z:assemble("LD", "B", 0x22)
        z:assemble("LD", "(HL)", "B") 
        end, 
        { H=0x66, L=0x77, B=0x22, [0x6677]=0x22 } },
    -- 0x71
    { "LD   (HL),C", function(z)
        z:assemble("LD", "HL", 0x6677) 
        z:assemble("LD", "C", 0x22)
        z:assemble("LD", "(HL)", "C") 
        end, 
        { H=0x66, L=0x77, C=0x22, [0x6677]=0x22 } },
    -- 0x72
    { "LD   (HL),D", function(z)
        z:assemble("LD", "HL", 0x6677) 
        z:assemble("LD", "D", 0x22)
        z:assemble("LD", "(HL)", "D") 
        end, 
        { H=0x66, L=0x77, D=0x22, [0x6677]=0x22 } },
    -- 0x73
    { "LD   (HL),E", function(z)
        z:assemble("LD", "HL", 0x6677) 
        z:assemble("LD", "E", 0x22)
        z:assemble("LD", "(HL)", "E") 
        end, 
        { H=0x66, L=0x77, E=0x22, [0x6677]=0x22 } },
    -- 0x74
    { "LD   (HL),H", function(z)
        z:assemble("LD", "HL", 0x6677) 
        z:assemble("LD", "(HL)", "H") 
        end, 
        { H=0x66, L=0x77, [0x6677]=0x66 } },
    -- 0x75
    { "LD   (HL),L", function(z)
        z:assemble("LD", "HL", 0x6677) 
        z:assemble("LD", "(HL)", "L") 
        end, 
        { H=0x66, L=0x77, [0x6677]=0x77 } },

    -- 0x76
    { "HALT", function(z) end, { PC=1 } },
    -- 0x77
    { "LD   (HL),A", function(z)
        z:assemble("LD", "HL", 0x6677) 
        z:assemble("LD", "A", 0x22)
        z:assemble("LD", "(HL)", "A") 
        end, 
        { H=0x66, L=0x77, A=0x22, [0x6677]=0x22 } },
    
    --0x78,
    { "LD A,B", function(z) z:LD("B", 0x7f) z:LD("A", "B") end, { ["B"]=0x7f, ["A"]=0x7f } },
    --0x79,
    { "LD A,C", function(z) z:LD("C", 0x7f) z:LD("A", "C") end, { ["C"]=0x7f, ["A"]=0x7f } },
    --0x7A,
    { "LD A,D", function(z) z:LD("D", 0x7f) z:LD("A", "D") end, { ["D"]=0x7f, ["A"]=0x7f } },
    --0x7B,
    { "LD A,E", function(z) z:LD("E", 0x7f) z:LD("A", "E") end, { ["E"]=0x7f, ["A"]=0x7f } },
    --0x7C,
    { "LD A,H", function(z) z:LD("H", 0x7f) z:LD("A", "H") end, { ["H"]=0x7f, ["A"]=0x7f } },
    --0x7D,
    { "LD A,L", function(z) z:LD("L", 0x7f) z:LD("A", "L") end, { ["L"]=0x7f, ["A"]=0x7f } },
    --0x7E,
    { "LD A,(HL)", function(z) 
            z:assemble("LD", "HL", 0x7001) 
            z:assemble("LD", "D", 0x7E)
            z:assemble("LD", "(HL)", "D") 
            z:LD("A", "(HL)")  
            end, { ["A"]=0x7E, H=0x70, L=0x01, D=0x7E, [0x7001]=0x7E } },
    --0x7F,
    { "LD A,A", function(z) z:LD("A", 0x7f) z:LD("A", "A")  end, { ["A"]=0x7f } },

--[[
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
--]]

-- }
--local temp_test = {

    -- 0x8F
    { "ADC A,A", function(z) z:OR("A") 
            z:LD("A", 0x00) z:assemble("ADD", "A", "A")
            z:LD("A", 0x01) z:assemble("ADC", "A", "A") end, 
        { A = 0x02, F = { "-S", "-Z", "-H", "-V", "-N", "-C" } } },
    
    { "ADC A,A carry set", function(z)
            z:LD("A", 0xFF) z:assemble("ADD", "A", "A")
            z:LD("A", 0x01) z:assemble("ADC", "A", "A") end, 
        { A = 0x03, F = { "-S", "-Z", "-H", "-V", "-N", "-C" } } },
    
    { "ADC A,A carry overflow", function(z)
            z:LD("A", 0xFF) z:assemble("ADD", "A", "A")
            z:LD("A", 0xFF) z:assemble("ADC", "A", "A") end, 
        { A = 0xFF, F = { "S", "-Z", "H", "-V", "-N", "C" } } },

    { "ADC A,A sign", function(z)
            z:LD("A", 0xFF) z:assemble("ADD", "A", "A")
            z:LD("A", 0x80) z:assemble("ADC", "A", "A") end, 
        { A = 0x01, F = { "-S", "-Z", "-H", "V", "-N", "C" } } },

    { "ADC A,A zero", function(z)
            z:LD("A", 0x00) z:assemble("ADD", "A", "A")
            z:LD("A", 0x80) z:assemble("ADC", "A", "A") end, 
        { A = 0x00, F = { "-S", "Z", "-H", "V", "-N", "C" } } },

--[[
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
    --]]
    
        -- 0xA0
    { "AND B", function(z) z:LD("A", 0x8F) z:LD("B", 0x01) z:AND("B") end, { A=0x01, B=0x01, F={"-Z", "-N", "H", "-P", "-S", "-C"} } },   -- odd number of bits = Parity clear
    { "AND B zero", function(z) z:LD("A", 0x80) z:LD("B", 0x01) z:AND("B") end, { A=0x00, B=0x01, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set
        -- 0xA1
    { "AND C", function(z) z:LD("A", 0x8F) z:LD("C", 0x01) z:AND("C") end, { A=0x01, C=0x01, F={"-Z", "-N", "H", "-P", "-S", "-C"} } },   -- odd number of bits = Parity clear
    { "AND C zero", function(z) z:LD("A", 0x80) z:LD("C", 0x01) z:AND("C") end, { A=0x00, C=0x01, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set
        -- 0xA2
    { "AND D", function(z) z:LD("A", 0x8F) z:LD("D", 0x01) z:AND("D") end, { A=0x01, D=0x01, F={"-Z", "-N", "H", "-P", "-S", "-C"} } },   -- odd number of bits = Parity clear
    { "AND D zero", function(z) z:LD("A", 0x80) z:LD("D", 0x01) z:AND("D") end, { A=0x00, D=0x01, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set
        -- 0xA3
    { "AND E", function(z) z:LD("A", 0x8F) z:LD("E", 0x01) z:AND("E") end, { A=0x01, E=0x01, F={"-Z", "-N", "H", "-P", "-S", "-C"} } },   -- odd number of bits = Parity clear
    { "AND E zero", function(z) z:LD("A", 0x80) z:LD("E", 0x01) z:AND("E") end, { A=0x00, E=0x01, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set
        -- 0xA4
    { "AND H", function(z) z:LD("A", 0x8F) z:LD("H", 0x01) z:AND("H") end, { A=0x01, H=0x01, F={"-Z", "-N", "H", "-P", "-S", "-C"} } },   -- odd number of bits = Parity clear
    { "AND H zero", function(z) z:LD("A", 0x80) z:LD("H", 0x01) z:AND("H") end, { A=0x00, H=0x01, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set
        -- 0xA5
    { "AND L", function(z) z:LD("A", 0x8F) z:LD("L", 0x01) z:AND("L") end, { A=0x01, L=0x01, F={"-Z", "-N", "H", "-P", "-S", "-C"} } },   -- odd number of bits = Parity clear
    { "AND L zero", function(z) z:LD("A", 0x80) z:LD("L", 0x01) z:AND("L") end, { A=0x00, L=0x01, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set
        -- 0xA6
--    { "AND (HL)", function(z) z:LD("A", 0x8F) z:LD("B", 0x01) z:AND("B") end, { A=0x01, H=0x01, F={"-Z", "-N", "H", "-P", "-S", "-C"} } },   -- odd number of bits = Parity clear
--    { "AND (HL) zero", function(z) z:LD("A", 0x80) z:LD("B", 0x01) z:AND("B") end, { A=0x00, H=0x01, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set
        -- 0xA6
    { "AND A", function(z) z:LD("A", 0x8F) z:AND("A") end, { A=0x8F, F={"-Z", "-N", "H", "-P", "S", "-C"} } },   -- odd number of bits = Parity clear
    { "AND A zero", function(z) z:LD("A", 0x00) z:AND("A") end, { A=0x00, F={"Z", "-N", "H", "P", "-S", "-C"} } },   -- even number of bits = Parity set

    --[[
    ["AND  (HL)"] =      0xA6,
    ["XOR  B"] =         0xA8,
    ["XOR  C"] =         0xA9,
    ["XOR  D"] =         0xAA,
    ["XOR  E"] =         0xAB,
    ["XOR  H"] =         0xAC,
    ["XOR  L"] =         0xAD,
    ["XOR  (HL)"] =      0xAE,
    ["XOR  A"] =         0xAF,
    --]]
    
    -- 0xB0
    { "OR B", function(z) z:LD("A", 0x80) z:LD("B", 0x01) z:OR("B") end, { A=0x81, B=0x01, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set
    -- 0xB1
    { "OR C", function(z) z:LD("A", 0x80) z:LD("C", 0x01) z:OR("C") end, { A=0x81, C=0x01, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set
    -- 0xB2
    { "OR D", function(z) z:LD("A", 0x80) z:LD("D", 0x01) z:OR("D") end, { A=0x81, D=0x01, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set
    -- 0xB3
    { "OR E", function(z) z:LD("A", 0x80) z:LD("E", 0x01) z:OR("E") end, { A=0x81, E=0x01, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set
    -- 0xB4
    { "OR H", function(z) z:LD("A", 0x80) z:LD("H", 0x01) z:OR("H") end, { A=0x81, H=0x01, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set
    -- 0xB6
    { "OR L", function(z) z:LD("A", 0x80) z:LD("L", 0x01) z:OR("L") end, { A=0x81, L=0x01, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set
    -- 0xB7
    { "OR (HL)", function(z) z:LD("A", 0x80) z:LD("HL", 0x6000) z:LD("(HL)", 0x01) z:OR("(HL)") end, { A=0x81, H=0x60, L=0x00, [0x6000]=0x01, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set

    --0xB7,
    -- extra checks for flags
    { "OR A", function(z) z:LD("A", 99) z:OR("A") end, { A=99, F={"-Z", "-N", "-H", "P", "-S", "-C"} } },   -- 99=0x63 even number of bits = Parity set
    { "OR A", function(z) z:LD("A", 0x99) z:OR("A") end, { A=0x99, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set
    { "OR A", function(z) z:LD("A", 0xFF) z:OR("A") end, { A=0xFF, F={"-Z", "-N", "-H", "P", "S", "-C"} } },   -- even number of bits = Parity set
    { "OR A non-negative", function(z) z:LD("A", 1) z:OR("A") end, { A=1, F={"-Z", "-N", "-H", "-P", "-S", "-C"} } }, -- odd number of bits = parity clear
    { "OR A parity even", function(z) z:LD("A", 3) z:OR("A") end, { A=3, F={"-Z", "-N", "-H", "P", "-S", "-C"} } }, -- even number of bits = parity set
    { "OR A parity odd", function(z) z:LD("A", 7) z:OR("A") end, { A=7, F={"-Z", "-N", "-H", "-P", "-S", "-C"} } },
    { "OR A zero", function(z) z:LD("A", 0) z:OR("A") end, { A=0, F={"Z", "-N", "-H", "P", "-S", "-C"} } }, -- even number of bits = parity set
--[[
    ["CP   B"] =         0xB8,
    ["CP   C"] =         0xB9,
    ["CP   D"] =         0xBA,
    ["CP   E"] =         0xBB,
    ["CP   H"] =         0xBC,
    ["CP   L"] =         0xBD,
    ["CP   (HL)"] =      0xBE,
    ["CP   A"] =         0xBF,
    ["RET  NZ"] =        0xC0,
    --]]
        
  -- 0xC1
    { "POP BC", function(z)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("LD", "HL", 0x1234)
            z:assemble("LD", "(0x6000)", "HL")
            z:assemble("POP", "BC")
        end, {SP=0x6002, H=0x12, L=0x34, B=0x12, C=0x34, [0x6000]=0x34, [0x6001]=0x12 } },
    { "POP BC (wrap)", function(z)
            z:assemble("LD", "SP", 0xFFFF)
            z:assemble("LD", "HL", 0x1234)
            z:assemble("LD", "(0xFFFF)", "HL")
            z:assemble("POP", "BC")
        end, {SP=0x1, H=0x12, L=0x34, B=0x12, C=0x34, [0xFFFF]=0x34, [0x0000]=0x12 } },
    
    --[[
    ["JP   NZ,!nn!"] =   0xC2,
    ["JP   !nn!"] =      0xC3,
    ["CALL NZ,!nn!"] =   0xC4,
    --]]
    
    -- 0xC5
    { "PUSH BC", function(z) 
            z:assemble("LD", "BC", 0x4321)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("PUSH", "BC")
            end, { B=0x43, C=0x21, SP=0x5FFE, [0x5FFE]=0x21, [0x5FFF]=0x43 } },
    { "PUSH BC wrap0", function(z) 
            z:assemble("LD", "BC", 0x4321)
            z:assemble("LD", "SP", 0x0000)
            z:assemble("PUSH", "BC")
            end, { B=0x43, C=0x21, SP=0xFFFE, [0xFFFE]=0x21, [0xFFFF]=0x43 } },
    { "PUSH BC wrap1", function(z) 
            z:assemble("NOP")       -- this will get overwritten
            z:assemble("LD", "BC", 0x4321)
            z:assemble("LD", "SP", 0x0001)
            z:assemble("PUSH", "BC")
            end, { B=0x43, C=0x21, SP=0xFFFF, [0xFFFF]=0x21, [0x0000]=0x43 } },

    --[[
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
    --]]
      -- 0xD1
    { "POP DE", function(z)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("LD", "HL", 0x1234)
            z:assemble("LD", "(0x6000)", "HL")
            z:assemble("POP", "DE")
        end, {SP=0x6002, H=0x12, L=0x34, D=0x12, E=0x34, [0x6000]=0x34, [0x6001]=0x12 } },
    { "POP DE (wrap)", function(z)
            z:assemble("LD", "SP", 0xFFFF)
            z:assemble("LD", "HL", 0x1234)
            z:assemble("LD", "(0xFFFF)", "HL")
            z:assemble("POP", "DE")
        end, {SP=0x1, H=0x12, L=0x34, D=0x12, E=0x34, [0xFFFF]=0x34, [0x0000]=0x12 } },
    
    --[[
    ["JP   NC,!nn!"] =   0xD2,
    ["OUT  (!n!),A"] =   0xD3,
    ["CALL NC,!nn!"] =   0xD4,
    --]]
    
        -- 0xD5
    { "PUSH DE", function(z) 
            z:assemble("LD", "DE", 0x4321)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("PUSH", "DE")
            end, { D=0x43, E=0x21, SP=0x5FFE, [0x5FFE]=0x21, [0x5FFF]=0x43 } },
    { "PUSH DE wrap0", function(z) 
            z:assemble("LD", "DE", 0x4321)
            z:assemble("LD", "SP", 0x0000)
            z:assemble("PUSH", "DE")
            end, { D=0x43, E=0x21, SP=0xFFFE, [0xFFFE]=0x21, [0xFFFF]=0x43 } },
    { "PUSH DE wrap1", function(z) 
            z:assemble("NOP")       -- this will get overwritten
            z:assemble("LD", "DE", 0x4321)
            z:assemble("LD", "SP", 0x0001)
            z:assemble("PUSH", "DE")
            end, { D=0x43, E=0x21, SP=0xFFFF, [0xFFFF]=0x21, [0x0000]=0x43 } },

    --[[
    ["SUB  A,!n!"] =     0xD6,
    ["RST  10H"] =       0xD7,
    ["RET  C"] =         0xD8,
    --]]
    -- 0xD9
    {"EXX", function(z)
        z:LD("HL", 0x1234)
        z:LD("DE", 0x5678)
        z:LD("BC", 0x90AB)
        z:assemble("EXX")
        z:LD("HL", 0xCDEF)
        z:LD("DE", 0x1122)
        z:LD("BC", 0x3344)
    end, { H=0xCD, L=0xEF, D=0x11, E=0x22, B=0x33, C=0x44, 
        H_=0x12, L_=0x34, D_=0x56, E_=0x78, B_=0x90, C_=0xAB} },
    --[[
    ["JP   C,!nn!"] =    0xDA,
    ["IN   A,(!n!)"] =   0xDB,
    ["CALL C,!nn!"] =    0xDC,
    ["SBC  A,!n!"] =     0xDE,
    ["RST  18H"] =       0xDF,
    ["RET  PO"] =        0xE0,
    --]]
      -- 0xE1
    { "POP HL", function(z)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("LD", "BC", 0x1234)
            z:assemble("PUSH", "BC")
            z:assemble("POP", "HL")
        end, {SP=0x6000, H=0x12, L=0x34, B=0x12, C=0x34, [0x5FFE]=0x34, [0x5FFF]=0x12 } },
    { "POP HL (wrap)", function(z)
            z:assemble("LD", "SP", 0x0001)
            z:assemble("LD", "BC", 0x1234)
            z:assemble("PUSH", "BC")
            z:assemble("POP", "HL")
        end, {SP=0x1, H=0x12, L=0x34, B=0x12, C=0x34, [0xFFFF]=0x34, [0x0000]=0x12 } },
    
    --[[
    ["JP   PO,!nn!"] =   0xE2,
    --]]
    

    -- 0xE3
    { "EX (SP), HL", function(z) 
            z:LD("SP", 0x6000)
            z:LD("HL", 0x1234)
            z:LD("A", 0xCD)
            z:LD("(0x6000)", "A")
            z:LD("A", 0xAB)
            z:LD("(0x6001)", "A")
            z:assemble("EX", "(SP)", "HL")
        end, { SP=0x6000, H=0xAB, L=0xCD, A=0xAB, [0x6000]=0x34, [0x6001]=0x12 } },
    { "EX (SP), HL  rollover", function(z) 
            z:LD("SP", 0xFFFF)
            z:LD("HL", 0x1234)
            z:LD("A", 0xCD)
            z:LD("(0xFFFF)", "A")
            z:LD("A", 0xAB)
            z:LD("(0x0000)", "A")
            z:assemble("EX", "(SP)", "HL")
        end, { SP=0xFFFF, H=0xAB, L=0xCD, A=0xAB, [0xFFFF]=0x34, [0x0000]=0x12 } },

    --[[
    ["CALL PO,!nn!"] =   0xE4,
    --]]
    
    -- 0xE5
    { "PUSH HL", function(z) 
            z:assemble("LD", "HL", 0x4321)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("PUSH", "HL")
            end, { H=0x43, L=0x21, SP=0x5FFE, [0x5FFE]=0x21, [0x5FFF]=0x43 } },
    { "PUSH HL wrap0", function(z) 
            z:assemble("LD", "HL", 0x4321)
            z:assemble("LD", "SP", 0x0000)
            z:assemble("PUSH", "HL")
            end, { H=0x43, L=0x21, SP=0xFFFE, [0xFFFE]=0x21, [0xFFFF]=0x43 } },
    { "PUSH HL wrap1", function(z) 
            z:assemble("NOP")       -- this will get overwritten
            z:assemble("LD", "HL", 0x4321)
            z:assemble("LD", "SP", 0x0001)
            z:assemble("PUSH", "HL")
            end, { H=0x43, L=0x21, SP=0xFFFF, [0xFFFF]=0x21, [0x0000]=0x43 } },

    --[[
    ["AND  !n!"] =       0xE6,
    ["RST  20H"] =       0xE7,
    ["RET  PE"] =        0xE8,
    ["JP   (HL)"] =      0xE9,
    ["JP   PE,!nn!"] =   0xEA,
    --]]
    
    -- 0xEB
    {"EX DE, HL", function(z)
        z:LD("HL", 0xCDEF)
        z:LD("DE", 0x5678)
        z:assemble("EX", "DE", "HL")
    end, { H=0xCD, L=0xEF, D=0x56, E=0x78, 
            H=0x56, L=0x78, D=0xCD, E=0xEF} },
    
    --[[
    ["CALL PE,!nn!"] =   0xEC,
    ["XOR  !n!"] =       0xEE,
    ["RST  28H"] =       0xEF,
    ["RET  P"] =         0xF0,
    --]]
      -- 0xF1
    { "POP AF", function(z)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("LD", "HL", 0x1234)
            z:assemble("LD", "(0x6000)", "HL")
            z:assemble("POP", "AF")
        end, {SP=0x6002, H=0x12, L=0x34, A=0x12, F=0x34, [0x6000]=0x34, [0x6001]=0x12 } },
    { "POP AF (wrap)", function(z)
            z:assemble("LD", "SP", 0xFFFF)
            z:assemble("LD", "HL", 0x1234)
            z:assemble("LD", "(0xFFFF)", "HL")
            z:assemble("POP", "AF")
        end, {SP=0x1, H=0x12, L=0x34, A=0x12, F=0x34, [0xFFFF]=0x34, [0x0000]=0x12 } },
    --[[
    ["JP   P,!nn!"] =    0xF2,
    --]]
    -- 0xF3
    { "DI", function(z) z:assemble("DI") end, { IFF1 = false, IFF2 = false } },
    
    --[[
    ["CALL P,!nn!"] =    0xF4,
    --]]
    -- 0xF5
    { "PUSH AF", function(z) 
            -- assumes F = 0xff
            z:assemble("LD", "A", 0x43)
            z:assemble("LD", "SP", 0x6000)
            z:assemble("PUSH", "AF")
            end, { A=0x43, F=0xFF, SP=0x5FFE, [0x5FFE]=0xFF, [0x5FFF]=0x43 } },
    { "PUSH AF wrap0", function(z) 
            z:assemble("LD", "A", 0x43)
            z:assemble("LD", "SP", 0x0000)
            z:assemble("PUSH", "AF")
            end, { A=0x43, F=0xFF, SP=0xFFFE, [0xFFFE]=0xFF, [0xFFFF]=0x43 } },
    { "PUSH AF wrap1", function(z) 
            z:assemble("NOP")       -- this will get overwritten
            z:assemble("LD", "A", 0x43)
            z:assemble("LD", "SP", 0x0001)
            z:assemble("PUSH", "AF")
            end, { A=0x43, F=0xFF, SP=0xFFFF, [0xFFFF]=0xFF, [0x0000]=0x43 } },

    --[[
    ["OR   !n!"] =       0xF6,
    ["RST  30H"] =       0xF7,
    ["RET  M"] =         0xF8,
    --]]
    
    -- 0xF9
    { "LD SP,HL", function(z) z:assemble("LD", "HL", 0x1234) z:assemble("LD", "SP", "HL") end, { H=0x12, L=0x34, SP=0x1234 } },

    --[[
    ["JP   M,!nn!"] =    0xFA,
    --]]
    -- 0xFB
    { "EI", function(z) z:assemble("EI") end, { IFF1 = true, IFF2 = true } },

    --[[
    ["CALL M,!nn!"] =    0xFC,
    ["CP   !n!"] =       0xFE,
    ["RST  38H"] =       0xFF,
--]]

}

run_batch(basic_instruction_tests)
--run_batch(temp_test)
--run_batch(CB_instruction_tests)
--run_batch(ED_instruction_tests)
--run_batch(DD_instruction_tests)
--run_batch(FD_instruction_tests)
--run_batch(DDCB_instruction_tests)
--run_batch(FDCB_instruction_tests)
--run_batch(memory_invalidation_tests)
--run_batch(more_advanced_tests)
--run_batch(interrupt_test)
--run_batch(R_reg_test)
--run_batch(Carry_Flag_seperate_from_F_flag_interactions_test)
