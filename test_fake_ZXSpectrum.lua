-- Load the Spectrum ROM and run through the Z80 emulator.
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

dofile("lua_z80.lua")


function run_Spectrum()
    local jit = Z80JIT:new()
    local f=io.open("ROMs/spectrum.rom", "rb")
    if not f then
        print("Spectrum ROM not loaded")
        print("Did you download it?")
        print("This script assumes it's located as 'ROMs/spectrum.rom'")
        print("ABORTING!")
        return
    end
    local code = f:read("*all")
    f:close()
    jit:load_memory(code, 0)
    jit:make_ROM(0,16384)
    jit:make_RAM(16384,16384)
    local cpu = Z80CPU:new()
    local status
    repeat
        status = jit:run_z80(cpu, cpu.PC)
    until status ~= "ok"
    print()
    print("Status =",status)    
    print(string.format("Next Address = 0x%x", cpu.PC))
    if status == "undefined" then
        local pc = cpu.PC - 1
        local data = jit:fetch_memory_string(pc, 5)
        local b1,b2,b3,b4,b5 = data:byte(1,5)
        print(string.format("%04x:%x %x %x %x %x", pc, b1,b2,b3,b4,b5))
    end
end

run_Spectrum()