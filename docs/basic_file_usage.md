Basic File Usage for LuaZ80 Files
==============================
Last updated by Rob Probin



lua_z80_test.lua
================
This is the work in progress test file.

Can be run directly by:
    lua lua_z80_test.lua

The the top of file, this line will enable or disable debugging with the debugger.
    Z80_debugger_enabled = true

Gives some basic idea of how the assembler and LuaZ80 translator work from a Lua code perspective.


test_fake_ZXSpectrum.lua
========================
At some point this will run the ZX Spectrum ROM though the emulator for testing.

A relatively simply file that loads a binary image of the Spectrium ROM and tries to execute it inside the LuaZ80 Translator.

At some point we need to provide fake peripherals for the Spectrum (maybe use a game framework like Löve 2D, but until we have sufficient op-codes working, it's rather  hypothetical.)


z80_ss_debug.lua
================
Single Steps Z80 code via the translator.

See 'docs/debugger_manual.md' for usage focused information.

It wraps the just-in-time translator and has a similar interface. 

    require("z80_ss_debug")
    
    local engine = jit
    if debug_on then
        engine = Z80_SS_DEBUG(cpu, jit)
    end

Where jit is the Z80JIT object and cpu is the Z80CPU object. 

Z80_SS_DEBUG uses ANSI attributes for highlighting various items on screen.


lua_z80.lua
===========
This is the main translator.

The public interface is currently split into two halves:

- Z80JIT does the on the fly compilation when running a block. In order to do this successfully, it also needs to represent the system memory
- Z80CPU provides a model of Z80 registers and not much more.


Internally there are several other classes and functions that are not intended to be used externally.

- A Z80Lump object is a chunk of 'compiled' (into Lua) Z80 machine code.
- The function z80_compile(memory, address, number_number_instructions_to_compile, pre_code) generates Lua code from Z80 binary op-codes. It's not intended to be user accessible and instead is called from Z80JIT. Produces a Z80Lump.
- The function decode_instruction(memory, address, instruction_table) compiles a single Z80 instruction. Used by z80_compile().

Because of forward and backward branches, varous patching needs to be done to instructions after each instruction is compiled - in a similar way to an assembler or other code generator.

A few areas where is there is quite a lot of work above running the basic operation of the instruction:
* Flags - these are tricky at the best of times with Z80. Quite a few emulators have bugs here still.
* Memory invalidation - where produced code is invalidated by writes to code-space.
* Memory write allow (think ROM vs. RAM).

We don't optimise host memory usage in this translator - we instead assume speed is the most critical aspect. Even then, not everything has been done to speed the operation of the translator - in order to make something that works.


    <<<add more information here>>>


Z80_disassembler.lua
====================
Used by the single step debugger to disassemble Z80.

Supplies two functions:
 - single_Z80_disassemble(memory, start)
 - multiple_Z80_disassemble(memory, start, number_of_instructions)

For multiple_Z80_disassemble() a memory block that has 'bad instructions' might result in misalignment of subsequent instructions or incorrect decoding.


Z80_assembler.lua
=================
Start of an assembler-in-Lua-code used for testing only.

This can be used to assemble Z80 assembler mnemonics inside Lua code. An example of this is (this is valid Lua code that has required the assembler).

    require("Z80_assembler")
    
    z = Z80_Assembler()
    z:LD("SP", 0)
    z:LD("HL", 0x1234)
    z:PUSH("HL")
    z:DS("fred", "hello")
    z:LD_A(255)
    z:LD_A(65)            -- the character 'A"
    z:DB(0xED, 0xED)      -- internal lua_z80 print-to-console instruction
    if not z:any_errors() then
        code = z:get_code()

For further functions that can be used, see the first few and last few definitions of 'Z80_assembler.lua'.

    <<<add more information about funtions supplied as part of Z80_Assembler class here>>>

