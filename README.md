LuaZ80
======

A Lua Z80 dynamic (binary) translator (similar to a JIT compiler) which translates Z80 to Lua and runs the result.

Written by Rob Probin. (All original work.)
Started June 2013.
Copyright (c) 2013 Rob Probin
For licensing see the individual Lua files (in summary GPL v2.)

**WARNING**
This software is not complete, but is a work in progress proof of concept.


Files
=====

lua_z80.lua - This is the main translator
lua_z80_test.lua - This is the work in progress test file.
z80_ss_debug.lua - Single Steps Z80 code via the translator.
test_fake_ZXSpectrum.lua - At some point this will run the ZX Spectrum ROM though the emulator for testing.
Z80_disassembler.lua - Used by the single step debugger to disassemble Z80.
Z80_assembler.lua - Start of assembler-in-Lua-code used for testing only.

