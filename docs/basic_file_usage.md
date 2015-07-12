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

<<<add more information here>>>


lua_z80.lua
===========
This is the main translator.

<<<add more information here>>>

Z80_disassembler.lua
====================
Used by the single step debugger to disassemble Z80.

<<<add more information here>>>


Z80_assembler.lua
=================
Start of an assembler-in-Lua-code used for testing only.

<<<add more information here>>>

