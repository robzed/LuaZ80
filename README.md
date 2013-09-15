LuaZ80
======

A Lua Z80 dynamic (binary) translator (similar to a JIT compiler) which translates Z80 to Lua and runs the result.

**WARNING** This software is not complete, but is a work-in-progress proof of concept.

The Z80 is an 8-bit microprocessor was used in many popular computers in the 1980's including the ZX80, ZX81, ZX Spectrum, Amstrad CPC 464 and TRS-80 Model 1.

Lua is a popular scripting language that where the standard interpreter is one of the faster dynamic programming language interpreters. (There is also an excellent Lua JIT native compiler - although this project hasn't been tested against that yet.)

A dynamic translator means that it could be as fast as Lua, at least in theory, rather than an Z80 interpreter running on top of a Lua interpreter (or compiler). However, see docs/technical.txt for current limiting factors on the speed.

Files
=====

- lua_z80.lua - This is the main translator.
- lua_z80_test.lua - This is the work in progress test file.
- z80_ss_debug.lua - Single Steps Z80 code via the translator.
- test_fake_ZXSpectrum.lua - At some point this will run the ZX Spectrum ROM though the emulator for testing.
- Z80_disassembler.lua - Used by the single step debugger to disassemble Z80.
- Z80_assembler.lua - Start of an assembler-in-Lua-code used for testing only.

Using it
========
You'll need Lua 5.2. I assume you know how to run a file from lua - it's basically
    lua filename.lua

I suggest running 'lua_z80_test.lua'. This will create some standard code and run it via a simple Z80 debugger. (The Z80 debugger is used if Z80_debugger_enabled is set to true at the top of the file). NOTE: The Z80 debugger issues VT100/ANSI escape sequenes to position the cursor. You might want to change this for some platforms.

If you have a Lua debugger (say the one in the excellent [ZeroBrane Studio][1] or [Eclipse LDT][2]) then you may single step the Lua code to get a good idea how the code works. Changing Z80_debugger_enabled to false at the top of 'lua_z80_test.lua' is a good idea, because this will just run the code, and avoid you have to step through the Z80 debugger code and avoid it emitting VT100/ANSI escape sequences and/or reading commands on every instruction.

[1]: http://studio.zerobrane.com "ZeroBrane Studio"
[2]: http://www.eclipse.org/koneki/ldt/ "Eclipse LDT"

More information
================

Source can be found here http://github.com/robzed/LuaZ80

Technical information can be found in docs/technical.txt

My site http://robprobin.com has some more information on the LuaZ80 and other 
some notes on a few other Z80 emulators on the LuaZ80 page.

Authors, Copyright and Licensing
=================================
Written by Rob Probin. Started June 2013. (All original work.)

Copyright (c) 2013 Rob Probin

For licensing see individual Lua files and LICENSE. 


