LuaZ80
======

A Lua Z80 dynamic (binary) translator (similar to a JIT compiler) which translates Z80 to Lua and runs the result.

**WARNING** This software is not complete, but is a work-in-progress proof of concept. Not all op-codes are implemented and it will not work properly. 
However some are working (e.g. those used in lua_z80_test.lua). 
For a more complete list see z80_regression_test.lua. 


Background
==========

The Z80 is an 8-bit microprocessor was used in many popular computers in the 1980's including the ZX80, ZX81, ZX Spectrum, Amstrad CPC 464 and TRS-80 Model 1.

Lua is a popular scripting language that where the standard interpreter is one of the faster dynamic programming language interpreters. (There is also an excellent Lua JIT native compiler - although this project hasn't been tested against that yet.)

A dynamic translator means that it could be as fast as Lua, at least in theory, rather than an Z80 interpreter running on top of a Lua interpreter (or compiler). However, see [docs/technical.txt][3] for current limiting factors on the speed.

There are a couple of places that Lua has been used to make Z80-style emulators:
- A Z80/Spectrum emulator written in Lua: https://github.com/ignacio/luagleck
- Also see the emulator in [3]

[3]: https://github.com/robzed/LuaZ80/blob/master/docs/technical.txt

Main Files
==========

- lua_z80.lua - This is the main translator.
- lua_z80_test.lua - This is the work in progress test file.
- z80_ss_debug.lua - Single Steps Z80 code via the translator.
- test_fake_ZXSpectrum.lua - At some point this will run the ZX Spectrum ROM though the emulator for testing.
- Z80_disassembler.lua - Used by the single step debugger to disassemble Z80.
- Z80_assembler.lua - Start of an assembler-in-Lua-code used for testing only.
- z80_regression_test.lua - Start of a file that tests all instructions to some basic level.

Latest Changes
==============

I've been working on z80_regression_test.lua to prove existing instruction work
and providing some new instructions as well.


What works
==========

Nothing properly :-) I'm certainly after some more help - this is very much a side-side-side project for me. But bit-by-bit it will get better.


I'm creating a more up to date status in docs/status.md but in summary: 

- Quite a few op-codes. (All single byte, all CBxx 2 byte, and more ... see z80_regression_test.lua)
- A basic debugger/monitor (similar to the old HiSoft DevPac Monitor in some ways)
- The disassembler (but there might be bugs or gaps)
- The parts of a crude assembler that can be used to assemble Z80 mnemonics from Lua.
- Regression test infrastructure)

All of this is written in pure Lua.




Using it
========
You'll need Lua 5.2. I assume you know how to run a file from lua - it's basically:

    lua filename.lua

You can run the unit tests by running the file 'z80_regression_test.lua'. Some might be disabled temporarly for speed - see the bottom of that file. There is usually more than one test per instruction.

I also suggest running 'lua_z80_test.lua'. This will create some standard code and run it via a simple Z80 debugger. (The Z80 debugger is used if Z80_debugger_enabled is set to true at the top of the file). NOTE: The Z80 debugger issues VT100/ANSI escape sequenes to position the cursor. You might want to change this for some platforms. For debugger help type ? or help in the debugger.

If you have a Lua debugger (say the one in the excellent [ZeroBrane Studio][1] or [Eclipse LDT][2]) then you may single step the Lua code to get a good idea how the code works. Changing Z80_debugger_enabled to false at the top of 'lua_z80_test.lua' is a good idea, because this will just run the code, and avoid you have to step through the Z80 debugger code and avoid it emitting VT100/ANSI escape sequences and/or reading commands on every instruction.

[1]: http://studio.zerobrane.com "ZeroBrane Studio"
[2]: http://www.eclipse.org/koneki/ldt/ "Eclipse LDT"

Trying to run Spectrum ROM
==========================

'lua test_fake_ZXSpectrum.lua' will try to run the ROM of the spectrum. It expects the ROM to be in ROMs/spectrum.rom; if it doesn't find it, it will tell you. Because not all op-codes are implemented, this won't yet be successful. But with the debugger the first one or two can be stepped.

I'll add more information about how to do that at some point here.

Documentation
=============
* This README.md file!
* Technical *background* information can be found in [docs/technical.txt](https://github.com/robzed/LuaZ80/blob/master/docs/technical.txt)
* Current status information about what works and what doesn't can be found in [docs/status.md](https://github.com/robzed/LuaZ80/blob/master/docs/status.md)
* Some information on the debugger/monitor can be found in [docs/debugger_manual.md](https://github.com/robzed/LuaZ80/blob/master/docs/debugger_manual.md)
* [docs/basic_file_usage.md](https://github.com/robzed/LuaZ80/blob/master/docs/basic_file_usage.md) supplies some more information on the basic file information about each of the main Lua files in the project.

More information
================

Source can be found here http://github.com/robzed/LuaZ80

My site http://robprobin.com has some more information on the LuaZ80 and other 
some notes on a few other Z80 emulators on the LuaZ80 page.

Authors, Copyright and Licensing
=================================
Written by Rob Probin. Started June 2013. (All original work.)

Copyright (c) 2013-2015 Rob Probin

For licensing see individual Lua files and LICENSE. 


