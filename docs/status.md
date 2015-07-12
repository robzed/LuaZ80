LuaZ80 Status Information
=========================
Last updated by Rob Probin

lua_z80.lua
===========
This is the main translator.

Done
----

Not Done
---------



lua_z80_test.lua
================
This is the work in progress test file.


z80_ss_debug.lua
================
Single Steps Z80 code via the translator.

Done
----
- Basic stepping, register and memory view and disassembly.
- Can show Lua code as well as Z80 disassembly.
- Colour codes memory view.

Not Done
---------
- Breakpoints at a Z80 address would probably be useful.
- Running until we have an unrecognised Z80 intrustruction might be useful?
- Listing a Z80 address and showing the Lua code might be useful.
- A more complex disassembly view migth be useful.


test_fake_ZXSpectrum.lua
========================
At some point this will run the ZX Spectrum ROM though the emulator for testing.

Not Done
---------
- Provide Spectrum-like peripherals (maybe via Lua framework like Löve 2D?)


Z80_disassembler.lua
====================
Used by the single step debugger to disassemble Z80.


Z80_assembler.lua
=================
Start of an assembler-in-Lua-code used for testing only.
