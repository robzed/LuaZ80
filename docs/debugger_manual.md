LuaZ80 Debugger Manual
=========================
By Rob Probin

Introduction
===========

This debugger was written *specifically* get the main lua z80 translator working. Therefore it's pretty minimal in regards to functionality. Hopefully it will grow it's humble beginnings - but at the moment it's focused on getting the main Z80-to-Lua translator working.


Supported Commands
=================

* ? - show help summary
* help - show help summary
* s - step an instruction. (This will step into a branch or call)
* step => same as s
* q or quit or exit => quit the monitor entirely back to command line.
* list n => show Lua source around line.
* mem address => show memory at address
* dis address => disassemble Z80 address
* cls => clear the screen. You might need to do this in order to clean up from things like help.


Display
=======

The following shows a typical display:

    PC*0000 21 05 00 34 3E 41 ED ED
    SP 0000 21 05 00 34 3E 41 ED ED
    IY 0000 21 05 00 34 3E 41 ED ED
    IX 0000 21 05 00 34 3E 41 ED ED
    HL 0000 21 05 00 34 3E 41 ED ED
    DE 0000 21 05 00 34 3E 41 ED ED
    BC 0000 21 05 00 34 3E 41 ED ED
    AF 0000              (raw _F) CPU.Carry = 0
    AF 0000              (get_F)
    
    PC Disassembly = LD   HL,0005H
    
    Lump=instance of class Z80Lump Z80Start: 0000
    Lua status = ok
    Lua Line: 3

The top is the registers, their contents and the data at the address of the register pairs.

    <<<Need to add more here>>>


Stepping
========

    <<< add info on this here >>>


Memory View
===========

This has colour coding as follows:

    <<<complete this section>>>


Lua Listing
===========
    <<< add info here >>>


