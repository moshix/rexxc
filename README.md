REXXC - A REXX compiler in REXX
===============================


This compiler compiles generic REXX into either either x86 or mainframe assembler.

It can be run either on MVS 3.8 TK4- with the BREXX interpreter 2.5 and up. Or it can be run with Regina REXX on Linux, Macos, Windows or any other place where Regina is available. 

REXXC will output either MVS assembler (Asssembler XF for S/370) or x86 assembler that can be compiled with NASM or FASM. It is a goal to eventually also output HLASM assembler to allow users to compile rexx code on later version of MVS. 

The full legal syntax of REXX is supported. ADDRESS options will be dealt with seperately. 

by PETERJ & MOSHIX

April 2022
