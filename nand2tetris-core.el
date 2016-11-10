;;; nand2tetris-core.el --- Major mode for HDL files in the nand2tetris course
;;; https://www.coursera.org/course/nand2tetris1

;; Copyright (C) 2015 Diego Berrocal

;; Author: Diego Berrocal <cestdiego@gmail.com>
;; Created: 31 August 2015

;; Keywords: nand2tetris, hdl
;; Homepage: http://www.github.com/CestDiego/nand2tetris.el/

;; This file is not part of GNU Emacs.

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Commentary:

;; Useful functions to make following the Nand2Tetris course easier.
;; See: https://www.coursera.org/course/nand2tetris1

;;; Code:
(defgroup nand2tetris nil
  "Major Mode for HDL files in (the) Nand2Tetris Course"
  :group 'nand2tetris)

(defcustom nand2tetris-core-base-dir "~/Downloads/nand2tetris"
  "Source directory where nadn2tetris has been downloaded."
  :group 'nand2tetris)

(defconst nand2tetris-core-builtin-chips
  '(("Add16" . (("description" .
                 "16-bit bitwise And:

IN a[16], b[16];
OUT out[16];

for i = 0..15: out[i] = (a[i] and b[i])")
                ("spec" . "Add16(a= ,b= ,out= )")))
    ("ALU" . (("description" .
               " The ALU (Arithmetic Logic Unit).

IN
    x[16], y[16],  // 16-bit inputs
    zx, // zero the x input?
    nx, // negate the x input?
    zy, // zero the y input?
    ny, // negate the y input?
    f,  // compute out = x + y (if 1) or x & y (if 0)
    no; // negate the out output?

OUT
    out[16], // 16-bit output
    zr, // 1 if (out == 0), 0 otherwise
    ng; // 1 if (out < 0),  0 otherwise

Computes one of the following functions:
x+y, x-y, y-x, 0, 1, nand2tetris-core-1, x, y, nand2tetris-core-x, nand2tetris-core-y, !x, !y,
x+1, y+1, x-1, y-1, x&y, x|y on two 16-bit inputs,
according to 6 input bits denoted zx,nx,zy,ny,f,no.
In addition, the ALU computes two 1-bit outputs:
if the ALU output == 0, zr is set to 1; otherwise zr is set to 0;
if the ALU output < 0, ng is set to 1; otherwise ng is set to 0.

Implementation: the ALU logic manipulates the x and y inputs
and operates on the resulting values, as follows:
if (zx == 1) set x = 0        // 16-bit constant
if (nx == 1) set x = !x       // bitwise not
if (zy == 1) set y = 0        // 16-bit constant
if (ny == 1) set y = !y       // bitwise not
if (f == 1)  set out = x + y  // integer 2's complement addition
if (f == 0)  set out = x & y  // bitwise and
if (no == 1) set out = !out   // bitwise not
if (out == 0) set zr = 1
if (out < 0) set ng = 1
")
              ("spec" . "ALU(x= ,y= ,zx= ,nx= ,zy= ,ny= ,f= ,no= ,out= ,zr= ,ng= )")))
    ("And16" .  (("description" .
                  "16-bit bitwise And:

IN a[16], b[16];
OUT out[16];

for i = 0..15: out[i] = (a[i] and b[i])")
                 ("spec" . "And16(a= ,b= ,out= )")))
    ("And" . (("description" .
               "And gate:

IN a, b;
OUT out;

out = 1 if (a == 1 and b == 1)
      0 otherwise")
              ("spec" . "And(a= ,b= ,out= )")))
    ("ARegister" . (("description" .
                     "A 16-Bit register called \"A Register\".

IN  in[16], load;
OUT out[16];

BUILTIN ARegister;
CLOCKED in, load;

If load[t-1]=1 then out[t] = in[t-1]
else out does not change (out[t] = out[t-1])

This built-in chip implementation has the side effect of
providing a GUI representation of a 16-bit register
called \"A register\" (typically used to store an address)")
                    ("spec" . "ARegister(in= ,load= ,out= )")))
    ("Bit" . (("description" .
               "1-bit register:

IN in, load;
OUT out;

If load[t] == 1 then out[t+1] = in[t]
                else out does not change (out[t+1] = out[t])")
              ("spec" . "Bit(in= ,load= ,out= )")))
    ("CPU" . (("description" .
               " The Hack CPU (Central Processing unit)

IN  inM[16],         // M value input  (M = contents of RAM[A])
    instruction[16], // Instruction for execution
    reset;           // Signals whether to re-start the current
                        // program (reset==1) or continue executing
                        // the current program (reset==0).

OUT outM[16],        // M value output
    writeM,          // Write to M?
    addressM[15],    // Address in data memory (of M)
    pc[15];          // address of next instruction

Consisting of an ALU,two registers named A and D, and a program
counter named PC. The CPU is designed to fetch and execute
instructions written in the Hack machine language. In particular,
functions as follows: Executes the inputted instruction according
to the Hack machine language specification. The D and A in the
language specification refer to CPU-resident registers, while M
refers to the external memory location addressed by A, i.e. to
Memory[A]. The inM input holds the value of this location. If the
current instruction needs to write a value to M, the value is
placed in outM, the address of the target location is placed in
the addressM output, and the writeM control bit is
asserted. (When writeM==0, any value may appear in outM). The
outM and writeM outputs are combinational: they are affected
instantaneously by the execution of the current instruction. The
addressM and pc outputs are clocked: although they are affected
by the execution of the current instruction, they commit to their
new values only in the next time step. If reset==1 then the CPU
jumps to address 0 (i.e. pc is set to 0 in next time step) rather
than to the address resulting from executing the current
instruction.")
              ("spec" . "CPU(inM= ,instruction= ,reset= ,outM= ,writeM= ,addressM= ,pc= )")))
    ("DFF" . (("description" .
               "Data Flip-flop:

IN  in;
OUT out;

out(t) = in(t-1) 
where t is the current time unit, or clock cycle.")
              ("spec" . "DFF(in= ,out= )")))
    ("DMux4Way" . (("description".
                    "4-way demultiplexor:

IN in, sel[2];
OUT a, b, c, d;

{a, b, c, d} = {in, 0, 0, 0} if sel == 00
               {0, in, 0, 0} if sel == 01
               {0, 0, in, 0} if sel == 10
               {0, 0, 0, in} if sel == 11")
                   ("spec" . "DMux4Way(in= ,sel= ,a= ,b= ,c= ,d= )")))
    ("DMux8Way" . (("description" .
                    "8-way demultiplexor:

IN in, sel[3];
OUT a, b, c, d, e, f, g, h;

{a, b, c, d, e, f, g, h} = {in, 0, 0, 0, 0, 0, 0, 0} if sel == 000
                           {0, in, 0, 0, 0, 0, 0, 0} if sel == 001
                           etc.
                           {0, 0, 0, 0, 0, 0, 0, in} if sel == 111")
                   ("spec" . "DMux8Way(in= ,sel= ,a= ,b= ,c= ,d= ,e= ,f= ,g= ,h= )")))
    ("DMux" . (("description" .
                "Demultiplexor:

IN in, sel;
OUT a, b;

{a, b} = {in, 0} if sel == 0
         {0, in} if sel == 1")
               ("spec" . "DMux(in= ,sel= ,a= ,b= )")))
    ("DRegister" . (("description" .
                   " A 16-Bit register called \"D Register\"

IN  in[16], load;
OUT out[16];

If load[t-1]=1 then out[t] = in[t-1]
else out does not change (out[t] = out[t-1])

This built-in chip implementation has the side effect of
providing a GUI representation of a 16-bit register
called \"D register\" (typically used to store data).")
                    ("spec" . "DRegister(in= ,load= ,out= )")))
    ("FullAdder" . (("description"
                     "FullAdder Chip:

IN a, b, c;  // 1-bit inputs
OUT sum,     // Right bit of a + b + c
    carry;   // Left bit of a + b + c

Computes the sum of three bits.")
                    ("spec" . "FullAdder(a= ,b= ,c= ,sum= ,carry= )")))
    ("HalfAdder" . (("description" .
                     "HafAdder Chip:
IN a, b;    // 1-bit inputs
OUT sum,    // Right bit of a + b 
    carry;  // Left bit of a + b

Computes the sum of two bits.")
                    ("spec" . "HalfAdder(a= ,b= ,sum= , carry= )")))
    ("Inc16" . (("description" .
                 "Inc16 Chip:

IN in[16];
OUT out[16];


16-bit incrementer:
out = in + 1 (arithmetic addition)")
                ("spec" . "Inc16(in= ,out= )")))
    ("Keyboard" . (("description" .
                    "The keyboard (memory map).

OUT out[16];   // The ASCII code of the pressed key,
               // or 0 if no key is currently pressed,
               // or one the special codes listed in Figure 5.5.

Outputs the code of the currently pressed key.

The built-in chip implementation has two side effects supplied
by the simulator. First, the keyboard memory map is continuously
being refreshed from the physical keyboard unit. Second, it
displays a keyboard icon and data entry GUI.")
                   ("spec" . "Keyboard(out= )")))
    ("Memory" . (("description" .
                 "The Memory:

IN in[16], load, address[15];
OUT out[16];

The complete address space of the Hack computer's memory,
including RAM and memory-mapped I/O.
The chip facilitates read and write operations, as follows:
    Read:  out(t) = Memory[address(t)](t)
    Write: if load(t-1) then Memory[address(t-1)](t) = in(t-1)
In words: the chip always outputs the value stored at the memory
location specified by address. If load==1, the in value is loaded
into the memory location specified by address. This value becomes
available through the out output from the next time step onward.
Address space rules:
Only the upper 16K+8K+1 words of the Memory chip are used.
Access to address>0x6000 is invalid. Access to any address in
the range 0x4000-0x5FFF results in accessing the screen memory
map. Access to address 0x6000 results in accessing the keyboard
memory map. The behavior in these addresses is described in the
Screen and Keyboard chip specifications given in the book.")
                 ("spec" . "Memory(in= ,load= ,address= ,out= )")))
    ("Mux16" . (("description" .
                 "16-bit multiplexor:

IN a[16], b[16], sel;
OUT out[16];

for i = 0..15 out[i] = a[i] if sel == 0
                       b[i] if sel == 1")
                ("spec" . "Mux16(a= ,b= ,sel= ,out= )")))
    ("Mux4Way16" . (("description" .
                     "4-way 16-bit multiplexor:

IN a[16], b[16], c[16], d[16], sel[2];
OUT out[16];

out = a if sel == 00
      b if sel == 01
      c if sel == 10
      d if sel == 11")
                    ("spec" . "Mux4Way16(a= ,b= ,c= ,d= ,sel= ,out= )")))
    ("Mux8Way16" . (("description" .
                     "8-way 16-bit multiplexor:

IN a[16], b[16], c[16], d[16],
    e[16], f[16], g[16], h[16],
    sel[3];
OUT out[16];

out = a if sel == 000
      b if sel == 001
      etc.
      h if sel == 111")
                    ("spec" . "Mux8Way16(a= ,b= ,c= ,d= ,e= ,f= ,g= ,h= ,sel= ,out= )")))
    ("Mux" . (("description" .
               "Multiplexor:

IN a, b, sel;
OUT out;

out = a if sel == 0
      b otherwise")
              ("spec" . "Mux(a= ,b= ,sel= ,out= )")))
    ("Nand" . (("description" .
                "Nand gate:

IN  a, b;
OUT out;

out = a Nand b.")
               ("spec" . "Nand(a= ,b= ,out= )")))
    ("Not16" . (("description" .
                 "16-bit Not:

IN in[16];
OUT out[16];

for i=0..15: out[i] = not in[i]")
                ("spec" . "Not16(in= ,out= )")))
    ("Not" . (("description" .
               "Not gate:

IN in;
OUT out;

out = not in")
              ("spec" . "Not(in= ,out= )")))
    ("Or16" . (("description" .
                "16-bit bitwise Or:

IN a[16], b[16];
OUT out[16];

for i = 0..15 out[i] = (a[i] or b[i])")
               ("spec" . "Or16(a= ,b= ,out= )")))
    ("Or8Way" . (("description" .
                  "8-way Or: 

IN in[8];
OUT out;

out = (in[0] or in[1] or ... or in[7])")
                 ("spec" . "Or8Way(in= ,out= )")))
    ("Or" . (("description" . "Or gate:

IN  a, b;
OUT out;

out = 1 if (a == 1 or b == 1)
        0 otherwise")
             ("spec" . "Or(a= ,b= ,out= )")))
    ("PC" . (("description" .
              "PC Chip:

IN in[16],load,inc,reset;
OUT out[16];

A 16-bit counter with load and reset control bits.
if      (reset[t] == 1) out[t+1] = 0
else if (load[t] == 1)  out[t+1] = in[t]
else if (inc[t] == 1)   out[t+1] = out[t] + 1  (integer addition)
else                    out[t+1] = out[t]")
             ("spec" . "PC(in= ,load= ,inc= ,reset= ,out= )")))
    ("RAM16K" . (("description" .
                  "RAM16K Chip

IN in[16], load, address[14];
OUT out[16];

Memory of 16K registers, each 16 bit-wide. Out holds the value
stored at the memory location specified by address. If load==1,
then the in value is loaded into the memory location specified by
address \(the loaded value will be emitted to out from the next
time step onward\).")
                 ("spec" . "RAM16K(in= ,load= ,address= ,out= )")))
    ("RAM4K" . (("description" .
                 "RAM4K Chip:

IN in[16], load, address[12];
OUT out[16];

Memory of 4K registers, each 16 bit-wide. Out holds the value
stored at the memory location specified by address. If load==1, then
the in value is loaded into the memory location specified by address
\(the loaded value will be emitted to out from the next time step onward\).")
                ("spec" . "RAM4K(in= ,load= ,address= ,out= )")))
    ("RAM512" . (("description" .
                  "RAM512 Chip

IN in[16], load, address[9];
OUT out[16];

Memory of 512 registers, each 16 bit-wide. Out holds the value
stored at the memory location specified by address. If load==1, then
the in value is loaded into the memory location specified by address
\(the loaded value will be emitted to out from the next time step onward\).")
                 ("spec" . "RAM512(in= ,load= ,address= ,out= )")))
    ("RAM64" . (("description" .
                 "RAM64 Chip:

IN in[16], load, address[6];
OUT out[16];

Memory of 64 registers, each 16 bit-wide. Out holds the value
stored at the memory location specified by address. If load==1, then
the in value is loaded into the memory location specified by address
\(the loaded value will be emitted to out from the next time step onward\).")
                ("spec" . "RAM64(in= ,load= ,address= ,out= )")))
    ("RAM8" . (("description" .
                "RAM8 Chip:

IN in[16], load, address[3];
OUT out[16];

Memory of 8 registers, each 16 bit-wide. Out holds the value
stored at the memory location specified by address. If load==1, then
the in value is loaded into the memory location specified by address
\(the loaded value will be emitted to out from the next time step onward\).")
               ("spec" . "RAM8(in= ,load= ,address= ,out= )")))
    ("Register" . (("description" .
                    "16-bit register:
IN in[16], load;
OUT out[16];

If load[t] == 1 then out[t+1] = in[t]
else out does not change")
                   ("spec" . "Register(in= ,load= ,out= )")))
    ("ROM32K" . (("description" .
                  "ROM32K Chip:

IN  address[15];
OUT out[16];

Read-Only memory (ROM) of 16K registers, each 16-bit wide.
The chip is designed to facilitate data read, as follows:
    out(t) = ROM32K[address(t)](t)
In words: the chip always outputs the value stored at the
memory location specified by address.

The built-in chip implementation has a GUI side-effect,
showing an array-like component that displays the ROM's
contents. The ROM32K chip is supposed to be pre-loaded with
a machine language program. To that end, the built-in chip
implementation also knows how to handle the \"ROM32K load Xxx\"
script command, where Xxx is the name of a text file containing
a program written in the Hack machine language.  When the
simulator encounters such a command in a test script, the code
found in the file is loaded into the simulated ROM32K unit.")
                 ("spec" . "ROM32K(address= ,out= )")))
    ("Screen" . (("description" .
                  "The Screen (memory map).

IN  in[16],        // what to write
    load,          // write-enable bit
    address[13];   // where to read/write
OUT out[16];       // Screen value at the given address

Functions exactly like a 16-bit 8K RAM:
   1. out(t)=Screen[address(t)](t)
   2. If load(t-1) then Screen[address(t-1)](t)=in(t-1)

The built-in chip implementation has the side effect of continuously
refreshing a visual 256 by 512 black-and-white screen, simulated
by the simulator. Each row in the visual screen is represented
by 32 consecutive 16-bit words, starting at the top left corner
of the visual screen. Thus the pixel at row r from the top and
column c from the left (0<=r<=255, 0<=c<=511) reflects the c%16
bit (counting from LSB to MSB) of the word found in
Screen[r*32+c/16].")
                 ("spec" . "Screen(in= ,load= ,address= ,out= )")))
    ("Xor" . (("description" .
               "Exclusive-or gate:

IN a, b;
OUT out;

out = !(a == b).")
              ("spec" . "Xor(a= ,b= ,out= )"))))
  "Built In Chips Alist.")

(provide 'nand2tetris-core)
;;; nand2tetris-core.el ends here
