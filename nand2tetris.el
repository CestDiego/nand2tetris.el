;;; nand2tetris.el --- Major mode for HDL files in the nand2tetris course
;;; https://www.coursera.org/course/nand2tetris1

;; Copyright (C) 2015 Diego Berrocal

;; Author: Diego Berrocal <cestdiego@gmail.com>
;; Created: 10 August 2015

;; Keywords: nand2tetris, hdl
;; Homepage: http://www.github.com/CestDiego/nand2tetris.el/
;; Version: 0.0.1
;; Package-Requires: ((company "0.5") (cl-lib "0.5.0"))

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions to make following the coursera course easier.

;;; Code:

(defvar nand2tetris-source-dir nil
  "Source directory where nadn2tetris has been downloaded.")

(defconst nand2tetris-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table))


;;; Scripts Integration
(defvar nand2tetris-tools-dir
  (expand-file-name "tools" nand2tetris-source-dir))

(setq
 nand2tetris-hardware-simulator (expand-file-name "HardwareSimulator.sh" nand2tetris-tools-dir)
 nand2tetris-assembler (expand-file-name "Assembler.sh" nand2tetris-tools-dir)
 nand2tetris-cpu-emulator (expand-file-name "CPUEmulator.sh" nand2tetris-tools-dir)
 nand2tetris-jack-compiler (expand-file-name "JackCompiler.sh" nand2tetris-tools-dir)
 nand2tetris-text-comparer (expand-file-name "TextComparer.sh" nand2tetris-tools-dir)
 nand2tetris-vm-emulator (expand-file-name "VMEmulator.sh" nand2tetris-tools-dir))

(defun nand2tetris/hardware-simulator ()
  "Summmon Hardware Simulator."
  (interactive)
  (shell-command nand2tetris-hardware-simulator))

(defun nand2tetris/assembler ()
  "Summon Assembler GUI."
  (interactive)
  (shell-command nand2tetris-assembler))

(defun nand2tetris/cpu-emulator ()
  "Summon CPU Emulator GUI."
  (interactive)
  (shell-command nand2tetris-cpu-emulator))

(defun nand2tetris/jack-compiler ()
  "Summon Jack Compiler GUI."
  (interactive)
  (shell-command nand2tetris-jack-compiler))

(defun nand2tetris/text-comparer ()
  "Summom the Text Comparer."
  (interactive)
  (shell-command nand2tetris-text-comparer))

(defun nand2tetris/vm-emulator ()
  "Summon the VM Emulator."
  (interactive)
  (shell-command nand2tetris-vm-emulator))

(defun nand2tetris//get-test-file (buffer)
  "Get the test file for BUFFER."
  (let ((test-file (concat
                    (file-name-sans-extension
                     (with-current-buffer buffer
                       (buffer-file-name))) ".tst")))
    (unless (file-exists-p test-file)
      (error "Could not find the test file for %s" (buffer-name)))
    test-file))

(defun nand2tetris//get-current-test-file ()
  "Get the test file for the current buffer."
  (interactive)
  (message
  (nand2tetris//get-test-file (current-buffer))))

(defun nand2tetris//get-compare-file (buffer)
  "Get the compare file for BUFFER."
  (let ((compare-file (concat
                    (file-name-sans-extension
                     (with-current-buffer buffer
                       (buffer-file-name))) ".cmp")))
    (unless (file-exists-p compare-file)
      (error "Could not find the compare file for %s" (buffer-name)))
    compare-file))

(defun nand2tetris//get-current-compare-file ()
  "Get the compare file for the current buffer."
  (interactive)
  (message
   (nand2tetris//get-compare-file (current-buffer))))


(defun nand2tetris/tests-current-hdl ()
  "Run `HardwareSimulator.sh' on current tst file."
  (interactive)
  (save-buffer)
  (shell-command (concat
                  nand2tetris-hardware-simulator " "
                  (nand2tetris//get-current-test-file))))

(defun nand2tetris/tests-current-hdl-elsewhere ()
  "Run `HardwareSimulator.sh' on current tst file, but on another locaion so it can use the builtin chips."
  (interactive)
  (let ((filename (file-name-base (buffer-file-name)))
        (hdl-buffer (current-buffer))
        (tst-file (nand2tetris//get-current-test-file))
        (cmp-file (nand2tetris//get-current-compare-file)))

    (copy-file tst-file (concat "/tmp/" filename ".tst") t)
    (copy-file cmp-file (concat "/tmp/" filename ".cmp") t)
    (with-temp-file (concat "/tmp/" filename ".hdl")
      (insert-buffer-substring hdl-buffer))
    (shell-command (concat nand2tetris-hardware-simulator " "
                           (concat "/tmp/" filename ".tst")))))

;;; Bindings
(defvar nand2tetris-mode-map
  (let ((map (make-sparse-keymap)))
    ;;Compile
    (define-key map "\C-c\C-c" 'nand2tetris/tests-current-hdl-elsewhere)
    (define-key map "\C-c\C-k" 'nand2tetris/tests-current-hdl)
    map)
  "Keymap for `nand2tetris-mode'.")


;;; Company
(require 'company)
(require 'cl-lib)

(defvar nand2tetris-builtin-chips
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
x+y, x-y, y-x, 0, 1, -1, x, y, -x, -y, !x, !y,
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

(defun company-nand2tetris--candidates (prefix)
  "Gather Candidates from `nand2tetris-builtin-chips' that match PREFIX."
  (let ((res))
    (dolist (option nand2tetris-builtin-chips)
      (let ((name (car option)))
        (when (string-prefix-p prefix name)
          (push name res))))
    res))

(defun nand2tetris-doc-buffer (doc)
  "Display documentation buffer with contents DOC."
  (let ((buf (get-buffer-create "*company-nand2tetris-doc*")))
    (with-current-buffer buf
      (view-mode -1)
      (erase-buffer)
      (insert doc)
      (goto-char (point-min))
      (view-mode 1)
      buf)))

(defun company-nand2tetris--doc-buffer (candidate)
  "Return documentation buffer for chosen CANDIDATE."
  (let ((doc (cdr (assoc "description"
                         (assoc candidate nand2tetris-builtin-chips)))))
    (and doc (nand2tetris-doc-buffer doc))))

(defun company-nand2tetris--annotation (candidate)
  "Get the specification of the chip defined by CANDIDATE as annotated text."
  (let ((spec (cdr (assoc "spec" (assoc candidate nand2tetris-builtin-chips)))))
    (format "    %s" spec)))

(defun company-nand2tetris--grab-symbol ()
  "Grab last symbol with a dotty syntax."
  (buffer-substring (point) (save-excursion (skip-syntax-backward "w_.")
                                                (point))))

(defun company-nand2tetris--prefix ()
  "Grab prefix at point."
  (or (company-nand2tetris--grab-symbol)
      'stop))

;;;###autoload
(defun company-nand2tetris (command &optional arg &rest ignored)
  "Company backend for `nand2tetris-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nand2tetris))
    (prefix (company-nand2tetris--prefix))
    (candidates (company-nand2tetris--candidates arg))
    (doc-buffer (company-nand2tetris--doc-buffer arg))
    (annotation (company-nand2tetris--annotation arg))))


;;; ElDoc
(require 'eldoc)
(defun nand2tetris-eldoc-function ()
  "Get help on SYMBOL using `help'.
Interactively, prompt for symbol."
  (let ((symbol (company-nand2tetris--grab-symbol))
        (enable-recursive-minibuffers t))
  (message (cdr (assoc "spec" (assoc symbol nand2tetris-builtin-chips))))))


;;; Yasnippet

(defconst nand2tetris::dir (file-name-directory (or load-file-name
                                                buffer-file-name)))
;;;###autoload
(defun nand2tetris//snippets-initialize ()
  "Initialize snippets directory."
  (let ((snip-dir (expand-file-name "snippets" nand2tetris::dir)))
    (add-to-list 'yas-snippet-dirs snip-dir t)
    (yas-load-directory snip-dir)))

;;;###autoload
(eval-after-load 'yasnippet
  '(nand2tetris//snippets-initialize))


;;; Font-lock and syntax
(defvar nand2tetris-font-lock-keywords
  ;;Keywords
  `(,(rx symbol-start
         (or "CHIP")
         symbol-end)
    (,(rx symbol-start (group (or "IN" "OUT" "PARTS" "BUILTIN" "CLOCKED")))
     (1 font-lock-variable-name-face))
    ;; CHIP <ChipName>
    (,(rx symbol-start (or "CHIP" "BUILTIN") (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-type-face))
    ;; <ChipName> (in=in, out=out);
    (,(rx symbol-start (group (1+ (or word ?_)))
          (? space) (seq "(" (0+ not-newline) ")"))
     (1 font-lock-variable-name-face))))

(define-derived-mode nand2tetris-mode prog-mode
  "nand2tetris"
  "Major mode for editing HDL files for the course Nand2Tetris.

\\{nand2tetris-mode-map}"

  (set (make-local-variable 'eldoc-documentation-function)
       #'nand2tetris-eldoc-function)

  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "//+\\s-*")

  (set (make-local-variable 'font-lock-defaults)
       '(nand2tetris-font-lock-keywords nil nil nil nil)))

(add-to-list 'auto-mode-alist
             `(,(concat (expand-file-name nand2tetris-source-dir) "\.*\\.hdl")
               . nand2tetris-mode))

(provide 'nand2tetris)
;;; nand2tetris.el ends here
