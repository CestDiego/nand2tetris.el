;;; nand2tetris.el --- Major Mode for HDL files in the Nand2Tetris Course
;;; https://www.coursera.org/course/nand2tetris1

;; Copyright (C) 2015 Diego Berrocal

;; Author: Diego Berrocal <cestdiego@gmail.com>
;; Created: 10 August 2015

;; Keywords: nand2tetris, hdl
;; Homepage: http://www.github.com/CestDiego/nand2tetris.el/
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions to make following the coursera course easier.

;;; Code:
(require 'cc-mode)

(defvar nand2tetris-source-dir nil
  "Source directory where nadn2tetris has been downloaded")

(defconst nand2tetris-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
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
  (interactive)
  (shell-command nand2tetris-hardware-simulator))

(defun nand2tetris/assembler ()
  (interactive)
  (shell-command nand2tetris-assembler))

(defun nand2tetris/cpu-emulator ()
  (interactive)
  (shell-command nand2tetris-cpu-emulator))

(defun nand2tetris/jack-compiler ()
  (interactive)
  (shell-command nand2tetris-jack-compiler))

(defun nand2tetris/text-comparer ()
  (interactive)
  (shell-command nand2tetris-text-comparer))

(defun nand2tetris/vm-emulator ()
  (interactive)
  (shell-command nand2tetris-vm-emulator))

(defun nand2tetris//get-current-tst-file ()
  (concat
   (file-name-sans-extension
    (buffer-file-name)) ".tst"))

(defun nand2tetris//get-current-cmp-file ()
  (concat
   (file-name-sans-extension
    (buffer-file-name)) ".cmp"))

(defun nand2tetris/tests-current-hdl ()
  (interactive)
  (save-buffer)
  (shell-command (concat
                  nand2tetris-hardware-simulator " "
                  (nand2tetris//get-current-tst-file))))

(defun nand2tetris/tests-current-hdl-elsewhere ()
  (interactive)
  (let ((filename (file-name-base (buffer-file-name)))
        (hdl-buffer (current-buffer))
        (tst-file (nand2tetris//get-current-tst-file))
        (cmp-file (nand2tetris//get-current-cmp-file)))

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
  '(("Add16" . (("spec" . "Add16(a= ,b= ,out= )")))
    ("ALU" . (("spec" . "ALU(x= ,y= ,zx= ,nx= ,zy= ,ny= ,f= ,no= ,out= ,zr= ,ng= )")))
    ("And16" .  (("spec" . "And16(a= ,b= ,out= )")))
    ("And" . (("spec" . "And(a= ,b= ,out= )")))
    ("ARegister" . (("spec" . "ARegister(in= ,load= ,out= )")))
    ("Bit" . (("spec" . "Bit(in= ,load= ,out= )")))
    ("CPU" . (("spec" . "CPU(inM= ,instruction= ,reset= ,outM= ,writeM= ,addressM= ,pc= )")))
    ("DFF" . (("spec" . "DFF(in= ,out= )")))
    ("DMux4Way" . (("spec" . "DMux4Way(in= ,sel= ,a= ,b= ,c= ,d= )")))
    ("DMux8Way" . (("spec" . "DMux8Way(in= ,sel= ,a= ,b= ,c= ,d= ,e= ,f= ,g= ,h= )")))
    ("DMux" . (("spec" . "DMux(in= ,sel= ,a= ,b= )")))
    ("DRegister" . (("spec" . "DRegister(in= ,load= ,out= )")))
    ("FullAdder" . (("spec" . "FullAdder(a= ,b= ,c= ,sum= ,carry= )")))
    ("HalfAdder" . (("spec" . "HalfAdder(a= ,b= ,sum= , carry= )")))
    ("Inc16" . (("spec" . "Inc16(in= ,out= )")))
    ("Keyboard" . (("spec" . "Keyboard(out= )")))
    ("Memory" . (("spec" . "Memory(in= ,load= ,address= ,out= )")))
    ("Mux16" . (("spec" . "Mux16(a= ,b= ,sel= ,out= )")))
    ("Mux4Way16" . (("spec" . "Mux4Way16(a= ,b= ,c= ,d= ,sel= ,out= )")))
    ("Mux8Way16" . (("spec" . "Mux8Way16(a= ,b= ,c= ,d= ,e= ,f= ,g= ,h= ,sel= ,out= )")))
    ("Mux" . (("spec" . "Mux(a= ,b= ,sel= ,out= )")))
    ("Nand" . (("spec" . "Nand(a= ,b= ,out= )")))
    ("Not16" . (("spec" . "Not16(in= ,out= )")))
    ("Not" . (("spec" . "Not(in= ,out= )")))
    ("Or16" . (("spec" . "Or16(a= ,b= ,out= )")))
    ("Or8Way" . (("spec" . "Or8Way(in= ,out= )")))
    ("Or" . (("spec" . "Or(a= ,b= ,out= )")))
    ("PC" . (("spec" . "PC(in= ,load= ,inc= ,reset= ,out= )")))
    ("RAM16K" . (("spec" . "RAM16K(in= ,load= ,address= ,out= )")))
    ("RAM4K" . (("spec" . "RAM4K(in= ,load= ,address= ,out= )")))
    ("RAM512" . (("spec" . "RAM512(in= ,load= ,address= ,out= )")))
    ("RAM64" . (("spec" . "RAM64(in= ,load= ,address= ,out= )")))
    ("RAM8" . (("spec" . "RAM8(in= ,load= ,address= ,out= )")))
    ("Register" . (("spec" . "Register(in= ,load= ,out= )")))
    ("ROM32K" . (("spec" . "ROM32K(address= ,out= )")))
    ("Screen" . (("spec" . "Screen(in= ,load= ,address= ,out= )")))
    ("Xor" . (("spec" . "Xor(a= ,b= ,out= )"))))
  "Built In Chips Alist")

(defun company-nand2tetris--candidates (prefix)
  (let ((res))
    (dolist (option nand2tetris-builtin-chips)
      (let ((name (car option)))
        (when (string-prefix-p prefix name)
          (push name res))))
    res))

(defun company-nand2tetris--annotation (candidate)
  (message candidate)
  (let ((spec (cdr (assoc "spec" (assoc candidate nand2tetris-builtin-chips)))))
    (format "\t%s" spec)))

(defun company-nand2tetris--grab-symbol ()
      (buffer-substring (point) (save-excursion (skip-syntax-backward "w_.")
                                                (point))))

(defun company-nand2tetris--prefix ()
  "Grab prefix at point."
  (or (company-nand2tetris--grab-symbol)
      'stop))

(defun company-nand2tetris (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nand2tetris))
    (prefix (company-nand2tetris--prefix))
    (candidates (company-nand2tetris--candidates arg))
    (annotation (company-nand2tetris--annotation arg))))


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

  (set (make-local-variable 'font-lock-defaults)
       '(nand2tetris-font-lock-keywords nil nil nil nil)))

(add-to-list 'auto-mode-alist
             `(,(concat (expand-file-name nand2tetris-source-dir) "\.*\\.hdl")
               . nand2tetris-mode))

(provide 'nand2tetris)
;;; nand2tetris.el ends here
