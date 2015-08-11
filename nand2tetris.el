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
  (shell-command nand2tetris-hardware-simulator))

(defun nand2tetris/assembler ()
  (shell-command nand2tetris-assembler))

(defun nand2tetris/cpu-emulator ()
  (shell-command nand2tetris-cpu-emulator))

(defun nand2tetris/jack-compiler ()
  (shell-command nand2tetris-jack-compiler))

(defun nand2tetris/text-comparer ()
  (shell-command nand2tetris-text-comparer))

(defun nand2tetris/vm-emulator ()
  (shell-command nand2tetris-vm-emulator))

(defun nand2tetris/get-current-tst-file ()
  (concat
   (file-name-sans-extension
    (buffer-file-name)) ".tst"))

(defun nand2tetris/tests-current-hdl ()
  (interactive)
  (save-buffer)
  (shell-command (concat
                  nand2tetris-hardware-simulator " "
                  (nand2tetris/get-current-tst-file))))


;;; Bindings
(defvar nand2tetris-mode-map
  (let ((map (make-sparse-keymap)))
    ;;Compile
    (define-key map "\C-c\C-c" 'nand2tetris/tests-current-hdl)
    map)
  "Keymap for `nand2tetris-mode'.")


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
