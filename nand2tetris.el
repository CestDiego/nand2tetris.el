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

(defvar nand2tetris-mode-map
  (let ((map (make-sparse-keymap)))
    ;;Compile
    (define-key map "\C-c\C-c" 'nand2tetris-)
    map)
  "Keymap for `nand2tetris-mode'.")

(defvar nand2tetris-font-lock-keywords
  ;;Keywords
  `(,(rx symbol-start
         (or "CHIP" "IN" "OUT" "PARTS")
         symbol-end)
    ;; CHIP <ChipName>
    (,(rx symbol-start "CHIP" (1+ space) (group (1+ (or word ?_))))
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
