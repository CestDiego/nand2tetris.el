;;; nand2tetris-assembler.el --- Assembler For the Nand2tetris Course
;;; https://www.coursera.org/course/nand2tetris-assembler1

;; Copyright (C) 2015 Diego Berrocal

;; Author: Diego Berrocal <cestdiego@gmail.com>
;; Created: 10 August 2015

;; Keywords: nand2tetris-assembler, hdl
;; Homepage: http://www.github.com/CestDiego/nand2tetris-assembler.el/
;; Version: 0.0.1
;; Package-Requires: ((company "0.5") (cl-lib "0.5.0") (yasnippet "0.8.0"))

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
;; See: https://www.coursera.org/course/nand2tetris-assembler1

;;; Code:
(defconst nand2tetris-assembler--default-symbols-alist
  '(("SP" #x0)
    ("LCL" #x1)
    ("ARG" #x2)
    ("THIS" #x3)
    ("THAT" #x4)
    ("R0" #x0)
    ("R1" #x1)
    ("R2" #x2)
    ("R3" #x3)
    ("R4" #x4)
    ("R5" #x5)
    ("R6" #x6)
    ("R7" #x7)
    ("R8" #x8)
    ("R9" #x9)
    ("R10" #xA)
    ("R11" #xB)
    ("R12" #xC)
    ("R13" #xD)
    ("R14" #xE)
    ("R15" #xF)
    ("SCREEN" #x4000)
    ("KBD" #x6000))
  "Default alist for the Predefined Symbols according to http://www.nand2tetris.org/chapters/chapter%2006.pdf")

(defun nand2tetris-assembler/read-lines (file)
  "Return a list of non-double-slash begginging lines of the
contents of FILE."
  (let ((filter-regexp "\s*//.*$"))
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t filter-regexp))))

(defvar nand2tetris-assembler--A-instruction-regexp
  (rx ?@ (group (* digit)))
  "Regular Expression for an A instruction")

(defvar nand2tetris-assembler--C-instruction-regexp
  (rx (group (* (or "M" "D" "A")))
      (or (seq ?=
               (group
                (* (or "M" "A" "D" ?+ ?- ?! ?| ?& "1"))
                (? (or line-end space "/"))))
          (seq ";"
               (group (* word)) (? (or line-end space "/")))))
  "Regular Expression for an A instruction")

(defun nand2tetris-assembler/parser (instruction)
  "Encapsulates access to the input code. Reads an assembly language command,
parses it, and provides convenient access to the command’s
components (fields and symbols). In addition, removes all white
space and comments"
  (let ((A-regexp nand2tetris-assembler--A-instruction-regexp)
        (C-regexp nand2tetris-assembler--C-instruction-regexp))
    (cond ((string-match A-regexp instruction)
           `("A" . ,(match-string 1 instruction)))
          ((string-match C-regexp instruction)
             `("C" . (("dest" . ,(match-string 1 instruction))
                      ("comp" . ,(match-string 2 instruction))
                      ("jump" . ,(match-string 3 instruction))))
           )
      )
    )
  )

(defun nand2tetris-assembler/format-A-instruction (decimal-number)
  "Convert DECIMAL-NUMBER to a 15-long string binary
representation with 0-padding if the binary representaton is not
big enough."
  (let* ((integer (string-to-int decimal-number))
         (binary-string (nand2tetris-assembler/int-to-binary-string integer)))
    (if (<= (length binary-string) 15)
        (while (not (equal 15 (length binary-string)))
          (setq binary-string (concat "0" binary-string)))
      (error "The A instruction gave a binary representation bigger than 2e15"))
    binary-string))

(defun nand2tetris-assembler/int-to-binary-string (integer)
  "Convert INTEGER into it's binary representation in string
format, borrowed from http://stackoverflow.com/a/20577329."
  (let ((res ""))
    (while (not (= integer 0))
      (setq res (concat (if (= 1 (logand integer 1)) "1" "0") res))
      (setq integer (lsh integer -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(defun nand2tetris-assembler/process-line (instruction)
  (let* ((parsed-instruction (nand2tetris-assembler/parser instruction))
         (type (car parsed-instruction))
         (parsed-data (cdr parsed-instruction)))
    (cond ((equal type "A" )
           (message
            (concat
            "A Instruction: "
            (nand2tetris-assembler/format-A-instruction parsed-data))))
          ((equal type "C") (print parsed-data)
           (message
            (concat
            "C Instruction: "
            (nand2tetris-assembler/format-C-instruction parsed-data))))
          )))

(defun nand2tetris-assembler/init ()
  "Init function for the Hack Assembly mode"
  (interactive)
  (let* ((current-file (buffer-file-name))
         (read-lines   (nand2tetris-assembler/read-lines current-file)))
    (mapcar 'nand2tetris-assembler/process-line read-lines)

    ))



;;; Bindings
(defvar nand2tetris-assembler-mode-map
  (let ((map (make-sparse-keymap)))
    ;;Compile
    (define-key map "\C-c\C-c" 'nand2tetris-assembler/init)
    map)
  "Keymap for `nand2tetris-assembler-mode'.")


;;; Syntax Table

(defconst nand2tetris-assembler-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table))



;;; Major Mode
(define-derived-mode nand2tetris-assembler-mode prog-mode
  "HACK Assembler"
  "Major mode for the HACK Assembler language.

\\{nand2tetris-assembler-mode-map}"

  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "//+\\s-*")

  ;; (set (make-local-variable 'font-lock-defaults)
  ;;      '(nand2tetris-font-lock-keywords nil nil nil nil))
  )

;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(concat (expand-file-name nand2tetris-base-dir) "\.*\\.asm")
               . nand2tetris-assembler-mode))

(provide 'nand2tetris-assembler)
