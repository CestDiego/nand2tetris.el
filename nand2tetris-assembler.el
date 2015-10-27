;;; nand2tetris-assembler.el --- Assembler For the Nand2tetris Course
;;; https://www.coursera.org/course/nand2tetris-assembler1

;; Copyright (C) 2015 Diego Berrocal

;; Author: Diego Berrocal <cestdiego@gmail.com>
;; Created: 10 August 2015

;; Keywords: nand2tetris-assembler, hdl
;; Homepage: http://www.github.com/CestDiego/nand2tetris-assembler.el/
;; Version: 0.0.1
;; Package-Requires: ((names "0.3.0") (nand2tetris "0.0.1"))

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

(eval-when-compile (require 'names))

(require 'nand2tetris)

;;;###autoload
(define-namespace nand2tetris-assembler

(defconst --default-symbols-alist
  '(("SP"     .  #x0)
    ("LCL"    .  #x1)
    ("ARG"    .  #x2)
    ("THIS"   .  #x3)
    ("THAT"   .  #x4)
    ("R0"     .  #x0)
    ("R1"     .  #x1)
    ("R2"     .  #x2)
    ("R3"     .  #x3)
    ("R4"     .  #x4)
    ("R5"     .  #x5)
    ("R6"     .  #x6)
    ("R7"     .  #x7)
    ("R8"     .  #x8)
    ("R9"     .  #x9)
    ("R10"    .  #xA)
    ("R11"    .  #xB)
    ("R12"    .  #xC)
    ("R13"    .  #xD)
    ("R14"    .  #xE)
    ("R15"    .  #xF)
    ("SCREEN" .  #x4000)
    ("KBD"    .  #x6000))
  "Default alist of Predefined Symbols for the Hack Assembly.
According to http://www.nand2tetris.org/chapters/chapter%2006.pdf")

(defconst --comp-symbols-alist
  '(("0"    .  "0101010")
    ("1"    .  "0111111")
    ("-1"   .  "0111010")
    ("D"    .  "0001100")
    ("A"    .  "0110000")
    ("M"    .  "1110000")
    ("!D"   .  "0001101")
    ("!A"   .  "0110001")
    ("!M"   .  "1110001")
    ("-D"   .  "0001111")
    ("-A"   .  "0110011")
    ("-M"   .  "1110011")
    ("D+1"  .  "0011111")
    ("A+1"  .  "0110111")
    ("M+1"  .  "1110111")
    ("D-1"  .  "0001110")
    ("A-1"  .  "0110010")
    ("M-1"  .  "1110010")
    ("D+A"  .  "0000010")
    ("D+M"  .  "1000010")
    ("D-A"  .  "0010011")
    ("D-M"  .  "1010011")
    ("A-D"  .  "0000111")
    ("M-D"  .  "1000111")
    ("D&A"  .  "0000000")
    ("D&M"  .  "1000000")
    ("D|A"  .  "0010101")
    ("D|M"  .  "1010101"))
  "Computation symbols relation with their binary representation.")

(defconst --dest-symbols-alist
  '((""    .  "000")
    ("M"   .  "001")
    ("D"   .  "010")
    ("A"   .  "100")
    ("AD"  .  "110")
    ("MD"  .  "011")
    ("AM"  .  "101")
    ("AMD" .  "111"))
  "Destination symbols relation with their binary representation.")

(defconst --jump-symbols-alist
  '((""    .  "000")
    ("JGT" .  "001")
    ("JEQ" .  "010")
    ("JGE" .  "011")
    ("JLT" .  "100")
    ("JNE" .  "101")
    ("JLE" .  "110")
    ("JMP" .  "111"))
  "Jump symbols relation with their binary representation.")

(defvar-local --symbols-alist
  --default-symbols-alist
  "Le symbols.")

(defun /read-lines (file)
  "Return list of non-double-slash begginging lines in FILE."
  (let ((filter-regexp "\s*//.*$"))
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" t filter-regexp))))

(defvar --A-instruction-regexp
  (rx ?@ (group (+ digit)))
  "Regular Expression for an A instruction.")

(defvar --C-instruction-regexp
  (rx
   ;;Destination regexp
   (* space)
   (? (seq
       (group (* (or "M" "D" "A")))
       ?=))
   ;; Computation regexp
   (seq (group
         (* (or "M" "A" "D" ?+ ?- ?! ?| ?& "1" "0")))
        (? (or line-end space "/")))
   ;; Jump regexp
   (? (seq ";"
           (group (* word))
           (? (or line-end space "/")))))
  "Regular Expression for an A instruction.")

(defun /parser (instruction)
  "Provides convenient access to the command’s components (fields and symbols).
In addition, removes all white space and comments in INSTRUCTION."
  (cond ((string-match --A-instruction-regexp instruction)
         `("A" . ,(match-string 1 instruction)))
        ((string-match --C-instruction-regexp instruction)
         `("C" . (("dest" . ,(match-string 1 instruction))
                  ("comp" . ,(match-string 2 instruction))
                  ("jump" . ,(match-string 3 instruction)))))))

(defun /format-A-instruction (decimal-number)
  "Convert DECIMAL-NUMBER to a 16-long string binary representation.
Resulting string has 0-padding if the binary representaton is not
big enough."
  (let* ((integer (string-to-number decimal-number))
         (binary-string (/int-to-binary-string integer)))
    (if (<= (length binary-string) 16)
        (while (not (equal 16 (length binary-string)))
          (setq binary-string (concat "0" binary-string)))
      (error "The A instruction gave a binary representation bigger than 2e15"))
    binary-string))

(defun /int-to-binary-string (integer)
  "Convert INTEGER into it's binary representation.
Borrowed from http://stackoverflow.com/a/20577329"
  (let ((res ""))
    (while (not (= integer 0))
      (setq res (concat (if (= 1 (logand integer 1)) "1" "0") res))
      (setq integer (lsh integer -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(defun /format-C-instruction (data-alist)
  "Return a valid C-Instruction from the fields in DATA-ALIST.
If one field retrieved by DATA-ALIST isn't matched, error is raised."
  (let*
      ((prefix-bits "111")
       (dest-symbol (or (cdr (assoc "dest" data-alist)) ""))
       (jump-symbol (or (cdr (assoc "jump" data-alist)) ""))
       (comp-symbol (or (cdr (assoc "comp" data-alist)) ""))
       (dest-bits (cdr (assoc dest-symbol
                              --dest-symbols-alist)))
       (comp-bits (cdr (assoc comp-symbol
                              --comp-symbols-alist)))
       (jump-bits (cdr (assoc jump-symbol
                              --jump-symbols-alist))))
    (unless dest-bits
      (error (concat "The Destination Symbol does not"
                     " match to the predefined symbols: " dest-symbol)))
    (unless comp-bits
      (error (concat "The Computation Symbol does not"
                     " match to the predefined symbols: " comp-symbol)))
    (unless jump-bits
      (error (concat "The Jump Symbol does not"
                     " match to the predefined symbols: " jump-symbol)))
    (concat prefix-bits comp-bits dest-bits jump-bits)))

(defun /process (instruction)
  "Assume INSTRUCTION is valid and output it's binary representation.
Instruction correctness given by
http://www.nand2tetris.org/chapters/chapter%2006.pdf Page 106.
Also the parsed lines do not start with `//' so we can assume it
will only have a instruction and maybe inline comments that are
going to be ignored by the regular expressions."
  (let* ((parsed-instruction (/parser instruction))
         (type (car parsed-instruction))
         (parsed-data (cdr parsed-instruction)))
    (cond
     ((equal type "A" )
      (insert
       (/format-A-instruction parsed-data)))
     ((equal type "C")
      ;; (print parsed-data)
      (insert
       (/format-C-instruction parsed-data))))
    (insert "\n")))

(defconst --label-regexp
  (rx "(" (group (+ (or word ?- ?_ ?. ?$))) ")" )
  "Regular Expression that matches the label tags.")

(defconst --variable-regexp
  (rx ?@ (group letter (* (or word ?- ?_ ?. ?$))))
  "Regular Expression that matches the variable tags.")

(defun /digest-labels (lines)
  "Read LINES and take away the labels.
The LABELS  are stored in `nand2tetris-assembler--symbols-alist'"
  (let ((i 0)
        (digested-lines '()))
    (dolist (line lines digested-lines)
      (if (string-match --label-regexp line)
          (let* ((label (match-string 1 line)))
            ;; Make sure label is not repeated if it is ERROR
            ;; Make sure this is not final line.
            (push `(,label . ,i) --symbols-alist))
        ;; Line isn't a label so..
        ;; Augment the counter
        (setq i (+ 1 i))
        ;; Let the line pass thru
        (setq digested-lines (append digested-lines (list line)))))))

(defun /replace-variables (lines)
  "Return a variable/label agnostic INSTRUCTION for each of the LINES.
Use the tables defined in `nand2tetris-assembler--symbols-alist'.
Take into consideration that this table stores integer values, so
that when returning the corresponding instruction we use the
`format' function with the %d escape."
  (let ((variable-index 16) ;; Start variables at 16
        (instructions '()))
    (dolist (line lines instructions)
      (if (string-match --variable-regexp line)
          (let* ((variable-name (match-string 1 line))
                 (value (cdr
                         (assoc variable-name --symbols-alist))))
            (when (equal "" variable-name)
              (error
               "In Replacing Variables: variable-name checks empty string"))
            (if value
                ;; There is a variable or label with that name in the alist
                (setq instructions (append instructions
                                           (list (format "@%d" value))))
              ;; We add the variable to the alist, we use the full name instead
              ;; of the let local variable because otherwise push isn't global.
              (push `(,variable-name . ,variable-index) --symbols-alist)
              ;; Let's replace it imnediatly so that we don't make another scan
              ;; of the lines
              (setq instructions
                    (append instructions (list (format "@%d" variable-index))))
              ;; Increase the counter
              (setq variable-index (+ 1 variable-index))))
        (setq instructions (append instructions (list line)))))))

(defun /init ()
  "Init function for the Hack Assembly mode."
  (interactive)
  ;; Initialize the symbols alist
  (let* ((current-file     (buffer-file-name))
         (read-lines       (/read-lines current-file))
         (digested-lines   (/digest-labels read-lines))
         (filename         (concat
                            (file-name-sans-extension buffer-file-name)
                            ".hack"))
         (instructions     (/replace-variables digested-lines)))
    (with-temp-file filename
      (mapcar #'/process instructions))))


;;; Bindings
(defvar -mode-map
  (let ((map (make-sparse-keymap)))
    ;;Compile
    (define-key map "\C-c\C-c" #'/init)
    map)
  "Keymap for `nand2tetris-assembler-mode'.")


;;; Syntax Table

(defconst -mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table))


;;; Major Mode
(define-derived-mode -mode prog-mode
  "HACK Assembler"
  "Major mode for the HACK Assembler language.

\\{nand2tetris-assembler-mode-map}"

  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "//+\\s-*")

  ;; (set (make-local-variable 'font-lock-defaults)
  ;;      '(nand2tetris-font-lock-keywords nil nil nil nil))
  )

:autoload
(add-to-list 'auto-mode-alist
             `(,(concat (expand-file-name nand2tetris-core-base-dir) "\.*\\.asm")
               . ,#'-mode))
)

(provide 'nand2tetris-assembler)
;;; nand2tetris-assembler.el ends here
