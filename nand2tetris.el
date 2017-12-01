;;; nand2tetris.el --- Major mode for HDL files in the nand2tetris course
;;; https://www.coursera.org/course/nand2tetris1

;; Copyright (C) 2015 Diego Berrocal

;; Author: Diego Berrocal <cestdiego@gmail.com>
;; Created: 10 August 2015

;; Keywords: nand2tetris, hdl
;; Homepage: http://www.github.com/CestDiego/nand2tetris.el/
;; Version: 1.1.0
;; Package-Requires: ((emacs "24"))

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
(require 'nand2tetris-core)
(require 'eldoc)
(require 'yasnippet)
(require 'rx)


;;;###autoload
(defconst nand2tetris-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table))



;;; Scripts Integration
(defcustom nand2tetris-tools-dir nil
  "The directory where the 'tools' (simulator, assembler, etc) are located."
  :group 'nand2tetris)
(defun nand2tetris-tools-dir () (or nand2tetris-tools-dir (expand-file-name "tools" nand2tetris-core-base-dir)))

(defcustom nand2tetris-hardware-simulator nil
  "Hardware Simulator Launcher."
  :group 'nand2tetris)
(defun nand2tetris-hardware-simulator () (or nand2tetris-hardware-simulator (expand-file-name "HardwareSimulator.sh" (nand2tetris-tools-dir))))

(defcustom nand2tetris-assembler nil
  "Assembler Launcher."
  :group 'nand2tetris)
(defun nand2tetris-assembler nil () (or nand2tetris-assembler nil (expand-file-name "HardwareSimulator.sh" (nand2tetris-tools-dir))))

(defcustom nand2tetris-cpu-emulator nil
  "CPU Emulator Launcher."
  :group 'nand2tetris)
(defun nand2tetris-cpu-emulator () (or nand2tetris-cpu-emulator (expand-file-name "CPUEmulator.sh" (nand2tetris-tools-dir))))

(defcustom nand2tetris-jack-compiler nil
  "Jack Compiler Launcher."
  :group 'nand2tetris)
(defun nand2tetris-jack-compiler () (or nand2tetris-jack-compiler (expand-file-name "JackCompiler.sh" (nand2tetris-tools-dir))))

(defcustom nand2tetris-text-comparer nil
  "Text Comparer Launcher."
  :group 'nand2tetris)
(defun nand2tetris-text-comparer () (or nand2tetris-text-comparer (expand-file-name "TextComparer.sh" (nand2tetris-tools-dir))))

(defcustom nand2tetris-vm-emulator nil
  "VM Emulator Launcher."
  :group 'nand2tetris)
(defun nand2tetris-vm-emulator () (or nand2tetris-vm-emulator (expand-file-name "VMEmulator.sh" (nand2tetris-tools-dir))))

(defun nand2tetris/hardware-simulator ()
  "Start Hardware Simulator.

See URL `http://www.nand2tetris.org/software.php'."
  (interactive)
  (call-process-shell-command (nand2tetris-hardware-simulator)))

(defun nand2tetris/assembler ()
  "Start Assembler GUI.

See URL `http://www.nand2tetris.org/software.php'."
  (interactive)
  (call-process-shell-command (nand2tetris-assembler)))

(defun nand2tetris/cpu-emulator ()
  "Start CPU Emulator GUI.

See URL `http://www.nand2tetris.org/software.php'."
  (interactive)
  (call-process-shell-command (nand2tetris-cpu-emulator)))

(defun nand2tetris/jack-compiler ()
  "Start Jack Compiler GUI.

See URL `http://www.nand2tetris.org/software.php'."
  (interactive)
  (call-process-shell-command (nand2tetris-jack-compiler)))

(defun nand2tetris/text-comparer ()
  "Summom Text Comparer.

See URL `http://www.nand2tetris.org/software.php'."
  (interactive)
  (call-process-shell-command (nand2tetris-text-comparer)))

(defun nand2tetris/vm-emulator ()
  "Start VM Emulator.

See URL `http://www.nand2tetris.org/software.php'."
  (interactive)
  (call-process-shell-command (nand2tetris-vm-emulator)))

(defun nand2tetris//get-test-file (buffer)
  "get the test file for buffer."
  (let ((test-file (concat
                    (file-name-sans-extension
                     (with-current-buffer buffer
                       (buffer-file-name))) ".tst")))
    (unless (file-exists-p test-file)
      (error "could not find the test file for %s" (buffer-name)))
    test-file))

(defun nand2tetris//get-current-test-file ()
  "get the test file for the current buffer."
  (interactive)
  (message
   (nand2tetris//get-test-file (current-buffer))))

(defun nand2tetris//get-compare-file (buffer)
  "get the compare file for buffer."
  (let ((compare-file (concat
                       (file-name-sans-extension
                        (with-current-buffer buffer
                          (buffer-file-name))) ".cmp")))
    (unless (file-exists-p compare-file)
      (error "could not find the compare file for %s" (buffer-name)))
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
  (shell-command (concat (nand2tetris-hardware-simulator) " " (nand2tetris//get-current-test-file))))

(defun nand2tetris/tests-current-hdl-elsewhere ()
  "Run `HardwareSimulator.sh' on current tst file, but on another locaion.
So it can use the builtin chips."
  (interactive)
  (let* ((filename          (file-name-base (buffer-file-name)))
         (hdl-buffer        (current-buffer))
         (tst-file          (nand2tetris//get-current-test-file))
         (cmp-file          (nand2tetris//get-current-compare-file))
         (should-create-dir t)
         (temp-dir          (file-name-as-directory (make-temp-file "nand2tetris-el" should-create-dir))))
    (copy-file tst-file (expand-file-name (concat filename ".tst") temp-dir) t)
    (copy-file cmp-file (expand-file-name (concat filename ".cmp") temp-dir) t)
    (with-temp-file (concat temp-dir filename ".hdl")
      (insert-buffer-substring hdl-buffer))
    (shell-command (concat (nand2tetris-hardware-simulator) " "
                           (expand-file-name (concat filename ".tst") temp-dir)))))


;;; Bindings
(defvar nand2tetris-mode-map
  (let ((map (make-sparse-keymap)))
    ;;Compile
    (define-key map "\C-c\C-c" #'nand2tetris/tests-current-hdl-elsewhere)
    (define-key map "\C-c\C-k" #'nand2tetris/tests-current-hdl)
    map)
  "Keymap for `nand2tetris-mode'n.")


;;; ElDoc
(defun nand2tetris//get-chip-at-line ()
  "Gets the chip currently used, so that placing the cursor at
any point in the line:
   Not16 (in=a, out=out)
Will return Not16"
  (save-excursion
    (beginning-of-line)
    (when (search-forward-regexp
           (rx (group (* word))
               (? space ) "(")
           nil
           t)
      (match-string 1))))

(defun nand2tetris/eldoc-function ()
  "Get help on SYMBOL using `help'.
Interactively, prompt for symbol."
  (let ((symbol (nand2tetris//get-chip-at-line))
        (enable-recursive-minibuffers t))
    (message (cdr (assoc "spec" (assoc symbol nand2tetris-core-builtin-chips))))))


;;; Yasnippet
(defconst nand2tetris--source-root-dir (file-name-directory (or load-file-name
                                                     buffer-file-name)))
;;;###autoload
(defun nand2tetris//snippets-initialize ()
  "Initialize snippets directory."
  (let ((snip-dir (expand-file-name "snippets" nand2tetris--source-root-dir)))
    (add-to-list 'yas-snippet-dirs snip-dir t)
    (yas-load-directory snip-dir)))

;;;###autoload
(eval-after-load 'yasnippet #'(nand2tetris//snippets-initialize))


;;; Indentation
(defun nand2tetris/indent-line ()
  "Indent current line as WPDL code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)                   ; First line is always non-indented
    (let ((not-indented t)
          (cur-indent 0)
          (nand2tetris-1 -1))
      (if (looking-at "^[ \t]*}") ; If the line we are looking at is the end of a block, then decrease the indentation
          (progn
            (save-excursion
              (forward-line nand2tetris-1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0) ; We can't indent past the left margin
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented ; Iterate backwards until we find an indentation hint
            (forward-line nand2tetris-1)
            (if (looking-at "^[ \t]*.*[})]") ; This hint indicates that we need to indent at the level of the END_ token
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^[ \t]*.*[({]$") ; This hint indicates that we need to indent an extra level
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width)) ; Do the actual indenting
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        ;; If we didn't see an indentation hint, then allow no indentation
        (indent-line-to 0)))))


;;; Font-Lock and Syntax
(defvar nand2tetris-font-lock-keywords
  `(;;Keywords
    ,(rx symbol-start
         (or "CHIP")
         symbol-end)
    (,(rx symbol-start (group (or "IN" "OUT" "PARTS" "BUILTIN" "CLOCKED")))
     (1 font-lock-variable-name-face))
    ;; Attribute names!
    (,(rx (group (* word)) (? (seq "[" (* (or digit ?.)) "]"))
          (* space) ?= (* (or word digit ?-)))
     (1 font-lock-constant-face))
    ;; CHIP <ChipName>
    (,(rx symbol-start (or "CHIP" "BUILTIN") (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-type-face))
    ;; <ChipName> (in=in, out=out);
    (,(rx symbol-start (group (1+ (or word ?_)))
          (? space) "(")
     (1 font-lock-variable-name-face))))

;;;###autoload
(define-derived-mode nand2tetris-mode prog-mode
  "nand2tetris"
  "Major mode for editing HDL files for the course Nand2Tetris.

\\{nand2tetris-mode-map}"

  (set (make-local-variable 'eldoc-documentation-function)
       #'nand2tetris/eldoc-function)

  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "//+\\s-*")
  (set (make-local-variable 'indent-line-function) #'nand2tetris/indent-line)

  (set (make-local-variable 'font-lock-defaults)
       `(,nand2tetris-font-lock-keywords nil nil nil nil)))

;;;###autoload
(add-to-list 'auto-mode-alist
             `(,(concat (expand-file-name nand2tetris-core-base-dir) "\.*\\.hdl")
               . 'nand2tetris-mode))


(provide 'nand2tetris)
;;; nand2tetris.el ends here
