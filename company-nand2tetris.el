;;; company-nand2tetris.el --- Company backend for nand2tetris major mode
;;; https://www.coursera.org/course/nand2tetris1

;; Copyright (C) 2015 Diego Berrocal

;; Author: Diego Berrocal <cestdiego@gmail.com>
;; Created: 10 August 2015

;; Keywords: nand2tetris, hdl, company
;; Homepage: http://www.github.com/CestDiego/nand2tetris.el/
;; Version: 0.0.1
;; Package-Requires: ((names "0.3.0") (nand2tetris "0.0.1") (company "0.5") (cl-lib "0.5.0"))

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
(eval-when-compile (require 'names))
(require 'nand2tetris)
(require 'company)
(require 'cl-lib)

;;;###autoload
(define-namespace company-nand2tetris

(defun /candidates (prefix)
  "Gather Candidates from `nand2tetris-core-builtin-chips' that match PREFIX."
  (let ((res))
    (dolist (option nand2tetris-core-builtin-chips)
      (let ((name (car option)))
        (when (string-prefix-p prefix name)
          (push name res))))
    res))

(defun /display-doc-buffer (candidate)
  "Return documentation buffer for chosen CANDIDATE."
  (let ((buf (get-buffer-create "*company-nand2tetris-doc*"))
        (doc (cdr (assoc "description"
                         (assoc candidate nand2tetris-core-builtin-chips)))))
    (when doc
      (with-current-buffer buf
        (view-mode -1)
        (erase-buffer)
        (insert doc)
        (goto-char (point-min))
        (view-mode 1)
        buf))))

(defun /get-annotation (candidate)
  "Get the specification of the chip defined by CANDIDATE as annotated text."
  (let ((spec (cdr (assoc "spec" (assoc candidate nand2tetris-core-builtin-chips)))))
    (format "    %s" spec)))

(defun /grab-symbol ()
  "Grab last symbol with a dotty syntax."
  (buffer-substring (point) (save-excursion (skip-syntax-backward "w_.")
                                                (point))))

(defun /grab-prefix ()
  "Grab prefix at point."
  (or (/grab-symbol)
      'stop))

)

;;;###autoload
(defun company-nand2tetris (command &optional arg &rest ignored)
  "Company backend for `nand2tetris-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive      (company-begin-backend 'company-nand2tetris))
    (prefix           (company-nand2tetris/grab-prefix))
    (candidates       (company-nand2tetris/candidates arg))
    (doc-buffer       (company-nand2tetris/display-doc-buffer arg))
    (annotation       (company-nand2tetris/get-annotation arg))))

(provide 'company-nand2tetris)
