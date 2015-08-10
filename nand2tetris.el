(defconst nand2tetris-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ' is a string delimiter
    (modify-syntax-entry ?' "\"" table)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" table)

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode nand2tetris-mode prog-mode "nand2tetris"
  :syntax-table nand2tetris-mode-syntax-table
  (font-lock-fontify-buffer))

(provide 'nand2tetris)
