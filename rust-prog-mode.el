;;; rust-prog-mode.el --- old rust-mode without treesitter -*-lexical-binding: t-*-
;;; Commentary:

;; rust-mode code deriving from prog-mode instead of rust-ts-mode

;;; Code:

;;;###autoload
(define-derived-mode rust-mode prog-mode "Rust"
  "Major mode for Rust code.

\\{rust-mode-map}"
  :group 'rust-mode
  :syntax-table rust-mode-syntax-table

  ;; Syntax
  (setq-local syntax-propertize-function #'rust-syntax-propertize)

  ;; Indentation
  (setq-local indent-line-function 'rust-mode-indent-line)

  ;; Fonts
  (setq-local font-lock-defaults
              '(rust-font-lock-keywords
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . rust-mode-syntactic-face-function)))

  ;; Misc
  (setq-local comment-start "// ")
  (setq-local comment-end   "")
  (setq-local open-paren-in-column-0-is-defun-start nil)

  ;; Auto indent on }
  (setq-local electric-indent-chars
              (cons ?} (and (boundp 'electric-indent-chars)
                            electric-indent-chars)))

  ;; Allow paragraph fills for comments
  (setq-local comment-start-skip "\\(?://[/!]*\\|/\\*[*!]?\\)[[:space:]]*")
  (setq-local paragraph-start
              (concat "[[:space:]]*\\(?:"
                      comment-start-skip
                      "\\|\\*/?[[:space:]]*\\|\\)$"))
  (setq-local paragraph-separate paragraph-start)
  (setq-local normal-auto-fill-function #'rust-do-auto-fill)
  (setq-local fill-paragraph-function #'rust-fill-paragraph)
  (setq-local fill-forward-paragraph-function #'rust-fill-forward-paragraph)
  (setq-local adaptive-fill-function #'rust-find-fill-prefix)
  (setq-local adaptive-fill-first-line-regexp "")
  (setq-local comment-multi-line t)
  (setq-local comment-line-break-function #'rust-comment-indent-new-line)
  (setq-local imenu-generic-expression rust-imenu-generic-expression)
  (setq-local imenu-syntax-alist '((?! . "w"))) ; For macro_rules!
  (setq-local beginning-of-defun-function #'rust-beginning-of-defun)
  (setq-local end-of-defun-function #'rust-end-of-defun)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local electric-pair-inhibit-predicate
              #'rust-electric-pair-inhibit-predicate-wrap)
  (add-function :before-until (local 'electric-pair-skip-self)
                #'rust-electric-pair-skip-self)
  ;; Configure prettify
  (setq prettify-symbols-alist rust-prettify-symbols-alist)
  (setq prettify-symbols-compose-predicate #'rust--prettify-symbols-compose-p)

  (add-hook 'before-save-hook rust-before-save-hook nil t)
  (add-hook 'after-save-hook rust-after-save-hook nil t))

(provide 'rust-prog-mode)
;;; rust-prog-mode.el ends here