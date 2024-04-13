;;; rust-mode-treesitter.el --- use native rust-ts-mode -*-lexical-binding: t-*-
;;; Commentary:

;; Derive from rust-ts-mode instead of prog-mode

;;; Code:

(require 'rust-mode)

;; Do not compile or load on Emacs releases that don't support
;; this.  See https://github.com/rust-lang/rust-mode/issues/520.
(when (version<= "29.1" emacs-version)
  (require 'treesit)
  (require 'rust-ts-mode)

  ;; HACK: `rust-ts-mode' adds itself to the `auto-mode-alist'
  ;; after us, so we need to readd `rust-mode' to the front of
  ;; the list after loading `rust-ts-mode'.
  (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

  (define-derived-mode rust-mode rust-ts-mode "Rust"
    "Major mode for Rust code.

\\{rust-mode-map}"
    :group 'rust-mode

    (add-hook 'before-save-hook rust-before-save-hook nil t)
    (add-hook 'after-save-hook rust-after-save-hook nil t)))

(provide 'rust-mode-treesitter)
;;; rust-mode-treesitter.el ends here
