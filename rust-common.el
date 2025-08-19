;;; rust-common.el --- Common code for both modes -*-lexical-binding: t-*-
;;; Commentary:

;; rust-common code for both prog-mode and tree-sitter one

;;; Code:
(require 'rust-rustfmt)

(defcustom rust-before-save-hook 'rust-before-save-method
  "Function for formatting before save."
  :type 'function
  :group 'rust-mode)

(defcustom rust-after-save-hook 'rust-after-save-method
  "Default method to handle rustfmt invocation after save."
  :type 'function
  :group 'rust-mode)

(defcustom rust-indent-offset 4
  "Indent Rust code by this number of spaces."
  :type 'integer
  :group 'rust-mode
  :safe #'integerp)

(provide 'rust-common)
;;; rust-common.el ends here
