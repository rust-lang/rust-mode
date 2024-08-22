;;; rust-mode.el --- A major-mode for editing Rust source code -*-lexical-binding: t-*-

;; Version: 1.0.6
;; Author: Mozilla <rust-mode@noreply.github.com>
;; Url: https://github.com/rust-lang/rust-mode
;; Keywords: languages
;; Package-Requires: ((emacs "25.1"))

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Commentary:

;; This package implements a major-mode for editing Rust source code.

;;; Code:
(require 'rust-common)

(eval-when-compile
  (require 'rx)
  (require 'subr-x))

(defvar rust-load-optional-libraries t
  "Whether loading `rust-mode' also loads optional libraries.
This variable might soon be remove again.")

(when rust-load-optional-libraries
  (require 'rust-cargo)
  (require 'rust-compile)
  (require 'rust-playpen)
  (require 'rust-rustfmt))

;;; Customization

(defgroup rust-mode nil
  "Support for Rust code."
  :link '(url-link "https://www.rust-lang.org/")
  :group 'languages)

(defcustom rust-mode-treesitter-derive nil
  "Whether rust-mode should derive from the new treesitter mode `rust-ts-mode'
instead of `prog-mode'. This option requires emacs29+."
  :version "29.1"
  :type 'boolean
  :group 'rustic)

;;; Faces

(define-obsolete-face-alias 'rust-unsafe-face
  'rust-unsafe "0.6.0")
(define-obsolete-face-alias 'rust-question-mark-face
  'rust-question-mark "0.6.0")
(define-obsolete-face-alias 'rust-builtin-formatting-macro-face
  'rust-builtin-formatting-macro "0.6.0")
(define-obsolete-face-alias 'rust-string-interpolation-face
  'rust-string-interpolation "0.6.0")

;;; Mode

(defvar rust-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") #'rust-dbg-wrap-or-unwrap)
    (when rust-load-optional-libraries
      (define-key map (kbd "C-c C-c C-u") 'rust-compile)
      (define-key map (kbd "C-c C-c C-k") 'rust-check)
      (define-key map (kbd "C-c C-c C-t") 'rust-test)
      (define-key map (kbd "C-c C-c C-r") 'rust-run)
      (define-key map (kbd "C-c C-c C-l") 'rust-run-clippy)
      (define-key map (kbd "C-c C-f") 'rust-format-buffer)
      (define-key map (kbd "C-c C-n") 'rust-goto-format-problem))
    map)
  "Keymap for Rust major mode.")

;;;###autoload
(autoload 'rust-mode "rust-mode" "Major mode for Rust code." t)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(provide 'rust-mode)

(if (and rust-mode-treesitter-derive
         (version<= "29.1" emacs-version))
    (require 'rust-mode-treesitter)
  (require 'rust-prog-mode))

(require 'rust-utils)

;;; rust-mode.el ends here
