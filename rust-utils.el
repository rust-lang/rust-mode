;;; rust-utils.el --- Various Rust utilities        -*- lexical-binding:t -*-
;;; Commentary:

;; This library implements various utilities for dealing with Rust
;; code.

;;; Code:

(require 'thingatpt)

(require 'rust-mode) ; for `rust-in-str' and `rust-looking-back-str'

;;; Promote module

(defun rust-promote-module-into-dir ()
  "Promote the module file visited by the current buffer into its own directory.

For example, if the current buffer is visiting the file `foo.rs',
then this function creates the directory `foo' and renames the
file to `foo/mod.rs'.  The current buffer will be updated to
visit the new file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer is not visiting a file.")
      (if (string-equal (file-name-nondirectory filename) "mod.rs")
          (message "Won't promote a module file already named mod.rs.")
        (let* ((basename (file-name-sans-extension
                          (file-name-nondirectory filename)))
               (mod-dir (file-name-as-directory
                         (concat (file-name-directory filename) basename)))
               (new-name (concat mod-dir "mod.rs")))
          (mkdir mod-dir t)
          (rename-file filename new-name 1)
          (set-visited-file-name new-name))))))

;;; dbg! macro

(defun rust-insert-dbg ()
  "Insert the dbg! macro."
  (cond ((region-active-p)
         (when (< (mark) (point))
           (exchange-point-and-mark))
         (let ((old-point (point)))
           (insert-parentheses)
           (goto-char old-point)))
        (t
         (when (rust-in-str)
           (up-list -1 t t))
         (insert "(")
         (forward-sexp)
         (insert ")")
         (backward-sexp)))
  (insert "dbg!"))

;;;###autoload
(defun rust-dbg-wrap-or-unwrap ()
  "Either remove or add the dbg! macro."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (rust-insert-dbg)

      (let ((beginning-of-symbol (ignore-errors (beginning-of-thing 'symbol))))
        (when beginning-of-symbol
          (goto-char beginning-of-symbol)))

      (let ((dbg-point (save-excursion
                         (or (and (looking-at-p "dbg!") (+ 4 (point)))
                             (ignore-errors
                               (while (not (rust-looking-back-str "dbg!"))
                                 (backward-up-list))
                               (point))))))
        (cond (dbg-point
               (goto-char dbg-point)
               (delete-char -4)
               (delete-pair))
              (t (rust-insert-dbg)))))))

(defun rust-toggle-mutability ()
  "Toggles the mutability of the variable defined on the current line"
  (interactive)
  (save-excursion
    (back-to-indentation)
    (forward-word)
    (if (string= " mut" (buffer-substring (point) (+ (point) 4)))
        (delete-region (point) (+ (point) 4))
      (insert " mut"))))
;;; _
(provide 'rust-utils)
;;; rust-utils.el ends here
