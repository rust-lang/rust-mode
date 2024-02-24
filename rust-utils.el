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

(defun rust-insert-dbg-sexp ()
  "Insert the dbg! macro around a sexp if possible, insert at current position
if not. Move cursor to the end of macro."
  (when (rust-in-str)
    (up-list -1 t t))
  (let ((safe-to-forward t))
    (save-excursion
      (condition-case nil
          (forward-sexp)
        (error (setq safe-to-forward nil)
               nil)))
    (cond
     ((not safe-to-forward)
      (rust-insert-dbg-alone))
     (t
      (insert "(")
      (forward-sexp)
      (insert ")")
      (backward-sexp)
      (insert "dbg!")
      (forward-sexp)))))

(defun rust-insert-dbg-region ()
  "Insert the dbg! macro around a region. Move cursor to the end of macro."
  (when (< (mark) (point))
    (exchange-point-and-mark))
  (let ((old-point (point)))
    (insert-parentheses)
    (goto-char old-point))
  (insert "dbg!")
  (forward-sexp))

(defun rust-insert-dbg-alone ()
  "Insert the dbg! macro alone. Move cursor in between the brackets."
  (insert "dbg!()")
  (backward-char))

;;;###autoload
(defun rust-dbg-wrap-or-unwrap ()
  "Either remove or add the dbg! macro."
  (interactive)

  (cond

   ;; region
   ((region-active-p)
    (rust-insert-dbg-region))

   ;; alone
   ((or (looking-at-p " *$") (looking-at-p " *//.*"))
    (rust-insert-dbg-alone))

   ;; symbol
   (t
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
            (t (rust-insert-dbg-sexp)))))
   )
  )

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
