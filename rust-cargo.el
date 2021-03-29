;;; rust-cargo.el --- Support for cargo                -*-lexical-binding: t-*-
;;; Commentary:

;; This library implements support for running `cargo'.

;;; Code:

(require 'json)

;;; Options

(defcustom rust-cargo-bin "cargo"
  "Path to cargo executable."
  :type 'string
  :group 'rust-mode)

(defcustom rust-always-locate-project-on-open nil
  "Whether to run `cargo locate-project' every time `rust-mode' is activated."
  :type 'boolean
  :group 'rust-mode)

;;; Buffer Project

(defvar-local rust-buffer-project nil)

(defun rust-buffer-project ()
  "Get project root if possible."
  (with-temp-buffer
    (let ((ret (call-process rust-cargo-bin nil t nil "locate-project")))
      (when (/= ret 0)
        (error "`cargo locate-project' returned %s status: %s" ret (buffer-string)))
      (goto-char 0)
      (let ((output (json-read)))
        (cdr (assoc-string "root" output))))))

(defun rust-update-buffer-project ()
  (setq-local rust-buffer-project (rust-buffer-project)))

(defun rust-maybe-initialize-buffer-project ()
  (setq-local rust-buffer-project nil)
  (when rust-always-locate-project-on-open
    (rust-update-buffer-project)))

(add-hook 'rust-mode-hook 'rust-maybe-initialize-buffer-project)

;;; Internal

(defun rust--compile (format-string &rest args)
  (when (null rust-buffer-project)
    (rust-update-buffer-project))
  (let ((default-directory
          (or (and rust-buffer-project
                   (file-name-directory rust-buffer-project))
              default-directory)))
    (compile (apply #'format format-string args))))

;;; Commands

(defun rust-check ()
  "Compile using `cargo check`"
  (interactive)
  (rust--compile "%s check" rust-cargo-bin))

(defun rust-compile ()
  "Compile using `cargo build`"
  (interactive)
  (rust--compile "%s build" rust-cargo-bin))

(defun rust-compile-release ()
  "Compile using `cargo build --release`"
  (interactive)
  (rust--compile "%s build --release" rust-cargo-bin))

(defun rust-run ()
  "Run using `cargo run`"
  (interactive)
  (rust--compile "%s run" rust-cargo-bin))

(defun rust-run-release ()
  "Run using `cargo run --release`"
  (interactive)
  (rust--compile "%s run --release" rust-cargo-bin))

(defun rust-test ()
  "Test using `cargo test`"
  (interactive)
  (rust--compile "%s test" rust-cargo-bin))

(defun rust-run-clippy ()
  "Run `cargo clippy'."
  (interactive)
  (when (null rust-buffer-project)
    (rust-update-buffer-project))
  (let* ((args (list rust-cargo-bin "clippy"
                     (concat "--manifest-path=" rust-buffer-project)))
         ;; set `compile-command' temporarily so `compile' doesn't
         ;; clobber the existing value
         (compile-command (mapconcat #'shell-quote-argument args " ")))
    (rust--compile compile-command)))

;;; _
(provide 'rust-cargo)
;;; rust-cargo.el ends here
