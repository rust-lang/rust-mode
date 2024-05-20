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

(defcustom rust-cargo-locate-default-arguments '("--workspace")
  "Arguments for `cargo locate-project`. Remove `--workspace` if you
would prefer to use the local crate Cargo.toml instead of the
worksapce for commands like `cargo check`."
  :type '(repeat string)
  :group 'rust-mode)

(defcustom rust-cargo-default-arguments ""
  "Default arguments when running common cargo commands."
  :type 'string
  :group 'rust-mode)

;;; Buffer Project

(defvar-local rust-buffer-project nil)

(defun rust-buffer-project ()
  "Get project root if possible."
  ;; Copy environment variables into the new buffer, since
  ;; with-temp-buffer will re-use the variables' defaults, even if
  ;; they have been changed in this variable using e.g. envrc-mode.
  ;; See https://github.com/purcell/envrc/issues/12.
  (let ((env process-environment)
        (path exec-path))
    (with-temp-buffer
      ;; Copy the entire environment just in case there's something we
      ;; don't know we need.
      (setq-local process-environment env)
      ;; Set PATH so we can find cargo.
      (setq-local exec-path path)
      (let ((ret
             (let ((args
                    (append
                     (list rust-cargo-bin nil (list (current-buffer) nil) nil
                           "locate-project")
                     rust-cargo-locate-default-arguments)))
               (apply #'process-file args))))
        (when (/= ret 0)
          (error "`cargo locate-project' returned %s status: %s" ret (buffer-string)))
        (goto-char 0)
        (let ((output (let ((json-object-type 'alist))
                        (json-read))))
          (cdr (assoc-string "root" output)))))))

(defun rust-buffer-crate ()
  "Try to locate Cargo.toml using `locate-dominating-file'."
  (let ((dir (locate-dominating-file default-directory "Cargo.toml")))
    (if dir dir default-directory)))

(defun rust-update-buffer-project ()
  (setq-local rust-buffer-project (rust-buffer-project)))

(defun rust-maybe-initialize-buffer-project ()
  (setq-local rust-buffer-project nil)
  (when rust-always-locate-project-on-open
    (rust-update-buffer-project)))

(add-hook 'rust-mode-hook #'rust-maybe-initialize-buffer-project)

;;; Internal

(defun rust--compile (comint format-string &rest args)
  (when (null rust-buffer-project)
    (rust-update-buffer-project))
  (let ((default-directory
          (or (and rust-buffer-project
                   (file-name-directory rust-buffer-project))
              default-directory))
        ;; make sure comint is a boolean value
        (comint (not (not comint))))
    (compile (apply #'format format-string args) comint)))

;;; Commands

(defun rust-check ()
  "Compile using `cargo check`"
  (interactive)
  (rust--compile nil "%s check %s" rust-cargo-bin rust-cargo-default-arguments))

(defun rust-compile ()
  "Compile using `cargo build`"
  (interactive)
  (rust--compile nil "%s build %s" rust-cargo-bin rust-cargo-default-arguments))

(defun rust-compile-release ()
  "Compile using `cargo build --release`"
  (interactive)
  (rust--compile nil "%s build --release" rust-cargo-bin))

(defun rust-run (&optional comint)
  "Run using `cargo run`

If optional arg COMINT is t or invoked with universal prefix arg,
output buffer will be in comint mode, i.e. interactive."
  (interactive "P")
  (rust--compile comint "%s run %s" rust-cargo-bin rust-cargo-default-arguments))

(defun rust-run-release (&optional comint)
  "Run using `cargo run --release`

If optional arg COMINT is t or invoked with universal prefix arg,
output buffer will be in comint mode, i.e. interactive."
  (interactive "P")
  (rust--compile comint "%s run --release" rust-cargo-bin))

(defun rust-test ()
  "Test using `cargo test`"
  (interactive)
  (rust--compile nil "%s test %s" rust-cargo-bin rust-cargo-default-arguments))

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
    (rust--compile nil compile-command)))

;;; _
(provide 'rust-cargo)
;;; rust-cargo.el ends here
