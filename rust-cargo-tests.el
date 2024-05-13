;;; rust-cargo-tests.el --- ERT tests for rust-cargo.el  -*- lexical-binding: t; -*-
(require 'rust-cargo)
(require 'ert)

(defun rust-test--wait-process-exit ()
  "Wait for comint process for current buffer to exit."
  (let ((proc (get-buffer-process (current-buffer))))
    (while (not (eq (process-status proc) 'exit))
      (sit-for 0.2))))

(defun rust-test--send-process-string (string)
  "Send STRING to comint process for current buffer."
  (let ((proc (get-buffer-process (current-buffer))))
    (comint-send-string proc string)))

(defmacro rust-test--with-main-file-buffer (expr)
  `(let* ((test-dir (expand-file-name "test-project/" default-directory))
          (main-file (expand-file-name "src/main.rs" test-dir)))
     (save-current-buffer
       (find-file main-file)
       ,expr)))

(defun rust-test--find-string (string)
  "Find STRING in current buffer."
  (goto-char (point-min))
  (not (null (search-forward string nil t))))


(ert-deftest rust-test-compile ()
  (skip-unless (executable-find rust-cargo-bin))
  (setq rust-cargo-default-arguments "")
  (rust-test--with-main-file-buffer
     (with-current-buffer (rust-compile)
       (rust-test--wait-process-exit)
       (should (rust-test--find-string "Compilation finished")))))

(ert-deftest rust-test-run ()
  (skip-unless (executable-find rust-cargo-bin))
  (setq rust-cargo-default-arguments "")
  (rust-test--with-main-file-buffer
     (with-current-buffer (rust-run)
       (rust-test--wait-process-exit)
       (should (rust-test--find-string "***run not interactive")))))

(ert-deftest rust-test-run-interactive ()
  (skip-unless (executable-find rust-cargo-bin))
  (setq rust-cargo-default-arguments "interactive")
  (rust-test--with-main-file-buffer
   ;; '(4) is default value when called interactively with universal argument
   (with-current-buffer (rust-run '(4))
     (rust-test--send-process-string "1234\n")
     (rust-test--wait-process-exit)
     (should (rust-test--find-string "***run interactive: 1234")))))
