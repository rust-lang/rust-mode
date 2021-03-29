;;; rust-playpen.el --- Support for the Rust Playground  -*- lexical-binding:t -*-
;;; Commentary:

;; This library implements support for the Rust Playground,
;; which is hosted at https://play.rust-lang.org and developed
;; at https://github.com/integer32llc/rust-playground.

;;; Code:

(eval-when-compile (require 'url))

;;; Options

(defcustom rust-playpen-url-format "https://play.rust-lang.org/?code=%s"
  "Format string to use when submitting code to the playpen."
  :type 'string
  :group 'rust-mode)

(defcustom rust-shortener-url-format "https://is.gd/create.php?format=simple&url=%s"
  "Format string to use for creating the shortened link of a playpen submission."
  :type 'string
  :group 'rust-mode)

;;; Commands

(defun rust-playpen-region (begin end)
  "Create a shareable URL for the region from BEGIN to END on the Rust playpen."
  (interactive "r")
  (let* ((data (buffer-substring begin end))
         (escaped-data (url-hexify-string data))
         (escaped-playpen-url (url-hexify-string
                               (format rust-playpen-url-format escaped-data))))
    (if (> (length escaped-playpen-url) 5000)
        (error "encoded playpen data exceeds 5000 character limit (length %s)"
               (length escaped-playpen-url))
      (let ((shortener-url (format rust-shortener-url-format escaped-playpen-url))
            (url-request-method "POST"))
        (url-retrieve shortener-url
                      (lambda (state)
                        ;; filter out the headers etc. included at the
                        ;; start of the buffer: the relevant text
                        ;; (shortened url or error message) is exactly
                        ;; the last line.
                        (goto-char (point-max))
                        (let ((last-line (thing-at-point 'line t))
                              (err (plist-get state :error)))
                          (kill-buffer)
                          (if err
                              (error "failed to shorten playpen url: %s" last-line)
                            (message "%s" last-line)))))))))

(defun rust-playpen-buffer ()
  "Create a shareable URL for the contents of the buffer on the Rust playpen."
  (interactive)
  (rust-playpen-region (point-min) (point-max)))

;;; _
(provide 'rust-playpen)
;;; rust-playpen.el ends here
