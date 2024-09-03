;;; rust-rustfmt.el --- Support for rustfmt         -*- lexical-binding:t -*-
;;; Commentary:

;; This library implements support for "rustfmt", a tool for
;; formatting Rust code according to style guidelines.

;;; Code:
;;; Options

(require 'rust-compile)
(require 'compile)

(defcustom rust-format-on-save nil
  "Format future rust buffers before saving using rustfmt."
  :type 'boolean
  :safe #'booleanp
  :group 'rust-mode)

(defcustom rust-format-show-buffer t
  "Show *rustfmt* buffer if formatting detected problems."
  :type 'boolean
  :safe #'booleanp
  :group 'rust-mode)

(defcustom rust-format-goto-problem t
  "Jump to location of first detected problem when formatting buffer."
  :type 'boolean
  :safe #'booleanp
  :group 'rust-mode)

(defcustom rust-rustfmt-bin "rustfmt"
  "Path to rustfmt executable."
  :type 'string
  :group 'rust-mode)

(defcustom rust-rustfmt-switches '("--edition" "2021")
  "Arguments to pass when invoking the `rustfmt' executable."
  :type '(repeat string)
  :group 'rust-mode)

;;; _

(defconst rust-rustfmt-buffername "*rustfmt*")

(define-compilation-mode rust-format-mode "rust-format"
  "Major mode for Rust compilation output."

  (setq-local compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rustc-refs rustc-refs-compilation-regexps))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rustc rustc-compilation-regexps))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rustc-colon rustc-colon-compilation-regexps))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'cargo cargo-compilation-regexps))
  (add-to-list 'compilation-error-regexp-alist-alist
               (cons 'rustc-panics rustc-panics-compilation-regexps))

  (setq-local compilation-error-regexp-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'rustc-refs)
  (add-to-list 'compilation-error-regexp-alist 'rustc)
  (add-to-list 'compilation-error-regexp-alist 'rustc-colon)
  (add-to-list 'compilation-error-regexp-alist 'cargo)
  (add-to-list 'compilation-error-regexp-alist 'rustc-panics)

  (add-hook 'next-error-hook #'rustc-scroll-down-after-next-error)

  (if (or compilation-auto-jump-to-first-error
          (eq compilation-scroll-output 'first-error))
      (set (make-local-variable 'compilation-auto-jump-to-next) t)))

(defun rust--format-call (buf)
  "Format BUF using rustfmt."
  (let ((path exec-path))
    (with-current-buffer (get-buffer-create rust-rustfmt-buffername)
      (setq-local exec-path path)
      (view-mode +1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring buf)
        (let* ((tmpf (make-temp-file "rustfmt"))
               (ret (apply #'call-process-region
                           (point-min)
                           (point-max)
                           rust-rustfmt-bin
                           t
                           `(t ,tmpf)
                           nil
                           rust-rustfmt-switches)))
          (unwind-protect
              (cond
               ((zerop ret)
                (if (not (string= (buffer-string)
                                  (with-current-buffer buf (buffer-string))))
                    ;; replace-buffer-contents was in emacs 26.1, but it
                    ;; was broken for non-ASCII strings, so we need 26.2.
                    (if (and (fboundp 'replace-buffer-contents)
                             (version<= "26.2" emacs-version))
                        (with-current-buffer buf
                          (replace-buffer-contents rust-rustfmt-buffername))
                      (copy-to-buffer buf (point-min) (point-max))))
                (let ((win (get-buffer-window rust-rustfmt-buffername)))
                  (if win
                      (quit-window t win)
                    (kill-buffer rust-rustfmt-buffername))))
               ((= ret 1)
                (erase-buffer)
                (insert-file-contents tmpf)

                (rust-format-mode) ;; render compilation errors in compilation-mode
                (setq-local compile-command (format "%s %s" rust-rustfmt-bin (buffer-file-name buf)))

                (rust--format-fix-rustfmt-buffer (buffer-name buf))
                (error "Rustfmt failed because of parsing errors, see %s buffer for details"
                       rust-rustfmt-buffername))
               ((= ret 3)
                (if (not (string= (buffer-string)
                                  (with-current-buffer buf (buffer-string))))
                    (copy-to-buffer buf (point-min) (point-max)))
                (erase-buffer)
                (insert-file-contents tmpf)
                (rust--format-fix-rustfmt-buffer (buffer-name buf))
                (error "Rustfmt could not format some lines, see %s buffer for details"
                       rust-rustfmt-buffername))
               (t
                (erase-buffer)
                (insert-file-contents tmpf)
                (rust--format-fix-rustfmt-buffer (buffer-name buf))
                (error "Rustfmt failed, see %s buffer for details"
                       rust-rustfmt-buffername)))
            (delete-file tmpf)))))))

;; Since we run rustfmt through stdin we get <stdin> markers in the
;; output. This replaces them with the buffer name instead.
(defun rust--format-fix-rustfmt-buffer (buffer-name)
  (save-match-data
    (with-current-buffer (get-buffer rust-rustfmt-buffername)
      (let ((inhibit-read-only t)
            (fixed (format "--> %s:" buffer-name)))
          (goto-char (point-min))
          (while (re-search-forward "--> \\(?:<stdin>\\|stdin\\):" nil t)
            (replace-match fixed))))))

;; If rust-mode has been configured to navigate to source of the error
;; or display it, do so -- and return true. Otherwise return nil to
;; indicate nothing was done.
(defun rust--format-error-handler ()
  (let ((ok nil))
    (when rust-format-show-buffer
      (display-buffer (get-buffer rust-rustfmt-buffername))
      (setq ok t))
    (when rust-format-goto-problem
      (rust-goto-format-problem)
      (setq ok t))
    ok))

(defun rust-goto-format-problem ()
  "Jumps to problem reported by rustfmt, if any.

In case of multiple problems cycles through them. Displays the
rustfmt complain in the echo area."
  (interactive)
  ;; This uses position in *rustfmt* buffer to know which is the next
  ;; error to jump to, and source: line in the buffer to figure which
  ;; buffer it is from.
  (let ((rustfmt (get-buffer rust-rustfmt-buffername)))
    (if (not rustfmt)
        (message "No %s, no problems." rust-rustfmt-buffername)
      (let ((target-buffer (with-current-buffer rustfmt
                             (save-excursion
                               (goto-char (point-min))
                               (when (re-search-forward "--> \\([^:]+\\):" nil t)
                                 (match-string 1)))))
            (target-point (with-current-buffer rustfmt
                            ;; No save-excursion, this is how we cycle through!
                            (let ((regex "--> [^:]+:\\([0-9]+\\):\\([0-9]+\\)"))
                              (when (or (re-search-forward regex nil t)
                                        (progn (goto-char (point-min))
                                               (re-search-forward regex nil t)))
                                (cons (string-to-number (match-string 1))
                                      (string-to-number (match-string 2)))))))
            (target-problem (with-current-buffer rustfmt
                              (save-excursion
                                (when (re-search-backward "^error:.+\n" nil t)
                                  (forward-char (length "error: "))
                                  (let ((p0 (point)))
                                    (if (re-search-forward "\nerror:.+\n" nil t)
                                        (buffer-substring p0 (point))
                                      (buffer-substring p0 (point-max)))))))))
        (when (and target-buffer (get-buffer target-buffer) target-point)
          (switch-to-buffer target-buffer)
          (goto-char (point-min))
          (forward-line (1- (car target-point)))
          (forward-char (1- (cdr target-point))))
        (unless rust-format-show-buffer
          (message target-problem))))))

(defconst rust--format-word "\
\\b\\(else\\|enum\\|fn\\|for\\|if\\|let\\|loop\\|\
match\\|struct\\|union\\|unsafe\\|while\\)\\b")
(defconst rust--format-line "\\(\n\\)")

;; Counts number of matches of regex beginning up to max-beginning,
;; leaving the point at the beginning of the last match.
(defun rust--format-count (regex max-beginning)
  (let ((count 0)
        save-point
        beginning)
    (while (and (< (point) max-beginning)
                (re-search-forward regex max-beginning t))
      (setq count (1+ count))
      (setq beginning (match-beginning 1)))
    ;; try one more in case max-beginning lies in the middle of a match
    (setq save-point (point))
    (when (re-search-forward regex nil t)
      (let ((try-beginning (match-beginning 1)))
        (if (> try-beginning max-beginning)
            (goto-char save-point)
          (setq count (1+ count))
          (setq beginning try-beginning))))
    (when beginning (goto-char beginning))
    count))

;; Gets list describing pos or (point).
;; The list contains:
;; 1. the number of matches of rust--format-word,
;; 2. the number of matches of rust--format-line after that,
;; 3. the number of columns after that.
(defun rust--format-get-loc (buffer &optional pos)
  (with-current-buffer buffer
    (save-excursion
      (let ((pos (or pos (point)))
            words lines columns)
        (goto-char (point-min))
        (setq words (rust--format-count rust--format-word pos))
        (setq lines (rust--format-count rust--format-line pos))
        (if (> lines 0)
            (if (= (point) pos)
                (setq columns -1)
              (forward-char 1)
              (goto-char pos)
              (setq columns (current-column)))
          (let ((initial-column (current-column)))
            (goto-char pos)
            (setq columns (- (current-column) initial-column))))
        (list words lines columns)))))

;; Moves the point forward by count matches of regex up to max-pos,
;; and returns new max-pos making sure final position does not include another match.
(defun rust--format-forward (regex count max-pos)
  (when (< (point) max-pos)
    (let ((beginning (point)))
      (while (> count 0)
        (setq count (1- count))
        (re-search-forward regex nil t)
        (setq beginning (match-beginning 1)))
      (when (re-search-forward regex nil t)
        (setq max-pos (min max-pos (match-beginning 1))))
      (goto-char beginning)))
  max-pos)

;; Gets the position from a location list obtained using rust--format-get-loc.
(defun rust--format-get-pos (buffer loc)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((max-pos (point-max))
            (words (pop loc))
            (lines (pop loc))
            (columns (pop loc)))
        (setq max-pos (rust--format-forward rust--format-word words max-pos))
        (setq max-pos (rust--format-forward rust--format-line lines max-pos))
        (when (> lines 0) (forward-char))
        (let ((initial-column (current-column))
              (save-point (point)))
          (move-end-of-line nil)
          (when (> (current-column) (+ initial-column columns))
            (goto-char save-point)
            (forward-char columns)))
        (min (point) max-pos)))))

(defun rust-format-diff-buffer ()
  "Show diff to current buffer from rustfmt.

Return the created process."
  (interactive)
  (unless (executable-find rust-rustfmt-bin)
    (error "Could not locate executable %S" rust-rustfmt-bin))
  (let* ((buffer
          (with-current-buffer
              (get-buffer-create "*rustfmt-diff*")
            (let ((inhibit-read-only t))
              (erase-buffer))
            (current-buffer)))
         (proc
          (apply #'start-process
                 "rustfmt-diff"
                 buffer
                 rust-rustfmt-bin
                 "--check"
                 (cons (buffer-file-name)
                       rust-rustfmt-switches))))
    (set-process-sentinel proc #'rust-format-diff-buffer-sentinel)
    proc))

(defun rust-format-diff-buffer-sentinel (process _e)
  (when (eq 'exit (process-status process))
    (if (> (process-exit-status process) 0)
        (with-current-buffer "*rustfmt-diff*"
          (let ((inhibit-read-only t))
            (diff-mode))
          (pop-to-buffer (current-buffer)))
      (message "rustfmt check passed."))))

(defun rust--format-buffer-using-replace-buffer-contents ()
  (condition-case err
      (progn
        (rust--format-call (current-buffer))
        (message "Formatted buffer with rustfmt."))
    (error
     (or (rust--format-error-handler)
         (signal (car err) (cdr err))))))

(defun rust--format-buffer-saving-position-manually ()
  (let* ((current (current-buffer))
         (base (or (buffer-base-buffer current) current))
         buffer-loc
         window-loc)
    (dolist (buffer (buffer-list))
      (when (or (eq buffer base)
                (eq (buffer-base-buffer buffer) base))
        (push (list buffer
                    (rust--format-get-loc buffer nil))
              buffer-loc)))
    (dolist (frame (frame-list))
      (dolist (window (window-list frame))
        (let ((buffer (window-buffer window)))
          (when (or (eq buffer base)
                    (eq (buffer-base-buffer buffer) base))
            (let ((start (window-start window))
                  (point (window-point window)))
              (push (list window
                          (rust--format-get-loc buffer start)
                          (rust--format-get-loc buffer point))
                    window-loc))))))
    (condition-case err
        (unwind-protect
            ;; save and restore window start position
            ;; after reformatting
            ;; to avoid the disturbing scrolling
            (let ((w-start (window-start)))
              (rust--format-call (current-buffer))
              (set-window-start (selected-window) w-start)
              (message "Formatted buffer with rustfmt."))
          (dolist (loc buffer-loc)
            (let* ((buffer (pop loc))
                   (pos (rust--format-get-pos buffer (pop loc))))
              (with-current-buffer buffer
                (goto-char pos))))
          (dolist (loc window-loc)
            (let* ((window (pop loc))
                   (buffer (window-buffer window))
                   (start (rust--format-get-pos buffer (pop loc)))
                   (pos (rust--format-get-pos buffer (pop loc))))
              (unless (eq buffer current)
                (set-window-start window start))
              (set-window-point window pos))))
      (error
       (or (rust--format-error-handler)
           (signal (car err) (cdr err)))))))

(defun rust-format-buffer ()
  "Format the current buffer using rustfmt."
  (interactive)
  (unless (executable-find rust-rustfmt-bin)
    (error "Could not locate executable \"%s\"" rust-rustfmt-bin))
  ;; If emacs version >= 26.2, we can use replace-buffer-contents to
  ;; preserve location and markers in buffer, otherwise we can try to
  ;; save locations as best we can, though we still lose markers.
  (save-excursion
    (if (version<= "26.2" emacs-version)
        (rust--format-buffer-using-replace-buffer-contents)
      (rust--format-buffer-saving-position-manually))))

(defun rust-enable-format-on-save ()
  "Enable formatting using rustfmt when saving buffer."
  (interactive)
  (setq-local rust-format-on-save t))

(defun rust-disable-format-on-save ()
  "Disable formatting using rustfmt when saving buffer."
  (interactive)
  (setq-local rust-format-on-save nil))

;;; Hooks

(defun rust-before-save-method ()
  (when rust-format-on-save
    (condition-case e
        (rust-format-buffer)
      (message (format "rust-before-save-hook: %S %S"
                     (car e)
                     (cdr e))))))

(defun rust-after-save-method ()
  (when rust-format-on-save
    (if (not (executable-find rust-rustfmt-bin))
        (error "Could not locate executable \"%s\"" rust-rustfmt-bin)
      (when (get-buffer rust-rustfmt-buffername)
        ;; KLDUGE: re-run the error handlers -- otherwise message area
        ;; would show "Wrote ..." instead of the error description.
        (or (rust--format-error-handler)
            (message "rustfmt detected problems, see %s for more."
                     rust-rustfmt-buffername))))))

;;; _
(provide 'rust-rustfmt)
;;; rust-rustfmt.el ends here
