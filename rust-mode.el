;;; rust-mode.el --- A major emacs mode for editing Rust source code

;; Version: 0.2.0
;; Author: Mozilla
;; Url: https://github.com/rust-lang/rust-mode
;; Keywords: languages

;;; Commentary:
;;

;;; Code:

(eval-when-compile (require 'misc)
                   (require 'rx))

;; for GNU Emacs < 24.3
(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

;; Syntax definitions and helpers
(defvar rust-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

(defvar rust-mode-character-literal-syntax-table
  (let ((table (make-syntax-table rust-mode-syntax-table)))
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "_" table)

    table))

(defgroup rust-mode nil
  "Support for Rust code."
  :link '(url-link "http://www.rust-lang.org/")
  :group 'languages)

(defcustom rust-indent-offset 4
  "Indent Rust code by this number of spaces."
  :type 'integer
  :group 'rust-mode)

(defcustom rust-indent-method-chain nil
  "Indent Rust method chains, aligned by the '.' operators"
  :type 'boolean
  :group 'rust-mode)

(defcustom rust-playpen-url-format "https://play.rust-lang.org/?code=%s"
  "Format string to use when submitting code to the playpen"
  :type 'string
  :group 'rust-mode)
(defcustom rust-shortener-url-format "http://is.gd/create.php?format=simple&url=%s"
  "Format string to use for creating the shortened link of a playpen submission"
  :type 'string
  :group 'rust-mode)

(defun rust-paren-level () (nth 0 (syntax-ppss)))
(defun rust-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun rust-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))
(defun rust-rewind-irrelevant ()
  (let ((starting (point)))
    (skip-chars-backward "[:space:]\n")
    (if (looking-back "\\*/" nil) (backward-char))
    (if (rust-in-str-or-cmnt)
        (rust-rewind-past-str-cmnt))
    (if (/= starting (point))
        (rust-rewind-irrelevant))))

(defun rust-align-to-expr-after-brace ()
  (save-excursion
    (forward-char)
    ;; We don't want to indent out to the open bracket if the
    ;; open bracket ends the line
    (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
      (when (looking-at "[[:space:]]")
    (forward-word 1)
    (backward-word 1))
      (current-column))))

(defun rust-rewind-to-beginning-of-current-level-expr ()
  (let ((current-level (rust-paren-level)))
    (back-to-indentation)
    (when (looking-at "->")
      (rust-rewind-irrelevant)
      (back-to-indentation))
    (while (> (rust-paren-level) current-level)
      (backward-up-list)
      (back-to-indentation))))

(defconst rust-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")

(defun rust-align-to-method-chain ()
  (save-excursion
    ;; for method-chain alignment to apply, we must be looking at
    ;; another method call or field access or something like
    ;; that. This avoids rather "eager" jumps in situations like:
    ;;
    ;; {
    ;;     something.foo()
    ;; <indent>
    ;;
    ;; Without this check, we would wind up with the cursor under the
    ;; `.`. In an older version, I had the inverse of the current
    ;; check, where we checked for situations that should NOT indent,
    ;; vs checking for the one situation where we SHOULD. It should be
    ;; clear that this is more robust, but also I find it mildly less
    ;; annoying to have to press tab again to align to a method chain
    ;; than to have an over-eager indent in all other cases which must
    ;; be undone via tab.

    (when (looking-at (concat "\s*\." rust-re-ident))
      (forward-line -1)
      (end-of-line)

      (let
          ;; skip-dot-identifier is used to position the point at the
          ;; `.` when looking at something like
          ;;
          ;;      foo.bar
          ;;         ^   ^
          ;;         |   |
          ;;         |  position of point
          ;;       returned offset
          ;;
          ((skip-dot-identifier
            (lambda ()
              (when (looking-back (concat "\\." rust-re-ident) nil)
                (forward-thing 'symbol -1)
                (backward-char)
                (- (current-column) rust-indent-offset)))))
        (cond
         ;; foo.bar(...)
         ((looking-back ")" nil)
          (backward-list 1)
          (funcall skip-dot-identifier))

         ;; foo.bar
         (t (funcall skip-dot-identifier)))))))

(defun rust-mode-indent-line ()
  (interactive)
  (let ((indent
         (save-excursion
           (back-to-indentation)
           ;; Point is now at beginning of current line
           (let* ((level (rust-paren-level))
                  (baseline
                   ;; Our "baseline" is one level out from the indentation of the expression
                   ;; containing the innermost enclosing opening bracket.  That
                   ;; way if we are within a block that has a different
                   ;; indentation than this mode would give it, we still indent
                   ;; the inside of it correctly relative to the outside.
                   (if (= 0 level)
                       0
                     (or
                      (when rust-indent-method-chain
                        (rust-align-to-method-chain))
                      (save-excursion
                        (rust-rewind-irrelevant)
                        (backward-up-list)
                        (rust-rewind-to-beginning-of-current-level-expr)
                        (+ (current-column) rust-indent-offset))))))
             (cond
              ;; Indent inside a non-raw string only if the the previous line
              ;; ends with a backslash that is inside the same string
              ((nth 3 (syntax-ppss))
               (let*
                   ((string-begin-pos (nth 8 (syntax-ppss)))
                    (end-of-prev-line-pos (when (> (line-number-at-pos) 1)
                                            (save-excursion
                                              (forward-line -1)
                                              (end-of-line)
                                              (point)))))
                 (when
                     (and
                      ;; If the string begins with an "r" it's a raw string and
                      ;; we should not change the indentation
                      (/= ?r (char-after string-begin-pos))

                      ;; If we're on the first line this will be nil and the
                      ;; rest does not apply
                      end-of-prev-line-pos

                      ;; The end of the previous line needs to be inside the
                      ;; current string...
                      (> end-of-prev-line-pos string-begin-pos)

                      ;; ...and end with a backslash
                      (= ?\\ (char-before end-of-prev-line-pos)))

                   ;; Indent to the same level as the previous line, or the
                   ;; start of the string if the previous line starts the string
                   (if (= (line-number-at-pos end-of-prev-line-pos) (line-number-at-pos string-begin-pos))
                       ;; The previous line is the start of the string.
                       ;; If the backslash is the only character after the
                       ;; string beginning, indent to the next indent
                       ;; level.  Otherwise align with the start of the string.
                       (if (> (- end-of-prev-line-pos string-begin-pos) 2)
                           (save-excursion
                             (goto-char (+ 1 string-begin-pos))
                             (current-column))
                         baseline)

                     ;; The previous line is not the start of the string, so
                     ;; match its indentation.
                     (save-excursion
                       (goto-char end-of-prev-line-pos)
                       (back-to-indentation)
                       (current-column))))))

              ;; A function return type is indented to the corresponding function arguments
              ((looking-at "->")
               (save-excursion
                 (backward-list)
                 (or (rust-align-to-expr-after-brace)
                     (+ baseline rust-indent-offset))))

              ;; A closing brace is 1 level unindented
              ((looking-at "}") (- baseline rust-indent-offset))

              ;; Doc comments in /** style with leading * indent to line up the *s
              ((and (nth 4 (syntax-ppss)) (looking-at "*"))
               (+ 1 baseline))

              ;; If we're in any other token-tree / sexp, then:
              (t
               (or
                ;; If we are inside a pair of braces, with something after the
                ;; open brace on the same line and ending with a comma, treat
                ;; it as fields and align them.
                (when (> level 0)
                  (save-excursion
                    (rust-rewind-irrelevant)
                    (backward-up-list)
                    ;; Point is now at the beginning of the containing set of braces
                    (rust-align-to-expr-after-brace)))

                (progn
                  (back-to-indentation)
                  ;; Point is now at the beginning of the current line
                  (if (or
                       ;; If this line begins with "else" or "{", stay on the
                       ;; baseline as well (we are continuing an expression,
                       ;; but the "else" or "{" should align with the beginning
                       ;; of the expression it's in.)
                       (looking-at "\\<else\\>\\|{")

                       (save-excursion
                         (rust-rewind-irrelevant)
                         ;; Point is now at the end of the previous ine
                         (or
                          ;; If we are at the first line, no indentation is needed, so stay at baseline...
                          (= 1 (line-number-at-pos (point)))
                          ;; ..or if the previous line ends with any of these:
                          ;;     { ? : ( , ; [ }
                          ;; then we are at the beginning of an expression, so stay on the baseline...
                          (looking-back "[(,:;?[{}]\\|[^|]|" nil)
                          ;; or if the previous line is the end of an attribute, stay at the baseline...
                          (progn (rust-rewind-to-beginning-of-current-level-expr) (looking-at "#")))))
                      baseline

                    ;; Otherwise, we are continuing the same expression from the previous line,
                    ;; so add one additional indent level
                    (+ baseline rust-indent-offset))))))))))

    (when indent
      ;; If we're at the beginning of the line (before or at the current
      ;; indentation), jump with the indentation change.  Otherwise, save the
      ;; excursion so that adding the indentations will leave us at the
      ;; equivalent position within the line to where we were before.
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))


;; Font-locking definitions and helpers
(defconst rust-mode-keywords
  '("as"
    "box" "break"
    "const" "continue" "crate"
    "do"
    "else" "enum" "extern"
    "false" "fn" "for"
    "if" "impl" "in"
    "let" "loop"
    "match" "mod" "move" "mut"
    "priv" "pub"
    "ref" "return"
    "self" "static" "struct" "super"
    "true" "trait" "type"
    "unsafe" "use"
    "virtual"
    "where" "while"))

(defconst rust-special-types
  '("u8" "i8"
    "u16" "i16"
    "u32" "i32"
    "u64" "i64"

    "f32" "f64"
    "float" "int" "uint" "isize" "usize"
    "bool"
    "str" "char"))

(defconst rust-re-CamelCase "[[:upper:]][[:word:][:multibyte:]_[:digit:]]*")
(defun rust-re-word (inner) (concat "\\<" inner "\\>"))
(defun rust-re-grab (inner) (concat "\\(" inner "\\)"))
(defun rust-re-grabword (inner) (rust-re-grab (rust-re-word inner)))
(defun rust-re-item-def (itype)
  (concat (rust-re-word itype) "[[:space:]]+" (rust-re-grab rust-re-ident)))

;; (See PR #42 -- this is just like `(regexp-opt words 'symbols)` from
;; newer Emacs versions, but will work on Emacs 23.)
(defun regexp-opt-symbols (words)
  (concat "\\_<" (regexp-opt words t) "\\_>"))

(defvar rust-mode-font-lock-keywords
  (append
   `(
     ;; Keywords proper
     (,(regexp-opt-symbols rust-mode-keywords) . font-lock-keyword-face)

     ;; Special types
     (,(regexp-opt-symbols rust-special-types) . font-lock-type-face)

     ;; Attributes like `#[bar(baz)]` or `#![bar(baz)]` or `#[bar = "baz"]`
     (,(rust-re-grab (concat "#\\!?\\[" rust-re-ident "[^]]*\\]"))
      1 font-lock-preprocessor-face keep)

     ;; Syntax extension invocations like `foo!`, highlight including the !
     (,(concat (rust-re-grab (concat rust-re-ident "!")) "[({[:space:][]")
      1 font-lock-preprocessor-face)

     ;; Field names like `foo:`, highlight excluding the :
     (,(concat (rust-re-grab rust-re-ident) ":[^:]") 1 font-lock-variable-name-face)

     ;; Module names like `foo::`, highlight including the ::
     (,(rust-re-grab (concat rust-re-ident "::")) 1 font-lock-type-face)

     ;; Lifetimes like `'foo`
     (,(concat "'" (rust-re-grab rust-re-ident) "[^']") 1 font-lock-variable-name-face)

     ;; CamelCase Means Type Or Constructor
     (,(rust-re-grabword rust-re-CamelCase) 1 font-lock-type-face)
     )

   ;; Item definitions
   (mapcar #'(lambda (x)
               (list (rust-re-item-def (car x))
                     1 (cdr x)))
           '(("enum" . font-lock-type-face)
             ("struct" . font-lock-type-face)
             ("type" . font-lock-type-face)
             ("mod" . font-lock-type-face)
             ("use" . font-lock-type-face)
             ("fn" . font-lock-function-name-face)
             ("static" . font-lock-constant-face)))))

(defun rust-look-for-raw-string (bound)
  ;; Find a raw string, but only if it's not in the middle of another string or
  ;; a comment

  (let* ((raw-str-regexp
          (rx
           (seq
            ;; The "r" starts the raw string.  Capture it as group 1 to mark it as such syntactically:
            (group "r")

            ;; Then either:
            (or
             ;; a sequence at least one "#" (followed by quote).  Capture all
             ;; but the last "#" as group 2 for this case.
             (seq (group (* "#")) "#\"")

             ;; ...or a quote without any "#".  Capture it as group 3. This is
             ;; used later to match the opposite quote only if this capture
             ;; occurred
             (group "\""))

            ;; The contents of the string:
            (*? anything)

            ;; If there are any backslashes at the end of the string, capture
            ;; them as group 4 so we can suppress the normal escape syntax
            ;; parsing:
            (group (* "\\"))

            ;; Then the end of the string--the backreferences ensure that we
            ;; only match the kind of ending that corresponds to the beginning
            ;; we had:
            (or
             ;; There were "#"s - capture the last one as group 5 to mark it as
             ;; the end of the string:
             (seq "\"" (backref 2) (group "#"))

             ;; No "#"s - capture the ending quote (using a backref to group 3,
             ;; so that we can't match a quote if we had "#"s) as group 6
             (group (backref 3))))))
         ;; If it matches, it ends up with the starting character of the string
         ;; as group 1, any ending backslashes as group 4, and the ending
         ;; character as either group 5 or group 6.

         (ret-list (save-excursion
                    (let* ((match-end (re-search-forward raw-str-regexp bound t))
                           (ret-list (and match-end (list match-end (match-beginning 0) (match-data) (point)))))
                      (when (and ret-list
                                 (save-excursion
                                   (goto-char (nth 1 ret-list))
                                   (not (rust-in-str-or-cmnt))))
                        ret-list)))))
    (when ret-list
      (goto-char (nth 3 ret-list))
      (set-match-data (nth 2 ret-list))
      (nth 0 ret-list))))

(defvar rust-mode-font-lock-syntactic-keywords
  (append
   ;; Handle single quoted character literals:
   (mapcar (lambda (re) (list re '(1 "\"") '(2 "\"")))
           '("\\('\\)[^']\\('\\)"
             "\\('\\)\\\\['nrt\"\\]\\('\\)"
             "\\('\\)\\\\x[[:xdigit:]]\\{2\\}\\('\\)"
             "\\('\\)\\\\u[[:xdigit:]]\\{4\\}\\('\\)"
             "\\('\\)\\\\U[[:xdigit:]]\\{8\\}\\('\\)"))
   ;; Handle raw strings:
   `((rust-look-for-raw-string (1 "|") (4 "_" nil t) (5 "|" nil t) (6 "|" nil t)))))

(defun rust-fill-prefix-for-comment-start (line-start)
  "Determine what to use for `fill-prefix' based on what is at the beginning of a line."
  (let ((result
         ;; Replace /* with same number of spaces
         (replace-regexp-in-string
          "\\(?:/\\*+\\)[!*]"
          (lambda (s)
            ;; We want the * to line up with the first * of the comment start
            (concat (make-string (- (length s) 2) ?\x20) "*"))
          line-start)))
    ;; Make sure we've got at least one space at the end
    (if (not (= (aref result (- (length result) 1)) ?\x20))
        (setq result (concat result " ")))
    result))

(defun rust-in-comment-paragraph (body)
  ;; We might move the point to fill the next comment, but we don't want it
  ;; seeming to jump around on the user
  (save-excursion
    ;; If we're outside of a comment, with only whitespace and then a comment
    ;; in front, jump to the comment and prepare to fill it.
    (when (not (nth 4 (syntax-ppss)))
      (beginning-of-line)
      (when (looking-at (concat "[[:space:]\n]*" comment-start-skip))
        (goto-char (match-end 0))))

    ;; We need this when we're moving the point around and then checking syntax
    ;; while doing paragraph fills, because the cache it uses isn't always
    ;; invalidated during this.
    (syntax-ppss-flush-cache 1)
    ;; If we're at the beginning of a comment paragraph with nothing but
    ;; whitespace til the next line, jump to the next line so that we use the
    ;; existing prefix to figure out what the new prefix should be, rather than
    ;; inferring it from the comment start.
    (let ((next-bol (line-beginning-position 2)))
      (while (save-excursion
               (end-of-line)
               (syntax-ppss-flush-cache 1)
               (and (nth 4 (syntax-ppss))
                    (save-excursion
                      (beginning-of-line)
                      (looking-at paragraph-start))
                    (looking-at "[[:space:]]*$")
                    (nth 4 (syntax-ppss next-bol))))
        (goto-char next-bol)))

    (syntax-ppss-flush-cache 1)
    ;; If we're on the last line of a multiline-style comment that started
    ;; above, back up one line so we don't mistake the * of the */ that ends
    ;; the comment for a prefix.
    (when (save-excursion
            (and (nth 4 (syntax-ppss (line-beginning-position 1)))
                 (looking-at "[[:space:]]*\\*/")))
      (goto-char (line-end-position 0)))
    (funcall body)))

(defun rust-with-comment-fill-prefix (body)
  (let*
      ((line-string (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
       (line-comment-start
        (when (nth 4 (syntax-ppss))
          (cond
           ;; If we're inside the comment and see a * prefix, use it
           ((string-match "^\\([[:space:]]*\\*+[[:space:]]*\\)"
                          line-string)
            (match-string 1 line-string))
           ;; If we're at the start of a comment, figure out what prefix
           ;; to use for the subsequent lines after it
           ((string-match (concat "[[:space:]]*" comment-start-skip) line-string)
            (rust-fill-prefix-for-comment-start
             (match-string 0 line-string))))))
       (fill-prefix
        (or line-comment-start
            fill-prefix)))
    (funcall body)))

(defun rust-find-fill-prefix ()
  (rust-in-comment-paragraph (lambda () (rust-with-comment-fill-prefix (lambda () fill-prefix)))))

(defun rust-fill-paragraph (&rest args)
  "Special wrapping for `fill-paragraph' to handle multi-line comments with a * prefix on each line."
  (rust-in-comment-paragraph
   (lambda ()
     (rust-with-comment-fill-prefix
      (lambda ()
        (let
            ((fill-paragraph-function
              (if (not (eq fill-paragraph-function 'rust-fill-paragraph))
                  fill-paragraph-function))
             (fill-paragraph-handle-comment t))
          (apply 'fill-paragraph args)
          t))))))

(defun rust-do-auto-fill (&rest args)
  "Special wrapping for `do-auto-fill' to handle multi-line comments with a * prefix on each line."
  (rust-with-comment-fill-prefix
   (lambda ()
     (apply 'do-auto-fill args)
     t)))

(defun rust-fill-forward-paragraph (arg)
  ;; This is to work around some funny behavior when a paragraph separator is
  ;; at the very top of the file and there is a fill prefix.
  (let ((fill-prefix nil)) (forward-paragraph arg)))

(defun rust-comment-indent-new-line (&optional arg)
  (rust-with-comment-fill-prefix
   (lambda () (comment-indent-new-line arg))))

;;; Imenu support
(defvar rust-imenu-generic-expression
  (append (mapcar #'(lambda (x)
                      (list nil (rust-re-item-def x) 1))
                  '("enum" "struct" "type" "mod" "fn" "trait"))
          `(("Impl" ,(rust-re-item-def "impl") 1)))
  "Value for `imenu-generic-expression' in Rust mode.

Create a flat index of the item definitions in a Rust file.

Imenu will show all the enums, structs, etc. at the same level.
Implementations will be shown under the `Impl` subheading.  Use
idomenu (imenu with `ido-mode') for best mileage.")

;;; Defun Motions

;;; Start of a Rust item
(defvar rust-top-item-beg-re
  (concat "^\\s-*\\(?:priv\\|pub\\)?\\s-*"
          (regexp-opt
           '("enum" "struct" "type" "mod" "use" "fn" "static" "impl"
             "extern" "impl" "static" "trait"))))

(defun rust-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.

With ARG, move backward multiple defuns.  Negative ARG means
move forward.

This is written mainly to be used as `beginning-of-defun-function' for Rust.
Don't move to the beginning of the line. `beginning-of-defun',
which calls this, does that afterwards."
  (interactive "p")
  (re-search-backward (concat "^\\(" rust-top-item-beg-re "\\)\\_>")
                      nil 'move (or arg 1)))

(defun rust-end-of-defun ()
  "Move forward to the next end of defun.

With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

Assume that this is called after beginning-of-defun. So point is
at the beginning of the defun body.

This is written mainly to be used as `end-of-defun-function' for Rust."
  (interactive)
  ;; Find the opening brace
  (if (re-search-forward "[{]" nil t)
      (progn
        (goto-char (match-beginning 0))
        ;; Go to the closing brace
        (condition-case err
            (forward-sexp)
          (scan-error
           ;; The parentheses are unbalanced; instead of being unable to fontify, just jump to the end of the buffer
           (goto-char (point-max)))))
    ;; There is no opening brace, so consider the whole buffer to be one "defun"
    (goto-char (point-max))))

;; Angle-bracket matching. This is kind of a hack designed to deal
;; with the fact that we can't add angle-brackets to the list of
;; matching characters unconditionally. Basically we just have some
;; special-case code such that whenever `>` is typed, we look
;; backwards to find a matching `<` and highlight it, whether or not
;; this is *actually* appropriate. This could be annoying so it is
;; configurable (but on by default because it's awesome).

(defcustom rust-blink-matching-angle-brackets t
  "Blink matching `<` (if any) when `>` is typed"
  :type 'boolean
  :group 'rust-mode)

(defvar rust-point-before-matching-angle-bracket 0)

(defvar rust-matching-angle-bracker-timer nil)

(defun rust-find-matching-angle-bracket ()
  (save-excursion
    (let ((angle-brackets 1)
          (start-point (point))
          (invalid nil))
      (while (and
              ;; didn't find a match
              (> angle-brackets 0)
              ;; we have no guarantee of a match, so give up eventually
              (< (- start-point (point)) blink-matching-paren-distance)
              ;; didn't hit the top of the buffer
              (> (point) (point-min))
              ;; didn't hit something else weird like a `;`
              (not invalid))
        (backward-char 1)
        (cond
         ((looking-at ">")
           (setq angle-brackets (+ angle-brackets 1)))
          ((looking-at "<")
           (setq angle-brackets (- angle-brackets 1)))
          ((looking-at "[;{]")
           (setq invalid t))))
      (cond
       ((= angle-brackets 0) (point))
       (t nil)))))

(defun rust-restore-point-after-angle-bracket ()
  (goto-char rust-point-before-matching-angle-bracket)
  (when rust-matching-angle-bracker-timer
    (cancel-timer rust-matching-angle-bracker-timer))
  (setq rust-matching-angle-bracker-timer nil)
  (remove-hook 'pre-command-hook 'rust-restore-point-after-angle-bracket))

(defun rust-match-angle-bracket-hook ()
  "If the most recently inserted character is a `>`, briefly moves point to matching `<` (if any)."
  (interactive)
  (when (and rust-blink-matching-angle-brackets
             (looking-back ">" nil))
    (let ((matching-angle-bracket-point (save-excursion
                                          (backward-char 1)
                                          (rust-find-matching-angle-bracket))))
      (when matching-angle-bracket-point
        (progn
          (setq rust-point-before-matching-angle-bracket (point))
          (goto-char matching-angle-bracket-point)
          (add-hook 'pre-command-hook 'rust-restore-point-after-angle-bracket)
          (setq rust-matching-angle-bracker-timer
                (run-at-time blink-matching-delay nil 'rust-restore-point-after-angle-bracket)))))))

(defun rust-match-angle-bracket ()
  "The point should be placed on a `>`. Finds the matching `<` and moves point there."
  (interactive)
  (let ((matching-angle-bracket-point (rust-find-matching-angle-bracket)))
    (if matching-angle-bracket-point
        (goto-char matching-angle-bracket-point)
      (message "no matching angle bracket found"))))

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'rust-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode rust-mode rust-parent-mode "Rust"
  "Major mode for Rust code."
  :group 'rust-mode
  :syntax-table rust-mode-syntax-table

  ;; Indentation
  (setq-local indent-line-function 'rust-mode-indent-line)

  ;; Fonts
  (setq-local font-lock-defaults '(rust-mode-font-lock-keywords nil nil nil nil (font-lock-syntactic-keywords . rust-mode-font-lock-syntactic-keywords)))

  ;; Misc
  (setq-local comment-start "// ")
  (setq-local comment-end   "")
  (setq-local indent-tabs-mode nil)

  ;; Allow paragraph fills for comments
  (setq-local comment-start-skip "\\(?://[/!]*\\|/\\*[*!]?\\)[[:space:]]*")
  (setq-local paragraph-start
       (concat "[[:space:]]*\\(?:" comment-start-skip "\\|\\*/?[[:space:]]*\\|\\)$"))
  (setq-local paragraph-separate paragraph-start)
  (setq-local normal-auto-fill-function 'rust-do-auto-fill)
  (setq-local fill-paragraph-function 'rust-fill-paragraph)
  (setq-local fill-forward-paragraph-function 'rust-fill-forward-paragraph)
  (setq-local adaptive-fill-function 'rust-find-fill-prefix)
  (setq-local adaptive-fill-first-line-regexp "")
  (setq-local comment-multi-line t)
  (setq-local comment-line-break-function 'rust-comment-indent-new-line)
  (setq-local imenu-generic-expression rust-imenu-generic-expression)
  (setq-local beginning-of-defun-function 'rust-beginning-of-defun)
  (setq-local end-of-defun-function 'rust-end-of-defun)
  (setq-local parse-sexp-lookup-properties t)
  (add-hook 'post-self-insert-hook 'rust-match-angle-bracket-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(defun rust-mode-reload ()
  (interactive)
  (unload-feature 'rust-mode)
  (require 'rust-mode)
  (rust-mode))

;; Issue #6887: Rather than inheriting the 'gnu compilation error
;; regexp (which is broken on a few edge cases), add our own 'rust
;; compilation error regexp and use it instead.
(defvar rustc-compilation-regexps
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)")
        (end-line   "\\([0-9]+\\)")
        (end-col    "\\([0-9]+\\)")
        (error-or-warning "\\(?:[Ee]rror\\|\\([Ww]arning\\)\\)"))
    (let ((re (concat "^" file ":" start-line ":" start-col
                      ": " end-line ":" end-col
                      " \\(?:[Ee]rror\\|\\([Ww]arning\\)\\):")))
      (cons re '(1 (2 . 4) (3 . 5) (6)))))
  "Specifications for matching errors in rustc invocations.
See `compilation-error-regexp-alist for help on their format.")

(eval-after-load 'compile
  '(progn
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'rustc rustc-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'rustc)))

;;; Functions to submit (parts of) buffers to the rust playpen, for
;;; sharing.
(defun rust-playpen-region (begin end)
  "Create a sharable URL for the contents of the current region
   on the Rust playpen."
  (interactive "r")
  (let* ((data (buffer-substring begin end))
         (escaped-data (url-hexify-string data))
         (escaped-playpen-url (url-hexify-string (format rust-playpen-url-format escaped-data))))
    (if (> (length escaped-playpen-url) 5000)
        (error "encoded playpen data exceeds 5000 character limit (length %s)"
               (length escaped-playpen-url))
      (let ((shortener-url (format rust-shortener-url-format escaped-playpen-url))
            (url-request-method "POST"))
        (url-retrieve shortener-url
                      (lambda (state)
                        ; filter out the headers etc. included at the
                        ; start of the buffer: the relevant text
                        ; (shortened url or error message) is exactly
                        ; the last line.
                        (goto-char (point-max))
                        (let ((last-line (thing-at-point 'line t))
                              (err (plist-get state :error)))
                          (kill-buffer)
                          (if err
                              (error "failed to shorten playpen url: %s" last-line)
                            (message "%s" last-line)))))))))

(defun rust-playpen-buffer ()
  "Create a sharable URL for the contents of the current buffer
   on the Rust playpen."
  (interactive)
  (rust-playpen-region (point-min) (point-max)))

(provide 'rust-mode)

;;; rust-mode.el ends here
