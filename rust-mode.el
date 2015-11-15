;;; rust-mode.el --- A major emacs mode for editing Rust source code -*-lexical-binding: t-*-

;; Version: 0.2.0
;; Author: Mozilla
;; Url: https://github.com/rust-lang/rust-mode
;; Keywords: languages

;;; Commentary:
;;

;; This file is not part of GNU Emacs. 

;; Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or 
;; http://www.apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT
;; or http://opensource.org/licenses/MIT>, at your option.

;;; Code:

(eval-when-compile (require 'rx)
                   (require 'compile)
                   (require 'url-vars))

(defvar electric-pair-inhibit-predicate)

;; for GNU Emacs < 24.3
(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defconst rust-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")

(defconst rust-re-non-standard-string
  (rx
   (or
    ;; Raw string: if it matches, it ends up with the starting character
    ;; of the string as group 1, any ending backslashes as group 4, and
    ;; the ending character as either group 5 or group 6.
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
      (group (backref 3))

      ;; If the raw string wasn't actually closed, go all the way to the end
      string-end))

    ;; Character literal: match the beginning ' of a character literal
    ;; as group 7, and the ending one as group 8
    (seq
     (group "'")
     (or
      (seq
       "\\"
       (or
        (: "U" (= 8 xdigit))
        (: "u" (= 4 xdigit))
        (: "x" (= 2 xdigit))
        (any "'nrt0\"\\")))
      (not (any "'\\"))
      )
     (group "'"))
    )
   ))

(defun rust-looking-back-str (str)
  "Like `looking-back' but for fixed strings rather than regexps (so that it's not so slow)"
  (let ((len (length str)))
    (and (> (point) len)
         (equal str (buffer-substring-no-properties (- (point) len) (point))))))

(defun rust-looking-back-symbols (SYMS)
  "Return non-nil if the point is just after a complete symbol that is a member of the list of strings SYMS"
  (save-excursion
    (let* ((pt-orig (point))
           (beg-of-symbol (progn (forward-thing 'symbol -1) (point)))
           (end-of-symbol (progn (forward-thing 'symbol 1) (point))))
      (and
       (= end-of-symbol pt-orig)
       (member (buffer-substring-no-properties beg-of-symbol pt-orig) SYMS)))))

(defun rust-looking-back-ident ()
  "Non-nil if we are looking backwards at a valid rust identifier"
  (let ((beg-of-symbol (save-excursion (forward-thing 'symbol -1) (point))))
    (looking-back rust-re-ident beg-of-symbol)))

(defun rust-looking-back-macro ()
  "Non-nil if looking back at an ident followed by a !"
  (save-excursion (backward-char) (and (= ?! (char-after)) (rust-looking-back-ident))))

;; Syntax definitions and helpers
(defvar rust-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Angle brackets.  We suppress this with syntactic fontification when
    ;; needed
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

(defgroup rust-mode nil
  "Support for Rust code."
  :link '(url-link "http://www.rust-lang.org/")
  :group 'languages)

(defcustom rust-indent-offset 4
  "Indent Rust code by this number of spaces."
  :type 'integer
  :group 'rust-mode
  :safe #'integerp)

(defcustom rust-indent-method-chain nil
  "Indent Rust method chains, aligned by the '.' operators"
  :type 'boolean
  :group 'rust-mode
  :safe #'booleanp)

(defcustom rust-playpen-url-format "https://play.rust-lang.org/?code=%s"
  "Format string to use when submitting code to the playpen"
  :type 'string
  :group 'rust-mode)
(defcustom rust-shortener-url-format "http://is.gd/create.php?format=simple&url=%s"
  "Format string to use for creating the shortened link of a playpen submission"
  :type 'string
  :group 'rust-mode)

(defcustom rust-match-angle-brackets t
  "Enable angle bracket matching.  Attempt to match `<' and `>' where
  appropriate."
  :type 'boolean
  :safe #'booleanp
  :group 'rust-mode)

(defun rust-paren-level () (nth 0 (syntax-ppss)))
(defun rust-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun rust-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))
(defun rust-rewind-irrelevant ()
  (let ((starting (point)))
    (skip-chars-backward "[:space:]\n")
    (if (rust-looking-back-str "*/") (backward-char))
    (if (rust-in-str-or-cmnt)
        (rust-rewind-past-str-cmnt))
    (if (/= starting (point))
        (rust-rewind-irrelevant))))
(defun rust-in-macro ()
  (save-excursion
    (when (> (rust-paren-level) 0)
      (backward-up-list)
      (rust-rewind-irrelevant)
      (or (rust-looking-back-macro)
          (and (rust-looking-back-ident) (save-excursion (backward-sexp) (rust-rewind-irrelevant) (rust-looking-back-str "macro_rules!")))
          (rust-in-macro))
      )))

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
              (when (and (rust-looking-back-ident) (save-excursion (forward-thing 'symbol -1) (= ?. (char-before))))
                (forward-thing 'symbol -1)
                (backward-char)
                (- (current-column) rust-indent-offset)))))
        (cond
         ;; foo.bar(...)
         ((rust-looking-back-str ")")
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
                          (looking-back "[(,:;?[{}]\\|[^|]|" (- (point) 2))
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

(defconst rust-re-type-or-constructor
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(defconst rust-re-pre-expression-operators "[-=!%&*/:<>[{(|.^;}]")
(defun rust-re-word (inner) (concat "\\<" inner "\\>"))
(defun rust-re-grab (inner) (concat "\\(" inner "\\)"))
(defun rust-re-item-def (itype)
  (concat (rust-re-word itype) "[[:space:]]+" (rust-re-grab rust-re-ident)))

;; (See PR #42 -- this is just like `(regexp-opt words 'symbols)` from
;; newer Emacs versions, but will work on Emacs 23.)
(defun regexp-opt-symbols (words)
  (concat "\\_<" (regexp-opt words t) "\\_>"))
(defconst rust-re-special-types (regexp-opt-symbols rust-special-types))

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
     (,rust-re-type-or-constructor 1 font-lock-type-face)
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

(defvar font-lock-beg)
(defvar font-lock-end)

(defun rust-font-lock-extend-region ()
  "Extend the region given by `font-lock-beg' and `font-lock-end'
  to include the beginning of a string or comment if it includes
  part of it.  Adjusts to include the r[#] of a raw string as
  well."

  (save-excursion
    (let ((orig-beg font-lock-beg)
          (orig-end font-lock-end))

      (let*
          ;; It's safe to call `syntax-ppss' here on positions that are
          ;; already syntactically fontified
          ((beg-ppss (syntax-ppss font-lock-beg))
           (beg-in-cmnt (and beg-ppss (nth 4 beg-ppss) (nth 8 beg-ppss)))
           (beg-in-str (and beg-ppss (nth 3 beg-ppss) (nth 8 beg-ppss))))

        (when (and beg-in-str (>= font-lock-beg beg-in-str))
          (setq font-lock-beg (nth 8 beg-ppss))
          (while (equal ?# (char-before font-lock-beg))
            (setq font-lock-beg (1- font-lock-beg)))
          (when (equal ?r (char-before font-lock-beg))
            (setq font-lock-beg (1- font-lock-beg))))

        (when (and beg-in-cmnt (> font-lock-beg beg-in-cmnt))
          (setq font-lock-beg beg-in-cmnt)))

      ;; We need to make sure that if the region ends inside a raw string, we
      ;; extend it out past the end of it.  But we can't use `syntax-ppss' to
      ;; detect that, becaue that depends on font-lock already being done, and we
      ;; are trying to figure out how much to font-lock before that.  So we use
      ;; the regexp directly.
      (save-match-data
        (goto-char font-lock-beg)
        (while (and (< (point) font-lock-end)
                    (re-search-forward rust-re-non-standard-string (buffer-end 1) t)
                    (<= (match-beginning 0) font-lock-end))
          (setq font-lock-end (max font-lock-end (match-end 0)))
          (goto-char (1+ (match-beginning 0)))))

      (or (/= font-lock-beg orig-beg)
          (/= font-lock-end orig-end))
      )))

(defun rust-conditional-re-search-forward (regexp bound condition)
  ;; Search forward for regexp (with bound).  If found, call condition and return the found
  ;; match only if it returns true.
  (let* (found
         found-ret-list
         (ret-list (save-excursion
                     (while (and (not found) (re-search-forward regexp bound t))
                       (setq
                        found-ret-list (list (point) (match-data))
                        found (save-match-data (save-excursion (ignore-errors (funcall condition)))))
                       ;; If the condition filters out a match, need to search
                       ;; again just after its beginning.  This will allow
                       ;; cases such as:
                       ;;    "bar" r"foo"
                       ;; where the filtered out search (r" r") should not
                       ;; prevent finding another one that begins in the middle
                       ;; of it (r"foo")
                       (when (not found)
                         (goto-char (1+ (match-beginning 0))))
                       )
                     (when found found-ret-list))))
    (when ret-list
      (goto-char (nth 0 ret-list))
      (set-match-data (nth 1 ret-list))
      (nth 0 ret-list))))

(defun rust-look-for-non-standard-string (bound)
  ;; Find a raw string or character literal, but only if it's not in the middle
  ;; of another string or a comment.

  (rust-conditional-re-search-forward
   rust-re-non-standard-string
   bound
   (lambda ()
     (let ((pstate (syntax-ppss (match-beginning 0))))
       (not
        (or
         (nth 4 pstate) ;; Skip if in a comment
         (and (nth 3 pstate) (wholenump (nth 8 pstate)) (< (nth 8 pstate) (match-beginning 0))) ;; Skip if in a string that isn't starting here
         ))))))

(defun rust-syntax-class-before-point ()
  (when (> (point) 1)
    (syntax-class (syntax-after (1- (point))))))

(defun rust-rewind-qualified-ident ()
  (while (rust-looking-back-ident)
    (backward-sexp)
    (when (save-excursion (rust-rewind-irrelevant) (rust-looking-back-str "::"))
      (rust-rewind-irrelevant)
      (backward-char 2)
      (rust-rewind-irrelevant))))

(defun rust-rewind-type-param-list ()
  (cond
   ((and (rust-looking-back-str ">") (equal 5 (rust-syntax-class-before-point)))
    (backward-sexp)
    (rust-rewind-irrelevant))

   ;; We need to be able to back up past the Fn(args) -> RT form as well.  If
   ;; we're looking back at this, we want to end up just after "Fn".
   ((member (char-before) '(?\] ?\) ))
    (let* ((is-paren (rust-looking-back-str ")"))
           (dest (save-excursion
                  (backward-sexp)
                  (rust-rewind-irrelevant)
                  (or
                   (when (rust-looking-back-str "->")
                     (backward-char 2)
                     (rust-rewind-irrelevant)
                     (when (rust-looking-back-str ")")
                       (backward-sexp)
                       (point)))
                   (and is-paren (point))))))
      (when dest
        (goto-char dest))))))

(defun rust-rewind-to-decl-name ()
  "If we are before an ident that is part of a declaration that
  can have a where clause, rewind back to just before the name of
  the subject of that where clause and return the new point.
  Otherwise return nil"
  
  (let* ((ident-pos (point))
         (newpos (save-excursion
                   (rust-rewind-irrelevant)
                   (rust-rewind-type-param-list)
                   (cond
                       ((rust-looking-back-symbols '("fn" "trait" "enum" "struct" "impl" "type")) ident-pos)

                       ((equal 5 (rust-syntax-class-before-point))
                        (backward-sexp)
                        (rust-rewind-to-decl-name))

                       ((looking-back "[:,'+=]" (1- (point)))
                        (backward-char)
                        (rust-rewind-to-decl-name))

                       ((rust-looking-back-str "->")
                        (backward-char 2)
                        (rust-rewind-to-decl-name))

                       ((rust-looking-back-ident)
                        (rust-rewind-qualified-ident)
                        (rust-rewind-to-decl-name))))))
    (when newpos (goto-char newpos))
    newpos))

(defun rust-is-in-expression-context (token)
  "Return t if what comes right after the point is part of an
  expression (as opposed to starting a type) by looking at what
  comes before.  Takes a symbol that roughly indicates what is
  after the point.

  This function is used as part of `rust-is-lt-char-operator' as
  part of angle bracket matching, and is not intended to be used
  outside of this context."

  (save-excursion
    (let ((postchar (char-after)))
      (rust-rewind-irrelevant)

      ;; A type alias or ascription could have a type param list.  Skip backwards past it.
      (when (member token '(ambiguous-operator open-brace))
        (rust-rewind-type-param-list))
      
      (cond

       ;; Certain keywords always introduce expressions
       ((rust-looking-back-symbols '("if" "while" "match" "return" "box" "in")) t)

       ;; "as" introduces a type
       ((rust-looking-back-symbols '("as")) nil)

       ;; An open angle bracket never introduces expression context WITHIN the angle brackets
       ((and (equal token 'open-brace) (equal postchar ?<)) nil)

       ;; An ident! followed by an open brace is a macro invocation.  Consider
       ;; it to be an expression.
       ((and (equal token 'open-brace) (rust-looking-back-macro)) t)
       
       ;; An identifier is right after an ending paren, bracket, angle bracket
       ;; or curly brace.  It's a type if the last sexp was a type.
       ((and (equal token 'ident) (equal 5 (rust-syntax-class-before-point)))
        (backward-sexp)
        (rust-is-in-expression-context 'open-brace))

       ;; If a "for" appears without a ; or { before it, it's part of an
       ;; "impl X for y", so the y is a type.  Otherwise it's
       ;; introducing a loop, so the y is an expression
       ((and (equal token 'ident) (rust-looking-back-symbols '("for")))
        (backward-sexp)
        (rust-rewind-irrelevant)
        (looking-back "[{;]" (1- (point))))
       
       ((rust-looking-back-ident)
        (rust-rewind-qualified-ident)
        (rust-rewind-irrelevant)
        (cond
         ((equal token 'open-brace)
          ;; We now know we have:
          ;;   ident <maybe type params> [{([]
          ;; where [{([] denotes either a {, ( or [.  This character is bound as postchar.
          (cond
           ;; If postchar is a paren or square bracket, then if the brace is a type if the identifier is one
           ((member postchar '(?\( ?\[ )) (rust-is-in-expression-context 'ident))

           ;; If postchar is a curly brace, the brace can only be a type if
           ;; ident2 is the name of an enum, struct or trait being declared.
           ;; Note that if there is a -> before the ident then the ident would
           ;; be a type but the { is not.
           ((equal ?{ postchar)
            (not (and (rust-rewind-to-decl-name)
                      (progn
                        (rust-rewind-irrelevant)
                        (rust-looking-back-symbols '("enum" "struct" "trait" "type"))))))
           ))
         
         ((equal token 'ambiguous-operator)
          (cond
           ;; An ampersand after an ident has to be an operator rather than a & at the beginning of a ref type
           ((equal postchar ?&) t)

           ;; A : followed by a type then an = introduces an expression (unless it is part of a where clause of a "type" declaration)
           ((and (equal postchar ?=)
                 (looking-back "[^:]:" (- (point) 2))
                 (not (save-excursion (and (rust-rewind-to-decl-name) (progn (rust-rewind-irrelevant) (rust-looking-back-symbols '("type"))))))))

           ;; "let ident =" introduces an expression--and so does "const" and "mut"
           ((and (equal postchar ?=) (rust-looking-back-symbols '("let" "const" "mut"))) t)

           ;; As a specific special case, see if this is the = in this situation:
           ;;     enum EnumName<type params> { Ident =
           ;; In this case, this is a c-like enum and despite Ident
           ;; representing a type, what comes after the = is an expression
           ((and
             (> (rust-paren-level) 0)
             (save-excursion
               (backward-up-list)
               (rust-rewind-irrelevant)
               (rust-rewind-type-param-list)
               (and
                (rust-looking-back-ident)
                (progn
                  (rust-rewind-qualified-ident)
                  (rust-rewind-irrelevant)
                  (rust-looking-back-str "enum")))))
            t)
           
           ;; Otherwise the ambiguous operator is a type if the identifier is a type
           ((rust-is-in-expression-context 'ident) t)))

         ((equal token 'colon)
          (cond
           ;; If we see a ident: not inside any braces/parens, we're at top level.
           ;; There are no allowed expressions after colons there, just types.
           ((<= (rust-paren-level) 0) nil)

           ;; We see ident: inside a list
           ((looking-back "[{,]" (1- (point)))
            (backward-up-list)

            ;; If a : appears whose surrounding paren/brackets/braces are
            ;; anything other than curly braces, it can't be a field
            ;; initializer and must be denoting a type.
            (when (looking-at "{")
              (rust-rewind-irrelevant)
              (rust-rewind-type-param-list)
              (when (rust-looking-back-ident)
                ;; We have a context that looks like this:
                ;;    ident2 <maybe type params> { [maybe paren-balanced code ending in comma] ident1:
                ;; the point is sitting just after ident2, and we trying to
                ;; figure out if the colon introduces an expression or a type.
                ;; The answer is that ident1 is a field name, and what comes
                ;; after the colon is an expression, if ident2 is an
                ;; expression.
                (rust-rewind-qualified-ident)
                (rust-is-in-expression-context 'ident))))


           ;; Otherwise, if the ident: appeared with anything other than , or {
           ;; before it, it can't be part of a struct initializer and therefore
           ;; must be denoting a type.
	   (t nil)
           ))
         ))

       ;; An operator-like character after a string is indeed an operator
       ((and (equal token 'ambiguous-operator)
             (member (rust-syntax-class-before-point) '(5 7 15))) t)

       ;; A colon that has something other than an identifier before it is a
       ;; type ascription
       ((equal token 'colon) nil)

       ;; A :: introduces a type (or module, but not an expression in any case)
       ((rust-looking-back-str "::") nil)
       
       ((rust-looking-back-str ":")
        (backward-char)
        (rust-is-in-expression-context 'colon))

       ;; A -> introduces a type
       ((rust-looking-back-str "->") nil)

       ;; If we are up against the beginning of a list, or after a comma inside
       ;; of one, back up out of it and check what the list itself is
       ((or
         (equal 4 (rust-syntax-class-before-point))
         (rust-looking-back-str ","))
        (backward-up-list)
        (rust-is-in-expression-context 'open-brace))

       ;; A => introduces an expression
       ((rust-looking-back-str "=>") t)

       ;; A == introduces an expression
       ((rust-looking-back-str "==") t)

       ;; These operators can introduce expressions or types
       ((looking-back "[-+=!?&*]" (1- (point)))
        (backward-char)
        (rust-is-in-expression-context 'ambiguous-operator))

       ;; These operators always introduce expressions.  (Note that if this
       ;; regexp finds a < it must not be an angle bracket, or it'd
       ;; have been caught in the syntax-class check above instead of this.)
       ((looking-back rust-re-pre-expression-operators (1- (point))) t)
       ))))

(defun rust-is-lt-char-operator ()
  "Return t if the < sign just after point is an operator rather
  than an opening angle bracket, otherwise nil."
  
  (let ((case-fold-search nil))
    (save-excursion
      (rust-rewind-irrelevant)
      ;; We are now just after the character syntactically before the <.
      (cond

       ;; If we are looking back at a < that is not an angle bracket (but not
       ;; two of them) then this is the second < in a bit shift operator
       ((and (rust-looking-back-str "<")
             (not (equal 4 (rust-syntax-class-before-point)))
             (not (rust-looking-back-str "<<"))))
       
       ;; On the other hand, if we are after a closing paren/brace/bracket it
       ;; can only be an operator, not an angle bracket.  Likewise, if we are
       ;; after a string it's an operator.  (The string case could actually be
       ;; valid in rust for character literals.)
       ((member (rust-syntax-class-before-point) '(5 7 15)) t)

       ;; If we are looking back at an operator, we know that we are at
       ;; the beginning of an expression, and thus it has to be an angle
       ;; bracket (starting a "<Type as Trait>::" construct.)
       ((looking-back rust-re-pre-expression-operators (1- (point))) nil)

       ;; If we are looking back at a keyword, it's an angle bracket
       ;; unless that keyword is "self", "true" or "false"
       ((rust-looking-back-symbols rust-mode-keywords)
        (rust-looking-back-symbols '("self" "true" "false")))

       ;; If we're looking back at an identifier, this depends on whether
       ;; the identifier is part of an expression or a type
       ((rust-looking-back-ident)
        (backward-sexp)
        (or
         ;; The special types can't take type param lists, so a < after one is
         ;; always an operator
         (looking-at rust-re-special-types)
         
         (rust-is-in-expression-context 'ident)))

       ;; Otherwise, assume it's an angle bracket
       ))))

(defun rust-electric-pair-inhibit-predicate-wrap (char)
  "Wraps the default `electric-pair-inhibit-predicate' to prevent
  inserting a \"matching\" > after a < that would be treated as a
  less than sign rather than as an opening angle bracket."
  (or
   (when (= ?< char)
     (save-excursion
       (backward-char)
       (rust-is-lt-char-operator)))
   (funcall (default-value 'electric-pair-inhibit-predicate) char)))

(defun rust-look-for-non-angle-bracket-lt-gt (bound)
  "Find an angle bracket (\"<\" or \">\") that should be part of
  a matched pair Relies on the fact that when it finds a < or >,
  we have already decided which previous ones are angle brackets
  and which ones are not.  So this only really works as a
  font-lock-syntactic-keywords matcher--it won't work at
  arbitrary positions without the earlier parts of the buffer
  having already been covered."

  (rust-conditional-re-search-forward
   "[<>]" bound
   (lambda ()
     (goto-char (match-beginning 0))
     (cond
      ;; If matching is turned off suppress all of them
      ((not rust-match-angle-brackets) t)

      ;; We don't take < or > in strings or comments to be angle brackets
      ((rust-in-str-or-cmnt) t)

      ;; Inside a macro we don't really know the syntax.  Any < or > may be an
      ;; angle bracket or it may not.  But we know that the other braces have
      ;; to balance regardless of the < and >, so if we don't treat any < or >
      ;; as angle brackets it won't mess up any paren balancing.
      ((rust-in-macro) t)
      
      ((looking-at "<")
       (rust-is-lt-char-operator))

      ((looking-at ">")
       (cond
        ;; Don't treat the > in -> or => as an angle bracket
        ((member (char-before (point)) '(?- ?=)) t)

        ;; If we are at top level and not in any list, it can't be a closing
        ;; angle bracket
        ((>= 0 (rust-paren-level)) t)

        ;; Otherwise, treat the > as a closing angle bracket if it would
        ;; match an opening one
        ((save-excursion
           (backward-up-list)
           (not (looking-at "<"))))))))))

(defvar rust-mode-font-lock-syntactic-keywords
  (append
   ;; Handle raw strings and character literals:
   `((rust-look-for-non-standard-string (1 "|" nil t) (4 "_" nil t) (5 "|" nil t) (6 "|" nil t) (7 "\"" nil t) (8 "\"" nil t)))
   ;; Find where < and > characters represent operators rather than angle brackets:
   '((rust-look-for-non-angle-bracket-lt-gt (0 "." t)))))

(defun rust-mode-syntactic-face-function (state)
  "Syntactic face function to distinguish doc comments from other comments."
  (if (nth 3 state) 'font-lock-string-face
    (save-excursion
      (goto-char (nth 8 state))
      (if (looking-at "/\\([*][*!][^*!]\\|/[/!][^/!]\\)")
          'font-lock-doc-face
        'font-lock-comment-face
    ))))

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
        (condition-case nil
            (forward-sexp)
          (scan-error
           ;; The parentheses are unbalanced; instead of being unable to fontify, just jump to the end of the buffer
           (goto-char (point-max)))))
    ;; There is no opening brace, so consider the whole buffer to be one "defun"
    (goto-char (point-max))))

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
  (add-to-list 'font-lock-extend-region-functions 'rust-font-lock-extend-region)
  (setq-local font-lock-defaults '(rust-mode-font-lock-keywords
                                   nil nil nil nil
                                   (font-lock-syntactic-keywords . rust-mode-font-lock-syntactic-keywords)
                                   (font-lock-syntactic-face-function . rust-mode-syntactic-face-function)
                                   ))

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
  (setq-local electric-pair-inhibit-predicate 'rust-electric-pair-inhibit-predicate-wrap))

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
        (msg-type   "\\(?:[Ee]rror\\|\\([Ww]arning\\)\\|\\([Nn]ote\\|[Hh]elp\\)\\)"))
    (let ((re (concat "^" file ":" start-line ":" start-col
                      ": " end-line ":" end-col
                      " " msg-type ":")))
      (cons re '(1 (2 . 4) (3 . 5) (6 . 7)))))
  "Specifications for matching errors in rustc invocations.
See `compilation-error-regexp-alist' for help on their format.")

;; Match test run failures and panics during compilation as
;; compilation warnings
(defvar cargo-compilation-regexps
  '("^\\s-+thread '[^']+' panicked at \\('[^']+', \\([^:]+\\):\\([0-9]+\\)\\)" 2 3 nil nil 1)
  "Specifications for matching panics in cargo test invocations.
See `compilation-error-regexp-alist' for help on their format.")

(eval-after-load 'compile
  '(progn
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'rustc rustc-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'rustc)
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'cargo cargo-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'cargo)))

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
