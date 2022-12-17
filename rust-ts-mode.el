;;; rust-ts-mode.el --- tree-sitter support  -*- lexical-binding: t; -*-

(require 'treesit)

(defvar rust-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'rust
   '((line_comment)  @font-lock-comment-face
     (block_comment) @font-lock-comment-face)

   :feature 'keyword
   :language 'rust
   '("as" @font-lock-keyword-face
     "async" @font-lock-keyword-face
     "await" @font-lock-keyword-face
     "break" @font-lock-keyword-face
     "const" @font-lock-keyword-face
     "continue" @font-lock-keyword-face
     "default" @font-lock-keyword-face
     "dyn" @font-lock-keyword-face
     "else" @font-lock-keyword-face
     "enum" @font-lock-keyword-face
     "extern" @font-lock-keyword-face
     "fn" @font-lock-keyword-face
     "for" @font-lock-keyword-face
     "if" @font-lock-keyword-face
     "impl" @font-lock-keyword-face
     "in" @font-lock-keyword-face
     "let" @font-lock-keyword-face
     "loop" @font-lock-keyword-face
     "macro_rules!" @font-lock-keyword-face
     "match" @font-lock-keyword-face
     "mod" @font-lock-keyword-face
     "move" @font-lock-keyword-face
     "pub" @font-lock-keyword-face
     "ref" @font-lock-keyword-face
     "return" @font-lock-keyword-face
     "static" @font-lock-keyword-face
     "struct" @font-lock-keyword-face
     "trait" @font-lock-keyword-face
     "type" @font-lock-keyword-face
     "union" @font-lock-keyword-face
     "unsafe" @font-lock-keyword-face
     "use" @font-lock-keyword-face
     "where" @font-lock-keyword-face
     "while" @font-lock-keyword-face
     (crate) @font-lock-keyword-face
     (mutable_specifier) @font-lock-keyword-face
     (use_list (self) @font-lock-keyword-face)
     (scoped_use_list (self) @font-lock-keyword-face)
     (scoped_identifier (self) @font-lock-keyword-face)
     (super) @font-lock-keyword-face)
   
   :feature 'string
   :language 'rust
   '((char_literal) @font-lock-string-face
     (string_literal) @font-lock-string-face
     (raw_string_literal) @font-lock-string-face)

   :feature 'type
   :language 'rust
   '((primitive_type) @font-lock-type-face
     (type_identifier) @font-lock-type-face
     ((scoped_identifier path: (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))
     ((scoped_identifier path: (scoped_identifier name: (identifier) @font-lock-type-face))
      (:match "^[A-Z]" @font-lock-type-face))
     ((scoped_type_identifier path: (identifier) @font-lock-type-face)
      (:match "^[A-Z]" @font-lock-type-face))
     ((scoped_type_identifier path: (scoped_identifier name: (identifier) @font-lock-type-face))
      (:match "^[A-Z]" @font-lock-type-face)))
   
   :language 'rust
   :feature 'constant
   '(((identifier) @font-lock-constant-face
      (:match "^[A-Z][A-Z\\d_]+$'" @font-lock-constant-face))
     (boolean_literal) @font-lock-constant-face
     (integer_literal) @font-lock-constant-face
     (float_literal) @font-lock-constant-face)

   :language 'rust
   :feature 'operator
   '("*" @font-lock-operator-face
     "&" @font-lock-operator-face
     "'" @font-lock-operator-face)

   :language 'rust
   :feature 'variable
   '((identifier) @font-lock-function-name-face)

   :language 'rust
   :feature 'constructor
   `(((identifier) @font-lock-constant-face
      (:match "^[A-Z]" @font-lock-constant-face))
     (struct_pattern
      type: (scoped_type_identifier
             name: (type_identifier) @font-lock-constant-face)))

   :language 'rust
   :override t
   :feature 'expression
   `((call_expression
      function: (identifier) @font-lock-function-name-face)
     (call_expression
      function: (field_expression
                 field: (field_identifier) @font-lock-function-name-face))
     (call_expression
      function: (scoped_identifier
                 "::"
                 name: (identifier) @font-lock-function-name-face))

     (macro_invocation
      macro: (identifier) @font-lock-preprocessor-face
      "!" @font-lock-preprocessor-face)

     (function_item (identifier) @font-lock-function-name-face)
     (function_signature_item (identifier) @font-lock-function-name-face))

   :language 'rust
   :feature 'punctuation
   '("::" @font-lock-punctuation-face
     ":" @font-lock-punctuation-face
     "." @font-lock-punctuation-face
     "," @font-lock-punctuation-face
     ";" @font-lock-punctuation-face

     (type_arguments
      "<" @font-lock-punctuation-face
      ">" @font-lock-punctuation-face)
     (type_parameters
      "<" @font-lock-punctuation-face
      ">" @font-lock-punctuation-face))

   :language 'rust
   :override t
   :feature 'label
   `(lifetime (identifier) @font-lock-constant-face)

   :language 'rust
   :feature 'attribute
   '((attribute_item) @font-lock-preprocessor-face
     (inner_attribute_item) @font-lock-preprocessor-face)

   )
  "Tree-sitter font-lock settings for `rust-mode'.")

(define-derived-mode rust-ts-mode prog-mode "rust"
  "Major mode for editing rust, powered by tree-sitter."
  :group 'rust
  :syntax-table rust-mode-syntax-table

  (unless (treesit-ready-p 'rust)
    (error "Tree-sitter for rust isn't available"))

  (treesit-parser-create 'rust)

  ;; Comments.
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  ;; Font-lock.
  (setq-local treesit-font-lock-settings rust-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '(( comment definition punctuation attribute )
                ( constant keyword preprocessor string type constructor expression )
                ( assignment constant escape-sequence literal property label)
                ( bracket delimiter error operator variable )))

  (treesit-major-mode-setup))

(provide 'rust-ts-mode)
;;; rust-ts-mode.el ends here

