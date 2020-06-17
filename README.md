[![MELPA](https://melpa.org/packages/rust-mode-badge.svg)](https://melpa.org/#/rust-mode)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Introduction](#introduction)
- [Installation](#installation)
    - [Melpa](#melpa)
    - [Manual installation](#manual-installation)
- [Feature guide](#feature-guide)
    - [Indentation](#indentation)
    - [Code formatting](#code-formatting)
    - [Running / testing / compiling code](#running--testing--compiling-code)
    - [Clippy](#clippy)
    - [Easy insertion of !dbg](#easy-insertion-of-dbg)
- [Other recommended packages](#other-recommended-packages)
    - [Auto-completion / code navigation](#auto-completion--code-navigation)
    - [flycheck](#flycheck)
    - [cargo.el](#cargoel)
    - [Rustic](#rustic)
- [For package maintainers](#for-package-maintainers)
    - [Tests](#tests)

<!-- markdown-toc end -->

# Introduction
`rust-mode` makes editing [Rust](http://rust-lang.org) code with Emacs
enjoyable. It requires Emacs 25 or later, and is included in both
[Emacs Prelude](https://github.com/bbatsov/prelude) and
[Spacemacs](https://github.com/syl20bnr/spacemacs) by default.

This mode provides:
- Syntax highlighting (for Font Lock Mode)
- Indentation
- Integration with Cargo, clippy and rustfmt

This mode does _not_ provide autocompletion, or jumping to function /
trait definitions. See [Auto-completion / code navigation](#auto-completion--code-navigation)
below for tips on how to enable this.


# Installation

## Melpa
The package is available on MELPA. Add this to your init.el.

``` elisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
```

Now you can install `rust-mode` with:

`M-x package-install rust-mode`

And put this in your config to load rust-mode automatically:

`(require 'rust-mode)`

## Manual installation
Clone this repository locally, and add this to your init.el:

``` elisp
(add-to-list 'load-path "/path/to/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
```

# Feature guide
## Indentation
Commands like <kbd>TAB</kbd> should indent correctly.

The Rust style guide recommends spaces rather than tabs for
indentation; to follow the recommendation add this to your init.el,
which forces indentation to always use spaces.

```elisp
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
```

## Code formatting

The `rust-format-buffer` function will format your code with
[rustfmt](https://github.com/rust-lang/rustfmt) if installed. By
default, this is bound to <kbd>C-c C-f</kbd>.

The variable `rust-format-on-save` enables automatic formatting on
save. For example, add the following in your init.el to enable format
on save:

``` elisp
(setq rust-format-on-save t)
```

## Running / testing / compiling code

The `rust-run`, `rust-test` and `rust-compile` functions shell out to
Cargo to run, test or build your code. Under the hood, these use the
standard Emacs `compile` function.

These are not bound by default. To bind these to keyboard shortcuts,
you can use the following in your init.el:

``` elisp
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
```

## Clippy
`rust-run-clippy` runs
[Clippy](https://github.com/rust-lang/rust-clippy), a linter.

## Easy insertion of !dbg
`rust-dbg-wrap-or-unwrap` either wraps or unwraps the current region
in `dbg!`. This can be useful for easily adding debug lines to your
program.

This is bound to <kbd>C-c C-d</kbd> by default.


# Other recommended packages

## Auto-completion / code navigation
This package does not provide integration with
[RLS](https://github.com/rust-lang/rls), which provides
auto-completion and code navigation. To use this you need an Emacs
package that supports LSP.

Two examples are:
- [LSP](https://github.com/emacs-lsp/lsp-mode)
- [eglot](https://github.com/joaotavora/eglot)

A lighter package that uses
[racer](https://github.com/racer-rust/racer) is
[emacs-racer](https://github.com/racer-rust/emacs-racer).

## flycheck
[flycheck](https://github.com/flycheck/flycheck) allows highlighting
compile errors and Clippy lints inline.

## cargo.el
[cargo.el](https://github.com/kwrooijen/cargo.el) provides a minor
mode for integration with Cargo, Rust's package manager.

## Rustic
[rustic](https://github.com/brotzeit/rustic) is a fork of rust-mode,
extending it with other features such as integration with LSP and with flycheck.


# For package maintainers

## Tests

The file `rust-mode-tests.el` contains tests that can be run via
[ERT](http://www.gnu.org/software/emacs/manual/html_node/ert/index.html).
You can use `run_rust_emacs_tests.sh` to run them in batch mode, if
you set the environment variable EMACS to a program that runs emacs.
