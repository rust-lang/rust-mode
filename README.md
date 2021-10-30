# rust-mode

[![MELPA](https://melpa.org/packages/rust-mode-badge.svg)](https://melpa.org/#/rust-mode)
[![](https://github.com/rust-lang/rust-mode/workflows/CI/badge.svg)](https://github.com/rust-lang/rust-mode/actions?query=workflow%3ACI)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [rust-mode](#rust-mode)
    - [Introduction](#introduction)
    - [Known issues](#known-issues)
    - [Installation](#installation)
        - [Melpa](#melpa)
        - [Manual installation](#manual-installation)
    - [Feature guide](#feature-guide)
        - [Indentation](#indentation)
        - [Code formatting](#code-formatting)
        - [Running / testing / compiling code](#running--testing--compiling-code)
        - [Clippy](#clippy)
        - [Easy insertion of dbg!](#easy-insertion-of-dbg)
    - [LSP](#lsp)
        - [eglot](#eglot)
        - [lsp-mode](#lsp-mode)
    - [Auto-completion](#auto-completion)
    - [Other recommended packages](#other-recommended-packages)
        - [flycheck](#flycheck)
        - [cargo.el](#cargoel)
        - [cargo-mode](#cargo-mode)
        - [rustic](#rustic)
    - [For package maintainers](#for-package-maintainers)
        - [Tests](#tests)
    - [Contributing](#contributing)

<!-- markdown-toc end -->

## Introduction

`rust-mode` makes editing [Rust](http://rust-lang.org) code with Emacs
enjoyable. It requires Emacs 25 or later, and is included in both
[Emacs Prelude](https://github.com/bbatsov/prelude) and
[Spacemacs](https://github.com/syl20bnr/spacemacs) by default.

This mode provides:
- Syntax highlighting (for Font Lock Mode)
- Indentation
- Integration with Cargo, clippy and rustfmt

This mode does _not_ provide autocompletion, or jumping to function /
trait definitions. See [Auto-completion / code
navigation](#auto-completion--code-navigation) below for tips on how
to enable this.

## Known issues

- `rust-syntax-propertize` and `adaptive-wrap-prefix-mode` can lead to
  severe lag when editing larger files
  (https://github.com/brotzeit/rustic/issues/107)

## Installation

### Melpa

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

### NonGNU ELPA

[NonGNU ELPA](https://elpa.nongnu.org/) can be used out of the box in
emacs28.

For older versions you need to add something like the following to
your init file:

``` elisp
(with-eval-after-load 'package (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
```

### Manual installation

Clone this repository locally, and add this to your init.el:

``` elisp
(add-to-list 'load-path "/path/to/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
```

## Feature guide

### Indentation

Commands like <kbd>TAB</kbd> should indent correctly.

The Rust style guide recommends spaces rather than tabs for
indentation; to follow the recommendation add this to your init.el,
which forces indentation to always use spaces.

```elisp
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
```

### Code formatting

The `rust-format-buffer` function will format your code with
[rustfmt](https://github.com/rust-lang/rustfmt) if installed. By
default, this is bound to <kbd>C-c C-f</kbd>.

The variable `rust-format-on-save` enables automatic formatting on
save. For example, add the following in your init.el to enable format
on save:

``` elisp
(setq rust-format-on-save t)
```

### Running / testing / compiling code

The `rust-run`, `rust-test`, `rust-compile` and `rust-check` functions
shell out to Cargo to run, test, build and check your code. Under the
hood, these use the standard Emacs `compile` function.

These are not bound by default. To bind these to keyboard shortcuts,
you can use the following in your init.el:

``` elisp
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
```

### Clippy

`rust-run-clippy` runs
[Clippy](https://github.com/rust-lang/rust-clippy), a linter.

### Easy insertion of dbg!

`rust-dbg-wrap-or-unwrap` either wraps or unwraps the current region
in `dbg!`. This can be useful for easily adding debug lines to your
program.

This is bound to <kbd>C-c C-d</kbd> by default.

## LSP

### eglot

A lightweight lsp client.

```elisp
(add-hook 'rust-mode-hook 'eglot-ensure)
```

### lsp-mode

Provides more features and you can enhance the functionality
by using additional packages. You can find more information in the
[lsp-mode wiki](https://emacs-lsp.github.io/lsp-mode/page/installation/#vanilla-emacs).

```elisp
(add-hook 'rust-mode-hook #'lsp)
```

## Auto-completion

You can either use a lsp client or [racer](https://github.com/racer-rust/racer)
with [emacs-racer](https://github.com/racer-rust/emacs-racer#installation).

## Other recommended packages

### flycheck

[flycheck](https://github.com/flycheck/flycheck) allows highlighting
compile errors and Clippy lints inline.

### cargo.el

[cargo.el](https://github.com/kwrooijen/cargo.el) provides a minor
mode for integration with Cargo, Rust's package manager.

### cargo-mode

[cargo-mode](https://github.com/ayrat555/cargo-mode) is an Emacs minor
mode which allows to dynamically select a Cargo command. The reasons
behind this package can be found in [the
post](https://www.badykov.com/emacs/2021/05/29/emacs-cargo-mode/).

### rustic

[rustic](https://github.com/brotzeit/rustic) is based on rust-mode,
extending it with other features such as integration with LSP and with
flycheck.


## For package maintainers

### Tests

Run elisp tests:

``` bash
make test
```

## Contributing

Contributions are very welcome. We are also looking for additional maintainers.
