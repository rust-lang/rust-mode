[![MELPA](https://melpa.org/packages/rust-mode-badge.svg)](https://melpa.org/#/rust-mode)

`rust-mode`: A major Emacs mode for editing Rust source code
============================================================

# Installation

`rust-mode` is available on MELPA. It is recommended to use 
[use-package](https://github.com/jwiegley/use-package).

# rustfmt

The `rust-format-buffer` function will format your code with
[rustfmt](https://github.com/rust-lang/rustfmt) if installed. By default, 
this is bound to `C-c C-f`.

Placing `(setq rust-format-on-save t)` in your init.el will enable automatic
running of `rust-format-buffer` when you save a buffer.

# Tests

The file `rust-mode-tests.el` contains tests that can be run via
[ERT](http://www.gnu.org/software/emacs/manual/html_node/ert/index.html).
You can use `run_rust_emacs_tests.sh` to run them in batch mode, if
you set the environment variable EMACS to a program that runs emacs.

# Other useful packages

[cargo.el](https://github.com/kwrooijen/cargo.el) Emacs Minor Mode for Cargo, Rust's Package Manager
[emacs-racer](https://github.com/racer-rust/emacs-racer) Racer support for Emacs
[rustic](https://github.com/brotzeit/rustic) Rust development environment for Emacs 
