[![MELPA](https://melpa.org/packages/rust-mode-badge.svg)](https://melpa.org/#/rust-mode)

# Emacs mode for editing Rust source code

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Installation via MELPA](#installation-via-melpa)
- [Manual Installation](#manual-installation)
- [rustfmt](#rustfmt)
- [Tests](#tests)
- [Other useful packages](#other-useful-packages)

<!-- markdown-toc end -->

# Installation via MELPA

`rust-mode` makes editing [Rust](http://rust-lang.org) code with Emacs
enjoyable. It requires Emacs 24 or later.
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

# Manual Installation 

Add this to your init.el:

``` elisp
(add-to-list 'load-path "/path/to/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
```

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
