`rust-mode`: A major Emacs mode for editing Rust source code
============================================================

`rust-mode` makes editing [Rust](http://rust-lang.org) code with Emacs
enjoyable.  `rust-mode` requires Emacs 24 or later.

## Installation

### Manual Installation

To install manually, check out this repository and add this to your
`.emacs` file:

```lisp
(add-to-list 'load-path "/path/to/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
```

This associates `rust-mode` with `.rs` files. To enable it explicitly, do
<kbd>M-x rust-mode</kbd>.

### `package.el` installation via MELPA

It can be more convenient to use Emacs's package manager to handle
installation for you if you use many elisp libraries. If you have
`package.el` but haven't added MELPA, the community
package source, yet, add this to `~/.emacs.d/init.el`:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

Then do this to load the package listing:

* <kbd>M-x eval-buffer</kbd>
* <kbd>M-x package-refresh-contents</kbd>

#### MELPA stable

MELPA stable only updates when a new version tag is created.  There isn't a
specified frequency for this project to release versions; if you are a MELPA
stable user and feel that the last release tag is too far out of date, open a
pull request that updates the version header in `rust-mode.el` and note that a
new version tag should be added when the request is merged.
See [Pull Request #178](https://github.com/rust-lang/rust-mode/pull/178) (the
first such pull request)) for an example of this.

#### Install `rust-mode`

One you have `package.el`, you can install `rust-mode` or any other
modes by choosing them from a list:

* <kbd>M-x package-list-packages</kbd>

Now, to install packages, move your cursor to them and press
<kbd>i</kbd>. This will mark the packages for installation. When
you're done with marking, press <kbd>x</kbd>, and ELPA will install
the packages for you (under `~/.emacs.d/elpa/`).

* or using <kbd>M-x package-install rust-mode</kbd>

### Package installation on Debian testing or unstable

```bash
apt install elpa-rust-mode
```

### Tests via ERT

The file `rust-mode-tests.el` contains tests that can be run via
[ERT](http://www.gnu.org/software/emacs/manual/html_node/ert/index.html).
You can use `run_rust_emacs_tests.sh` to run them in batch mode, if
you set the environment variable EMACS to a program that runs emacs.

## Features

### Formatting with [rustfmt][rfmt]

The `rust-format-buffer` function will format your code with
[rustfmt][rfmt] if installed. By default, this is bound to `C-c C-f`.

Placing `(setq rust-format-on-save t)` in your `~/.emacs` will enable automatic
running of `rust-format-buffer` when you save a buffer.

[rfmt]: https://crates.io/crates/rustfmt/

## License

`rust-mode` is distributed under the terms of both the MIT license and the
Apache License (Version 2.0).

See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) for details.
