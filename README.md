`rust-mode`: A major Emacs mode for editing Rust source code
============================================================

`rust-mode` makes editing [Rust](http://rust-lang.org) code with Emacs
enjoyable.


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

If you use a version of Emacs prior to 24 that doesn't include
`package.el`, you can get it from [here](http://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el).

If you have an older ELPA `package.el` installed from tromey.com, you
should upgrade in order to support installation from multiple sources.
The ELPA archive is deprecated and no longer accepting new packages,
so the version there (1.7.1) is very outdated.

#### Install `rust-mode`

One you have `package.el`, you can install `rust-mode` or any other
modes by choosing them from a list:

* <kbd>M-x package-list-packages</kbd>

Now, to install packages, move your cursor to them and press
<kbd>i</kbd>. This will mark the packages for installation. When
you're done with marking, press <kbd>x</kbd>, and ELPA will install
the packages for you (under `~/.emacs.d/elpa/`).

* or using <kbd>M-x package-install rust-mode</kbd>

### Tests via ERT

The file `rust-mode-tests.el` contains tests that can be run via
[ERT](http://www.gnu.org/software/emacs/manual/html_node/ert/index.html).
You can use `run_rust_emacs_tests.sh` to run them in batch mode, if
you set the environment variable EMACS to a program that runs emacs.

To test it under emacs 23, which does not ship with ERT, download ert.el from
https://raw.githubusercontent.com/ohler/ert/c619b56c5bc6a866e33787489545b87d79973205/lisp/emacs-lisp/ert.el
and put it in a place where emacs can find it.  (ERT has shipped with emacs
since emacs 24.)
