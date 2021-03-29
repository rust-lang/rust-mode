-include config.mk

.PHONY: test

all: lisp

PKG = rust-mode

EMACS ?= emacs
EMACS_ARGS ?=

ELS   = rust-mode.el
ELS  += rust-compile.el
ELCS  = $(ELS:.el=.elc)

DEPS  =

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

lisp: $(ELCS) loaddefs

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) \
	$(LOAD_PATH) --funcall batch-byte-compile $<

test:
	@$(EMACS) -Q --batch -L . -l rust-mode.el \
	-l rust-mode-tests.el -f ert-run-tests-batch-and-exit

CLEAN  = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf "Cleaning...\n"
	@rm -rf $(CLEAN)

loaddefs: $(PKG)-autoloads.el

define LOADDEFS_TMPL
;;; $(PKG)-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(PKG)-autoloads.el ends here
endef
export LOADDEFS_TMPL
#'

$(PKG)-autoloads.el: $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(EMACS) -Q --batch --eval "(progn\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"

