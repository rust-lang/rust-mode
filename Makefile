-include config.mk

.PHONY: test

all: lisp

PKG = rust-mode

EMACS ?= emacs
EMACS_ARGS ?=
EASK ?= eask

DEPS  =

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

# TODO: add checkdoc and lint
ci: build compile test

build:
	$(EASK) package
	$(EASK) install

compile:
	@printf "Compiling $<\n"
	$(EASK) compile

test:
	$(EASK) test ert rust-mode-tests.el

checkdoc:
	$(EASK) lint checkdoc

lint:
	$(EASK) lint package

CLEAN = $(PKG)-autoloads.el

clean:
	@printf "Cleaning...\n"
	@rm -rf $(CLEAN)
	$(EASK) clean all

$(PKG)-autoloads.el:
	@printf "Generating $@\n"
	$(EASK) autoloads
