#!/bin/sh
# Copyright 2014 The Rust Project Developers. See the COPYRIGHT
# file at the top-level directory of this distribution and at
# http://rust-lang.org/COPYRIGHT.
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.
#
# This runs the test for emacs rust-mode.
# Either $EMACS must be set, or it must be possible to find emacs via PATH.

if [ -z "$EMACS" ]; then
    EMACS=emacs
elif [ ! $(which "$EMACS") ]; then
   echo "You must set EMACS to a program that runs emacs."
   exit 1
fi

"$EMACS" -batch -l rust-mode.el -l rust-mode-tests.el -f ert-run-tests-batch-and-exit
