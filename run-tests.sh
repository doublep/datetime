#! /bin/sh

# Usage: ./run-tests.sh [ERT-SELECTOR]
#
# You can also set EMACS and ERT_SELECTOR variables in the
# environment.  If ERT_SELECTOR is empty (both on command line and in
# environment), it defaults to t (i.e., everything).

# If `local-environment.el' exists, it is loaded before `datetime.el'.
# Can be used e.g. to make `extmap' package loadable.  By the time
# `local-environment.el' is loaded, Emacs packaging system is already
# initialized.

set -e

OWN_DIRECTORY=$(dirname $0)
cd $OWN_DIRECTORY

if [ -z "$EMACS" ]; then
    EMACS=emacs
fi

if [ -n "$1" ]; then
    ERT_SELECTOR=$1
fi

if [ -z "$ERT_SELECTOR" ]; then
    ERT_SELECTOR=t
fi

cd test
javac ProcessTimestamp.java
cd ..

$EMACS --batch                                                                                                               \
       --eval "(message \"Using Emacs %s\" (emacs-version))"                                                                 \
       --eval "(progn (require 'package) (package-initialize))"                                                              \
       --eval "(when (locate-file \"local-environment.el\" (list (car load-path))) (load \"local-environment.el\" nil t t))" \
       -l datetime.el                                                                                                        \
       -l test/test.el                                                                                                       \
       -l test/format.el                                                                                                     \
       -l test/parse.el                                                                                                      \
       --eval "(ert-run-tests-batch-and-exit (quote ${ERT_SELECTOR}))"

$EMACS --batch                                                                                                               \
       --eval "(progn (require 'package) (package-initialize))"                                                              \
       --eval "(when (locate-file \"local-environment.el\" (list (car load-path))) (load \"local-environment.el\" nil t t))" \
       --eval "(setq byte-compile-error-on-warn t)"                                                                          \
       --eval "(batch-byte-compile)" datetime.el
