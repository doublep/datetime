#! /usr/bin/env bash

set -e

if [ -z "$EMACS" ] ; then
    EMACS=emacs
fi

cd dev
javac HarvestData.java
cd ..

# Emacs only reads single lines from stdin...
java -cp dev HarvestData | tr "\n" " "                                                            \
  | $EMACS -batch                                                                                 \
          --eval "(progn (require 'package) (package-initialize) (require 'extmap))"              \
          --eval "(extmap-from-alist \"locale-data.extmap\" (read-minibuffer \"\") :overwrite t)"
