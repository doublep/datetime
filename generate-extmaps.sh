#! /usr/bin/env bash

set -e

OWN_DIRECTORY=$(dirname $0)
cd $OWN_DIRECTORY

if [ -z "$EMACS" ] ; then
    EMACS=emacs
fi

cd dev
javac HarvestData.java
cd ..

# Emacs only reads single lines from stdin...
java -cp dev HarvestData --locales | tr "\n" " "                                                                                \
  | $EMACS -batch                                                                                                               \
          --eval "(progn (require 'package) (package-initialize))"                                                              \
          --eval "(when (locate-file \"local-environment.el\" (list (car load-path))) (load \"local-environment.el\" nil t t))" \
          --eval "(require 'extmap)"                                                                                            \
          --eval "(extmap-from-alist \"locale-data.extmap\" (read-minibuffer \"\") :overwrite t)"

java -cp dev HarvestData --timezones | tr "\n" " "                                                                              \
  | $EMACS -batch                                                                                                               \
          --eval "(progn (require 'package) (package-initialize))"                                                              \
          --eval "(when (locate-file \"local-environment.el\" (list (car load-path))) (load \"local-environment.el\" nil t t))" \
          --eval "(require 'extmap)"                                                                                            \
          --eval "(extmap-from-alist \"timezone-data.extmap\" (read-minibuffer \"\") :overwrite t)"
