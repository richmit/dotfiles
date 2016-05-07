#!/bin/bash

# Simple script to copy the emacs dot files in this repo into place

if [ -e README.md ] ; then
  BAKEXT='.'`date '+%s'`

  echo BEFORE
  echo ' '
  ls -l ~/.emacs ~/.Xdefaults ~/.emacs.d/mjr-dark-theme.el
  
  for f in ~/.emacs ~/.Xdefaults ~/.emacs.d/mjr-dark-theme.el; do
    if [ -e "$f" ] ; then
      echo cp "$f" "$f$BAKEXT"
    fi
  done

  cp emacs             ~/.emacs
  cp Xdefaults         ~/.Xdefaults
  test -e ~/.emacs.d || mkdir ~/.emacs.d
  cp mjr-dark-theme.el ~/.emacs.d/mjr-dark-theme.el

  echo AFTER
  echo ' '
  ls -l ~/.emacs ~/.Xdefaults ~/.emacs.d/mjr-dark-theme.el ~/*.$BAKEXT
  
else
  echo "RUN THIS SCRIPT FROM INSIDE THE GIT REPO!!!"
fi
