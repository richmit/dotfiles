#!/bin/bash

# Simple script to copy the emacs dot files in this repo into place

if [ -e README.md ] ; then
  BAKEXT='.'`date '+%s'`

  echo ' '
  
  for f in ~/.emacs ~/.Xdefaults ~/.emacs.d/mjr-dark-theme.el ~/.emacs.d/init.el; do
    if [ -e "$f" ] ; then
      echo BACKUP "$f" to "$f$BAKEXT"
      cp "$f" "$f$BAKEXT"
    fi
  done

  if [ -e ~/.emacs ] ; then
    rm ~/.emacs
  fi

  if [ ! -e ~/.emacs.d ] ; then
   echo "MAKE DIR: ~/.emacs.d"
   mkdir ~/.emacs.d
   fi

  echo "INSTALL: init.el"
  cp init.el           ~/.emacs.d/init.el

  echo "INSTALL: mjr-dark-theme.el"
  cp mjr-dark-theme.el ~/.emacs.d/mjr-dark-theme.el
  
else
  echo "RUN THIS SCRIPT FROM INSIDE THE GIT REPO!!!"
fi
