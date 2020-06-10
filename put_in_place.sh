#!/bin/bash

# Simple script to copy the emacs dot files in this repo into place

if [ -e README.md ] ; then
  BAKEXT='.'`date '+%s'`

  echo ' '
  
  for f in ~/.Rprofile ~/.emacs ~/.emacs.d/mjr-dark-theme.el ~/.emacs.d/init.el; do
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

  echo "INSTALL: Rprofile"
  cp Rprofile ~/.Rprofile
  # Put a copy in the windows home directory so R works outside of Emacs
  if [ -e ~/winHome ]; then
    cp Rprofile ~/winHome/.Rprofile
  fi
  
else
  echo "RUN THIS SCRIPT FROM INSIDE THE GIT REPO!!!"
fi
