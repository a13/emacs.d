#!/bin/sh
CONFDIR=$HOME/.emacs.d
if [ -e $CONFDIR ]
then
    test -L $CONFDIR || mv $CONFDIR ${CONFDIR}.backup
fi
ln -Tsf `pwd` $HOME/.emacs.d

mkdir -p $CONFDIR/elpa
