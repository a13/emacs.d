#!/bin/sh
CONFDIR=$HOME/.emacs.d
if [ -e $CONFDIR ]
then 
    test  -L $CONFDIR || mv $CONFDIR ${CONFDIR}.backup
fi
ln -Tsf `pwd` $HOME/.emacs.d

cd $CONFDIR/conf.d
cat enabled | while read prio file; do
    test "$file" && ln -sf  ../conf.avail/${file}.el ${prio}${file}.el
done

mkdir -p $CONFDIR/elpa
