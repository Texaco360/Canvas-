#!/bin/bash

LAZBUILD="/usr/bin/lazbuild"
PROJECT="/home/koenraad/Code/Canvas/project1.lpi"

# Modify .lpr file in order to avoid nothing-to-do-bug (http://lists.lazarus.freepascal.org/pipermail/lazarus/2016-February/097554.html)
echo. >> "/home/koenraad/Code/Canvas/project1.lpr"

if $LAZBUILD $PROJECT; then
  if [ $1 = "test" ]; then
    "/home/koenraad/Code/Canvas/project1" 
  fi
fi
