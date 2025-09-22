#!/bin/bash

LAZBUILD="/usr/bin/lazbuild"
PROJECT="/mnt/c/Users/decro/Code/Objectpascal/Test Canvas/project1.lpi"

# Modify .lpr file in order to avoid nothing-to-do-bug (http://lists.lazarus.freepascal.org/pipermail/lazarus/2016-February/097554.html)
echo >> "$PROJECT"

if "$LAZBUILD" "$PROJECT"; then

  if [ "$1" = "test" ]; then
    "/mnt/c/Users/decro/Code/Objectpascal/Test Canvas/project1" 
  fi
fi
