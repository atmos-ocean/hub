#!/bin/sh

exe=runncl.sh
ncl=$(basename $0 .sh).ncl

$exe $ncl "Hello!"


exit 0
