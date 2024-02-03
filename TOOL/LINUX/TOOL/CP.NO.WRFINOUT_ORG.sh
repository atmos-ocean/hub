#!/bin/bash

if [ $# -ne 2 ]; then
COMMAND=$(basename $0)
echo
echo ERROR in $COMMAND : WRONG ARGUMENTS
echo
echo USAGE : $COMMAND INDIR OUTDIR
echo
exit 1
fi

INDIR=$1
OUTDIR=$2

rsync -av ${INDIR}/* ./${OUTDIR} --exclude "*d*:*" --exclude "wrfi*" \
--exclude "wrfl*" --exclude "wrfb*" --exclude "wrffd*" --exclude "rsl.*.*" \
--exclude "log*.*" \
