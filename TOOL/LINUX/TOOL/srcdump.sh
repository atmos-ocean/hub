#!/bin/bash

export LANG=C

echo "$(basename $(pwd))"
echo "============================-"
echo '[TOC]'
echo "  "
date -R
hostname
pwd
echo
echo '```'
echo "$(basename $0) $@"
echo '```'
echo "  "
echo '### HOW TO RUN'
echo "  "
echo '### INFO'
echo '**Machine info**'
cat /proc/cpuinfo|grep processor | tail -1; cat /proc/cpuinfo|grep "model name" |tail -1; cat /proc/meminfo|head -1
echo "  "
echo '### SOURCE FILES'
for x in "$@"
do
  if [ -f $x ]; then
    echo "- $x"
  fi
done
echo "  "



for i in $*; do
  src=$1

  if [ -f $src ]; then

    ext=${src##*.}

    if [ $ext = "py"  ]; then
      type=python
    elif [ $ext = "sh"  ]; then
      type=bash
    elif [ $ext = "f90" -o $ext = "f" -o $ext = "F" -o $ext = "F90" ]; then
      type=fortran
    elif [ $ext = "c"  ]; then
      type=c
    else
      type=""
    fi

  echo "#### $src"
  echo '```'${type}
  cat $1
  #cat $1 |nkf -s
echo '```'
  echo
  
  fi

  shift
  echo "  "

done

exit 0
