#!/bin/bash

TIMESTAMP=$(date -R)
HOST=$(hostname)
CWD=$(pwd)
COMMAND="$0 $@"
LOG=$(basename $0 .sh)_$(date "+%y%m%d-%H%M).LOG

echo "# "             |tee    $LOG
echo "# $TIMESTAMP  " |tee -a $LOG
echo "# $HOST       " |tee -a $LOG
echo "# $CWD        " |tee -a $LOG
echo "# $COMMAND    " |tee -a $LOG
echo "# "             |tee -a $LOG

ORG=$1
COPY=$2

cp -arv $ORG $COPY | tee -a $LOG

ls -lh --time-style=long-iso $ORG  |tee -a $LOG
ls -lh --time-style=long-iso $COPY |tee -a $LOG


exit 0
