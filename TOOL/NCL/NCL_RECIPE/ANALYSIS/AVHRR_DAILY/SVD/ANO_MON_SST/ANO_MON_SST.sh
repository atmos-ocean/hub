#!/bin/bash
#
# Sun, 05 Jan 2020 20:05:41 +0900
# calypso.bosai.go.jp
# /work05/manda/NCL/SVD
# manda
#

LOG=$(basename $0 .sh).LOG
# LOG=$(basename $0 .sh)_$(date +"%y%m%d_%H%M).LOG

date -R  >$LOG
hostname >>$LOG
pwd      >>$LOG
NCL=$(basename $0 .sh).ncl
ls -lh --time-style=long-iso $(basename $0) >>$LOG
ls -lh --time-style=long-iso $(basename $NCL) >>$LOG

echo     >>$LOG


#ncl -nQ $NCL 2>&1 |tee -a $LOG
unbuffer ncl -nQ $NCL >> $LOG 2>&1

echo
echo "Done $(basename $0)"
echo
echo "LOG: $LOG"
echo
