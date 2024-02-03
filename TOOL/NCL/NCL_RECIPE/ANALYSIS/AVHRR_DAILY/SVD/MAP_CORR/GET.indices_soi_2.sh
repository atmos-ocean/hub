#!/bin/bash
#
# Mon, 06 Jan 2020 09:08:42 +0900
# calypso.bosai.go.jp
# /work05/manda/NCL/MAP_CORR
# manda
#

LOG=$(basename $0 .sh).LOG
# LOG=$(basename $0 .sh)_$(date +"%y%m%d_%H%M").LOG

date -R  >$LOG
hostname >>$LOG
pwd      >>$LOG
ls -lh --time-style=long-iso $(basename $0) >>$LOG
echo     >>$LOG

wget -nv -nc -nd \
https://www.ncl.ucar.edu/Applications/Scripts/indices_soi_2.ncl \
 2>&1 |tee -a $LOG


echo
echo "Done $(basename $0)"
echo
echo "LOG: $LOG"
echo
