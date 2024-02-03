#!/bin/bash

if [ $# -lt 1 ]; then
echo ERROR in $0 : NO ARG.
echo USAGE $0 ODIR
exit 1
fi

ODIR=$1


DRY= #-n

CWD=$(pwd)
#ODIR=${ODIR:-.}
#DRY=${DRY:--n}

DDIR=$(date +"%Y-%m-%d_%H")_BAK_${ODIR}
 LOG=$(date +"%Y-%m-%d_%H")_BAK_${ODIR}.LOG

date -R >$LOG
pwd    >>$LOG
echo   >>$LOG

mkdir -vp $DDIR |tee -a $LOG
echo >>$LOG

D1=$(date -R)
rsync -arv $DRY $ODIR/ $DDIR/ \
 --exclude OUT_* --exclude *.mod --exclude *.exe --exclude *.bin \
 --exclude *.out --exclude *.dat --exclude *.nc  --exclude *.gif \
 --exclude *.eps  --exclude *.tgz --exclude *.gz \
 --exclude *.BIN --exclude *.o --exclude *.a \
 --exclude  fort.* \
 --max-size=1M \
>> $LOG
#--exclude *.png 
D2=$(date -R)

echo "START: $D1"|tee -a $LOG
echo "END  : $D2"|tee -a $LOG
echo >>$LOG

echo "END $(basename $0)"|tee -a $LOG
echo |tee -a $LOG
du -sch $DDIR |tee -a $LOG
echo |tee -a $LOG
cp -av $LOG $DDIR |tee -a $LOG
cp -av $0   $DDIR |tee -a $LOG

#rm -rf ./$LOG

exit 0

