#!/bin/bash

#ORG=.
#DEST=BAK_${ORG}

if [ $# -eq 2 ]; then
ORG=$1
DEST=$2
else
echo
echo ERROR in $0 : WRONG ARGUMENTS
echo USAGE: $0 ORG DEST
echo
exit 1
fi

if [ ! -f $ORG -a ! -d $ORG ]; then
echo
echo ERROR in $0 : NO SUCH FILE/DIRECTORY, $ORG
echo
exit 1
fi

LOG=$(basename $0 .sh)_$ORG.log

date -R |tee $LOG
pwd     |tee -a $LOG
echo >>$LOG
echo $0 $@ >>$LOG
echo >>$LOG

cp -avr ${ORG} ./${DEST} |tee -a $LOG

echo
echo LOG FILE: $LOG
echo

exit 0
