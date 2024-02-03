#!/bin/bash

HOST=zamanda@hfront01.bosai.go.jp
ORGDIR_ROOT=${HOST}:/work07/thotspot/zamanda/WRF3.7.1/WRFV3/test

DESTDIR_ROOT=/work06/manda/WRF_HF1

LOGDIR=LOG_$(basename $0 .sh)
mkdir -vp $LOGDIR
LOG=$LOGDIR/$(basename $0 .sh)_$(date +"%Y%m%d_%H%M%S").LOG

date -R |tee      >$LOG
pwd     |tee  -a >>$LOG
echo $0 $@   |tee  -a >>$LOG

echo
echo "GETTING WRFOUT FILES ..." |tee -a $LOG
echo

echo 
echo Qms+7Ffcu4
echo
echo CTL+z
echo jobs -l
echo bg 1
echo

nohup rsync -avr ${ORGDIR_ROOT}/  ${DESTDIR_ROOT}/ \
--exclude "wrfi*" --exclude "wrfl*" \
--exclude "wrfb*" --exclude "wrffd*" --exclude "rsl.*.*"  \
|tee -a $LOG



#expect -c "
#set timeout 10
#spawn rsync -avr ${orgdir}/wrfout* ${orgdir}/namelist.*  ${destdir}
#expect \"Enter passphrase for key '/home/manda/.ssh/id_rsa':\" {
#        send \"rz387a\n\"
#        expect \"nb01607@ht210f.bosai.go.jp's password:\"
#        send \"Qms+7Ffcu4\n\"
#expect {\"100%\" { exit 0 }} # 100%‚Ì•¶Žš—ñ‚ª•\Ž¦‚³‚ê‚½‚çexit‚·‚é
#}
#"

echo |tee -a $LOG
echo "DONE." |tee -a $LOG
echo  |tee -a $LOG
echo "ORGDIR  : ${orgdir_root}" |tee -a $LOG
echo "DESTDIR : ${destdir_root}"  |tee -a $LOG
ls -lhd --time-style=long-iso $DESTDIR_ROOT/${CASE}.${RUN}/* |tee -a $LOG
echo |tee -a $LOG
echo |tee -a $LOG
echo |tee -a $LOG

du -sch ${DESTDIR_ROOT}/*

exit 0
