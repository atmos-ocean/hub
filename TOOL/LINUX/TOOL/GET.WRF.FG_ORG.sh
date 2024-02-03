#!/bin/bash

# DOWNLOAD TIME
#2019-04-24_07-49
#/work05/manda/WRF.Result
#manda@calypso
#$ GET.WRF.sh H30.12.MSM
#
#ORGDIR  : nb01607@ht210f.bosai.go.jp:/data2/nb01/nb01607//WRF3.7.1/WRFV3/test/H30.12.MSM
#DESTDIR : /work05/manda/WRF.Result/H30.12.MSM
#çáåv 61G
#-rw-r----- 1 manda manda  61G  4åé 23 22:13 2019 wrfout_d01_2018-07-04_00:00:00
#
# 2019-04-24_08-06

HOST=zamanda@hfront01.bosai.go.jp
ORGDIR_ROOT=${HOST}:/work07/thotspot/zamanda

if [ $# -lt 1 ]; then
echo
echo "ERROR in $(basename $0) : NO ARGUMENT"
echo "USAGE : $(basename $0) RUNNAME"
echo 
echo "ORGDIR : RUNNAME"
echo
exit 1
fi 
CASE=H30.TREND
runname=$1

orgdir=${ORGDIR_ROOT}/WRF3.7.1/WRFV3.${CASE}/test/${runname}

#if [ ! -d $orgdir ]; then
#echo
#echo ERROR in $0:NO SUCH DIR., $orgdir
#echo
#exit 1
#fi

destdir_root=.

destdir=${destdir_root}/${runname}
rm -rvf ${runname}
mkdir -vp ${runname}

#cwd=$(pwd)
#echo
#echo CHANGE DIR TO ${destdir}
#echo
#cd ${destdir}

echo
echo "GETTING WRFOUT FILES ..."
echo

expect -c "
set timeout 10
spawn rsync -avr ${orgdir}/* ${destdir} \
 --exclude "wrfi*" --exclude "wrfl*" --exclude "wrfb*" \
 --exclude "wrffd*"
expect \"Authenticated with partial success.\" {
        expect \"zamanda@hfront01.bosai.go.jp's password:\"
        send \"Qms+7Ffcu4\n\"}
interact
"

echo
echo "DONE."
echo 
echo "ORGDIR  : ${orgdir}"
echo "DESTDIR : ${destdir}" 
echo "$(ls -lh --time-style=long-iso $destdir)"
echo
exit 0
