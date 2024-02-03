#!/bin/bash

MM=06; DD=18 #20 #19
Y=2022
INDIR=/work01/DATA/MSM/MSM-P/$Y/
 ODIR=/work01/DATA/MSM.NCL/MSM-P/$Y

mkd $ODIR

NCL=TEST.NCL.OUT.ncl

MMDD=${MM}${DD}

 IN=$INDIR/${MMDD}.nc
OUT=$ODIR/${MMDD}_NCL.nc

runncl.sh $NCL $IN $OUT
if [ $? -ne 0 ]; then
echo "mmmmmmmmmmmmmm"; echo ERROR IN $NCL; echo "mmmmmmmmmmmmmm"
fi
if [ -f $OUT ]; then
echo "MMMMMMMMMMMMMMM"; echo OUTPUT: $OUT; echo "MMMMMMMMMMMMMMM"
ncdump -h $OUT
echo
else
echo "mmmmmmmmmmmmmm"; echo NO SUCH FILE, $OUT; echo "mmmmmmmmmmmmmm"
exit 1
fi

