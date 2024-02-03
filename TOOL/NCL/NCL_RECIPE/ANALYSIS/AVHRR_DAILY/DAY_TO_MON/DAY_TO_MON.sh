#!/bin/sh

exe=runncl.sh
ncl1=$(basename $0 .sh).ncl
ncl2=CHECK_MON_AVE.ncl

YYYYS=1983
YYYYE=2019
MM=12

YYYY=$YYYYS
while [ $YYYY -le $YYYYE ]; do

YYYYMM=${YYYY}${MM}

echo "========================================"
echo $YYYYMM
echo "========================================"

$exe $ncl1 $YYYYMM

if [ $YYYY = "$YYYYS" -o $YYYY = "$YYYYS" ]; then
$exe $ncl2 $YYYYMM
fi

YYYY=$(expr $YYYY + 1)
done

exit 0