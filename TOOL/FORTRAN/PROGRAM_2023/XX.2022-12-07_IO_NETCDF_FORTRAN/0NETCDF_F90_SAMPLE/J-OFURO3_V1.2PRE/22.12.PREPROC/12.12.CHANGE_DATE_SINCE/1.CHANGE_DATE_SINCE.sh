#/bin/bash

YS=1988
YE=2022
#YS=1988
#YE=1997

VLIST="LHF QA QS SST TA10 WND"
VLIST="QS"

for VAR in $VLIST; do

INDIR=/work01/DATA/J-OFURO3/V1.2_PRE/${VAR}
ODIR=/work01/DATA/J-OFURO3/V1.2_PRE/HOURS.SINCE.1800-01-01/${VAR}
mkdir -vp $ODIR

YYYY=$YS
while [ $YYYY -le $YE ]; do

IN=$INDIR/J-OFURO*${VAR}*DAILY*${YYYY}.nc
if [ ! -f $IN ];then echo NO SUCH FILE,$IN;exit 1;fi
OUT=$ODIR/$(basename $IN .nc)_18000101.nc

STR1="days since ${yyyy}-01-01"
STR1="hours since 1800-01-01 00:00"

ncap2 -O -s \
'@units="hours since 1800-01-01 00:00}";time=udunits(time,@units);time@units=@units' \
$IN $OUT

echo
if [ -f $OUT ]; then
echo $OUT
fi

YYYY=$(expr $YYYY + 1)
done #YYYY

done # VAR
