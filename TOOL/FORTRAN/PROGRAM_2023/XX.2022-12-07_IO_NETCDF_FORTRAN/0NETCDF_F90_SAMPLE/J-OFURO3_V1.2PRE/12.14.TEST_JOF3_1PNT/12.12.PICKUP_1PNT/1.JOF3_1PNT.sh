
YS=1988; YE=2022
VLIST="LHF QA QS SST TA10 WND"
LON=123; LAT=21
for VAR in $VLIST; do

Y=$YS

while [ $Y -le $YE ];do
INDIR=/work01/DATA/J-OFURO3/V1.2_PRE/HOURS.SINCE.1800-01-01/${VAR}
INFLE=J-OFURO*${VAR}*DAILY*${Y}_*.nc
IN=$INDIR/$INFLE
if [ ! -f $IN ];then echo NO SUCH FILE,$IN;exit 1;fi

ODIR=OUT_${LON}_${LAT}/${VAR}
mkdir -vp $ODIR
OUT=$ODIR/J-OFURO3_${VAR}_DAILY_${LON}_${LAT}_${Y}.nc

rm -vf $OUT
cdo -remapbil,lon=${LON}_lat=${LAT} $IN $OUT
if [ -f $OUT ];then
echo MMMMM OUTPUT: $OUT
else
echo EEEEE NO SUCH FILE: $OUT
fi

Y=$(expr $Y + 1)
done #Y

done #VAR
