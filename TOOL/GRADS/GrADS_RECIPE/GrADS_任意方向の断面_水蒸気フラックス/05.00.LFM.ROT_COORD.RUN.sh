#!/bin/bash


LINE1="A"; LON1_1=124; LAT1_1=26.7; LON1_2=130; LAT1_2=31.5

LINE2="B"; LON2_1=124.72; LAT2_1=25.8; LON2_2=130.72; LAT2_2=30.6

LINE3="C"; LON3_1=128.5; LAT3_1=25.8; LON3_2=128.5; LAT3_2=30.6

LINE4="D"; LON4_1=130.2; LAT4_1=25.8; LON4_2=130.2; LAT4_2=30.6


#FH_LIST="03 06"
FH_LIST="00"

YYYY=2022; MM=06; MMM=JUN; DDLIST="19" #"18 19"


EXELIST="\
05.12.LFM.MAP.CSC4_ROT.WVF.sh \
"
# 03.12.LFM.MAP.CSC4_SST_EPT.sh \

# 03.10.LFM.MAP.CSC4_SST_W.sh \
# 03.06.LFM.MAP.CSC4_2.sh
# 03.08.LFM.MAP.CSC4_W.sh

#03.04.LFM.MAP.CSC4.sh \
#16.26.LFM.MAP.TEMP_CSC.sh \

for FH in $FH_LIST; do

for EXE in $EXELIST; do

for DD in $DDLIST; do

if [ $DD == "18" ]; then
HHLIST="14 15 16 17 18 19 20 21 22 23"
elif [ $DD == "19" ]; then
HHLIST="00 01 02 03 04 05 06 07 08 09 10 12"
#HHLIST="00"
fi

for HH in $HHLIST; do

echo "NNNNN $EXE $YYYY $MM $DD $HH"
YMDH=${YYYY}${MM}${DD}${HH}

if [ $EXE == "16.26.LFM.MAP.TEMP_CSC.sh" ]; then
$EXE $FH ${YMDH} $LINE1 ${LON1_1} ${LAT1_1} ${LON1_2} ${LAT1_2}

elif [ $EXE == "04.08.LFM.SNAPSHOT.WVF.sh" -o \
       $EXE == "04.10.LFM.ROT_COORD_WVF.sh" ]; then
$EXE $FH ${YMDH} $LINE1 ${LON1_1} ${LAT1_1} ${LON1_2} ${LAT1_2} \
                 $LINE2 ${LON2_1} ${LAT2_1} ${LON2_2} ${LAT2_2} 
else

$EXE $FH ${YMDH} $LINE1 ${LON1_1} ${LAT1_1} ${LON1_2} ${LAT1_2} \
                 $LINE2 ${LON2_1} ${LAT2_1} ${LON2_2} ${LAT2_2} \
                 $LINE3 ${LON3_1} ${LAT3_1} ${LON3_2} ${LAT3_2} \
                 $LINE4 ${LON4_1} ${LAT4_1} ${LON4_2} ${LAT4_2} \

fi

echo "NNNNN DONE $EXE  $YYYY $MM $DD $HH"
done #HHLIST

done #DDLIST
done #EXELIST

done #FH




EXELIST="\
"

