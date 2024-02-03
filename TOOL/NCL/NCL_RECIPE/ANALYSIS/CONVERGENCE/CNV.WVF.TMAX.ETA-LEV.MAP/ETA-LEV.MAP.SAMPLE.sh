#!/bin/sh
export LANG=C

#exe=./runncl.sh
exe=runncl.sh
nml=$(basename $0 .sh).nml

# AREA AVERAGE
min_lat=24
max_lat=34
min_lon=120
max_lon=129

# $ ls -I"*FULL.BAK" /work05/manda/WRF3.7.1/WPS.STORE/ |grep Kyushu.170704|sed -e 's/WPS.//'

#2017
wrfout_time="2017-07-04_06:00:00"
domain=d03
wrfout=wrfout_${domain}_${wrfout_time}
sdatetime="2017-07-05_03:00:00"
edatetime="2017-07-05_12:00:00"

runname_list="\
Kyushu.170704_R07.02 \
"
# NOTE: Kyushu.170704_R07.02 = ABOM

#Kyushu.170704_R07.HIMSST \
#Kyushu.170704_R07.MGDSST \
#Kyushu.170704_R07.NAVO \
#Kyushu.170704_R07.OISST \
#Kyushu.170704_R07.UKMO \
#Kyushu.170704_R07.DR_B \
#Kyushu.170704_R07.RTG05 \
#Kyushu.170704_R07.RTG083 \
#Kyushu.170704_R07.JPL_OUROCEAN \

#2018
#runname="H30.7.GOUU.00.MGDCLM"
#runname="H30.7.GOUU.00.MGD"
#runname="H30.7.GOUU.00.01"
#start=20180708
#  end=20180708 #7

#2012
#runname="Kyushu.120710_R18.JPL_MUR"
#runname="Kyushu.120710_R18.JPL_OUROCEAN"
#runname="Kyushu.120710_R18.RTG083"
#runname="Kyushu.120710_R18.UKMO"
#runname="Kyushu.120710_R18.NCEI"
#runname="Kyushu.120710_R18.NAVO"
#runname="Kyushu.120710_R18.OISST"
#runname="Kyushu.120710_R18.ABOM"
#runname="Kyushu.120710_R18.MGDSST"
#start=20120710
#  end=20120715 #7



ncl1=$(basename $0 .sh).ncl



for runname in $runname_list; do

indir_root=/work05/manda/WRF3.7.1/WRFV3/test
indir=${indir_root}/${runname}
if [ ! -d $indir ]; then
echo
echo ERROR in $0 : NO SUCH DIR, $indir
echo
exit 1
fi
infle=${indir}/${wrfout}
if [ ! -f $infle ]; then
echo
echo ERROR in $0 : NO SUCH FILE, $infle
echo
exit 1
fi

cat<<EOF>$nml
runname =${runname}
indir =${indir}
domain =${domain}
wrfout =${wrfout}
sdatetime =${sdatetime}
edatetime =${edatetime}
min_lat = ${min_lat}
max_lat = ${max_lat}
min_lon = ${min_lon}
max_lon = ${max_lon}
EOF

$exe $ncl1 "$nml"

done #for runname

exit 0
