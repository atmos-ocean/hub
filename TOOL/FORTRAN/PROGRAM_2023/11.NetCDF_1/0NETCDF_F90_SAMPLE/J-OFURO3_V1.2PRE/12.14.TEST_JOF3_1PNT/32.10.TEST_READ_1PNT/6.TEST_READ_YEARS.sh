#!/bin/bash
#
# Sun, 26 Nov 2023 15:20:27 +0900
# localhost.localdomain
# /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/12.14.TEST_JOF3_1PNT/32.10.TEST_READ_1PNT
#
src=$(basename $0 .sh).F90
exe=$(basename $0 .sh).exe
nml=$(basename $0 .sh).nml

#f90=ifort
#DOPT=" -fpp -CB -traceback -fpe0 -check all"
#OPT=" -fpp -convert big_endian -assume byterecl"
#OPT2=" -I/usr/local/netcdf-c-4.8.0/include -L/usr/local/netcdf-c-4.8.0/lib -lnetcdff -lnetcdf"

f90=gfortran
OPT2=" -I/usr/local/netcdf-c-4.8.0/include -L/usr/local/netcdf-c-4.8.0/lib -lnetcdff -lnetcdf"

# OpenMP
#OPT2=" -fopenmp "

PLON=123; PLAT=21

echo Compiling ${src} ...
echo
echo ${f90} ${DOPT} ${OPT} ${src} -o ${exe}
echo
${f90} ${DOPT} ${OPT} ${OPT2} ${src} -o ${exe}
if [ $? -ne 0 ]; then

echo
echo "=============================================="
echo "   COMPILE ERROR!!!"
echo "=============================================="
echo TERMINATED.; echo; exit 1
fi
echo "Done Compile."
echo; ls -lh ${exe}; echo



YS=1988; YE=2022
Y=$YS

while [ $Y -le $YE ];do

year=$Y
a=$(expr $year % 400); b=$(expr $year % 100); c=$(expr $year % 4)

if [ $a -eq 0 -o \( $c -eq 0 -a $b -ne 0 \) ]; then
  NM=366
else
  NM=365
fi

VNAME1=QA; VNAME2=QS; VNAME3=SST; VNAME4=TA10; VNAME5=WND

INDIR="/work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/12.14.TEST_JOF3_1PNT/12.12.PICKUP_1PNT/OUT_${PLON}_${PLAT}/"
INFLE1=J-OFURO3_QA_DAILY_${PLON}_${PLAT}_${Y}.nc
INFLE2=J-OFURO3_QS_DAILY_${PLON}_${PLAT}_${Y}.nc
INFLE3=J-OFURO3_SST_DAILY_${PLON}_${PLAT}_${Y}.nc
INFLE4=J-OFURO3_TA10_DAILY_${PLON}_${PLAT}_${Y}.nc
INFLE5=J-OFURO3_WND_DAILY_${PLON}_${PLAT}_${Y}.nc

OFLE1=J-OFURO3_QA_DAILY_${PLON}_${PLAT}_${Y}_CHK.nc
OFLE2=J-OFURO3_QS_DAILY_${PLON}_${PLAT}_${Y}_CHK.nc
OFLE3=J-OFURO3_SST_DAILY_${PLON}_${PLAT}_${Y}_CHK.nc
OFLE4=J-OFURO3_TA10_DAILY_${PLON}_${PLAT}_${Y}_CHK.nc
OFLE5=J-OFURO3_WND_DAILY_${PLON}_${PLAT}_${Y}_CHK.nc

if [ ! -f ${INDIR}${VNAME1}/${INFLE1} ];then echo NO SUCH FILE,${INDIR}${VNAME1}/${INFLE1};exit 1;fi
if [ ! -f ${INDIR}${VNAME2}/${INFLE2} ];then echo NO SUCH FILE,${INDIR}${VNAME2}/${INFLE2};exit 1;fi
if [ ! -f ${INDIR}${VNAME3}/${INFLE3} ];then echo NO SUCH FILE,${INDIR}${VNAME3}/${INFLE3};exit 1;fi
if [ ! -f ${INDIR}${VNAME4}/${INFLE4} ];then echo NO SUCH FILE,${INDIR}${VNAME4}/${INFLE4};exit 1;fi
if [ ! -f ${INDIR}${VNAME5}/${INFLE5} ];then echo NO SUCH FILE,${INDIR}${VNAME5}/${INFLE5};exit 1;fi

ODIR="OUT_$(basename $0 .sh)/"
mkdir -vp $ODIR
OFLE=$(basename $INFLE .nc)_TEST.nc
if [ -f $ODIR/$FLE ]; then rm -vf $ODIR/$OFLE; fi
cat<<EOF>$nml
&para
VNAME1="$VNAME1"
VNAME2="$VNAME2"
VNAME3="$VNAME3"
VNAME4="$VNAME4"
VNAME4="$VNAME4"
VNAME5="$VNAME5"
INDIR="$INDIR"
INFLE1="$INFLE1"
INFLE2="$INFLE2"
INFLE3="$INFLE3"
INFLE4="$INFLE4"
INFLE5="$INFLE5"
 ODIR="$ODIR"
OFLE1="$OFLE1"
OFLE2="$OFLE2"
OFLE3="$OFLE3"
OFLE4="$OFLE4"
OFLE5="$OFLE5"
   NM=$NM
&end
EOF

echo "MMMMM"
echo "MMMMM ${exe} : ${Y}"
echo "MMMMM"
${exe} < ${nml}
if [ $? -ne 0 ]; then
echo "EEEEE  ERROR in $exe: RUNTIME ERROR!!!"
echo "EEEEE ${Y}"
fi
echo "MMMMM DONE ${Y}"; echo

echo
rm -vf $nml

Y=$(expr $Y + 1)
done #Y

rm -vf $exe
