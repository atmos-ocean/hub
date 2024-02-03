#!/bin/bash

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

# CHECK LEAP YEAR
a=$(expr $Y % 400); b=$(expr $Y % 100); c=$(expr $Y % 4)
if [ $a -eq 0 -o \( $c -eq 0 -a $b -ne 0 \) ]; then
  NM=366
else
  NM=365
fi

VLIST="QA QS SST TA10 WND"
INDIR="/work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/12.14.TEST_JOF3_1PNT/12.12.PICKUP_1PNT/OUT_${PLON}_${PLAT}/"
ODIR="OUT_$(basename $0 .sh)/"

for VAR in $VLIST; do
mkdir -vp $ODIR/${VAR}
done #VAR

cat<<EOF>$nml
&para
INDIR="$INDIR"
 ODIR="$ODIR"
 Y=${Y}
 NV=5
 IM=1
 JM=1
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
