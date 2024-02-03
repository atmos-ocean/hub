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

VAR=LHF; PLON=123; PLAT=21
Y=1988; NM=366
INDIR="/work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/12.14.TEST_JOF3_1PNT/12.12.PICKUP_1PNT/OUT_${PLON}_${PLAT}/${VAR}/"
INFLE=J-OFURO3_LHF_DAILY_${PLON}_${PLAT}_${Y}.nc
ODIR=OUT_$(basename $0 .sh)/${VAR}
mkdir -vp $ODIR
OFLE=$(basename $INFLE .nc)_TEST.nc
if [ -f $ODIR/$FLE ]; then rm -vf $ODIR/$OFLE; fi

cat<<EOF>$nml
&para
INDIR="$INDIR"
INFLE="$INFLE"
 ODIR="$ODIR"
 OFLE="$OFLE"
   NM=$NM
 VNAME="$VAR"
&end
EOF
IN=$INDIR/$INFLE
if [ ! -f $IN ];then echo NO SUCH FILE,$IN;exit 1;fi



echo
echo ${src}.
echo
ls -lh --time-style=long-iso ${src}
echo

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

echo
echo ${exe} is running ...
echo
D1=$(date -R)
#${exe}
${exe} < ${nml}
if [ $? -ne 0 ]; then
echo
echo "=============================================="
echo "   ERROR in $exe: RUNTIME ERROR!!!"
echo "=============================================="
echo
echo TERMINATED.
echo
D2=$(date -R)
echo "START: $D1"
echo "END:   $D2"
exit 1
fi
echo
echo "Done ${exe}"
echo
D2=$(date -R)
echo "START: $D1"
echo "END:   $D2"
rm -vf $exe $nml
