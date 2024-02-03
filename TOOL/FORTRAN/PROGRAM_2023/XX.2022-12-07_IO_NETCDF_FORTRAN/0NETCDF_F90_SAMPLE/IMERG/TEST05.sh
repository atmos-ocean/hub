#!/bin/bash
#
# Tue, 14 Nov 2023 14:29:47 +0900
# localhost.localdomain
# /work09/am/00.WORK/2022.ECS2022/34.IMERG/12.12.TEST.READ.IMERG
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

#DOPT=" -ffpe-trap=invalid,zero,overflow,underflow -fcheck=array-temps,bounds,do,mem,pointer,recursion"

INDIR="/work01/DATA/IMERG/LATE/"
INFLE="3B-HHR-L.MS.MRG.3IMERG.20220619-S190000-E192959.1140.V06C.HDF5"
ODIR="./"
OFLE="3B-HHR-L.MS.MRG.3IMERG.20220619-S190000-E192959.1140.V06C.HDF5.nc4"

cat<<EOF>$nml
&para
INDIR="$INDIR"
INFLE="$INFLE"
 ODIR="$ODIR"
 OFLE="$OFLE"
&end
EOF
IN=$INDIR/$INFLE
if [ ! -f $IN ];then echo NO SUCH FILE,$IN;exit 1;fi


echo Compiling ${src} ...
echo
echo ${f90} ${DOPT} ${OPT}  ${OPT2} ${src} -o ${exe}
echo
${f90} ${DOPT} ${OPT}  ${OPT2} ${src} -o ${exe}
if [ $? -ne 0 ]; then
echo
echo "=============================================="
echo "   COMPILE ERROR!!!"
echo "=============================================="
echo
echo TERMINATED.
echo
exit 1
fi
echo "Done Compile."
echo
ls -lh ${exe}
echo

echo
echo ${exe} is running ...
echo
D1=$(date -R)
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
