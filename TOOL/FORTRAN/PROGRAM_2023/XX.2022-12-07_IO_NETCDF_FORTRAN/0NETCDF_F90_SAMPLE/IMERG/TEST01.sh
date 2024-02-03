#!/bin/bash
#
# Tue, 14 Nov 2023 14:29:47 +0900
# localhost.localdomain
# /work09/am/00.WORK/2022.ECS2022/34.IMERG/12.12.TEST.READ.IMERG
#
src=$(basename $0 .sh).F90
exe=$(basename $0 .sh).exe
nml=$(basename $0 .sh).nml

f90=ifort
DOPT=" -fpp -CB -traceback -fpe0 -check all"
OPT=" -fpp -convert big_endian -assume byterecl"
OPT2="-L/usr/local/netcdf-c-4.8.0/lib -lnetcdff -lnetcdf -I/usr/local/netcdf-c-4.8.0/include"

#f90=gfortran
#DOPT=" -ffpe-trap=invalid,zero,overflow,underflow -fcheck=array-temps,bounds,do,mem,pointer,recursion"

#cat<<EOF>$nml
#&para
#&end
#EOF


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
${exe}
# ${exe} < ${nml}
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
