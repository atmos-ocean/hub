#!/bin/bash
#
# Mon, 04 Sep 2023 14:16:13 +0900
# localhost.localdomain
# /work09/am/00.WORK/2022.RW3A/16.00.EAV.TRAJ.RW3A
#
src=$(basename $0 .sh).F90
exe=$(basename $0 .sh).exe
nml=$(basename $0 .sh).nml

f90=ifort
DOPT=" -fpp -CB -traceback -fpe0" # -check all"
OPT=" -fpp -convert big_endian -assume byterecl"

#f90=gfortran
#DOPT=" -ffpe-trap=invalid,zero,overflow,underflow -fcheck=array-temps,bounds,do,mem,pointer,recursion"
#OPT=" -L. -lncio_test -O2 "

# OpenMP
#OPT2=" -fopenmp "

cat<<EOF>$nml
&para
&end
EOF

echo
echo Created ${nml}.
echo
ls -lh --time-style=long-iso ${nml}
echo


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
echo
echo "   COMPILE ERROR!!!"
echo
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
echo
echo "   ERROR in $exe: RUNTIME ERROR!!!"
echo
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
