#!/bin/bash

src=$(basename $0 .sh).F90; SUB=EPT_B80.F90
exe=$(basename $0 .sh).exe
nml=$(basename $0 .sh).nml

f90=ifort
DOPT=" -fpp -CB -traceback -fpe0 -check all"
OPT=" -fpp -convert big_endian -assume byterecl"
LOPT="-I -L -l"

#f90=gfortran
#DOPT=" -ffpe-trap=invalid,zero,overflow,underflow -fcheck=array-temps,bounds,do,mem,pointer,recursion"
#OPT=" -L. -lncio_test -O2 "

# OpenMP
#OPT2=" -fopenmp "

cat<<EOF >$nml
&para
TC=27.0
RH=90.0
P=1005.0
&end
EOF

echo; echo Created ${nml}.
ls -lh --time-style=long-iso ${nml}; echo


echo ${src}.; ls -lh --time-style=long-iso ${src}; echo

echo Compiling ${src} ...; echo
echo ${f90} ${DOPT} ${OPT} ${src} ${SUB} -o ${exe}; echo
${f90} ${DOPT} ${OPT} ${OPT2} ${src} ${SUB} -o ${exe}

if [ $? -ne 0 ]; then
echo "EEEEE COMPILE ERROR!"
echo "EEEEE TERMINATED."; echo
exit 1
fi
echo "Done Compile."; echo
ls -lh ${exe}
echo

echo;echo ${exe} is running ...; echo

D1=$(date -R)

echo MMMMM; echo MMMMM EPT BOLTON 1980; echo MMMMM

${exe} < ${nml}
if [ $? -ne 0 ]; then
echo;echo "EEEEE ERROR in $exe: RUNTIME ERROR!"
echo "EEEEE TERMINATED."; echo

D2=$(date -R)
echo "START: $D1 END:   $D2"
exit 1
fi
echo; echo "Done ${exe}";echo
D2=$(date -R)
echo "START: $D1 END:   $D2"
