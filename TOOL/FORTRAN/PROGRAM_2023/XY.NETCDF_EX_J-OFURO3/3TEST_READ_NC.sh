#!/bin/bash
#
# Mon, 08 Jan 2024 10:51:36 +0900
# localhost.localdomain
# /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/22.12.DECOMP_GLOBE/42.12.FRACTIONAL_CONTRIBUTION
#
src=$(basename $0 .sh).F90
SUB="SUB_NetCDF.F90"
exe=$(basename $0 .sh).exe
nml=$(basename $0 .sh).nml

#f90=ifort
#DOPT=" -fpp -CB -traceback -fpe0 -check all"
#OPT=" -fpp -convert big_endian -assume byterecl"

f90=gfortran
#DOPT=" -ffpe-trap=invalid,zero,overflow,underflow -fcheck=array-temps,bounds,do,mem,pointer,recursion"
#OPT=" -L. -lncio_test -O2 "
OPT2=" -I/usr/local/netcdf-c-4.8.0/include -L/usr/local/netcdf-c-4.8.0/lib -lnetcdff -lnetcdf"

# OpenMP
#OPT2=" -fopenmp "

echo Compiling ${src} ...
echo
echo ${f90} ${DOPT} ${OPT} ${OPT2} ${SUB} ${src} -o ${exe}
echo
${f90} ${DOPT} ${OPT} ${OPT2} ${SUB} ${src} -o ${exe}
if [ $? -ne 0 ]; then

echo
echo "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"
echo
echo "   COMPILE ERROR!!!"
echo
echo "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"
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

YS=1988;YE=2022
Y=$YS
while [ $Y -le $YE ];do

# CHECK LEAP YEAR
a=$(expr $Y % 400); b=$(expr $Y % 100); c=$(expr $Y % 4)
if [ $a -eq 0 -o \( $c -eq 0 -a $b -ne 0 \) ]; then
  NM=366
else
  NM=365
fi

cat<<EOF>$nml
&para
Y=${Y}
NM=${NM}
IM=1440
JM=720
&end
EOF

D1=$(date -R)
${exe} < ${nml}
if [ $? -ne 0 ]; then
echo
echo "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"
echo
echo "   ERROR in $exe: RUNTIME ERROR!!!"
echo
echo "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"
echo
echo TERMINATED.
echo
D2=$(date -R)
echo "TERMINATED AT Y=$Y"
exit 1
fi

Y=$(expr $Y + 1)
done #Y

echo; echo DONE ${exe}.
rm -vf $exe
#D2=$(date -R)
#echo "START: $D1"
#echo "END:   $D2"
