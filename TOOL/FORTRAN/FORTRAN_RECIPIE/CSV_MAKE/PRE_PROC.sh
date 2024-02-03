#!/bin/bash
#
# Description:
#
src=$(basename PRE_PROC.sh .sh).F90
exe=$(basename $src .f90).exe
f90=ifort
opt="-CB -traceback -fpe0 " # -convert big_endian -assume byterecl"  

temp=$(basename $src .f90)
prog_name="$(echo $temp| sed -e 's/\./_/g' | sed -e 's/\-/_/g')"


echo Compiling ${src} ...
echo
echo ${f90} ${opt} ${src} -o ${exe}
echo
${f90} ${opt} ${src} -o ${exe}
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
${exe}
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
exit 1
fi
echo
echo "Done ${exe}"
echo

