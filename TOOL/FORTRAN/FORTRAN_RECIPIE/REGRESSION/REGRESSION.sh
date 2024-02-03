#!/bin/bash
src=$(basename $0 .sh).f90
exe=$(basename $0 .sh).exe
nml=$(basename $0 .sh).nml

f90=ifort
opt="-CB -traceback -fpe0 -convert big_endian -assume byterecl"
#opt="-convert big_endian -assume byterecl"

BIN=$(basename $0 .sh).BIN
CTL=$(basename $BIN .BIN).CTL

NT=365
cat <<EOF>$nml
&para
BIN="$BIN"
NT=${NT}
A1=1.0
P1=365
A21=1.0
P21=365
L21=0
A22=0.1
P22=10
L22=3
&end
EOF

cat <<EOF>$CTL
dset ^$BIN
title ${STN}
options big_endian
undef -999.90
xdef 1 levels 130
ydef 1 levels 32
zdef 1 levels 1000
tdef ${NT} linear 01JAN2000 1DY
vars 3
X1  1 0 INDEPEDENT (INPUT)
X2  1 0 DEPENDENT  (OUTPUT)
XR  1 0 REGRESSION
endvars
EOF



echo
echo ${src}
echo
ls -lh --time-style=long-iso ${src}
echo

echo Compiling ${src} ...
echo
echo ${f90} ${opt} ${src} ${SUB} -o ${exe}
echo
${f90} ${opt} ${src}  ${SUB} -o ${exe}
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
#${exe}
${exe} < ${nml}
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
#ls -lh --time-style=long-iso -lh ${ODIR}/${BFLE}
#echo
#ls -lh --time-style=long-iso -lh ${ODIR}/${OFLE}
#echo
rm -v $exe $nml
