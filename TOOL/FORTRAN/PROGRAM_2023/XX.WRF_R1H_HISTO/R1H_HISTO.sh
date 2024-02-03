#!/bin/bash

src=$(basename $0 .sh).F90; exe=$(basename $0 .sh).exe
nml=$(basename $0 .sh).nml
SUB="DATE_CAL.F90"

f90=ifort
DOPT=" -fpp -CB -traceback -fpe0 -check all"
OPT=" -fpp -convert big_endian -assume byterecl"

#f90=gfortran
#DOPT=" -ffpe-trap=invalid,zero,overflow,underflow -fcheck=array-temps,bounds,do,mem,pointer,recursion"
#OPT=" -L. -lncio_test -O2 "
# OpenMP
#OPT2=" -fopenmp "

RUNNAME=RW3A.00.03.05.05.0000.01
#RUNNAME=RW3A.00.03.05.05.0702.01
DOMAIN=d01

cat<<EOF>$nml
&para
INDIR="/work00/DATA/HD01/RW3A.ARWpost.DAT/basic_p/ARWpost_${RUNNAME}/",
PREFIX="${RUNNAME}.${DOMAIN}.basic_p.01HR_"
OUT="${RUNNAME}.${DOMAIN}.01HR_HISTO.TXT"
IM=599,
JM=599,
KM=30,
NM=73,
YR0=2021, !00Z12AUG2021
MO0=8,
DY0=12,
HR0=0,
MI0=0,
DH=1,
&end
EOF

echo; echo Created ${nml}.; echo
ls -lh --time-style=long-iso ${nml}; echo


echo
echo ${src}.
echo
ls -lh --time-style=long-iso ${src}
echo

echo Compiling ${src} ...
echo
echo ${f90} ${DOPT} ${OPT} ${src} ${SUB} -o ${exe}
echo
${f90} ${DOPT} ${OPT} ${OPT2} ${src} ${SUB} -o ${exe}
if [ $? -ne 0 ]; then

echo; echo "=============================================="; echo
echo "   COMPILE ERROR!!!"
echo; echo "=============================================="; echo
echo TERMINATED.; echo
exit 1
fi
echo "Done Compile."
echo; ls -lh ${exe}; echo

echo; echo ${exe} is running ...; echo
D1=$(date -R)
${exe} < ${nml}
if [ $? -ne 0 ]; then
echo
echo; echo "=============================================="; echo
echo "   RUNTIME ERROR!!!"
echo; echo "=============================================="; echo
echo TERMINATED.; echo
D2=$(date -R)
echo "START: $D1"; echo "END:   $D2"
exit 1
fi
echo; echo "Done ${exe}"; echo

D2=$(date -R)
echo "START: $D1"; echo "END:   $D2"

<<COMMENT
VARS   30
U             30  0  x-wind component (m s-1)
V             30  0  y-wind component (m s-1)
W             30  0  z-wind component (m s-1)
Q2             1  0  QV at 2 M (kg kg-1)
T2             1  0  TEMP at 2 M (K)
U10            1  0  U at 10 M (m s-1)
V10            1  0  V at 10 M (m s-1)
QVAPOR        30  0  Water vapor mixing ratio (kg kg-1)
QCLOUD        30  0  Cloud water mixing ratio (kg kg-1)
QRAIN         30  0  Rain water mixing ratio (kg kg-1)
HGT            1  0  Terrain Height (m)
RAINC          1  0  ACCUMULATED TOTAL CUMULUS PRECIPITATION (mm)
RAINRC         1  0  RAIN RATE CONV (mm per output interval)
RAINNC         1  0  ACCUMULATED TOTAL GRID SCALE PRECIPITATION (mm)
RAINRNC        1  0  RAIN RATE NON-CONV (mm per output interval)
XLAND          1  0  LAND MASK (1 FOR LAND, 2 FOR WATER) (-)
PBLH           1  0  PBL HEIGHT (m)
HFX            1  0  UPWARD HEAT FLUX AT THE SURFACE (W m-2)
QFX            1  0  UPWARD MOISTURE FLUX AT THE SURFACE (kg m-2 s-1)
LH             1  0  LATENT HEAT FLUX AT THE SURFACE (W m-2)
SST            1  0  SEA SURFACE TEMPERATURE (K)
ept           30  0  Equivalent Potential Temperature (K)
sept          30  0  Saturated Equivalent Potential Temperature (K)
pressure      30  0  Model pressure (hPa)
height        30  0  Model height (km)
tk            30  0  Temperature (K)
theta         30  0  Potential Temperature (K)
rh            30  0  Relative Humidity (%)
slp            1  0  Sea Levelp Pressure (hPa)
dbz           30  0  Reflectivity (-)
ENDVARS
COMMENT
