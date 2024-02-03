#!/bin/sh
export LANG=C

exe=runncl.sh
nml=$(basename $0 .sh).nml
ncl=$(basename $0 .ALL.sh).ncl

wrfout_time="2017-07-03_12:00:00"
domain=d03
wrfout=wrfout_${domain}_${wrfout_time}
sdatetime="2017-07-05_01:00:00"
edatetime="2017-07-05_13:00:00"

CASE=K17
RUN=R11

ie=5
je=5

i=0
while [ $i -le $ie ]; do

j=0
while [ $j -le $je ]; do

JJ=$(printf "%02d" $j)
II=$(printf "%02d" $i)

runname=${CASE}.${RUN}.${II}.${JJ}

indir_root=/work05/manda/WRF.RESULT2/
indir=${indir_root}/${CASE}.${RUN}_OUT/${runname}
if [ ! -d $indir ]; then
echo
echo ERROR in $0 : NO SUCH DIR, $indir
echo
exit 1
fi
infle=${indir}/${wrfout}
if [ ! -f $infle ]; then
echo
echo ERROR in $0 : NO SUCH FILE, $infle
echo
exit 1
fi

cat<<EOF>$nml
runname =${runname}
indir =${indir}
domain =${domain}
wrfout =${wrfout}
sdatetime =${sdatetime}
edatetime =${edatetime}
EOF

$exe $ncl "$nml"

j=$(expr $j + 1)

done #j

i=$(expr $i + 1)

done #i

exit 0

# NCL WRAPIT (FORTRAN)
# If you have a Fortran function or procedure that you'd like to 
# call from NCL, you can do it by "wrapping" this function using 
# the WRAPIT script.
#
# https://www.ncl.ucar.edu/Document/Tools/WRAPIT.shtml#Step_1
# https://www.ncl.ucar.edu/Document/Tools/WRAPIT.shtml#Step_2
