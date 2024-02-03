#!/bin/sh
export LANG=C

exe=runncl.sh
nml=$(basename $0 .sh).nml
ncl=$(basename $0 .sh).ncl

wrfout_time="2017-07-03_12:00:00"
domain=d03
wrfout=wrfout_${domain}_${wrfout_time}
sdatetime="2017-07-05_00:00:00"
edatetime="2017-07-05_12:00:00"

CASE=K17
RUN=R11

runname_list="\
${CASE}.${RUN}.00.00 \
"

for runname in $runname_list; do

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

done #for runname

exit 0

# NCL WRAPIT (FORTRAN)
# If you have a Fortran function or procedure that you'd like to 
# call from NCL, you can do it by "wrapping" this function using 
# the WRAPIT script.
#
# https://www.ncl.ucar.edu/Document/Tools/WRAPIT.shtml#Step_1
# https://www.ncl.ucar.edu/Document/Tools/WRAPIT.shtml#Step_2
