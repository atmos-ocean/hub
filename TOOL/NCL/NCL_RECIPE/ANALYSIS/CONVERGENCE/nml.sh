#!/bin/sh
export LANG=C

exe=runncl.sh
nml=$(basename $0 .sh).nml
ncl=$(basename $0 .sh).ncl

indir=INPUT
wrfout_time="2017-07-03_12:00:00"
domain=d03
wrfout=wrfout_${domain}_${wrfout_time}
sdatetime="2017-07-05_00:00:00"
edatetime="2017-07-05_12:00:00"

runname=RUNNAME

cat<<EOF>$nml
runname =${runname}
indir =${indir}
domain =${domain}
wrfout =${wrfout}
sdatetime =${sdatetime}
edatetime =${edatetime}
EOF

$exe $ncl "$nml"


exit 0
