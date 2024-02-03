#!/bin/bash
TIME1=00Z12AUG2021; TIME2=00Z12AUG2021 #00Z15AUG2021
GS=$(basename $0 .sh).GS

INROOT=/work00/DATA/WRF.RW3A/HD01/RW3A.ARWpost.DAT/basic_p
CTL1=$INROOT/ARWpost_RW3A.00.03.05.05.0000.01/RW3A.00.03.05.05.0000.01.d01.basic_p.01HR.ctl
if [ ! -f $CTL1 ];then echo NO SUCH FILE,$CTL1;exit 1;fi

CTL2=$INROOT/ARWpost_RW3A.00.03.05.05.0702.01/RW3A.00.03.05.05.0702.01.d01.basic_p.01HR.ctl
if [ ! -f $CTL2 ];then echo NO SUCH FILE,$CTL2;exit 1;fi

KIND='-kind white->deepskyblue->mediumblue->green->yellow->magenta->red'
CLEVS='-levs 0.5 1 4 16 32 64' #mm/hr
LONW=128;LONE=121;LATS=30;LATN=34.5

cat <<EOF>$GS
'open $CTL1';'open $CTL2'
'set time $TIME1';'q dims';line=sublin(result,5);say line;t1=subwrd(line,9)
'set time $TIME2';'q dims';line=sublin(result,5);say line;t2=subwrd(line,9)

t=t1
while (t <= t2)
'set t 't
'q dims';line=sublin(result,5)
wrd=subwrd(line,6);hh=substr(wrd,1,2);dd=substr(wrd,4,2)
mmm=substr(wrd,6,3);yyyy=substr(wrd,9,4)
if (mmm='JAN') ;mm=01 ;endif; if (mmm='FEB') ;mm=02 ;endif
if (mmm='MAR') ;mm=03 ;endif; if (mmm='APR') ;mm=04 ;endif
if (mmm='MAY') ;mm=05 ;endif; if (mmm='JUN') ;mm=06 ;endif
if (mmm='JUL') ;mm=07 ;endif; if (mmm='AUG') ;mm=08 ;endif
if (mmm='SEP') ;mm=09 ;endif; if (mmm='OCT') ;mm=10 ;endif
if (mmm='NOV') ;mm=11 ;endif; if (mmm='DEC') ;mm=12 ;endif

say 'MMMMM 'wrd' 'yyyy' 'mm' 'dd' 'hh


t=t+1
endwhile ;#t

'quit'
EOF

grads -bcl $GS
