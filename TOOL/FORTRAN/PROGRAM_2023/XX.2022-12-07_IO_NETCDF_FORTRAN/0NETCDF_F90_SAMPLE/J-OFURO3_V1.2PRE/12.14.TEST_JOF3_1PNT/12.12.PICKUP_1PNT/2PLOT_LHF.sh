#!/bin/bash

# Sun, 26 Nov 2023 20:04:07 +0900
# /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/12.14.TEST_JOF3_1PNT/12.12.PICKUP_1PNT

#YYYYMMDDHH=2011012100
#YYYY=${YYYYMMDDHH:0:4}; MM=${YYYYMMDDHH:4:2}; DD=${YYYYMMDDHH:6:2}; HH=${YYYYMMDDHH:8:2}

#if [ $MM = "01" ]; then MMM="JAN"; fi; if [ $MM = "02" ]; then MMM="FEB"; fi
#if [ $MM = "03" ]; then MMM="MAR"; fi; if [ $MM = "04" ]; then MMM="APR"; fi
#if [ $MM = "05" ]; then MMM="MAY"; fi; if [ $MM = "06" ]; then MMM="JUN"; fi
#if [ $MM = "07" ]; then MMM="JUL"; fi; if [ $MM = "08" ]; then MMM="AUG"; fi
#if [ $MM = "09" ]; then MMM="SEP"; fi; if [ $MM = "10" ]; then MMM="OCT"; fi
#if [ $MM = "11" ]; then MMM="NOV"; fi; if [ $MM = "12" ]; then MMM="DEC"; fi

#TIME=${HH}Z${DD}${MMM}${YYYY}

Y=2000 #1988 #2022
CTL=OUT_123_21/LHF/J-OFURO3_LHF_DAILY_123_21_${Y}.nc
if [ ! -f $CTL ];then echo NO SUCH FILE,$CTL;exit 1;fi

GS=$(basename $0 .sh).GS
FIG=$(basename $CTL .nc).PDF

# LONW= ;LONE= ; LATS= ;LATN=
# LEV=

# LEVS="-3 3 1"
# LEVS=" -levs -3 -2 -1 0 1 2 3"
# KIND='midnightblue->deepskyblue->lightcyan->white->orange->red->crimson'
# FS=2
# UNIT=

HOST=$(hostname);CWD=$(pwd);NOW=$(date -R);CMD="$0 $@"
TEXT="J-OFURO3_V1.2PRE LHF"

cat << EOF > ${GS}

'sdfopen ${CTL}'

xmax = 1; ymax = 1

ytop=9

xwid = 5.0/xmax; ywid = 5.0/ymax

xmargin=0.5; ymargin=0.1

nmap = 1
ymap = 1
#while (ymap <= ymax)
xmap = 1
#while (xmap <= xmax)

xs = 1.5 + (xwid+xmargin)*(xmap-1); xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1); ys = ye - ywid

# SET PAGE
'set vpage 0.0 8.5 0.0 11'
'set parea 'xs ' 'xe' 'ys' 'ye

'cc'

# SET COLOR BAR
# 'color ${LEVS} -kind ${KIND} -gxout shaded'

# 'set lon ${LONW} ${LONE}'; 'set lat ${LATS} ${LATN}'
# 'set lev ${LEV}'
# 'set time ${TIME}'
'set x 1';'set y 1'
'set t 1 365'
'set cmark 0'
'd LHF'


# GET COORDINATES OF 4 CORNERS
'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

# LEGEND COLOR BAR
#x1=xl; x2=xr; y1=yb-0.5; y2=y1+0.1
#'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.1 -fh 0.13 -fs $FS -ft 3 -line on -edge circle'
#x=xr; y=y1
#'set strsiz 0.12 0.15'; 'set string 1 r 3 0'
#'draw string 'x' 'y' ${UNIT}'


# TEXT
x=xl ;# (xl+xr)/2; y=yt+0.2
'set strsiz 0.12 0.15'; 'set string 1 c 3 0'
'draw string 'x' 'y' ${TEXT}'

# HEADER
'set strsiz 0.12 0.14'; 'set string 1 l 3 0'
xx = 0.2; yy=yt+0.5
'draw string ' xx ' ' yy ' ${GS}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${CMD}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${NOW}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${HOST}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${CWD}'

'gxprint ${FIG}'
'quit'
EOF

grads -bcp "$GS"
# rm -vf $GS

echo
if [ -f $FIG ]; then
echo "OUTPUT : "
ls -lh --time-style=long-iso $FIG
fi
echo

echo "DONE $(basename $0)."
echo
