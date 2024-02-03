#!/bin/bash

# Sat, 11 Nov 2023 15:50:49 +0900
# /work01/DATA/IMERG/LATE

YMDHM=$1
YMDHM=${YMDHM:-202206191900}
YYYY=${YMDHM:0:4}; MM=${YMDHM:4:2}; DD=${YMDHM:6:2}; 
HH=${YMDHM:8:2}; MI=${YMDHM:10:2}

if [ $MM = "01" ]; then MMM="JAN"; fi; if [ $MM = "02" ]; then MMM="FEB"; fi
if [ $MM = "03" ]; then MMM="MAR"; fi; if [ $MM = "04" ]; then MMM="APR"; fi
if [ $MM = "05" ]; then MMM="MAY"; fi; if [ $MM = "06" ]; then MMM="JUN"; fi
if [ $MM = "07" ]; then MMM="JUL"; fi; if [ $MM = "08" ]; then MMM="AUG"; fi
if [ $MM = "09" ]; then MMM="SEP"; fi; if [ $MM = "10" ]; then MMM="OCT"; fi
if [ $MM = "11" ]; then MMM="NOV"; fi; if [ $MM = "12" ]; then MMM="DEC"; fi

TIME=${HH}:${MI}Z${DD}${MMM}${YYYY}

# https://gpm1.gesdisc.eosdis.nasa.gov/opendap/hyrax/GPM_L3/GPM_3IMERGHHL.06/2022/170/3B-HHR-L.MS.MRG.3IMERG.20220619-S000000-E002959.0000.V06C.HDF5.html

INFLE=$(ls 3B-HHR-L.MS.MRG.3IMERG.${YYYY}${MM}${DD}-S${HH}${MI}00-E*.V06C.HDF5.nc4)
IN=$INFLE
if [ ! -f $IN ];then echo ERROR NO SUCH FILE: $IN;exit 1;fi

TEXT="IMERG 30m ${HH}:${MI}UTC${DD}${MMM}${YYYY}"

GS=$(basename $0 .sh).GS
FIG=$(basename $0 .sh)_IMERG_30m_${YYYY}${MM}${DD}_${HH}${MI}.PDF

LONW=122 ;LONE=130 ; LATS=25 ;LATN=31.5

LEVS=" -levs 1 5 10 15 20 25 30 35 40 45 50"
KIND='(255,255,255)->(245,245,245)->(175,237,237)->(152,251,152)->(67,205,128)->(59,179,113)->(250,250,210)->(255,255,0)->(255,164,0)->(255,0,0)->(205,55,0)->(199,20,133)->(237,130,237)->(255,0,255)'
#KIND='white->antiquewhite->mistyrose->lightpink->mediumvioletred->navy->darkblue->blue->dodgerblue->aqua'
FS=1
UNIT=mm/h

HOST=$(hostname);CWD=$(pwd);NOW=$(date -R);CMD="$0 $@"

cat << EOF > ${GS}

'sdfopen ${IN}'

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
'set grid off';'set grads off'

# SET COLOR BAR
'color ${LEVS} -kind ${KIND} -gxout shaded'

'set lon ${LONW} ${LONE}'; 'set lat ${LATS} ${LATN}'
# 'set lev ${LEV}'
# 'set time ${TIME}'

'set mpdset hires'

'set xlint 2';'set ylint 1'
'd precipitationca'

# GET COORDINATES OF 4 CORNERS
'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

# LEGEND COLOR BAR
x1=xl; x2=xr-0.7; y1=yb-0.5; y2=y1+0.1
'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.1 -fh 0.13 -fs $FS -ft 3 -line on -edge circle'
x=xr; y=y1
'set strsiz 0.12 0.15'; 'set string 1 r 3 0'
'draw string 'x' 'y' ${UNIT}'


# TEXT
x=(xl+xr)/2; y=yt+0.2
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
rm -vf $GS

echo
if [ -f $FIG ]; then
echo "OUTPUT : "
ls -lh --time-style=long-iso $FIG
fi
echo

echo "DONE $(basename $0)."
echo
