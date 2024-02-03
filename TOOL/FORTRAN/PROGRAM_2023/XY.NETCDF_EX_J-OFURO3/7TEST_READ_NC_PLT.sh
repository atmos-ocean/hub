#!/bin/bash

# Mon, 08 Jan 2024 13:03:26 +0900
# /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/22.12.DECOMP_GLOBE/42.12.FRACTIONAL_CONTRIBUTION

YYYYMMDDHH=1988081000
YYYY=${YYYYMMDDHH:0:4}; MM=${YYYYMMDDHH:4:2}; DD=${YYYYMMDDHH:6:2}; HH=${YYYYMMDDHH:8:2}

if [ $MM = "01" ]; then MMM="JAN"; fi; if [ $MM = "02" ]; then MMM="FEB"; fi
if [ $MM = "03" ]; then MMM="MAR"; fi; if [ $MM = "04" ]; then MMM="APR"; fi
if [ $MM = "05" ]; then MMM="MAY"; fi; if [ $MM = "06" ]; then MMM="JUN"; fi
if [ $MM = "07" ]; then MMM="JUL"; fi; if [ $MM = "08" ]; then MMM="AUG"; fi
if [ $MM = "09" ]; then MMM="SEP"; fi; if [ $MM = "10" ]; then MMM="OCT"; fi
if [ $MM = "11" ]; then MMM="NOV"; fi; if [ $MM = "12" ]; then MMM="DEC"; fi

TIME=${HH}Z${DD}${MMM}${YYYY}
TEXT="J-OFURO3 V1.2_PRE LHF $DD $MMM $YYYY"

#CTL=$(basename $0 .sh).CTL
#if [ ! -f $CTL ];then echo NO SUCH FILE,$CTL;exit 1;fi

INDIR=.
INFLE1=7TEST_READ_NC_LHF_${YYYY}.nc
IN1=$(pwd)/${INDIR}/${INFLE1}
if [ ! -f $IN1 ];then echo NO SUCH FILE,$IN1;exit 1;fi

GS=$(basename $0 .sh).GS
FIG=$(basename $0 .sh)_${YYYYMMDDHH}.PDF

# LONW= ;LONE= ; LATS= ;LATN=
# LEV=

LEVS="-80 80 10"
# LEVS=" -levs -3 -2 -1 0 1 2 3"
KIND='midnightblue->blue->deepskyblue->lightcyan->yellow->orange->red->crimson'
FS=2
UNIT=W/m2

HOST=$(hostname);CWD=$(pwd);NOW=$(date -R);CMD="$0 $@"

cat << EOF > ${GS}

'sdfopen ${IN1}'

xmax = 2; ymax = 3;nmax=5


'set vpage 0.0 8.5 0.0 11'

ytop=9
xwid = 5.0/xmax; ywid = 6.0/ymax
xmargin=0.5; ymargin=0.5

'cc'

nmap = 1;ymap = 1
while (ymap <= ymax)
xmap = 1
while (xmap <= xmax)

xs = 1 + (xwid+xmargin)*(xmap-1); xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1); ys = ye - ywid

# SET PAGE
'set parea 'xs ' 'xe' 'ys' 'ye

# SET COLOR BAR
'color ${LEVS} -kind ${KIND} -gxout shaded'

# 'set lon ${LONW} ${LONE}'; 'set lat ${LATS} ${LATN}'
# 'set lev ${LEV}'
'set time ${TIME}'
'set xlopts 1 3 0.07';'set ylopts 1 3 0.07';

if(nmap=1);'d LHFRAW';endif
if(nmap=2);'d LHFCLM';endif
if(nmap=3);'d LHFSST';endif
if(nmap=4);'d LHFWND';endif
if(nmap=5);'d LHFQA';endif

# GET COORDINATES OF 4 CORNERS
'q gxinfo'
line3=sublin(result,3);xl=subwrd(line3,4); xr=subwrd(line3,6);
line4=sublin(result,4);yb=subwrd(line4,4);yt=subwrd(line4,6)

if(nmap=1);title='RAW';endif
if(nmap=2);title='CLM';endif
if(nmap=3);title='SST';endif
if(nmap=4);title='WND';endif
if(nmap=5);title='QA';endif

x=(xl+xr)/2; y=yt+0.2
'set strsiz 0.12 0.15'; 'set string 1 c 3 0'
'draw string 'x' 'y' 'title

xmap=xmap+1; nmap=nmap+1
if(nmap >= nmax);break;endif
endwhile ;#x
ymap=ymap+1
endwhile ;#y

# LEGEND COLOR BAR
x1=1; x2=7; y1=yb-1; y2=y1+0.1
'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.1 -fh 0.13 -fs $FS -ft 3 -line on -edge circle'
x=x2+0.5; y=y1
'set strsiz 0.12 0.15'; 'set string 1 l 3 0'
'draw string 'x' 'y' ${UNIT}'


# TEXT
x=4; y=10
'set strsiz 0.12 0.15'; 'set string 1 c 3 0'
'draw string 'x' 'y' ${TEXT}'

# HEADER
'set strsiz 0.1 0.12'; 'set string 1 l 3 0'
xx = 0.1; yy=10
'draw string ' xx ' ' yy ' ${INFLE}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${INDIR}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${CWD}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${CMD}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${NOW}'

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
