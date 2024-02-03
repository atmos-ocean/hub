# GrADS_2種類の量の鉛直プロファイル

/work03/am/2022.SNOW_KOIKE/22.12.VPR2
2022-11-12_17-35

```
$ VPR2.sh 2014 02 14 18
```



VPR2.sh

```bash
#!/bin/bash

YYYY=$1; MM=$2; DD=$3; HH=$4

STN=KOFU 
LON=$(echo "scale=4;130+34/60+6/3600"|bc)
LAT=$(echo "scale=4;35+39/60+44/3600"|bc)

VAR1=QV; VAR2=RH
INDIR=/work01/DATA/MSM/MSM-P/${YYYY}
INFLE=${MM}${DD}.nc
IN=${INDIR}/${INFLE}

if [ ! -f $IN ];then echo ERROR in $0: NO SUCH FILE, IN; echo; exit 1;fi
if [ $MM == "01" ]; then MMM="JAN"; fi; if [ $MM == "02" ]; then MMM="FEB"; fi
if [ $MM == "03" ]; then MMM="MAR"; fi; if [ $MM == "04" ]; then MMM="APR"; fi
if [ $MM == "05" ]; then MMM="MAY"; fi; if [ $MM == "06" ]; then MMM="JUN"; fi
if [ $MM == "07" ]; then MMM="JUL"; fi; if [ $MM == "08" ]; then MMM="AUG"; fi
if [ $MM == "09" ]; then MMM="SEP"; fi; if [ $MM == "10" ]; then MMM="OCT"; fi
if [ $MM == "11" ]; then MMM="NOV"; fi; if [ $MM == "12" ]; then MMM="DEC"; fi

TIME=${HH}Z${DD}${MMM}${YYYY}

CTL=$(basename $0 .sh).CTL
GS=$(basename $0 .sh).GS

FIG=${STN}_${YYYY}-${MM}-${DD}_${HH}.pdf #.eps

LEV="1000 500"
TIME=${HH}Z${DD}${MMM}${YYYY}


HOST=$(hostname);CWD=$(pwd);NOW=$(date -R);CMD="$0 $@"

cat << EOF > ${GS}

# Sat, 12 Nov 2022 16:53:17 +0900

# p5820.bio.mie-u.ac.jp

# /work03/am/2022.SNOW_KOIKE/22.12.VPR2

'sdfopen ${IN}'

xmax = 1; ymax = 1

ytop=8.8

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

'set lon ${LON}'; 'set lat ${LAT}'
'set lev ${LEV}'
'set time ${TIME}'
'set grid off';'set grads off'

say 'MMMMM QV (WATER VAPOR MIXING RATIO)'
'tc=(temp-273.16)'
'td=tc-( (14.55+0.114*tc)*(1-0.01*rh) + pow((2.5+0.007*tc)*(1-0.01*rh),3) + (15.9+0.117*tc)*pow((1-0.01*rh),14) )'
'vapr= 6.112*exp((17.67*td)/(td+243.5))'
'e= vapr*1.001+(lev-100)/900*0.0034'
'QV = 0.62197*(e/(lev-e))'

say 'MMMMM RH'
'set vrange2 0 110'
'set xlpos 0 top'
'set xlint 20'
'set ccolor 2'
'd rh'

'trackplot 100 1000 100 500 -c 2 -l 3 -t 1'

'set ylab off'

'set vrange 0 5'
'set xlint 1'
'set xlpos 0 bottom'
'set ccolor 3'
'd QV*1000'


# GET COORDINATES OF 4 CORNERS

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)

# X-AXIS LABEL

x=(xl+xr)/2; y=yb-0.5
'set strsiz 0.12 0.15'; 'set string 1 c 3 0'
'draw string 'x' 'y' Qv [g/kg]'

# X-AXIS LABEL

x=(xl+xr)/2; y=yt+0.4
'set strsiz 0.12 0.15'; 'set string 1 c 3 0'
'draw string 'x' 'y' RH [%]'

# Y-AXIS LABEL

x=xl-0.7; y=(yb+yt)/2
'set strsiz 0.12 0.15'; 'set string 1 c 3 90'
'draw string 'x' 'y' P [hPa]'

# TITLE

x=(xl+xr)/2; y=yt+0.7
'set strsiz 0.12 0.15'; 'set string 1 c 3 0'
'draw string 'x' 'y' ${STN}(${LAT}N,${LON}E) ${HH}UTC${DD}${MMM}${YYYY}'

# HEADER

'set strsiz 0.12 0.14'; 'set string 1 l 3 0'
xx = 0.2; yy=yt+1.2
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

echo "DONE $0."
echo
```

