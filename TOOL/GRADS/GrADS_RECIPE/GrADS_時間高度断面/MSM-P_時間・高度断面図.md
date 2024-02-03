MSM-P時間高度断面
=================

### スクリプト

**TIME-HEIGHT.sh**

```bash
#!/bin/bash

YYYY=2014; MM=02;MMM=FEB; DD=13
INDIR=/work01/DATA/MSM/MSM-P/$YYYY
INFLE=${MM}${DD}.nc
GS=$(basename $0 .sh).GS

VAROUT=T; VAR=temp
STN=KOFU
LATD=35; LATM=39; LATS=39
LOND=138; LONM=34; LONS=42
LAT=$(echo "scale=5;$LATD+$LATM/60.0+$LATS/3600. " | bc)
LON=$(echo "scale=5;$LOND+$LONM/60.0+$LONS/3600. " | bc)
LEV="1000 700"
FIG=${STN}_${VAROUT}_${YYYY}${MM}${DD}_${LON}_${LAT}_2.eps
#FIG=${STN}_${VAROUT}_${YYYY}${MM}${DD}_${LON}_${LAT}.eps
TIME="15Z${DD}${MMM}${YYYY} 06Z14${MMM}${YYYY}"
echo $FIG

LEVS="-12 6 1 "
#LEVS=" -levs -3 -2 -1 0 1 2 3"
KIND='midnightblue->deepskyblue->lightcyan->white->orange->red->crimson'
FS=2
#UNIT=

HOST=$(hostname); CWD=$(pwd); NOW=$(date -R); CMD="$0 $@"

cat << EOF > ${GS}

# Fri, 21 Oct 2022 15:54:00 +0900
# p5820.bio.mie-u.ac.jp
# /work03/am/2022.SNOW_KOIKE/12.12.TIME-HEIGHT

'open MSM-P.CTL'
#'sdfopen ${INDIR}/${INFLE}'

xmax = 1
ymax = 1

ytop=9

xwid = 5.0/xmax
ywid = 5.0/ymax

xmargin=0.5
ymargin=0.1

nmap = 1
ymap = 1
#while (ymap <= ymax)
xmap = 1
#while (xmap <= xmax)

xs = 1.5 + (xwid+xmargin)*(xmap-1)
xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1)
ys = ye - ywid

# SET PAGE

'set vpage 0.0 8.5 0.0 11'
'set parea 'xs ' 'xe' 'ys' 'ye

'cc'
'set grid off';'set grads off'

# SET COLOR BAR

'color ${LEVS} -kind ${KIND} -gxout shaded'

'set lon ${LON}'
'set lat ${LAT}'
'set lev ${LEV}'
'set time ${TIME}'

 'd ${VAR}-273.15 '


# GET COORDINATES OF 4 CORNERS

'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

# LEGEND COLOR BAR

x1=xl; x2=xr
y1=yb-0.8; y2=y1+0.1
'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.1 -fh 0.13 -fs $FS -ft 3 -line on -edge circle'
x=xl
y=yt+0.2
'set strsiz 0.15 0.18'
'set string 1 l 3 0'
'draw string 'x' 'y' $STN ${VAROUT} $YYYY $MM $DD'


# TEXT

#x=xl ;# (xl+xr)/2; 
#y=yt+0.2
#'set strsiz 0.12 0.15'
#'set string 1 c 3 0'
#'draw string 'x' 'y' ${TEXT}'

# HEADER

'set strsiz 0.12 0.14'
'set string 1 l 3 0'
xx = 0.2
yy=yt+0.5
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




### コントロールファイル

**MSM-P.CTL**

```
dset /work01/DATA/MSM/MSM-P/%y4/%m2%d2.nc
title  MSM-P
undef 9.96921e+36
UNPACK scale_factor add_offset
dtype netcdf
options template yrev
xdef 241 linear 120 0.125
ydef 253 levels 22.4 22.5 22.6 22.7 22.8 22.9 23 23.1
 23.2 23.3 23.4 23.5 23.6 23.7 23.8 23.9 24 24.1
 24.2 24.3 24.4 24.5 24.6 24.7 24.8 24.9 25 25.1
 25.2 25.3 25.4 25.5 25.6 25.7 25.8 25.9 26 26.1
 26.2 26.3 26.4 26.5 26.6 26.7 26.8 26.9 27 27.1
 27.2 27.3 27.4 27.5 27.6 27.7 27.8 27.9 28 28.1
 28.2 28.3 28.4 28.5 28.6 28.7 28.8 28.9 29 29.1
 29.2 29.3 29.4 29.5 29.6 29.7 29.8 29.9 30 30.1
 30.2 30.3 30.4 30.5 30.6 30.7 30.8 30.9 31 31.1
 31.2 31.3 31.4 31.5 31.6 31.7 31.8 31.9 32 32.1
 32.2 32.3 32.4 32.5 32.6 32.7 32.8 32.9 33 33.1
 33.2 33.3 33.4 33.5 33.6 33.7 33.8 33.9 34 34.1
 34.2 34.3 34.4 34.5 34.6 34.7 34.8 34.9 35 35.1
 35.2 35.3 35.4 35.5 35.6 35.7 35.8 35.9 36 36.1
 36.2 36.3 36.4 36.5 36.6 36.7 36.8 36.9 37 37.1
 37.2 37.3 37.4 37.5 37.6 37.7 37.8 37.9 38 38.1
 38.2 38.3 38.4 38.5 38.6 38.7 38.8 38.9 39 39.1
 39.2 39.3 39.4 39.5 39.6 39.7 39.8 39.9 40 40.1
 40.2 40.3 40.4 40.5 40.6 40.7 40.8 40.9 41 41.1
 41.2 41.3 41.4 41.5 41.6 41.7 41.8 41.9 42 42.1
 42.2 42.3 42.4 42.5 42.6 42.7 42.8 42.9 43 43.1
 43.2 43.3 43.4 43.5 43.6 43.7 43.8 43.9 44 44.1
 44.2 44.3 44.4 44.5 44.6 44.7 44.8 44.9 45 45.1
 45.2 45.3 45.4 45.5 45.6 45.7 45.8 45.9 46 46.1
 46.2 46.3 46.4 46.5 46.6 46.7 46.8 46.9 47 47.1
 47.2 47.3 47.4 47.5 47.6
zdef 16 levels 1000 975 950 925 900 850 800 700
 600 500 400 300 250 200 150 100
tdef 8 linear 00Z13FEB2014 180mn
vars 6
z=>z  16  t,z,y,x  geopotential height
w=>w  16  t,z,y,x  vertical velocity in p
u=>u  16  t,z,y,x  eastward component of wind
v=>v  16  t,z,y,x  northward component of wind
temp=>temp  16  t,z,y,x  temperature
rh=>rh  16  t,z,y,x  relative humidity
endvars
```