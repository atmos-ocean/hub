#!/bin/bash

#MMMMMMMMMMMMMM INCLUDE ###################
. ./06.LFM.MAKE.CTL.sh
#MMMMMMMMMMMMMM INCLUDE ###################

FH=$1; YMDH=$2;
LINE1=$3;    LON1_1=$4;     LAT1_1=$5;    LON1_2=$6;    LAT1_2=$7
LINE2=$8;    LON2_1=$9;     LAT2_2=${10}; LON2_2=${11}; LAT2_2=${12}
LINE3=${13}; LON3_1=${14}; LAT3_2=${15}; LON3_2=${16}; LAT3_2=${17}
LINE4=${18}; LON4_1=${19}; LAT4_2=${20}; LON4_2=${21}; LAT4_2=${22}

YMDH=${YMDH:-2022061900}
YYYY=${YMDH:0:4}; MM=${YMDH:4:2}; DD=${YMDH:6:2}; HH=${YMDH:8:2}

echo MMMMMMM DATE ${YYYY} ${MM} ${DD} ${HH}

if [ $MM = "01" ]; then MMM="JAN"; fi; if [ $MM = "02" ]; then MMM="FEB"; fi
if [ $MM = "03" ]; then MMM="MAR"; fi; if [ $MM = "04" ]; then MMM="APR"; fi
if [ $MM = "05" ]; then MMM="MAY"; fi; if [ $MM = "06" ]; then MMM="JUN"; fi
if [ $MM = "07" ]; then MMM="JUL"; fi; if [ $MM = "08" ]; then MMM="AUG"; fi
if [ $MM = "09" ]; then MMM="SEP"; fi; if [ $MM = "10" ]; then MMM="OCT"; fi
if [ $MM = "11" ]; then MMM="NOV"; fi; if [ $MM = "12" ]; then MMM="DEC"; fi

FH=${FH:-00}

MODEL=LFM

LONW=122.0 ; LONE=134; LATS=24.5; LATN=35 #MAP AREA

echo MMMMMMM TRANSECT
LINE1=${LINE1:-A}
LON1_1=${LON1_1:-124}; LAT1_1=${LAT1_1:-26.7}
LON1_2=${LON1_2:-130}; LAT2B=${LAT1_2:-31.5}
echo $LON1_1 $LAT1_1 $LON1_2 $LAT1_2

LINE2=${LINE2:-B}
LON2_1=${LON2_1:-124.72}; LAT2_1=${LAT2_1:-25.8}
LON2_2=${LON2_2:-130.72}; LAT2_2=${LAT2_2:-30.6}
echo $LON2_1 $LAT2_1 $LON2_2 $LAT2_2

LINE3=${LINE3:-C}
LON3_1=${LON3_1:-124.72}; LAT3_1=${LAT3_1:-25.8}
LON3_2=${LON3_2:-130.72}; LAT3_2=${LAT3_2:-30.6}
echo $LON3_1 $LAT3_1 $LON3_2 $LAT3_2

LINE4=${LINE4:-C}
LON4_1=${LON4_1:-124.72}; LAT4_1=${LAT4_1:-25.8}
LON4_2=${LON4_2:-130.72}; LAT4_2=${LAT4_2:-30.6}
echo $LON4_1 $LAT4_1 $LON4_2 $LAT4_2

DLON=0.02; DLAT=$DLON

XM1=$(echo "scale=0; (${LON1_2}-${LON1_1})/${DLON}" | bc)
YM1=$(echo "scale=0; (${LAT1_2}-${LAT1_1})/${DLAT}" | bc)
echo ; echo MMMMMMMM XM1 = $XM1; echo MMMMMMMM YM1 = $YM1; echo

XM2=$(echo "scale=0; (${LON2_2}-${LON2_1})/${DLON}" | bc)
YM2=$(echo "scale=0; (${LAT2_2}-${LAT2_1})/${DLAT}" | bc)
echo ; echo MMMMMMMM XM2 = $XM2; echo MMMMMMMM YM2 = $YM2; echo

XM3=$(echo "scale=0; (${LON3_2}-${LON3_1})/${DLON}" | bc)
YM3=$(echo "scale=0; (${LAT3_2}-${LAT3_1})/${DLAT}" | bc)
echo ; echo MMMMMMMM XM3 = $XM3; echo MMMMMMMM YM3 = $YM3; echo

XM4=$(echo "scale=0; (${LON4_2}-${LON4_1})/${DLON}" | bc)
YM4=$(echo "scale=0; (${LAT4_2}-${LAT4_1})/${DLAT}" | bc)
echo ; echo MMMMMMMM XM4 = $XM4; echo MMMMMMMM YM4 = $YM4; echo

MMDD=${MM}${DD}

DATE=${HH}UTC${DD}${MMM}${YYYY}

VAR1='mag(wvx.1,wvy.1)'; CMIN1=0.05; CMAX1=0.4; CINT1=0.05; CLEV1=0.1

VAR2=VPT.1; V2LEV=1000; 
#CINT2M=1;CINT2C=1;CLEV2C="300 301 302 303 304 305 306 308 310 312 314 316 318"; CLEV2M=304
CINT2M=1;CINT2C=2;CLEV2C="300 304 308 312 316 320 324 328 332"; CLEV2M=304

VAR3=MAUL.1; V3LEV=850; 
RGB='set rgb 98 128 128 128 -75'
VAR4=RH; CLEV4="95"
XTITLE_CSC="Latitude North" ;#"Longitude East"
YTITLE_CSC="Pressure [hPa]" ;#"Longitude East"

VECUNIT="kg/m\`a2\`n/s"

LEVCSC1=1000; LEVCSC2=500; CLEVCSC=1

LEVCSC3=1000; LEVCSC4=900; CLEVCSC=1


INDIR=/work01/DATA/LFM_PROC_ROT/FH${FH}
#INFLE=LFM_PRS_FH${FH}_PROC_V_${YYYY}-${MM}-${DD}_${HH}.nc
INFLE=LFM_PRS_FH${FH}_VALID_${YYYY}-${MM}-${DD}_${HH}_THERMO.nc
IN=${INDIR}/${INFLE}

if [ ! -f $IN ]; then echo ERROR: NO SUCH FILE, $IN; exit 1; fi

FIGDIR=FH${FH}/$LINE; mkd $FIGDIR
FIG=${FIGDIR}/${MODEL}_FH${FH}_${MMDD}${HH}_CSC4_ROT.WVF_${LINE1}_${LINE2}_${LINE3}_${LINE4}.pdf

SCLV=40
#KIND='magenta->red->gold->papayawhip->white->azure->palegreen->blue->cyan'
KIND='white->lightyellow->wheat->gold->orange->tomato->red->firebrick'


HOST=$(hostname); CWD=$(pwd); TIMESTAMP=$(date -R); CMD="$0 $@"
GS=$(basename $0 .sh).GS

echo MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
echo CREATE CTL FILE

CTL1=LFM.CTL; echo $CTL1

INDIR1=/work01/DATA/LFM_PROC_ROT_v2/FH${FH}/
INFLE_CHK=LFM_PRS_FH${FH}_VALID_${YYYY}-${MM}-${DD}_${HH}_THERMO.nc
INFLE1=LFM_PRS_FH${FH}_VALID_%y4-%m2-%d2_%h2_THERMO.nc
DSET1=$INDIR1/${INFLE1}
DSET_CHK=$INDIR1/${INFLE_CHK}
if [ ! -f $DSET_CHK ]; then echo NO SUCH FILE, $DSET_CHK; echo; exit 1; fi

LFM_MAKE_CTL $CTL1 $DSET1
echo MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

echo NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
echo CREATE CTL FILE

CTLR1=LFM_ROT_1.CTL; echo $CTLR1

INDIR=/work01/DATA/LFM_PROC_ROT_v2/FH${FH}
INFLE_CHK=LFM_PRS_FH${FH}_VALID_${YYYY}-${MM}-${DD}_${HH}_ROT_UV_${LON1_1}-${LAT1_1}-${LON1_2}-${LAT1_2}.nc
INFLE=LFM_PRS_FH${FH}_VALID_%y4-%m2-%d2_%h2_ROT_UV_${LON1_1}-${LAT1_1}-${LON1_2}-${LAT1_2}.nc

if [ ! -f $INDIR/${INFLE_CHK} ]; then echo NO SUCH FILE, $INDIR/${INFLE_CHK}; echo; exit 1; fi

DSET=$INDIR/${INFLE}

LFM_CTL_ROTATED_COORD $CTLR1 $DSET
echo NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

echo OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
echo CREATE CTL FILE

CTLR2=LFM_ROT_2.CTL; echo $CTLR2

INDIR=/work01/DATA/LFM_PROC_ROT_v2/FH${FH}
INFLE_CHK=LFM_PRS_FH${FH}_VALID_${YYYY}-${MM}-${DD}_${HH}_ROT_UV_${LON2_1}-${LAT2_1}-${LON2_2}-${LAT2_2}.nc
INFLE=LFM_PRS_FH${FH}_VALID_%y4-%m2-%d2_%h2_ROT_UV_${LON2_1}-${LAT2_1}-${LON2_2}-${LAT2_2}.nc

if [ ! -f $INDIR/${INFLE_CHK} ]; then echo NO SUCH FILE, $INDIR/${INFLE_CHK}; echo; exit 1; fi

DSET=$INDIR/${INFLE}

LFM_CTL_ROTATED_COORD $CTLR2 $DSET
echo OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

echo PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
echo CREATE CTL FILE

CTLR3=LFM_ROT_3.CTL; echo $CTLR3

INDIR=/work01/DATA/LFM_PROC_ROT_v2/FH${FH}
INFLE_CHK=LFM_PRS_FH${FH}_VALID_${YYYY}-${MM}-${DD}_${HH}_ROT_UV_${LON3_1}-${LAT3_1}-${LON3_2}-${LAT3_2}.nc
INFLE=LFM_PRS_FH${FH}_VALID_%y4-%m2-%d2_%h2_ROT_UV_${LON3_1}-${LAT3_1}-${LON3_2}-${LAT3_2}.nc

if [ ! -f $INDIR/${INFLE_CHK} ]; then echo NO SUCH FILE, $INDIR/${INFLE_CHK}; echo; exit 1; fi

DSET=$INDIR/${INFLE}

LFM_CTL_ROTATED_COORD $CTLR3 $DSET
echo PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP

echo QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
echo CREATE CTL FILE

CTLR4=LFM_ROT_4.CTL; echo $CTLR4

INDIR=/work01/DATA/LFM_PROC_ROT_v2/FH${FH}
INFLE_CHK=LFM_PRS_FH${FH}_VALID_${YYYY}-${MM}-${DD}_${HH}_ROT_UV_${LON4_1}-${LAT4_1}-${LON4_2}-${LAT4_2}.nc
INFLE=LFM_PRS_FH${FH}_VALID_%y4-%m2-%d2_%h2_ROT_UV_${LON4_1}-${LAT4_1}-${LON4_2}-${LAT4_2}.nc

if [ ! -f $INDIR/${INFLE_CHK} ]; then echo NO SUCH FILE, $INDIR/${INFLE_CHK}; echo; exit 1; fi

DSET=$INDIR/${INFLE}

LFM_CTL_ROTATED_COORD $CTLR4 $DSET
echo QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ


GS=$(basename $0 .sh).GS

cat <<EOF>$GS

'open $CTL1' ;#'q ctlinfo'; say result
'open $CTLR1'; 'open $CTLR2'; 'open $CTLR3'; 'open $CTLR4'

'cc'; 'set grid off'



say 'MMMMM SET FIGURE SIZE'
'set vpage 0.0 11.5 0 8.5' ;#0.0 8.5 0 10.5'

xmax=2; ymax=2; xleft=0.2; ytop=7

xwid =  6/xmax; ywid =  5/ymax
xmargin=0.4; xmargin2=0.6; ymargin=1



say; say; say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM 1 MAP MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'

ymap=1; xmap=1

xs = xleft + (xwid+xmargin)*(xmap-1); xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1) ; ys = ye - ywid

'set parea 'xs ' 'xe' 'ys' 'ye
'set grads off'

'set lon $LONW $LONE'; 'set lat $LATS $LATN'
'set time ${HH}Z${DD}${MMM}${YYYY}'
'q dims'; say result; line=sublin(result,5); datetime=subwrd(line,6)
'set lev $V1LEV'
'q dims'; say sublin(result,4)


# 78 53 36 ;#DARK BROWN
'set mpdset hires'; 'set rgb 99 160 82 45'; 'set map 99 1 2'
'set xlopts 1 3 0.1'; 'set xlevs 122 125 128 131'
'set ylopts 1 3 0.1'; 'set ylint 2'


'set xlab on'; 'set xlevs 122 125 128 131'; 'set ylab on'

say 'CONTOUR $VAR2 $V2LEV'
'set lev $V2LEV'
'set gxout contour'
'set ccolor 0'; 'set cthick 5'
'set clab off'; 'set cint $CINT2M'
'q dims'; say sublin(result,4)
'd $VAR2'

'set xlab off';'set ylab off'

'set ccolor 1'; 'set cthick 1'
'set clab off'; 'set cint $CINT2M'
'd $VAR2'

'set ccolor 1'; 'set cthick 1'
'set clab on'; 'set clevs $CLEV2M'
'd $VAR2'


say 'MMMMM VECTOR $V2LEV'
'set gxout vector'; 'set cthick 1'
'set arrscl 0.5 $SCLV'; 'set arrlab off'
'set lev $V2LEV'
'q dims'; say sublin(result,4)

'set ccolor 0'; 'set cthick 6'
'vec skip(u,25,25);v -SCL 0.5 $SCLV -P 20 20'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
xx=xr-0.55; yy=yb-0.3

'set ccolor 1'; 'set cthick 3'
'vec skip(u,25,25);v -SCL 0.5 $SCLV -P 'xx' 'yy' -SL m/s'


say 'MMMMM LINE'
'trackplot $LON1_1 $LAT1_1 $LON1_2 $LAT1_2 -c 1 -l 1 -t 4'
'trackplot $LON2_1 $LAT2_1 $LON2_2 $LAT2_2 -c 1 -l 1 -t 4'
'trackplot $LON3_1 $LAT3_1 $LON3_2 $LAT3_2 -c 1 -l 1 -t 4'
'trackplot $LON4_1 $LAT4_1 $LON4_2 $LAT4_2 -c 1 -l 1 -t 4'


say 'MMMMM VECTOR $V3LEV'
'set gxout vector'
'set arrscl 0.5 $SCLV'; 'set arrlab off'
'set ccolor 0'; 'set cthick 6'
'set lev $V3LEV'
'q dims'; say sublin(result,4)

#'set ccolor 0'; 'set cthick 5'
#'vec skip(u,25,25);v -SCL 0.5 $SCLV -P 20 20'
#'set rgb 97 120 120 120'

'set ccolor 14'; 'set cthick 2'
'vec skip(u,25,25);v -SCL 0.5 $SCLV -P 20 20'


say 'MMMMM CONTOUR $VAR2 $CLEV2M'
'set clopts 1 2 0.07'
'set gxout contour'
'set ccolor 1'; 'set cthick 1'
'set clab on'; 'set clevs $CLEV2M'
'set lev $V2LEV'
'd $VAR2'

say 'MMMMM MAUL'
'set gxout contour'; 'set cthick 10'; 
'set ccolor 98'
#'set clevs 1'
'set lev $V3LEV'
'q dims'; say sublin(result,4)
'd ${VAR3}'

say 'NNNNN LINE NAME'
'q w2xy ${LON1_1} ${LAT1_1}'; xx=subwrd(result,3); yy=subwrd(result,6)
'set strsiz 0.1 0.12'; 
'set string 0 c 10'; 'draw string 'xx-0.05' 'yy-0.05' $LINE1'
'set string 1 c 3'; 'draw string 'xx-0.05' 'yy-0.05' $LINE1'

'q w2xy ${LON2_1} ${LAT2_1}'; xx=subwrd(result,3); yy=subwrd(result,6)
'set strsiz 0.1 0.12'; 'set string 1 c 3'
'set string 0 c 10'; 'draw string 'xx-0.05' 'yy-0.05' $LINE2'
'set string 1 c 3'; 'draw string 'xx-0.05' 'yy-0.05' $LINE2'

'q w2xy ${LON3_1} ${LAT3_1}'; xx=subwrd(result,3); yy=subwrd(result,6)
'set strsiz 0.1 0.12'; 'set string 1 c 3'
'set string 0 c 10'; 'draw string 'xx-0.05' 'yy-0.05' $LINE3'
'set string 1 c 3'; 'draw string 'xx-0.05' 'yy-0.05' $LINE3'

'q w2xy ${LON4_1} ${LAT4_1}'; xx=subwrd(result,3); yy=subwrd(result,6)
'set strsiz 0.1 0.12'; 'set string 1 c 3'
'set string 0 c 10'; 'draw string 'xx-0.05' 'yy-0.05' $LINE4'
'set string 1 c 3'; 'draw string 'xx-0.05' 'yy-0.05' $LINE4'


'set strsiz 0.1 0.12'; 'set string 1 c 3'
xx=(xl+xr)/2; yy=yt+0.14; 'draw string 'xx' 'yy' Vaild $DATE (FH${FH})'
'set string 1 l 3'
xx=xl-0.3; yy=yy+0.18; 'draw string 'xx' 'yy' ${VAR2}(${V2LEV}) ${VAR3}(${V3LEV})'
XSLEFT=xl; YTTOP=yt



say; say; say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM 2 CROSS SECTION MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'
'set xlab on'; 'set ylab on'

'set time ${HH}Z${DD}${MMM}${YYYY}'
'set grads off'

line = $LINE1
lon1 = $LON1_1; lon2 = $LON1_2; lat1 = $LAT1_1; lat2 = $LAT1_2
YM=${YM1}
say lon1' 'lon2' 'lat1' 'lat2

'set x 1'; 'set y 1'
'set lev $LEVCSC1 $LEVCSC2' ;# 'set zlog on'

'collect 2 free'; 'collect 3 free'; 'collect 4 free'
'collect 5 free'; 'collect 6 free'; 'collect 7 free'; 
'collect 8 free'; 'collect 9 free' 


lat = lat1
while (lat <= lat2)
  lon = lon1 + (lon2-lon1)*(lat-lat1) / (lat2-lat1)
  'collect 2 gr2stn($VAR2,'lon','lat')'
  'collect 3 gr2stn($VAR3,'lon','lat')'
  'collect 4 gr2stn($VAR4,'lon','lat')'
  'collect 5 gr2stn(wvxr.2,'lon','lat')'
  'collect 6 gr2stn(wvyr.2,'lon','lat')'
  'collect 7 gr2stn(dum0.2,'lon','lat')'
  lat = lat + $DLAT
endwhile
'set x 1 'YM
'set xaxis 'lat1' 'lat2

say 'MMMMM SET FIGURE SIZE'
ymap=1; xmap=2

xs = xleft + (xwid+xmargin)*(xmap-1); xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1) ; ys = ye - ywid

'set parea 'xs ' 'xe' 'ys' 'ye



'set xlint 1'; 'set xlevs 25 26 27 28 29 30 31 32 33 34 35 36'; 'set ylint 50'

say; say 'MMMM $VAR1 CROSS SECTION'
'set gxout shade2'
'color $CMIN1 $CMAX1 $CINT1 -kind $KIND'
'd mag(coll2gr(5,-u),coll2gr(6,-u))'

'set xlab off';'set ylab off'

'set gxout contour'
'set clab off'; 'set cint $CINT2C'; 'set ccolor 0'; 'set cthick 6'
'd coll2gr(2,-u)'

'set clab off'; 'set cint $CINT2C'; 'set ccolor 15'; 'set cthick 2'
'd coll2gr(2,-u)'

'set clab on'; 'set clevs $CLEV2C'; 'set ccolor 15'; 'set cthick 2'; 'set clopts 15 2 0.07'
'd coll2gr(2,-u)'

say; say 'MMMMM WVF CROSS SECTION ALONG COMP.'
'set gxout vector'
'set cthick 5'; 'set ccolor  0'
'vec.gs skip(coll2gr(5,-u),25,1);coll2gr(7,-u) -SCL 0.5 0.5 -P 20 20 -SL m/s'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=xr-0.7; yy=yb-0.27
'set cthick  2'; 'set ccolor  1';'set strsiz 0.08 0.1';'set string 1 c 2 0'
'vec.gs skip(coll2gr(5,-u),25,1);coll2gr(7,-u) -SCL 0.5 0.5 -P 'xx' 'yy' -SL ${VECUNIT}'

say; say 'MMMMM WVF CROSS SECTION ACROSS COMP.'
'set gxout contour'
'set clab off'; 'set cint 0.05'; 'set ccolor 0'; 'set cthick 4'
'd coll2gr(6,-u)'
'set clab on'; 'set cint 0.05'; 'set cthick 1'
'set ccolor 2'; 'set clopts 2 2 0.07'
'd coll2gr(6,-u)'


say; say 'MMMM MAUL CROSS SECTION'
'set gxout grfill'
'$RGB' ; 'set ccolor 98';'set cthick 10'
#'set cint $CINT2'
'd coll2gr(3,-u)'



say; say 'MMMM RH CROSS SECTION'
'set gxout contour'
'set clevs $CLEV4'; 'set ccolor 0';' set cthick 5';'set clopts 4 2 0.07'
'd coll2gr(4,-u)'
'set clevs $CLEV4'; 'set ccolor 4';' set cthick 2';'set clopts 4 2 0.07'
'd coll2gr(4,-u)'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.1 0.12'; 'set string 1 c 3 0'
xx=(xl+xr)/2; yy=yt+0.14; 'draw string 'xx' 'yy' Valid $DATE (FH${FH})'
xx=(xl+xr)/2; yy=yy+0.18; 'draw string 'xx' 'yy' $LINE1 (' lat1 'N,' lon1 'E)-(' lat2 ',' lon2 'E)'
xx=(xl+xr)/2; yy=yb-0.3; 'draw string 'xx' 'yy' $XTITLE_CSC'

'set strsiz 0.1 0.12'; 'set string 1 c 3 90'
xx=xl-0.5; yy=(yt+yb)/2; 'draw string 'xx' 'yy' $YTITLE_CSC'



say; say; say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM 3 CROSS SECTION MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'
'set xlab on'; 'set ylab on'

'set time ${HH}Z${DD}${MMM}${YYYY}'
'set grads off'

line = $LINE2
lon1 = $LON2_1; lon2 = $LON2_2; lat1 = $LAT2_1; lat2 = $LAT2_2
YM=${YM3}
say lon1' 'lon2' 'lat1' 'lat2

'set x 1'; 'set y 1'
'set lev $LEVCSC1 $LEVCSC2' ;# 'set zlog on'

'collect 2 free'; 'collect 3 free'; 'collect 4 free'
'collect 5 free'; 'collect 6 free'; 'collect 7 free'; 

lat = lat1
while (lat <= lat2)
  lon = lon1 + (lon2-lon1)*(lat-lat1) / (lat2-lat1)
  'collect 2 gr2stn($VAR2,'lon','lat')'
  'collect 3 gr2stn($VAR3,'lon','lat')'
  'collect 4 gr2stn($VAR4,'lon','lat')'
  'collect 5 gr2stn(wvxr.3,'lon','lat')'
  'collect 6 gr2stn(wvyr.3,'lon','lat')'
  'collect 7 gr2stn(dum0.3,'lon','lat')'
  lat = lat + $DLAT
endwhile
'set x 1 'YM
'set xaxis 'lat1' 'lat2

say 'MMMMM SET FIGURE SIZE'
ymap=1; xmap=3

xs = xleft + (xwid+xmargin2)*(xmap-1); xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1) ; ys = ye - ywid

'set parea 'xs ' 'xe' 'ys' 'ye

'set xlint 1'; 'set xlevs 25 26 27 28 29 30 31 32 33 34 35 36'; 'set ylint 50'

say; say 'MMMM $VAR1 CROSS SECTION'
'set gxout shade2'
'color $CMIN1 $CMAX1 $CINT1 -kind $KIND'
'd mag(coll2gr(5,-u),coll2gr(6,-u))'

'set xlab off';'set ylab off'

say; say 'MMMM $VAR2 CROSS SECTION'
'set gxout contour'

'set clab off'; 'set cint $CINT2C'; 'set ccolor 0'; 'set cthick 6'
'd coll2gr(2,-u)'

'set clab off'; 'set cint $CINT2C'; 'set ccolor 15'; 'set cthick 2'
'd coll2gr(2,-u)'

'set clab on'; 'set clevs $CLEV2C'; 'set ccolor 15'; 'set cthick 2'; 'set clopts 15 2 0.07'
'd coll2gr(2,-u)'



say; say 'MMMMM WVF CROSS SECTION ALONG COMP.'
'set gxout vector'
'set cthick 5'; 'set ccolor  0'
'vec.gs skip(coll2gr(5,-u),25,1);coll2gr(7,-u) -SCL 0.5 0.5 -P 20 20 -SL m/s'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=xr-0.7; yy=yb-0.27
'set cthick  2'; 'set ccolor  1';'set strsiz 0.08 0.1';'set string 1 c 2 0'
'vec.gs skip(coll2gr(5,-u),25,1);coll2gr(7,-u) -SCL 0.5 0.5 -P 'xx' 'yy' -SL ${VECUNIT}'

say; say 'MMMMM WVF CROSS SECTION ACROSS COMP.'
'set gxout contour'
'set clab off'; 'set cint 0.05'; 'set ccolor 0'; 'set cthick 4'
'd coll2gr(6,-u)'
'set clab on'; 'set cint 0.05'; 'set cthick 1'
'set ccolor 2'; 'set clopts 2 2 0.07'
'd coll2gr(6,-u)'

say; say 'MMMM MAUL CROSS SECTION'
'set gxout grfill'
'set ccolor 98';'set cthick 10'
#'set cint $CINT2'
'd coll2gr(3,-u)'



say; say 'MMMM RH CROSS SECTION'
'set gxout contour'
'set clevs $CLEV4'; 'set ccolor 0';' set cthick 5';'set clopts 4 2 0.07'
'd coll2gr(4,-u)'
'set clevs $CLEV4'; 'set ccolor 4';' set cthick 2';'set clopts 4 2 0.07'
'd coll2gr(4,-u)'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.1 0.12'; 'set string 1 c 3 0'
xx=(xl+xr)/2; yy=yt+0.14; 'draw string 'xx' 'yy' Valid $DATE (FH${FH})'
xx=(xl+xr)/2; yy=yy+0.18; 'draw string 'xx' 'yy' $LINE2 (' lat1 'N,' lon1 'E)-(' lat2 ',' lon2 'E)'
xx=(xl+xr)/2; yy=yb-0.3; 'draw string 'xx' 'yy' $XTITLE_CSC'

'set strsiz 0.1 0.12'; 'set string 1 c 3 90'
xx=xl-0.5; yy=(yt+yb)/2; 'draw string 'xx' 'yy' $YTITLE_CSC'



say 'mmmmm COLOR BAR'
x1=xr+0.1; x2=x1+0.1; y1=yb; y2=yt-0.3
'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.08 -fh 0.1 -fs 1 -ft 3 -line on -edge circle'
xx=x1-0.05; yy=y2+0.15; 'set string 1 l 2 0';'set strsiz 0.08 0.1';
'draw string ' xx ' ' yy ' ${VECUNIT}'



say; say; say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM 4 CROSS SECTION MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'
'set xlab on'; 'set ylab on'

'set time ${HH}Z${DD}${MMM}${YYYY}'
'set grads off'

line = $LINE3
lon1 = $LON3_1; lon2 = $LON3_2; lat1 = $LAT3_1; lat2 = $LAT3_2
YM=${YM4}
say lon1' 'lon2' 'lat1' 'lat2

'set x 1'; 'set y 1'
'set lev $LEVCSC3 $LEVCSC4' ;# 'set zlog on'

'collect 2 free'; 'collect 3 free'; 'collect 4 free'
'collect 5 free'; 'collect 6 free'; 'collect 7 free'; 

lat = lat1
while (lat <= lat2)
  lon = lon1 + (lon2-lon1)*(lat-lat1) / (lat2-lat1)
  'collect 2 gr2stn($VAR2,'lon','lat')'
  'collect 3 gr2stn($VAR3,'lon','lat')'
  'collect 4 gr2stn($VAR4,'lon','lat')'
  'collect 5 gr2stn(wvxr.4,'lon','lat')'
  'collect 6 gr2stn(wvyr.4,'lon','lat')'
  'collect 7 gr2stn(dum0.4,'lon','lat')'
  lat = lat + $DLAT
endwhile
'set x 1 'YM
'set xaxis 'lat1' 'lat2

say 'MMMMM SET FIGURE SIZE'
ymap=2; xmap=2

xs = xleft + (xwid+xmargin)*(xmap-1); xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1) ; ys = ye - ywid/2

'set parea 'xs ' 'xe' 'ys' 'ye



'set xlint 1'; 'set xlevs 25 26 27 28 29 30 31 32 33 34 35 36'; 'set ylint 50'

say; say 'MMMM $VAR1 CROSS SECTION'
'set gxout shade2'
'color $CMIN1 $CMAX1 $CINT1 -kind $KIND'
'd mag(coll2gr(5,-u),coll2gr(6,-u))'

'set xlab off';'set ylab off'

say; say 'MMMM $VAR2 CROSS SECTION'
'set gxout contour'

'set clab off'; 'set cint $CINT2C'; 'set ccolor 0'; 'set cthick 6'
'd coll2gr(2,-u)'

'set xlab off'; 'set ylab off'

'set clab off'; 'set cint $CINT2C'; 'set ccolor 15'; 'set cthick 2'
'd coll2gr(2,-u)'

'set clab on'; 'set clevs $CLEV2C'; 'set ccolor 15'; 'set cthick 2'; 'set clopts 15 2 0.07'
'd coll2gr(2,-u)'

say; say 'MMMMM WVF CROSS SECTION ALONG COMP.'
'set gxout vector'
'set cthick 5'; 'set ccolor  0'
'vec.gs skip(coll2gr(5,-u),25,1);coll2gr(7,-u) -SCL 0.5 0.5 -P 20 20 -SL m/s'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=xr-0.7; yy=yb-0.27
'set cthick  2'; 'set ccolor  1';'set strsiz 0.08 0.1';'set string 1 c 2 0'
'vec.gs skip(coll2gr(5,-u),25,1);coll2gr(7,-u) -SCL 0.5 0.5 -P 'xx' 'yy' -SL ${VECUNIT}'

say; say 'MMMMM WVF CROSS SECTION ACROSS COMP.'
'set gxout contour'
'set clab off'; 'set cint 0.05'; 'set ccolor 0'; 'set cthick 4'
'd coll2gr(6,-u)'
'set clab on'; 'set cint 0.05'; 'set cthick 1'
'set ccolor 2'; 'set clopts 2 2 0.07'
'd coll2gr(6,-u)'

say; say 'MMMM MAUL CROSS SECTION'
'set gxout grfill'
'set ccolor 98';'set cthick 10'
#'set cint $CINT2'
'd coll2gr(3,-u)'



say; say 'MMMM RH CROSS SECTION'
'set gxout contour'
'set clevs $CLEV4'; 'set ccolor 0';' set cthick 5';'set clopts 4 2 0.07'
'd coll2gr(4,-u)'
'set clevs $CLEV4'; 'set ccolor 4';' set cthick 2';'set clopts 4 2 0.07'
'd coll2gr(4,-u)'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.1 0.12'; 'set string 1 c 3 0'
xx=(xl+xr)/2; yy=yt+0.14; 'draw string 'xx' 'yy' Valid $DATE (FH${FH})'
xx=(xl+xr)/2; yy=yy+0.18; 'draw string 'xx' 'yy' $LINE3 (' lat1 'N,' lon1 'E)-(' lat2 ',' lon2 'E)'
xx=(xl+xr)/2; yy=yb-0.3; 'draw string 'xx' 'yy' $XTITLE_CSC'

'set strsiz 0.1 0.12'; 'set string 1 c 3 90'
xx=xl-0.5; yy=(yt+yb)/2; 'draw string 'xx' 'yy' $YTITLE_CSC'



say; say; say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM 5 CROSS SECTION MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'
'set xlab on'; 'set ylab on'

'set time ${HH}Z${DD}${MMM}${YYYY}'
'set grads off'

line = $LINE4
lon1 = $LON4_1; lon2 = $LON4_2; lat1 = $LAT4_1; lat2 = $LAT4_2
YM=${YM4}
say lon1' 'lon2' 'lat1' 'lat2

'set x 1'; 'set y 1'
'set lev $LEVCSC3 $LEVCSC4' ;# 'set zlog on'

'collect 2 free'; 'collect 3 free'; 'collect 4 free'
'collect 5 free'; 'collect 6 free'; 'collect 7 free'; 

lat = lat1
while (lat <= lat2)
  lon = lon1 + (lon2-lon1)*(lat-lat1) / (lat2-lat1)
  'collect 2 gr2stn($VAR2,'lon','lat')'
  'collect 3 gr2stn($VAR3,'lon','lat')'
  'collect 4 gr2stn($VAR4,'lon','lat')'
  'collect 5 gr2stn(wvxr.5,'lon','lat')'
  'collect 6 gr2stn(wvyr.5,'lon','lat')'
  'collect 7 gr2stn(dum0.5,'lon','lat')'
  lat = lat + $DLAT
endwhile
'set x 1 'YM
'set xaxis 'lat1' 'lat2

say 'MMMMM SET FIGURE SIZE'
ymap=2; xmap=3

xs = xleft + (xwid+xmargin2)*(xmap-1); xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1) ; ys = ye - ywid/2

'set parea 'xs ' 'xe' 'ys' 'ye

'set xlint 1'; 'set xlevs 25 26 27 28 29 30 31 32 33 34 35 36'; 'set ylint 50'

say; say 'MMMM $VAR1 CROSS SECTION'
'set gxout shade2'
'color $CMIN1 $CMAX1 $CINT1 -kind $KIND'
'd mag(coll2gr(5,-u),coll2gr(6,-u))'

'set xlab off';'set ylab off'

say; say 'MMMM $VAR2 CROSS SECTION'
'set gxout contour'

'set clab off'; 'set cint $CINT2C'; 'set ccolor 0'; 'set cthick 6'
'd coll2gr(2,-u)'

'set xlab off'; 'set ylab off'

'set clab off'; 'set cint $CINT2C'; 'set ccolor 15'; 'set cthick 2'
'd coll2gr(2,-u)'

'set clab on'; 'set clevs $CLEV2C'; 'set ccolor 15'; 'set cthick 2'; 'set clopts 15 2 0.07'
'd coll2gr(2,-u)'



say; say 'MMMMM WVF CROSS SECTION ALONG COMP.'
'set gxout vector'
'set cthick 5'; 'set ccolor  0'
'vec.gs skip(coll2gr(5,-u),25,1);coll2gr(7,-u) -SCL 0.5 0.5 -P 20 20 -SL m/s'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=xr-0.7; yy=yb-0.27
'set cthick  2'; 'set ccolor  1';'set strsiz 0.08 0.1';'set string 1 c 2 0'
'vec.gs skip(coll2gr(5,-u),25,1);coll2gr(7,-u) -SCL 0.5 0.5 -P 'xx' 'yy' -SL ${VECUNIT}'

say; say 'MMMMM WVF CROSS SECTION ACROSS COMP.'
'set gxout contour'
'set clab off'; 'set cint 0.05'; 'set ccolor 0'; 'set cthick 4'
'd coll2gr(6,-u)'
'set clab on'; 'set cint 0.05'; 'set cthick 1'
'set ccolor 2'; 'set clopts 2 2 0.07'
'd coll2gr(6,-u)'

say; say 'MMMM MAUL CROSS SECTION'
'set gxout grfill'
'set ccolor 98';'set cthick 10'
#'set cint $CINT2'
'd coll2gr(3,-u)'



say; say 'MMMM RH CROSS SECTION'
'set gxout contour'
'set clevs $CLEV4'; 'set ccolor 0';' set cthick 5';'set clopts 4 2 0.07'
'd coll2gr(4,-u)'
'set clevs $CLEV4'; 'set ccolor 4';' set cthick 2';'set clopts 4 2 0.07'
'd coll2gr(4,-u)'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.1 0.12'; 'set string 1 c 3 0'
xx=(xl+xr)/2; yy=yt+0.14; 'draw string 'xx' 'yy' Valid $DATE (FH${FH})'
xx=(xl+xr)/2; yy=yy+0.18; 'draw string 'xx' 'yy' $LINE4 (' lat1 'N,' lon1 'E)-(' lat2 ',' lon2 'E)'
xx=(xl+xr)/2; yy=yb-0.3; 'draw string 'xx' 'yy' $XTITLE_CSC'

'set strsiz 0.1 0.12'; 'set string 1 c 3 90'
xx=xl-0.5; yy=(yt+yb)/2; 'draw string 'xx' 'yy' $YTITLE_CSC'



say; say; say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM HEADER MMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'
xx = XSLEFT - 0.2; yy=YTTOP+0.1
'set strsiz 0.1 0.12'; 'set string 1 l 3 0'
yy = yy+0.8; 'draw string ' xx ' ' yy ' ${FIG}'
yy = yy+0.23; 'draw string ' xx ' ' yy ' ${CWD}'
yy = yy+0.23; 'draw string ' xx ' ' yy ' ${CMD} $@'
yy = yy+0.23; 'draw string ' xx ' ' yy ' ${TIMESTAMP}'

'gxprint $FIG'

'quit'
EOF


grads -bcl $GS
rm -vf $GS
if [ -f $FIG ];then echo; echo OUTPUT $FIG; echo; fi
