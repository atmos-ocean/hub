
DSET=MSM

GS=$(basename $0 .sh).GS

LONW=122.0 ; LONE=134 ;*LONE=150
LATS=24.5  ; LATN=35  ;*  LATN=47.5

YYYY=2022; MM=06; MMM=JUN; DD=18; HH=18; MMDD=${MM}${DD}
YYYY=2022; MM=06; MMM=JUN; DD=18; HH=21; MMDD=${MM}${DD}
YYYY=2022; MM=06; MMM=JUN; DD=19; HH=00; MMDD=${MM}${DD}
#YYYY=2022; MM=06; MMM=JUN; DD=19; HH=03; MMDD=${MM}${DD}
#YYYY=2022; MM=06; MMM=JUN; DD=19; HH=06; MMDD=${MM}${DD}

DATE=${HH}UTC${DD}${MMM}${YYYY}
VAR1=EPT; V1LEV=975; CMIN1=330; CMAX1=356; CINT1=2; CLEV1=344
VAR2=VPT; V2LEV=975; CINT2=1; CLEV2C="300 301 302 303 304 305 306 307 308" ; CLEV2M=304
VAR3=MAUL; V3LEV=850
VAR4=RH; CLEV4="95"
XTITLE_CSC="Latitude North" ;#"Longitude East"
YTITLE_CSC="Pressure [hPa]" ;#"Longitude East"

DLON=0.02; DLAT=$DLON; LON1=124; LAT1=25; LON2=129; LAT2=30.5
DLON=0.02; DLAT=$DLON; LON1=124; LAT1=26.7; LON2=130; LAT2=31.5
DLON=0.02; DLAT=$DLON; LON1=124; LAT1=26.7; LON2=130; LAT2=31.5


XM=$(echo "scale=0; (${LON2}-${LON1})/${DLON}" | bc)
YM=$(echo "scale=0; (${LAT2}-${LAT1})/${DLAT}" | bc)

echo ; echo MMMMMMMM XM = $XM; echo MMMMMMMM YM = $YM; echo

LEVCSC1=1000; LEVCSC2=600
CLEVCSC=1

INDIR=/work01/DATA/MSM.NCL/MSM-P/${YYYY}
INFLE=${MMDD}_NCL.nc
IN=${INDIR}/${INFLE}

if [ ! -f $IN ]; then echo ERROR: NO SUCH FILE, $IN; exit 1; fi

FIG=${DSET}_${VAR1}_${VAR2}_${MMDD}${HH}_${LON1}_${LAT1}-${LON2}_${LAT2}_NCL_CSC.pdf

SCLV=40
KIND='lightcyan->cyan->mediumspringgreen->lawngreen->moccasin->orange->orangered->red->magenta'


HOST=$(hostname); CWD=$(pwd); TIMESTAMP=$(date -R); CMD="$0 $@"
GS=$(basename $0 .sh).GS



cat <<EOF>$GS

var='$VAR'

'sdfopen $IN' ;# 'q ctlinfo'; say result

'cc'; 'set grid off'


say 'mmmmmmmm MAP mmmmmmmm'

say 'MMMMM SET FIGURE SIZE'
'set vpage 0.0 8.5 0 10.5'

xmax=2; ymax=2
xleft=0.5; ytop=9

xwid =  6/xmax; ywid =  5/ymax
xmargin=0.6;    ymargin=0.5

ymap=1; xmap=1

xs = xleft + (xwid+xmargin)*(xmap-1); xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1) ; ys = ye - ywid

'set parea 'xs ' 'xe' 'ys' 'ye



'set grads off'


say 'SHADE $VAR'

'set lon $LONW $LONE'; 'set lat $LATS $LATN'
'set time ${HH}Z${DD}${MMM}${YYYY}'
'q dims'; say result; line=sublin(result,5); datetime=subwrd(line,6)
'set lev $V1LEV'
'q dims'; say sublin(result,4)


# 78 53 36 ;#DARK BROWN
'set mpdset hires'; 'set rgb 99 160 82 45'; 'set map 99 1 2'
'set xlopts 1 3 0.12'; 'set xlint 4'
'set ylopts 1 3 0.12'; 'set ylint 2'


'set xlab on'; 'set xlevs 123 126 129 132 132'; 'set ylab on'

'set gxout shade2'
'color $CMIN1 $CMAX1 $CINT1 -kind $KIND'
'd $VAR1'

'set xlab off'; 'set ylab off'


say 'CONTOUR $VAR2 $V2LEV'
'set gxout contour'
'set ccolor 0'; 'set cthick 5'
'set clab off'; 'set cint $CINT2'
'set lev $V2LEV'
'q dims'; say sublin(result,4)
'd $VAR2'

'set ccolor 1'; 'set cthick 1'
'set clab off'; 'set cint $CINT2'
'set lev $V2LEV'
'd $VAR2'




say 'MMMMM VECTOR $V2LEV'
'set gxout vector'; 'set cthick 1'
'set arrscl 0.5 $SCLV'; 'set arrlab off'
'set lev $V2LEV'
'q dims'; say sublin(result,4)

#'set ccolor 0'; 'set cthick 5'
#'set arrscl 0.5 $SCLV'
#'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 $SCLV -P 20 20'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
xx=xr-0.55; yy=yb-0.3
'set rgb 97 120 120 120'
'set ccolor 97'; 'set cthick 2'
'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 $SCLV -P 'xx' 'yy' -SL m/s'

#'set strsiz 0.08 0.1'; 'set string 1 c 2 0'
#xx=xx+0.45; yy=yy-0.1
#'draw string 'xx' 'yy'  m/s'


say 'MMMMM LINE'
'trackplot $LON1 $LAT1 $LON2 $LAT2 -c 1 -l 1 -t 6'


say 'MMMMM VECTOR $V3LEV'
'set gxout vector'
'set arrscl 0.5 $SCLV'; 'set arrlab off'
'set ccolor 0'; 'set cthick 6'
'set lev $V3LEV'
'q dims'; say sublin(result,4)

'set ccolor 0'; 'set cthick 5'
'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 $SCLV -P 20 20'
'set ccolor 1'; 'set cthick 2'
'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 $SCLV -P 20 20'

say 'CONTOUR $VAR2 $CLEV2M'
'set clopts 1 2 0.07'
'set gxout contour'
'set ccolor 1'; 'set cthick 1'
'set clab on'; 'set clevs $CLEV2M'
'set lev $V2LEV'
'd $VAR2'

say 'MMMMM MAUL'
'set gxout contour'; 'set cthick 10'; 
'set rgb 98 128 0 128 -200'; 'set ccolor 98'
#'set clevs 1'
'set lev $V3LEV'
'q dims'; say sublin(result,4)
'd ${VAR3}'



'set strsiz 0.1 0.12'; 'set string 1 c 3'
xx=(xl+xr)/2; yy=yt+0.15; 'draw string 'xx' 'yy' ${VAR1}(${V1LEV}) ${VAR2}(${V2LEV}) ${VAR3}(${V3LEV})'
XSLEFT=xl


'set xlab on'; 'set ylab on'

'set time ${HH}Z${DD}${MMM}${YYYY}'
'set grads off'

lon1 = $LON1; lon2 = $LON2; lat1 = $LAT1; lat2 = $LAT2

'set x 1'; 'set y 1'
'set lev $LEVCSC1 $LEVCSC2' ;# 'set zlog on'

'collect 1 free'; 'collect 2 free'; 'collect 3 free'; 'collect 4 free'
lat = lat1
while (lat <= lat2)
  lon = lon1 + (lon2-lon1)*(lat-lat1) / (lat2-lat1)
  'collect 1 gr2stn($VAR1,'lon','lat')'
  'collect 2 gr2stn($VAR2,'lon','lat')'
  'collect 3 gr2stn($VAR3,'lon','lat')'
  'collect 4 gr2stn($VAR4,'lon','lat')'
  lat = lat + $DLAT
endwhile
'set x 1 ${YM}'
'set xaxis 'lat1' 'lat2

#lon = lon1
#while (lon <= lon2)
#  lat = lat1 + (lat2-lat1)*(lon-lon1) / (lon2-lon1)
#  'collect 1 gr2stn($VAR1,'lon','lat')'
#  'collect 2 gr2stn($VAR2,'lon','lat')'
#  'collect 3 gr2stn($VAR3,'lon','lat')'
#  lon = lon + $DLON
#endwhile
#'set x 1 ${XM}'
#'set xaxis 'lon1' 'lon2

say 'MMMMM SET FIGURE SIZE'
ymap=1; xmap=2

xs = xleft + (xwid+xmargin)*(xmap-1); xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1) ; ys = ye - ywid

'set parea 'xs ' 'xe' 'ys' 'ye



'set xlint 1'; 'set xlevs 25 26 27 28 29 30 31 32 33 34 35 36'; 'set ylint 50'

'set clab on'

'set cint $CLEVCSC'
'color $CMIN1 $CMAX1 $CINT1 -kind $KIND'
'd coll2gr(1,-u)'

'set xlab off'; 'set ylab off'


say; say 'MMMM $VAR2 CROSS SECTION'
'set gxout contour'

'set clab off'; 'set cint $CINT2'; 'set ccolor 0'; 'set cthick 6'
'd coll2gr(2,-u)'

'set clab off'; 'set cint $CINT2'; 'set ccolor 1'; 'set cthick 2'
'd coll2gr(2,-u)'

'set clab on'; 'set clevs $CLEV2C'; 'set ccolor 1'; 'set cthick 2' 'set clopts 1 2 0.07'
'd coll2gr(2,-u)'



say; say 'MMMM MAUL CROSS SECTION'
'set gxout grfill'
'set rgb 98 128 0 128 -200'; 'set ccolor 98';'set cthick 10'
#'set cint $CINT2'
'd coll2gr(3,-u)'



say; say 'MMMM RH CROSS SECTION'
'set gxout contour'
###'set rgb 98 128 0 128 -200'; 'set ccolor 98';'set cthick 10'
'set clevs $CLEV4'; 'set ccolor 0';' set cthick 5';'set clopts 4 2 0.07'
'd coll2gr(4,-u)'
'set clevs $CLEV4'; 'set ccolor 4';' set cthick 2';'set clopts 4 2 0.07'
'd coll2gr(4,-u)'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.1 0.12'; 'set string 1 c 3 0'
xx=(xl+xr)/2; yy=yt+0.15; 'draw string 'xx' 'yy' (${LAT1}N,${LON1}E)-(${LAT2}N,${LON2}E)'
xx=(xl+xr)/2; yy=yb-0.3; 'draw string 'xx' 'yy' $XTITLE_CSC'

'set strsiz 0.1 0.12'; 'set string 1 c 3 90'
xx=xl-0.6; yy=(yt+yb)/2; 'draw string 'xx' 'yy' $YTITLE_CSC'


say 'mmmmm COLOR BAR'
x1=xr+0.2; x2=x1+0.1; y1=yb; y2=yt-0.3
'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.1 -fh 0.13 -fs 2 -ft 3 -line on -edge circle'
xx=(x1+x2)/2; yy=y2+0.15; 'set string 1 c 3 0'
'draw string ' xx ' ' yy 'EPT [K]'

say 'mmmmm HEADER'
'set strsiz 0.13 0.16'; 'set string 1 c 4 0'
xx=(xleft+xe+0.4)/2; yy=yt+0.18
yy = yy+0.25; 'draw string ' xx ' ' yy ' $DATE'

xx = XSLEFT; 
'set strsiz 0.1 0.12'; 'set string 1 l 3 0'
yy = yy+0.3; 'draw string ' xx ' ' yy ' ${FIG}'
yy = yy+0.23; 'draw string ' xx ' ' yy ' ${CMD} $@'
yy = yy+0.23; 'draw string ' xx ' ' yy ' ${TIMESTAMP}'



'gxprint $FIG'

'quit'
EOF


grads -bcp $GS
rm -vf $GS
if [ -f $FIG ];then echo; echo OUTPUT $FIG; echo; fi
