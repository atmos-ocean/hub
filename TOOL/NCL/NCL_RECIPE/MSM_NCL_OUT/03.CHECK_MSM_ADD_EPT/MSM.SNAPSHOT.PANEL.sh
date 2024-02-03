
GS=$(basename $0 .sh).GS

YYYY=2022
MMDD=0619
VAR=ept
INDIR=/work01/DATA/MSM.NCL/MSM-P/${YYYY}
#INDIR=/work01/DATA/MSM/MSM-P/${YYYY}
INFLE=${MMDD}_NCL.nc
IN=${INDIR}/${INFLE}
if [ ! -f $IN ]; then echo ERROR: NO SUCH FILE, $IN; exit 1; fi

OUT=$(basename $0 .sh)_MSM_NCL_CALC_${MMDD}_$VAR.eps

cat <<EOF>$GS

lonw=122.0 ; lone=134 ;*lone=150
lats=24.5  ; latn=35  ;*  latn=47.5

lev=950
ci=20

var='$VAR'

SCLV=40

KIND='lightcyan->cyan->mediumspringgreen->lawngreen->moccasin->orange->orangered->red'
CMIN=328; CMAX=356; CINT=4

CLEV=344

'sdfopen $IN'

'q ctlinfo'; say result

'cc'

'set lev 'lev

pha=lev

say pha

'set lon 'lonw' 'lone; 'set lat 'lats' 'latn

######

t1=1
'set xlab on'; 'set ylab on'

'mul 3 3 1 3 -yint 0.5 -yini 0.5'
'set grads off'

'set t 't1

'q dims'; say result; line=sublin(result,5); datetime=subwrd(line,6)



'set mpdset hires'; 'set map 1 1 3'
'set xlopts 1 3 0.12'; 'set xlint 4'
'set ylopts 1 3 0.12'; 'set ylint 2'

### ept

'set gxout shade2'
'color 'CMIN' 'CMAX' 'CINT' -kind 'KIND
'd ept'

'set xlab off'; 'set ylab off'


'set gxout contour'
'set ccolor 0'; 'set cthick 1'
'set clab off'; 'set cint 2'

'd ept'

### vector

'set gxout vector'
'set arrscl 0.5 'SCLV
'set arrlab off'
'set ccolor 0'
'set cthick 6'

'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'

'set ccolor 1'; 'set cthick 2'
'set arrscl 0.5 'SCLV

'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20 '

 'set strsiz 0.10 0.15'
 'set string 1 c 5 0'

# 'draw string 6.6 0.95  m/s'

'set gxout contour'
'set ccolor 0'; 'set cthick 1'; 'set clopts 1 2 0.08'
'set clab on'; 'set clevs 'CLEV

'd ept'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.12 0.14'; 'set string 1 c 3'
xx=(xl+xr)/2; yy=yt+0.15; 'draw string 'xx' 'yy' 'datetime


t1=2

'mul 3 3 2 3 -yini 0.5'
'set xlab on'; 'set ylab on'

'set t 't1
'set grads off'

'q dims'; say result; line=sublin(result,5); datetime=subwrd(line,6)

'set xlopts 1 3 0.12'; 'set xlint 4'
'set ylopts 1 3 0.12'; 'set ylint 2'

'set gxout shade2'

'color 'CMIN' 'CMAX' 'CINT' -kind 'KIND

'd ept'

'set xlab off'; 'set ylab off'

'set gxout contour'
'set ccolor 0'; 'set cthick 1'
'set clab off'; 'set cint 2'


'd ept'

'set gxout vector'
'set arrscl 0.5 'SCLV
'set arrlab off'
'set ccolor 0'; 'set cthick 6'

'vec skip(u,7,7);skip(v,7,7) -P 20 20'
'set ccolor 1'; 'set cthick 2'

'set arrscl 0.5 'SCLV
'vec skip(u,7,7);skip(v,7,7) -P 20 20'
 'set strsiz 0.10 0.15'
 'set string 1 c 5 0'

'set gxout contour'
'set ccolor 0'; 'set cthick 1'; 'set clopts 1 2 0.08'
'set clab on'; 'set clevs 'CLEV

'd ept'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.12 0.14'; 'set string 1 c 3'
xx=(xl+xr)/2; yy=yt+0.15; 'draw string 'xx' 'yy' 'datetime



t1=3

'mul 3 3 3 3  -yini 0.5'
'set xlab on'; 'set ylab on'
'set grads off'

'set t 't1

'q dims'

say result; line=sublin(result,5); datetime=subwrd(line,6)


'set mpdset hires'; 'set map 1 1 3'
'set xlopts 1 3 0.12'; 'set xlint 4'
'set ylopts 1 3 0.12'; 'set ylint 2'

'set gxout shade2'

'color 'CMIN' 'CMAX' 'CINT' -kind 'KIND

'd ept'

'set xlab off'; 'set ylab off'

'set gxout contour'
'set ccolor 0'; 'set cthick 1'
'set clab off'; 'set cint 2'

'd ept'


'set gxout vector'
'set arrscl 0.5 'SCLV
'set arrlab off'
'set ccolor 0'
'set cthick 6'
'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'

'set ccolor 1'
'set cthick 2'
'set arrscl 0.5 'SCLV
'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'
 'set strsiz 0.10 0.15'
 'set string 1 c 5 0'

'set gxout contour'
'set ccolor 0'; 'set cthick 1'; 'set clopts 1 2 0.08'
'set clab on'; 'set clevs 'CLEV

'd ept '

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.12 0.14'; 'set string 1 c 3'
xx=(xl+xr)/2; yy=yt+0.15; 'draw string 'xx' 'yy' 'datetime



t1=4
'mul 3 3 1 2 -yint 0.5  -yini 0.5'
'set xlab on'; 'set ylab on'
'set grads off'


'set t 't1
'q dims'
say result

line=sublin(result,5)
datetime=subwrd(line,6)


'set mpdset hires'; 'set map 1 1 3'
'set xlopts 1 3 0.12'; 'set xlint 4'
'set ylopts 1 3 0.12'; 'set ylint 2'

'set gxout shade2'

'color 'CMIN' 'CMAX' 'CINT' -kind 'KIND

'd ept'

'set xlab off'; 'set ylab off'

'set gxout contour'
'set ccolor 0'; 'set cthick 1'
'set clab off'; 'set cint 2'

'd ept'


'set gxout vector'
'set arrscl 0.5 'SCLV
'set arrlab off'
'set ccolor 0'
'set cthick 6'
'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'

'set ccolor 1'
'set cthick 2'
'set arrscl 0.5 'SCLV

'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'
 'set strsiz 0.10 0.15'
 'set string 1 c 5 0'


'set gxout contour'
'set ccolor 0'; 'set cthick 0'; 'set clopts 1 2 0.08'
'set clab on'; 'set clevs 'CLEV
'd ept'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.12 0.14'; 'set string 1 c 3'
xx=(xl+xr)/2; yy=yt+0.15; 'draw string 'xx' 'yy' 'datetime



t1=5

'mul 3 3 2 2  -yini 0.5'
'set xlab on'; 'set ylab on'

'set grads off'

'set t 't1
'q dims'; say result; line=sublin(result,5); datetime=subwrd(line,6)



'set mpdset hires'; 'set map 1 1 3'
'set xlopts 1 3 0.12'; 'set xlint 4'
'set ylopts 1 3 0.12'; 'set ylint 2'

'set gxout shade2'

'color 'CMIN' 'CMAX' 'CINT' -kind 'KIND

'd ept'

'set xlab off'; 'set ylab off'

'set gxout contour'
'set ccolor 0'; 'set cthick 1'
'set clab off'; 'set cint 2'
'd ept'

'set gxout vector'
'set arrscl 0.5 'SCLV
'set arrlab off'
'set ccolor 0'
'set cthick 6'
'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'

'set ccolor 1'; 'set cthick 2'
'set arrscl 0.5 'SCLV

'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'
 'set strsiz 0.10 0.15'
 'set string 1 c 5 0'

'set gxout contour'

'set gxout contour'
'set ccolor 0'; 'set cthick 0'; 'set clopts 1 2 0.08'
'set clab on'; 'set clevs 'CLEV
'd ept'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.12 0.14'; 'set string 1 c 3'
xx=(xl+xr)/2; yy=yt+0.15; 'draw string 'xx' 'yy' 'datetime



t1=6

'mul 3 3 3 2  -yini 0.5'
'set xlab on'; 'set ylab on'

'set grads off'

'set t 't1
'q dims'
say result; line=sublin(result,5); datetime=subwrd(line,6)

'set mpdset hires'; 'set map 1 1 3'
'set xlopts 1 3 0.12'; 'set xlint 4'
'set ylopts 1 3 0.12'; 'set ylint 2'

'set gxout shade2'

'color 'CMIN' 'CMAX' 'CINT' -kind 'KIND

'd ept'

'set xlab off'; 'set ylab off'

'set gxout contour'
'set ccolor 0'; 'set cthick 1'
'set clab off'; 'set cint 2'

'd ept'

'set gxout vector'
'set arrscl 0.5 'SCLV
'set arrlab off'
'set ccolor 0'
'set cthick 6'

'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'

'set ccolor 1'
'set cthick 2'
'set arrscl 0.5 'SCLV

'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'

 'set strsiz 0.10 0.15'
 'set string 1 c 5 0'

'set gxout contour'
'set ccolor 0'; 'set cthick 0'; 'set clopts 1 2 0.08'
'set clab on'; 'set clevs 'CLEV
'd ept'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.12 0.14'; 'set string 1 c 3'
xx=(xl+xr)/2; yy=yt+0.15; 'draw string 'xx' 'yy' 'datetime


t1=7

'mul 3 3 1 1  -yini 0.5'
'set xlab on'; 'set ylab on'

'set grads off'

'set t 't1
'q dims'; say result; line=sublin(result,5); datetime=subwrd(line,6)


'set mpdset hires'; 'set map 1 1 3'
'set xlopts 1 3 0.12'; 'set xlint 4'
'set ylopts 1 3 0.12'; 'set ylint 2'

'set gxout shade2'

'color 'CMIN' 'CMAX' 'CINT' -kind 'KIND

'd ept'

'set xlab off'; 'set ylab off'

'set gxout contour'
'set ccolor 0'; 'set cthick 1'
'set clab off'; 'set cint 2'

'd ept'

'set gxout vector'
'set arrscl 0.5 'SCLV
'set arrlab off'
'set ccolor 0'
'set cthick 6'

'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'

'set ccolor 1'
'set cthick 2'
'set arrscl 0.5 'SCLV
'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'

 'set strsiz 0.10 0.15'
 'set string 1 c 5 0'

'set gxout contour'
'set ccolor 0'; 'set cthick 0'; 'set clopts 1 2 0.08'
'set clab on'; 'set clevs 'CLEV
'd ept'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
'set strsiz 0.12 0.14'; 'set string 1 c 3'
xx=(xl+xr)/2; yy=yt+0.15; 'draw string 'xx' 'yy' 'datetime


t1=8
'mul 3 3 2 1  -yini 0.5'
'set xlab on'; 'set ylab on'

'set grads off'

'set t 't1
'q dims'; say result; line=sublin(result,5); datetime=subwrd(line,6)

'set mpdset hires'; 'set map 1 1 3'
'set xlopts 1 3 0.12'; 'set xlint 4'
'set ylopts 1 3 0.12'; 'set ylint 2'

'set gxout shade2'

'color 'CMIN' 'CMAX' 'CINT' -kind 'KIND

'd ept'

'set xlab off'; 'set ylab off'

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)

x1=xr+0.95; x2=x1+0.1
y1=yb; y2=yt
'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.1 -fh 0.13 -fs 2 -ft 3 -line on -edge circle'

x=(x1+x2)/2; y=y2+0.14
'set strsiz 0.1 0.12'; 'set string 1 c 3'
'draw string 'x' 'y' [K]'

'set gxout contour'
'set ccolor 0'; 'set cthick 1'
'set clab off'; 'set cint 2'

'd ept'

'set gxout vector'
'set arrscl 0.5 'SCLV
'set arrlab on'; 'set ccolor 0'; 'set cthick 6'


'vec skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 20 20'
'set ccolor 1'
'set cthick 2'
'set arrscl 0.5 'SCLV

xx=xr+0.3; yy=yb+0.35
'vec.gs skip(u,7,7);skip(v,7,7) -SCL 0.5 'SCLV' -P 'xx' 'yy' -SL m/s'

'set gxout contour'
'set ccolor 0'; 'set cthick 0'; 'set clopts 1 2 0.08'
'set clab on'; 'set clevs 'CLEV

'd ept'

'set strsiz 0.12 0.14'; 'set string 1 c 3'
xx=(xl+xr)/2; yy=yt+0.15; 'draw string 'xx' 'yy' 'datetime

'gxprint $OUT'

'quit'
EOF


grads -bcp $GS
if [ -f $OUT ];then echo; echo OUTPUT $OUT; echo; fi
