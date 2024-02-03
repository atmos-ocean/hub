HOST=$(hostname)
CWD=$(pwd)
TIMESTAMP=$(date -R)
CMD="$0 $@"
GS=$(basename $0 .sh).GS

STN=UKATA
INT=1HR
CTL=REGRESSION.CTL

TIME1=01JAN2000
TIME2=31DEC2000

FIGDIR=. #FIG_$(basename $0 .sh)
#mkdir -vp $FIGDIR
FIG=$FIGDIR/$(basename $CTL .CTL)_${TIME1}_${TIME1}.eps


MIN1=-1.5
MAX1=1.5

MIN2=-1.5
MAX2=1.5

MIN3=-1.5
MAX3=1.5


cat <<EOF>$GS
'open $CTL'
'cc'

'set time $TIME1 $TIME2'

'set vpage 0.0 8.5 0 10.5'

xmax=1
ymax=3

xleft=1
ytop=9

xwid =  5/xmax
ywid =  5/ymax
xmargin=0.5
ymargin=0.5

ymap=1
xmap=1

xs = xleft + (xwid+xmargin)*(xmap-1)
xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1)
ys = ye - ywid

'set parea 'xs ' 'xe' 'ys' 'ye

#'set tlsupp year'
'set grads off'
'set grid off'
'set xlopts 1 3 0.1'
'set ylopts 1 3 0.1'

'set gxout line'
'set ccolor 3'
'set cmark 0'
'set cthick 2'
'set vrange ${MIN1} ${MAX1}'
'set ylint 0.5'
'd X1'

'set xlab off'
'set ylab off'

'zeroline'

'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

x=xl-0.7 ;# (xl+xr)/2
y=(yt+yb)/2
'set strsiz 0.12 0.15'
'set string 1 c 4 90'
'draw string 'x' 'y' X1'

'set strsiz 0.12 0.14'
'set string 1 l 2 0'
xx = 0.2

yy=yt+0.5
'draw string ' xx ' ' yy ' ${GS}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${CMD}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${TIMESTAMP}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${HOST}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${CWD}'



ymap=2
xmap=1

xs = xleft + (xwid+xmargin)*(xmap-1)
xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1)
ys = ye - ywid

'set parea 'xs ' 'xe' 'ys' 'ye

#'set tlsupp year'
'set grads off'
'set grid off'
'set xlab on'
'set ylab on'
'set xlopts 1 3 0.1'
'set ylopts 1 3 0.1'

'set gxout line'
'set ccolor 3'
'set cmark 0'
'set cthick 1'
'set vrange ${MIN2} ${MAX2}'
'set ylint 0.5'
'd X2'

'set xlab off'
'set ylab off'

#'zeroline'

'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

x=xl-0.7 ;# (xl+xr)/2
y=(yt+yb)/2
'set strsiz 0.12 0.15'
'set string 1 c 4 90'
'draw string 'x' 'y' X2'



ymap=3
xmap=1

xs = xleft + (xwid+xmargin)*(xmap-1)
xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1)
ys = ye - ywid

'set parea 'xs ' 'xe' 'ys' 'ye

#'set tlsupp year'
'set grads off'
'set grid off'
'set xlab on'
'set ylab on'
'set xlopts 1 3 0.1'
'set ylopts 1 3 0.1'

'set gxout line'
'set ccolor 3'
'set cmark 0'
'set cthick 1'
'set vrange ${MIN3} ${MAX3}'
'set ylint 0.5'
'd XR'

'set xlab off'
'set ylab off'



'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

x=xl-0.7 ;# (xl+xr)/2
y=(yt+yb)/2
'set strsiz 0.12 0.15'
'set string 1 c 4 90'
'draw string 'x' 'y' XR'


'gxprint $FIG'

'close 1'
'quit'
EOF

grads -bcp "$GS"

rm -v $GS

echo
ls --time-style=long-iso -lh $FIG
echo

