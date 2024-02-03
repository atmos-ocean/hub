# 低気圧の中心位置_簡易版(GrADS使用)

```bash
/work09/am/00.WORK/2022.ECS2022/22.12.WRF/12.12.LOW.CENTER
```



## CTLファイル

```bash
/work09/am/00.WORK/2022.ECS2022/22.12.WRF/12.12.LOW.CENTER
$ ls /work00/DATA/HD04/RW3A.00.04/01HR/ARWpost_RW3A.00.04.05.05.0000.01/*ctl
/work00/DATA/HD04/RW3A.00.04/01HR/ARWpost_RW3A.00.04.05.05.0000.01/RW3A.00.04.05.05.0000.01.d01.basic_p.01HR.ctl
```



## 計算手順

```
'xmin=aminlocx(slp,lon=$CLONW,lon=$CLONE,lat=$CLATS,lat=$CLATN)'
'd xmin';imin=subwrd(result, 4)
cntlon=120.56867+(imin-1)*0.01351351
'ymin=aminlocy(slp,lon=$CLONW,lon=$CLONE,lat=$CLATS,lat=$CLATN)'
'd ymin';jmin=subwrd(result, 4)
cntlat=18.56023+(jmin-1)*0.01351351
```



## 実行例

```bash
/work09/am/00.WORK/2022.ECS2022/22.12.WRF/12.12.LOW.CENTER
$ LOW.CENTER.sh 

OUTPUT : 
-rw-r--r--. 1 am oc 69K 2023-06-30 12:46 LOW.CENTER_RW3A.00.04.05.05.0000.01_20210813_00.PDF
```



## スクリプト

```bash
/work09/am/00.WORK/2022.ECS2022/22.12.WRF/12.12.LOW.CENTER
$ cat LOW.CENTER.sh 
#!/bin/bash

YYYY=2021; MM=08; DD=13; HH=00

if [ $MM = "01" ]; then MMM="JAN"; fi; if [ $MM = "02" ]; then MMM="FEB"; fi
if [ $MM = "03" ]; then MMM="MAR"; fi; if [ $MM = "04" ]; then MMM="APR"; fi
if [ $MM = "05" ]; then MMM="MAY"; fi; if [ $MM = "06" ]; then MMM="JUN"; fi
if [ $MM = "07" ]; then MMM="JUL"; fi; if [ $MM = "08" ]; then MMM="AUG"; fi
if [ $MM = "09" ]; then MMM="SEP"; fi; if [ $MM = "10" ]; then MMM="OCT"; fi
if [ $MM = "11" ]; then MMM="NOV"; fi; if [ $MM = "12" ]; then MMM="DEC"; fi

TIME=${HH}Z${DD}${MMM}${YYYY}

CNAME=RW3A.00.04
RUNNAME=${CNAME}.05.05.0000.01; DOMAIN=d01

CLONW=120 ;CLONE=132 ; CLATS=26 ;CLATN=34.5

INDIR=/work00/DATA/HD04/${CNAME}/01HR/ARWpost_${RUNNAME}
if [ ! -d $INDIR ];then echo NO SUCH DIR,$INDIR;exit 1; fi

CTL=${RUNNAME}.${DOMAIN}.basic_p.01HR.ctl
if [ ! -f $INDIR/$CTL ];then echo NO SUCH FILE,$INDIR/$CTL;exit 1;fi

GS=$(basename $0 .sh).GS
FIG=$(basename $0 .sh)_${RUNNAME}_${YYYY}${MM}${DD}_${HH}.PDF

LONW=120 ;LONE=132 ; LATS=26 ;LATN=34.5

HOST=$(hostname);CWD=$(pwd);NOW=$(date -R);CMD="$0 $@"

cat << EOF > ${GS}

'open ${INDIR}/${CTL}'

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

'set lon ${LONW} ${LONE}'; 'set lat ${LATS} ${LATN}'
'set z 1' ;#'set lev ${LEV}'
'set time ${TIME}'

'set grid off'
'set gxout contour'
'set ccolor 1'
'set clskip 2'
'd slp'

'xmin=aminlocx(slp,lon=$CLONW,lon=$CLONE,lat=$CLATS,lat=$CLATN)'
'd xmin';imin=subwrd(result, 4)
cntlon=120.56867+(imin-1)*0.01351351
'ymin=aminlocy(slp,lon=$CLONW,lon=$CLONE,lat=$CLATS,lat=$CLATN)'
'd ymin';jmin=subwrd(result, 4)
cntlat=18.56023+(jmin-1)*0.01351351

'markplot 'cntlon' 'cntlat' -c 4 -m 3 -s 0.1' 

'q w2xy 'cntlon' 'cntlat
xx=subwrd(result,3); yy=subwrd(result,6)
'set strsiz 0.12 0.15'
'set string 0 c 8 0'
'draw string 'xx+0.1' 'yy-0.15' 'cntlon
'draw string 'xx+0.1' 'yy-0.35' 'cntlat
'set string 1 c 3 0'
'draw string 'xx+0.1' 'yy-0.15' 'cntlon
'draw string 'xx+0.1' 'yy-0.35' 'cntlat


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
x=(xl+xr)/2; y=yt+0.2
'set strsiz 0.12 0.15'; 'set string 1 c 3 0'
'draw string 'x' 'y' ${RUNNAME} ${TIME}'

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

echo "DONE $0."
echo
```

