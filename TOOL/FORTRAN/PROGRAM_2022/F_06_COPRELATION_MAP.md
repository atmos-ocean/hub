
# プログラミング入門_06

1点相関の分布図

[[_TOC_]]

## 概要

## 準備

```bash
$ cd $HOME
```

```bash
$ cd 2022_PROGRAM
```

```bash
$ mkdir -vp 06
```

```bash
$ cd 06
```

```bash
$ pwd
/work03/gu1/LAB/2022_PROGRAM/06
```
```bash
$ ift
```
もしくは,  
```bash
$ source  /opt/intel/oneapi/setvars.sh --force
```



### 入力データの構造

```
dset ^hgt.season.1958-2005.BIN
title Z500 DJF NCEP1
undef -9.96921e+36
xdef 144 linear 0 2.5
ydef 73 linear -90 2.5
zdef 1 linear 0 1
tdef 48 linear 00Z01JAN1958 12mo
vars 1
z500 0 99 GPH 500 hPa [m]
endvars
```

経度方向144個，緯度方向73個のデータが各時刻ごとに48個並んでいる  

ダイレクト・アクセスで読む  

レコード長は, 144x73x4  



## プログラム作成

### 入力部

#### プログラム作成

`vi READ_Z500_DJF.f90`

`i`

```FORTRAN
program READ_Z500_DJF

character INDIR*500,INFLE*500,IN*1000
integer::IM=144,JM=73,NM=48
real,allocatable::z(:,:,:)

INDIR="/work01/DATA/NCEP1/12.00.MON.2.SEASON"
!INDIR="/work03/gu1/LAB/2022_PROGRAM/12.12.NCEP_PREPRO/12.00.MON.2.SEASON"
INFLE="hgt.season.1958-2005.BIN"
IN=trim(INDIR)//"/"//trim(INFLE)

allocate(z(IM,JM,NM))

isize=IM*JM*4

open(11,file=IN,action="read",form="unformatted",access="direct",&
recl=isize)

irec=0
do n=1,NM
irec=irec+1
print *,irec
read(11,rec=irec)Z(:,:,n)
end do !n

end program READ_Z500_DJF
```

`Esc`

`:wq`

```bash
$ ifort -assume byterecl 12.00.READ.Z500.DJF.f90 -o READ.Z500.DJF.exe
```

#### 参考

`-assume byterecl`

ifortのダイレクトアクセスのレコード長(open文で recl= で設定する値) の単位は, デフォルトでは，「ワード 」と呼ばれる量である。1ワード＝4バイトである。

一般的な「バイト」単位でレコード長を指定するには,  コンパイルオプション `-assume byterecl` を指定する必要がある.

#### 実行

```
$ READ.Z500.DJF.exe
```

#### エラーが出た時の対応

##### エラー箇所の特定

```
$ ifort -traceback -assume byterecl 12.00.READ.Z500.DJF.f90 -o READ.Z500.DJF.exe
```



### 計算

#### プログラム作成

```bash
$ cp 12.00.READ.Z500.DJF.f90 14.00.CALC.R1P.f90
```

```bash
$ vi 14.00.CALC.R1P.f90
```

```fortran
program CALC_R1P

character INDIR*500,INFLE*500,IN*1000
integer::IM=144,JM=73,NM=48
real,allocatable::z(:,:,:),CORR(:,:)
real,allocatable,dimension(:)::Z0, Z1,Z0_AN,Z1_AN
integer I0,J0

INDIR="/work01/DATA/NCEP1/12.00.MON.2.SEASON"
!INDIR="/work03/gu1/LAB/2022_PROGRAM/12.12.NCEP_PREPRO/12.00.MON.2.SEASON"
INFLE="hgt.season.1958-2005.BIN"
IN=trim(INDIR)//"/"//trim(INFLE)

allocate(z(IM,JM,NM), Z0(NM), Z1(NM), Z0_AN(NM), Z1_AN(NM))
allocate(CORR(IM,JM))

I0=INT(200/2.5); J0=INT(110/2.5)
! 160W=200E
! 20N=110 (LAT OF SP := -90)
print *,"I0=",I0,"J0=",J0

isize=IM*JM*4

open(11,file=IN,action="read",form="unformatted",access="direct",&
recl=isize)

irec=0
do n=1,NM
irec=irec+1
read(11,rec=irec)Z(:,:,n)
end do !n

close(21)

Z0(:)=Z(I0,J0,:)
! print *,Z0
Z0_AV=SUM(Z0)/FLOAT(NM)
Z0_AN(:)=Z0(:)-Z0_AV

do j=1,JM
do i=1,IM

Z1(:)=Z(I,J,:)

Z1_AV=SUM(Z1)/FLOAT(NM)
Z1_AN(:)=Z1(:)-Z1_AV

Z0_DOT_Z1 = DOT_PRODUCT(Z0_AN, Z1_AN)
Z0MAG2 = DOT_PRODUCT(Z0_AN, Z0_AN)
Z1MAG2 = DOT_PRODUCT(Z1_AN, Z1_AN)
Z0MAG=SQRT(Z0MAG2)
Z1MAG=SQRT(Z1MAG2)

CORR(i,j)=Z0_DOT_Z1/(Z0MAG*Z1MAG)

print *,I,J,Z0_DOT_Z1,Z0MAG,Z1MAG,CORR(i,j)

end do !i
end do !j

end program CALC_R1P
```

```bash
$ ifort -assume byterecl 14.00.CALC.R1P.f90 -o 14.00.CALC.R1P.exe
```

```bash
$ 14.00.CALC.R1P.exe
```



### 出力

```bash
$ cp 14.00.CALC.R1P.f90 16.00.ONE_PNT_CORR.f90
```

`$ vi 14.00.CALC.R1P.f90`

`i`

```Fortran
program CALC_R1P

character INDIR*500,INFLE*500,IN*1000, OFLE*500
integer::IM=144,JM=73,NM=48
real,allocatable::z(:,:,:),CORR(:,:)
real,allocatable,dimension(:)::Z0, Z1,Z0_AN,Z1_AN
integer I0,J0

INDIR="/work01/DATA/NCEP1/12.00.MON.2.SEASON"
!INDIR="/work03/gu1/LAB/2022_PROGRAM/12.12.NCEP_PREPRO/12.00.MON.2.SEASON"
INFLE="hgt.season.1958-2005.BIN"
IN=trim(INDIR)//"/"//trim(INFLE)

OFLE="hgt.DJF.CORR.BIN"

allocate(z(IM,JM,NM), Z0(NM), Z1(NM), Z0_AN(NM), Z1_AN(NM))
allocate(CORR(IM,JM))

I0=INT(200/2.5); J0=INT(110/2.5)
! 160W=200E
! 20N=110 (LAT OF SP := -90)
print *,"I0=",I0,"J0=",J0

isize=IM*JM*4

open(11,file=IN,action="read",form="unformatted",access="direct",&
recl=isize)

irec=0
do n=1,NM
irec=irec+1
read(11,rec=irec)Z(:,:,n)
end do !n

Z0(:)=Z(I0,J0,:)
! print *,Z0
Z0_AV=SUM(Z0)/FLOAT(NM)
Z0_AN(:)=Z0(:)-Z0_AV

do j=1,JM
do i=1,IM

Z1(:)=Z(I,J,:)

Z1_AV=SUM(Z1)/FLOAT(NM)
Z1_AN(:)=Z1(:)-Z1_AV

Z0_DOT_Z1 = DOT_PRODUCT(Z0_AN, Z1_AN)
Z0MAG2 = DOT_PRODUCT(Z0_AN, Z0_AN)
Z1MAG2 = DOT_PRODUCT(Z1_AN, Z1_AN)
Z0MAG=SQRT(Z0MAG2)
Z1MAG=SQRT(Z1MAG2)

CORR(i,j)=Z0_DOT_Z1/(Z0MAG*Z1MAG)

!print *,I,J,Z0_DOT_Z1,Z0MAG,Z1MAG,CORR(i,j)

end do !i
end do !j


open(21,file=OFLE,form="unformatted",access="direct",&
recl=isize)
irec=1
write(21,rec=irec)CORR
close(21)

print *;print '(A,A)','OUTPUT: ',trim(OFLE)

end program CALC_R1P
```

```bash
$ ifort  -assume byterecl  16.00.ONE_PNT_CORR.f90 -o ONE_PNT_CORR.exe
```

```
$ ONE_PNT_CORR.exe
```

 I0=          80 J0=          44

OUTPUT: hgt.DJF.CORR.BIN



### 作図

```bash
$ vi 16.02.CHK.CORR.sh
```

```bash
#!/bin/bash

CTL=hgt.DJF.CORR.CTL
GS=$(basename $0 .sh).GS

RLON=200;RLAT=20

LATS=0 ;LATN=90

FIG=$(basename $CTL .CTL).eps

HOST=$(hostname);CWD=$(pwd);NOW=$(date -R);CMD="$0 $@"

cat << EOF > ${GS}

'open ${CTL}'

xmax = 1;ymax = 1

ytop=9

xwid = 5.0/xmax;ywid = 5.0/ymax

xmargin=0.5;ymargin=0.1

nmap = 1
ymap = 1
#while (ymap <= ymax)
xmap = 1
#while (xmap <= xmax)

xs = 1.5 + (xwid+xmargin)*(xmap-1);xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1);ys = ye - ywid

# SET PAGE
'set vpage 0.0 8.5 0.0 11'
'set parea 'xs ' 'xe' 'ys' 'ye

'cc'


# 'set lon ${LONW} ${LONE}'
'set lat ${LATS} ${LATN}'
# 'set lev ${LEV}'
'set t 1'

'set mproj nps'; 'set frame off'; 'set grid on 1 1'
'set ylint 90'; 'set xlint 30'

'set gxout contour'
'd R'

'markplot $RLON $RLAT -m 3 -s 0.1 -c 1'

'circlon 30' 
#'circlat 30 90 0.18'


# GET COORDINATES OF 4 CORNERS
'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

# HEADER
'set strsiz 0.12 0.14'; 'set string 1 l 3 0'
xx = 0.2
yy=yt+0.5;   'draw string ' xx ' ' yy ' ${FIG}'
yy = yy+0.2; 'draw string ' xx ' ' yy ' ${CMD}'
yy = yy+0.2; 'draw string ' xx ' ' yy ' ${NOW}'
yy = yy+0.2; 'draw string ' xx ' ' yy ' ${HOST}'
yy = yy+0.2; 'draw string ' xx ' ' yy ' ${CWD}'

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

```bash
$ 16.02.CHK.CORR.sh 
.....
OUTPUT : 
-rw-r--r--. 1 gu1 oc 203K 2022-06-10 07:10 hgt.DJF.CORR.eps
```



## 付録

**以下は参考資料**。上記のプログラム実行に必須のものもではない。

入力データ作成に使ったスクリプト (NCAR COMMAND LANGUAGE使用)

12.00.CREATE.1958-2005.DJF.Z500.ncl

```
INFLE="hgt.mon.mean.nc"
OFLE="hgt.season.1958-2005.nc"

f = addfile(INFLE, "r")
print(f)

lev=f->level
dim=dimsizes(lev)
nk=dim(0)
delete(dim)

do k=0,nk-1
if(lev(k).eq.500.)then 
print("lev(k)="+lev(k))
kdx=k
end if
end do


time=f->time
utc_date=cd_calendar(time,0)

year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 

dim=dimsizes(time)
nt=dim(0)
delete(dim)

ys=1958
ms=1
ye=2005
me=12

do n=0,nt-1
if(year(n).eq.ys .and. month(n).eq.ms)then 
print(year(n)+" "+month(n))
ndx0=n
end if
if(year(n).eq.ye .and. month(n).eq.me)then 
print(year(n)+" "+month(n))
ndx1=n
end if

end do

z=f->hgt

z_cut=z(ndx0:ndx1,kdx,:,:)


z_sea = month_to_season(z_cut, "DJF")

printVarSummary(z_sea)

system("rm -f "+OFLE)

out = addfile(OFLE,"c")
out->z500=z_sea

system("ls -lh --time-style=long-iso "+OFLE)
```



14.00.CHK.Z500.ncl (NCAR COMMAND LANGUAGE使用)

```
INFLE="hgt.season.1958-2005.nc"

f = addfile(INFLE, "r")

time=f->time
utc_date=cd_calendar(time,0)

year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 

dim=dimsizes(time)
nt=dim(0)
delete(dim)

print(year(0)+" "+month(0))
print(year(nt-1)+" "+month(nt-1)) 

z=f->z500

z1=z(0,:,:)

;printVarSummary(z1)




FIG="Z500_DJF_TEST_1958"
TYP="eps"

wks = gsn_open_wks(TYP, FIG)

res            = True                          ; plot mods desired
res@gsnPolar   = "NH"                          ; specify the hemisphere
plot=gsn_csm_contour_map_polar(wks,z1(:,:),res)
```



16.00.NC2BIN.sh (GrADS使用)

```
#!/bin/bash

# Thu, 09 Jun 2022 22:16:53 +0900
# p5820.bio.mie-u.ac.jp
# /work03/gu1/LAB/2022_PROGRAM/12.12.NCEP_PREPRO/12.00.MON.2.SEASON

GS=$(basename $0 .sh).GS

INFLE=hgt.season.1958-2005.nc
BIN=$(basename $INFLE .nc).BIN

rm -f $BIN

HOST=$(hostname); CWD=$(pwd); NOW=$(date -R); CMD="$0 $@"

cat << EOF > ${GS}

# Thu, 09 Jun 2022 22:16:53 +0900
# p5820.bio.mie-u.ac.jp
# /work03/gu1/LAB/2022_PROGRAM/12.12.NCEP_PREPRO/12.00.MON.2.SEASON

'sdfopen ${INFLE}'
'q ctlinfo'; say result

'set x 1 144'
'set t 1 48'
'set fwrite $BIN'
'set gxout fwrite'
'd z500'
'disable fwrite'

'quit'
EOF

grads -bcp "$GS"
rm -vf $GS

ls -lh $BIN
```



16.02.CHK.BIN.sh (GrADS使用)

```
#!/bin/bash

# Thu, 09 Jun 2022 22:25:39 +0900
# p5820.bio.mie-u.ac.jp
# /work03/gu1/LAB/2022_PROGRAM/12.12.NCEP_PREPRO/12.00.MON.2.SEASON

CTL=hgt.season.1958-2005.CTL
GS=$(basename $0 .sh).GS


# LONW= ;LONE=
LATS=0 ;LATN=90; T=48
# LEV=
# TIME=

FIG=$(basename $0 .sh)_T_${T}.eps

# LEVS="-3 3 1"
# LEVS=" -levs -3 -2 -1 0 1 2 3"
# KIND='midnightblue->deepskyblue->lightcyan->white->orange->red->crimson'
# FS=2
# UNIT=

HOST=$(hostname);CWD=$(pwd);NOW=$(date -R);CMD="$0 $@"

cat << EOF > ${GS}

# Thu, 09 Jun 2022 22:25:39 +0900
# p5820.bio.mie-u.ac.jp
# /work03/gu1/LAB/2022_PROGRAM/12.12.NCEP_PREPRO/12.00.MON.2.SEASON

'open ${CTL}'

xmax = 1;ymax = 1

ytop=9

xwid = 5.0/xmax;ywid = 5.0/ymax

xmargin=0.5;ymargin=0.1

nmap = 1
ymap = 1
#while (ymap <= ymax)
xmap = 1
#while (xmap <= xmax)

xs = 1.5 + (xwid+xmargin)*(xmap-1);xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1);ys = ye - ywid

# SET PAGE
'set vpage 0.0 8.5 0.0 11'
'set parea 'xs ' 'xe' 'ys' 'ye

'cc'

# SET COLOR BAR
# 'color ${LEVS} -kind ${KIND} -gxout shaded'

# 'set lon ${LONW} ${LONE}'
'set lat ${LATS} ${LATN}'
# 'set lev ${LEV}'
'set t $T'

'set mproj nps'

'set gxout contour'
'd z500'

'circlon 30' 
#'circlat 30 90 0.18'


# GET COORDINATES OF 4 CORNERS
'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

# LEGEND COLOR BAR
#x1=xl; x2=xr
#y1=yb-0.5; y2=y1+0.1
#'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.1 -fh 0.13 -fs $FS -ft 3 -line on -edge circle'
#x=xr
#y=y1
#'set strsiz 0.12 0.15'
#'set string 1 r 3 0'
#'draw string 'x' 'y' ${UNIT}'



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
'draw string ' xx ' ' yy ' ${FIG}'
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



18.00.NC2BIN_1PNT.sh (GrADS使用)

```
#!/bin/bash

# Thu, 09 Jun 2022 22:16:53 +0900
# p5820.bio.mie-u.ac.jp
# /work03/gu1/LAB/2022_PROGRAM/12.12.NCEP_PREPRO/12.00.MON.2.SEASON

GS=$(basename $0 .sh).GS

INFLE=hgt.season.1958-2005.nc
#LON=-165;LAT=20
LON=-165;LAT=50

BIN=$(basename $INFLE .nc)_${LAT}_${LON}.BIN

rm -f $BIN

HOST=$(hostname); CWD=$(pwd); NOW=$(date -R); CMD="$0 $@"

cat << EOF > ${GS}

# Thu, 09 Jun 2022 22:16:53 +0900
# p5820.bio.mie-u.ac.jp
# /work03/gu1/LAB/2022_PROGRAM/12.12.NCEP_PREPRO/12.00.MON.2.SEASON

'sdfopen ${INFLE}'
'q ctlinfo'; say result

'set lon ${LON}'
'set lat ${LAT}'
'set t 1 48'
'set fwrite $BIN'
'set gxout fwrite'
'd z500'
'disable fwrite'

'quit'
EOF

grads -bcp "$GS"
rm -vf $GS

ls -lh $BIN
```



18.02.CHK.BIN.1PNT.sh (GrADS使用)

```
#!/bin/bash

#CTL=hgt.season.1958-2005_20_-165.CTL
CTL=hgt.season.1958-2005_50_-165.CTL
GS=$(basename $0 .sh).GS

FIG=$(basename $CTL .CTL).eps

HOST=$(hostname);CWD=$(pwd);NOW=$(date -R);CMD="$0 $@"

cat << EOF > ${GS}

'open ${CTL}'

xmax = 1;ymax = 1

ytop=9

xwid = 5.0/xmax;ywid = 5.0/ymax

xmargin=0.5;ymargin=0.1

nmap = 1; ymap = 1; xmap = 1

xs = 1.5 + (xwid+xmargin)*(xmap-1);xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1);ys = ye - ywid

# SET PAGE
'set vpage 0.0 8.5 0.0 11'
'set parea 'xs ' 'xe' 'ys' 'ye

'cc'

'set t 1 48'

'set gxout line'; 'set cmark 0'
'd z500'


# GET COORDINATES OF 4 CORNERS
'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

# HEADER
'set strsiz 0.12 0.14'
'set string 1 l 3 0'
xx = 0.2
yy=yt+0.5
'draw string ' xx ' ' yy ' ${FIG}'
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



## 上達のためのポイント

**エラーが出た時の対応の仕方でプログラミングの上達の速度が大幅に変わる**。

ポイントは次の3つである

1. エラーメッセージをよく読む
2. エラーメッセージを検索し，ヒットしたサイトをよく読む
3. 変数に関する情報を書き出して確認する

エラーメッセージは，プログラムが不正終了した直接の原因とその考えられる理由が書いてあるので，よく読むことが必要不可欠である。

記述が簡潔なため，内容が十分に理解できないことも多いが，その場合**エラーメッセージをブラウザで検索**してヒットした記事をいくつか読んでみる。

エラーの原因だけでなく，**考えうる解決策**が記載されていることも良くある。

エラーを引き起こしていると思われる箇所の**変数の情報**や**変数の値そのものを書き出して**，**期待した通りにプログラムが動作しているか確認する**ことも重要である。

エラーの場所が特定できれば，エラーの修正の大部分は完了したと考えてもよいほどである。

エラーメッセージや検索してヒットするウェブサイトは英語で記載されていることも多いが，**重要な情報は英語で記載されていることが多い**ので，よく読むようにする。

重要そうに思われるが，一回で理解できないものは，PDFなどに書き出して後で繰り返し読んでみる。どうしても**内容が頭に入らないものは印刷してから読む**。
