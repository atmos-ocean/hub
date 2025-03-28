GrADSバイナリ入出力
===========================

[[_TOC_]]

Fortranで作成したバイナリデータをGrADSで表示する
---------------------------------------------------------------------


### 情報源

- http://akyura.sakura.ne.jp/study/GrADS/kouza/grads.html
- https://seesaawiki.jp/ykamae_grads-note/d/GrADS%20ctl%A5%D5%A5%A1%A5%A4%A5%EB%A4%CE%BA%EE%C0%AE
- BINARY_DATA.pptx

### 水平2次元のデータ
- x,y方向の2次元データ
- z方向は1層のみ
- 時間方向には1時刻のみ

`$ cat xy2d.f90`
```fortran
program xy2d

real,allocatable::slp(:,:)
integer nx,ny
real lon, lat, lonc, latc
real slpc
character(len=1000):: ofle,ofle_chk
real,parameter::undef=-1.0E30

ofle='xy2d.bin'
ofle_chk='xy2d.chk.txt'

nx=360
ny=180

rlon=40
rlat=20

dx=1.
dy=1.

lonc=180.
latc=25.0

slpa=1014.
dslp=2.0

open(22,file=ofle_chk)

allocate(slp(nx,ny))
slp(:,:)=undef

do j=1,ny
  lat=-90.0 + dx*float(j-1) + 0.5

  do i=1,nx
    lon=0.0 + dx*float(i-1) + 0.5
    slp(i,j)=slpa + dslp*exp( -((lon-lonc)/(rlon))**2 -((lat-latc)/(rlat))**2)

    write(22,*) lon,lat,slp(i,j)

  end do !i

end do !j

INQUIRE (IOLENGTH=nrec) slp
print *,'nrec   =',nrec
print *,'nx*ny*4=',nx*ny*4

open(21,file=ofle,form="unformatted",&
access="direct",recl=nrec)

irec=1
write(21,rec=irec)slp
close(21)


print *
print '(A,A)','OUTPUT: ',trim(ofle)
print '(A,A)','OUTPUT FOR CHECKING: ',trim(ofle_chk)

print *

end program xy2d
```

`$ ifort xy2d.f90 -o xy2d.exe`

`$ ll xy2d.exe`
-rwxrwxr-x 1 am am 14K  8月 22 14:55 xy2d.exe

`$ xy2d.exe`
 nrec   =      259200
 nx*ny*4=      259200

OUTPUT: xy2d.bin
OUTPUT FOR CHECKING: xy2d.chk.txt

`$ cat xy2d.ctl`
```
dset ^xy2d.bin
TITLE XY2D TEST DATA
UNDEF -1.0E30
XDEF 360 LINEAR  0.50 1.0
YDEF 180 LINEAR -89.5 1.0
ZDEF 1 LEVELS 1
TDEF 1 LINEAR 00z01AUG2019 12hr
VARS 1
slp  1 99 SEA LEVEL PRESSURE hPa
ENDVARS
```

`$ cat xy2d.gs`
```grads
'open xy2d.ctl'

ofle='xy2d.slp.png'

say
'q file'
say result
say

'cc'
'set gxout shaded'
'd slp'
'cbarn'

'gxprint 'ofle

say
say 'OUTPUT:'
'!ls -lh 'ofle
say

'allclose'
'quit'
```
`$ grads -bcp "xy2d.gs"`

File 1 : XY2D TEST DATA
  Descriptor: xy2d.ctl
  Binary: xy2d.bin
  Type = Gridded
  Xsize = 360  Ysize = 180  Zsize = 1  Tsize = 1  Esize = 1
  Number of Variables = 1
     slp  1  99  SEA LEVEL PRESSURE hPa



OUTPUT:
-rw-rw-r-- 1 am am 98K  8月 22 15:04 xy2d.slp.png

closing 1 files
GX Package Terminated 



#### 注意

GrADS独特の引用符`'`のつけ方に注意  
`'gxprint 'ofle`  
`'!ls -lh 'ofle ` 

ofleはGrADSスクリプト (xy2.gs)の中でのみ有効な変数名として認識されている。この場合は、引用符で**囲まない**。



-----

#### コラム: ガウス分布について

- https://openbook4.me/sections/1538

- http://www.eng.niigata-u.ac.jp/~nomoto/7.html

-----





### 定点の時系列データ

- x,y,z方向の次元=1  
- 時間のみ変化する。  
- データ数は4種類  

```fortran
bin=trim(odir)//'/'//trim(bfle)
INQUIRE (IOLENGTH=nrec) A1(1)

open(22,file=bin,form="unformatted",access="direct",recl=nrec)
print *,'nrec   =',nrec
print *,'1*1*4=',4
irec=0
do i=1,365
irec=irec+1
write(22,rec=irec)A1(i)
irec=irec+1
write(22,rec=irec)A2(i)
irec=irec+1
write(22,rec=irec)A3(i)
irec=irec+1
write(22,rec=irec)A4(i)
enddo !i
```

```bash
f90=ifort
opt="-CB -traceback -fpe0 -convert big_endian -assume byterecl"
src=HOGE.f90
exe=$(basename $src .f90).exe
${f90} ${opt} ${src} -o ${exe}
```

```
dset ^AGO_WANOU_DAY_MEAN_SEASON_SMOOTH.bin
title DATA_FILE
options big_endian
undef -999.00
xdef 1 levels 130
ydef 1 levels 32
zdef 1 levels 1000
tdef 365 linear 1JAN1700 1DY
vars 4
A1 1 0 T0.5m
A2 1 0 T2.0m
A3 1 0 T5.0m
A4 1 0 T8.0m
endvars
```



定点のデータ  

鉛直方向に複数個のデータがある場合  

```bash
print *,'OUTPUT BIN'
bin=trim(odir)//'/'//trim(bfle)
INQUIRE (IOLENGTH=nrec) XC1(1)

open(22,file=bin,form="unformatted",access="direct",recl=nrec)
print *,'nrec   =',nrec
print *,'1*1*4=',4
irec=0
do i=1,nt
irec=irec+1
write(22,rec=irec)XC1(i)
irec=irec+1
write(22,rec=irec)XC2(i)
irec=irec+1
write(22,rec=irec)XC3(i)
irec=irec+1
write(22,rec=irec)XC4(i)
```



```bash
dset ^${BFLE}
title DATA_FILE
options big_endian
undef -999.00
xdef 1 levels 130
ydef 1 levels 32
zdef 4 levels 0.5 2.0 5.0 8.0
tdef 157800 linear 1JAN2003 1HR
vars 1
XCS 4 0 T CLIM SMO
endvars
```





GrADSで作成したバイナリデータをFortranで読む
---------------------------------------------------------------------

### x,y,z方向1次元で時間の次元だけ変化する  
/work05/manda/20.AGO_WAN/04-01.OISST.2/09-07.OISST_SMO3MO.1983-2020  
```
'q dims'
say result
say

say
say 'BINARY OUT'
say
'set gxout fwrite'
'set fwrite -be ${BIN}'
'd SSTAV'

```
```Fortran
!Default file number is: 1 
!X is fixed     Lon = 136.875  X = 109
!Y is fixed     Lat = 34.125  Y = 98
!Z is fixed     Lev = 0  Z = 1
!T is varying   Time = 00Z01JAN1983 to 23Z31DEC2020  T = 1 to 13881
!E is fixed     Ens = 1  E = 1

CHARACTER*100 INFLE

INFLE="PL_TSR_OI-AGO_O_AAVE_136.3_137.05_33.9_34.1_1983-2020.BIN"

OPEN(11,FILE=INFLE,ACTION='READ',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=4)


DO I=1,13881 !365*(2020-1983)
IREC=I
READ(11,REC=IREC) A
PRINT *,A
END DO

CLOSE(11)

END PROGRAM CHECK_BIN
```

BIG ENDIAN  
ENDIANについて  
https://www.uquest.co.jp/embedded/learning/lecture05.html  
IFORT COMPILE OPTION  

```
set FC = "ifort"
set COPT = "-convert big_endian -assume byterecl"
```
GrADS CTL FILE
```bash
OPTIONS TEMPLATE byteswapped
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
