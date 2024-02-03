NCL TIPS
===============
[[_TOC_]]
シェルスクリプトからnclスクリプトを呼び出す 
--------------------------------------------
### shell.sh
```
#!/bin/sh

exe=runncl.sh
ncl=$(basename $0 .sh).ncl

$exe $ncl "Hello!"


exit 0
```

### shell.ncl
```
begin

wallClock1 = systemfunc("date -R") ; retrieve wall clock time

scriptname_in = getenv("NCL_ARG_1")
scriptname=systemfunc("basename " + scriptname_in + " .ncl")
arg    = getenv("NCL_ARG_2")

print("script name ="+scriptname_in)
print("ARG         ="+arg)
print("")


wallClock2 = systemfunc("date -R") ; retrieve wall clock time

print("DONE "+scriptname_in+".")
print("")
print("STARTED : "+wallClock1)
print("ENDED   : "+wallClock2)
print("")

end
```

### runncl.sh
```
#!/bin/bash
#
# Universal wrapper script for ncl. 
# Pass arguments from the command line to environment variables
#
# version 0.1, Thierry Corti, C2SM ETH Zurich
# 

E_BADARGS=65

if [ ! -n "$1" ]
then
  echo "Usage: `basename $0` script.ncl argument1 argument2 etc."
  exit $E_BADARGS
fi  

# save number of arguments to environment variable NCL_N_ARG
export NCL_N_ARGS=$#

# save command line arguments to environment variable NCL_ARG_#
for ((index=1; index<=$#; index++))
do
  eval export NCL_ARG_$index=\$$index
done   

# run ncl
ncl -Q -n $1
```



### 引数の有無で処理を分ける

```ncl
A    = getenv("NCL_ARG_2")
if (ismissing(A) .eq. True)then
A="01"
end if
print("A="+A)
```

#### 引数無し

```
$ ncl SCRIPT.ncl
A=01
```

#### 引数有 

(引数は55)

```
$ ncl SCRIPT.ncl 55
A=55
```

注：getenvで読んだ変数(この場合A)は文字型変数になるので注意。数字として扱う際は，toint, tofloat, todouble等で変換する。

namelistファイルを使う
--------------------------------------

### nml.sh
```
#!/bin/sh
export LANG=C

exe=runncl.sh
nml=$(basename $0 .sh).nml
ncl=$(basename $0 .sh).ncl

indir=INPUT
wrfout_time="2017-07-03_12:00:00"
domain=d03
wrfout=wrfout_${domain}_${wrfout_time}
sdatetime="2017-07-05_00:00:00"
edatetime="2017-07-05_12:00:00"

runname=RUNNAME

cat<<EOF>$nml
runname =${runname}
indir =${indir}
domain =${domain}
wrfout =${wrfout}
sdatetime =${sdatetime}
edatetime =${edatetime}
EOF

$exe $ncl "$nml"


exit 0
```

### nml.ncl
```
begin

wallClock1 = systemfunc("date -R") ; retrieve wall clock time

scriptname_in = getenv("NCL_ARG_1")
scriptname=systemfunc("basename " + scriptname_in + " .ncl")
nml    = getenv("NCL_ARG_2")

print("script name ="+scriptname_in)
print("namelist    ="+nml)
print("")

print("READ NAMELIST FILE")

runname=systemfunc("grep runname "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
indir=systemfunc("grep indir "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
domain=systemfunc("grep domain "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
wrfout=systemfunc("grep wrfout "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
sdatetime=systemfunc("grep sdatetime "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
edatetime=systemfunc("grep edatetime "+nml+ "|cut -f2 -d'='|cut -f1 -d','")

print("DONE.")
print("")

wallClock2 = systemfunc("date -R") ; retrieve wall clock time


print("CHECK NAMELIST FILE")

print("runname:   "+runname)
print("indir:     "+indir)
print("wrfout:    "+wrfout )
print("sdatetime: "+sdatetime)
print("edatetime: "+edatetime)
print("")

print("DONE "+scriptname_in+".")
print("")
print("STARTED : "+wallClock1)
print("ENDED   : "+wallClock2)
print("")

end
```



FORTRAN77のサブルーチンを呼び出す
--------------------------------------

https://www.ncl.ucar.edu/Document/Tools/WRAPIT.shtml

```bash
$ WRAPIT wrapit_lib.f
 
WRAPIT Version: 120209
COMPILING wrapit_lib.f
LINKING
END WRAPIT
 
$ ncl -nQ wrapit.ncl
HELLO!
   5.0000000   
```
### wrapit.ncl
```ncl
external wrapit_lib "./wrapit_lib.so"

begin

text="HELLO!"
arg=5.0

wrapit_lib::F77SUB(text,arg)

end
```

### wrapit_lib.f
```
C NCLFORTSTART
      SUBROUTINE F77SUB(TEXT,ARG)
      implicit none
      character*(*) TEXT
      real ARG
C NCLEND

      write(6,'(A)')TEXT
      write(6,*    )ARG
      RETURN
      end
```



FORTRAN90のサブルーチンを呼び出す
--------------------------------------

https://www.ncl.ucar.edu/Document/Tools/WRAPIT.shtml




図にヘッダーを入れる
--------------------------------------
```
figfile="FIG"
type="ps"
wks = gsn_open_wks(type,figfile)
```
```
xh=0.1   ;Left of header lines
yh=0.9   ;Top of header lines
dyh=0.02 ;Line spacing
fh=0.01  ;Font Height

txres               = True
txres@txFontHeightF = fh

txres@txJust="CenterLeft"

today = systemfunc("date -R")
cwd =systemfunc("pwd")
lst = systemfunc("ls -lh --time-style=long-iso "+infle)


gsn_text_ndc(wks,today,  xh,yh,txres)
yh=yh-dyh
gsn_text_ndc(wks,"Current dir: "+cwd,      xh,yh,txres)

yh=yh-dyh
gsn_text_ndc(wks,lst,xh,yh,txres)

yh=yh-dyh
gsn_text_ndc(wks,"Output: "+figfile+"."+type, xh,yh,txres)
```



## データをNetCDFに書き出す

NetCDFでの書き出しも，読み込みと同様にすることでできる。つまり，まずは書き出したいファイルをaddfileを用いて指定する。
例えば，ファイル名をoutfileとすると，新しいファイルの作成にはオプション"c"を指定することに注意して，

```bash
out = addfile(outfile,"c")
```

とする。スクリプト内の変数uをuという変数名で，変数vをvという変数名で書き出したいときには，

```bash
out->u = u
out->v = v
```

とすれば書き込みできる。

スクリプト内の変数名とファイルでの変数名を変えることも可能である。

**u**,**v**が**スクリプトの中**で使用している変数の名称，<u>ucomp</u>, <u>vcomp</u>が<u>書き込み先のファイルの中</u>での変数の名称とすると，下記のようになる。

```
out->ucomp = u
out->vcomp = v
```



以上が最も簡便ななやり方である。しかし，ファイルや変数，座標に対して細かい指定をすることが必要な場合があり，その場合には以下の例2のような方法を使う。また，こちらのやり方のほうが速いらしい。

例2： 時間time[長さNT]，緯度lat[長さNY]，経度lon[長さNX]の座標をもつ3次元配列u，vがあるとする。これをwind.ncというファイルに書き込む。まずは，ファイルを開く。



```fortran
system("rm -f wind.nc")           ;; すでにそのファイルがあれば消す
out = addfile("wind.nc","c")
```

ファイルの各変数が持っている座標変数の名前やサイズの配列を作っておく。

```fortran
dimNames = (/"time","lat","lon"/)
dimSizes = (/NT,NY,NX/)
dimUnlim = (/True,False,False/)   ;; timeには配列に無限長(unlimited)を持たせている
setfileoption?でNetCDFの書き込みオプション"DefineMode"をTrueに指定する。
setfileoption(out,"DefineMode",True)
```

ファイルそのものにつく属性等を指定する。

```
fatts = True
fatts@creation_date = systemfunc("date")  ;; dateコマンドの戻り値
fileattdef(out,fatts)
```

ファイルに含まれる座標変数を指定する。

```fortran
filedimdef(out,dimNames,dimSizes,dimUnlim)
```

ファイルに含まれる全ての変数を指定する。

```fortran
filevardef(out,"time",typeof(time),getvardims(time))
filevardef(out,"lat",typeof(lat),  getvardims(lat))
filevardef(out,"lon",typeof(lon),  getvardims(lon))
filevardef(out,"ucomp",typeof(u),  getvardims(u))   ;; 変数uをucompという名前で書くことにする
filevardef(out,"vcomp",typeof(v),  getvardims(v))
```

変数のattributeを指定する。

```fortran
filevarattdef(out,"time", time)
filevarattdef(out,"lat",  lat)
filevarattdef(out,"lon",  lon)
filevarattdef(out,"ucomp",u)
filevarattdef(out,"vcomp",v)
```

値を書き込む(値のみの代入を行う)。

```fortran
out->time  = (/time/)
out->lat   = (/lat/)
out->lon   = (/lon/)
out->ucomp = (/u/)
out->vcomp = (/v/)
```


なお，デフォルトでは2GB以上のファイルを書き出すことができないと思われる。大きなNetCDFファイルを作成したい場合には，あらかじめ，setfileoptionでNetCDFの"Format"オプションを"LargeFile"に指定する。

```fortran
setfileoption("nc","Format","LargeFile")
```





## データをバイナリで書き出す

### 直接アクセス方式で書き出す

```
print("### BINARY")
OFLE="OUT.bin"
do n=0,nt-1
 fbindirwrite(OFLE,AAV(n,:) )
end do
```

;
; https://www.ncl.ucar.edu/Document/Functions/Built-in/fbindirwrite.shtml
; The binary file will be the same as one opened via Fortran's open statement:
; open(..., form="unformatted", access="direct", recl= )   
; recl is system dependent  

#### 直接アクセス形式について

http://www.it-shikaku.jp/top30.php?hidari=05-03-03.php&migi=km05-03.php

### binaryファイル読み書き

Direct Access:

読み出し            data = fbindirread(path,rec,dim,type)

書き出し fbindirwrite; 

追加(append)で書き出されるので，あらかじめ出力するべきファイルを消去する操作をしておくほうが安全．

#### 書き出し

```
x1=new((/5,10/),float)
x2=x1
do j=0,4
  do i=0,9
    x1(j,i)=j*10+i
    x2(j,i)=x1(j,i)+100
  end do
end do

wrfl="./test.bin"
system("rm -f "+wrfl); あらかじめ消去しておく
fbindirwrite(wrfl,x1)
fbindirwrite(wrfl,x2)
```



#### 読み出し

```
rdfl="./test.bin"
rec=0
dim=(/5,10/)
x1=fbindirread(rdfl,rec,dim,"float")
rdfl="./test.bin"
rec=0; 読み出しレコード開始位置，配列のサイズが単位，ゼロ始り
dim=(/5,10/)
x1=fbindirread(rdfl,rec,dim,"float")
rec=1
x2=fbindirread(rdfl,rec,dim,"float")
```



### GrADSで描画

/work03/am/2021.BARENTS.WARMING/06.00.00.WRF.BOSAI.VALIDATE/03.00.00.T-Z.DAT.CREATE.BY.NCL

#### CTLファイルの例

```
dset ^OUT.bin
TITLE ERA5 pressure level
options zrev
UNDEF  1.e+20f
XDEF 1 linear 1 1
YDEF 1 linear 1 1
ZDEF 20 levels 1000 975 950 925 900 875 850 825 800 775 750 700 650 600 550 500 450 400 350 300
TDEF 248 linear 00Z01JAN2006 180mn
VARS 1
TAAV 20 99  AAV T
ENDVARS
```

#### 参考：GrADSスクリプトの一部

```
'set x 1' ;# lon ${LONW}'
'set y 1' ;# lat ${LATS}'
'set lev ${LEV}'
'set time ${TIME}'
```

```
'set zlog on'
```


## テキストで書き出す
```
; Check data
do n=0,nt-1
  print(stimes(n)+" "+hfx_i(n)+" "+lh_i(n)+" "+sst_i(n)+" "+t2_i(n)\
        +" "+q2_i(n)+" "+Ws10_i(n)+" "+psfc_i(n))
end do

; Output file
ofle="table.out.txt"

;Header line
header = (/\
"# Input: "+ infile1, \
"# Input: "+ infle2 , \
"# datetime            hfx       lh        sst         T2         Q2     Ws10        PSFC"/)
hlist=[/header/]

; Create rows in table
alist= [/stimes, hfx_i, lh_i, sst_i, t2_i, q2_i, Ws10_i, psfc_i/]

; Output the table
write_table(ofle, "w", hlist, "%s")
write_table(ofle, "a", alist, "%s %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f")
```


## トラブル・シューティング

### 緯度・経度の単位の指定

```
check_for_y_lat_coord: Warning: Data either does not contain
a valid latitude coordinate array or doesn't contain one at all.
A valid latitude coordinate array should have a 'units'
attribute equal to one of the following values: 
    'degrees_north' 'degrees-north' 'degree_north' 'degrees north' 'degrees_N' 'Degrees_north' 'degree_N' 'degreeN' 'degreesN' 'deg north'
```

#### 緯度経度の単位を指定してもエラーが出る場合がある

```
lon@units="degrees_east"
lat@units="degrees_north"
```

#### 原因

```
plot = gsn_csm_contour_map_ce(wks,sst-273.15,opts)
```

演算を行うと（ここでは，`sst-273.15`），単位に関する情報が消えることがある。

#### 対策

```
sst=  short2flt(f->analysed_sst(0,:,:))-273.15
lon = f->lon
lat = f->lat
lon@units="degrees_east"
lat@units="degrees_north"
```

演算を行ってから，単位に関する情報を付加する。
