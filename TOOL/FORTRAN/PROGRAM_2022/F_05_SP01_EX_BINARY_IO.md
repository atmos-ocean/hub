# データ構造とバイナリファイルの入出力

[[_TOC_]]

## はじめに

近年，デジタル化されたデータをコンピューターを使って解析する機会が非常に多くなっている（**データ・サイエンス**）。デジタルデータを解析する際に，**まず最初にやるべきことは，データの構造を把握することである**。

おおまかにいえば，データの構造とは**ファイルの中のデータの並べ方**のことである。

ここでは，気象学・海洋学における典型的なデータ構造を解説するとともに，そのような構造をもつデータのFORTRANでの読み書きについて練習する。

ここでは，最も単純な形式のプレーンバイナリ (plain binary)という種類のファイルを扱う。

プレーンバイナリファイルの基礎知識については下記で学んだ。

https://gitlab.com/infoaofd/lab/-/blob/master/FORTRAN/PROGRAM_2022/F_05_BINARY_FILE.md

**プレーンバイナリの読み書きは，GrADSではできない（もしくは非常に時間を要する）作業をプログラミングで処理する際の基礎となる**。



## 典型的なデータ構造

ここでは，気象学・海洋学における典型的なデータ構造を説明する。

### 参考文献

この節 (典型的なデータ構造)の内容に関しての詳細については，下記の文献が参考になる。

​	広田 勇, 1999. 気象解析学-観測データの表現論. 東京大学出版会, 184pp.

​	松山 洋, 谷本 陽一, 2008. UNIX/Windows/Macintoshを使った実践 気候データ解析―気候学・気象学・海洋学	などの報告書・論文を書く人が知っておきたい3つのポイント. ‎ 古今書院, 118pp.

上記文献は研究室の書架にある。

### 記号の約束

便宜上下記の約束で記号を用いる。  

```fortran
i, j, k: 空間座標もしくは，空間座標を表す配列番号
```

```fortran
m: 時刻もしくは，時刻を表す配列番号
```

l (アルファベットのエル)は数字の1(いち)と見間違えることがあるので，ここでは使わない。  



### 1次元データ

下記のような並びのデータを1次元データと呼ぶ。  

```fortran
x(1), x(2), ... 
```

このタイプのデータの例として，**時系列データ**，**鉛直プロファイル**, **測線上のデータ**がある。  



#### 時系列データ

mを時刻を表し，xを何かの測定量としたとき, xがmの関数として，  

```fortran
x(1), x(2), x(3), ..., x(m), ...
```

のように表される場合，xのことを時系列データと呼ぶ。  



#### 鉛直プロファイル

kが高度（もしくは気圧・水圧）を表し，xを何かの測定量としたとき, xがkの関数として，  

```fortran
x(1), x(2), x(3), ..., x(k), ...
```

のように表される場合，xのことを**鉛直プロファイル** (vertical profile)と呼ぶ。  



#### 測線上のデータ

地図上の2地点間を線分で結び，その線分上の何点かに測点を設け，測定を行ったとする。     

iを測点番号，xを何かの測定量としたとき, xがiの関数として，  

```fortran
x(1), x(2), x(3), ..., x(i), ...
```

表すことができる。      

この種のデータに特別な名称はないが，ここでは測線上のデータと呼ぶことにする。  



### 2次元データ

下記のような並びのデータのことを2次元データと呼ぶ  

```fortran
x(1,1), x(1,2), ... 
x(2,1), x(2,2), ...
...
```

このタイプのデータの例として，  

- 水平分布
- 時間‐緯度断面
-  時間‐経度断面
-  時間-高度断面

がある。  



#### 水平分布

例えば，i, jを経度，緯度として，ある固定された高度上のデータx(i, j)が得られたとき，このようなデータを水平分布と呼ぶ。  

実際には水平面上ではないが，地表面上のデータや等圧面上のデータも便宜的に水平分布と呼ぶことがある。  



#### 時間‐経度断面

**東西方向**に何点か測点を取って，各測点において一定期間の連続測定を行ったとする。この場合，測定されたデータxは，測点番号i, 時刻mの関数として，x (i, m)と表すことができる。このような種類のデータを**時間‐経度断面**と呼ぶことがある。  



#### 時間‐緯度断面

**南北方向**に何点か測点を取って，各測点において一定期間の連続測定を行ったとする。この場合，測定されたデータxは，測点番号j, 時刻nの関数として，x (j, m)と表すことができる。このような種類のデータを**時間‐緯度断面**と呼ぶことがある。



#### 時間-高度断面

**測定点を固定**して，一定期間の連続測定を行ったとする。この場合，測定されたデータxは，高度k, 時刻mの関数として，x (k, m)と表すことができる。このような種類のデータを**時間‐緯度断面**と呼ぶことがある。



### 3次元データ

下記のような並びのデータのことを3次元データと呼ぶ

```fortran
x(1,1,1), x(1,2,1), ... 
x(2,1,1), x(2,2,1), ...
...
x(1,1,2), x(1,2,2), ... 
x(2,1,2), x(2,2,2), ...
...
```

このタイプのデータの例として，**空間3次元の分布**, **水平分布の時間変化**がある。



#### 空間3次元の分布

i, jを経度，緯度，kを高度として，ある時刻において測定された量がi, j, kの関数として，x(i, j, k)と表されたとする。このようなデータに対する特別な名称はないが，ここでは便宜上**空間3次元の分布**と呼ぶ。



#### 水平分布の時間変化

i, jを経度，緯度として，ある時刻mにおいて測定された量がi, j, mの関数として，x(i, j, m)と表されたとする。このようなデータに対する特別な名称はないが，ここでは便宜上**水平分布の時間変化**と呼ぶ。



### 4次元データ

ここから先は少々抽象的になっていくが，**空間3次元の分布が基本になることが多い**。下記のような並びのデータのことを4次元データと呼ぶ

```fortran
x(1,1,1,1), x(1,2,1,1), ... 
x(2,1,1,1), x(2,2,1,1), ...
...
x(1,1,2,1), x(1,2,2,1), ... 
x(2,1,2,1), x(2,2,2,1), ...
...
...
x(1,1,1,2), x(1,2,1,2), ... 
x(2,1,1,2), x(2,2,1,2), ...

```

このタイプのデータの例として，**空間3次元分布の時間変化**がある。

i, j, kを経度，緯度, 高度として，ある時刻mにおいて測定された量がi, j, k, mの関数として，x(i, j, k, m)と表されたとする。このようなデータに対する特別な名称はないが，ここでは便宜上, 空間3次元分布の時間変化と呼ぶ。



### 5次元データ

コンピューターシミュレーションなどで，計算条件をいくつか変えて計算を行い，結果の違いを調べることがよくある（**アンサンブル・シミュレーション**）。

この場合，i, j, k, mを経度, 緯度, 高度, 時刻として，nを実験番号とすると，シミュレーションで得られる量xはi, j, k, m, nの関数として，x(i, j, k, m, n)と表すことができる。

いまのところ (2022年現在), 気象・海洋学で5次元以上の大きい次元のデータを扱う機会はそれほど多くない。



### 次元の縮約

3次元以上の次元を持つデータは，紙や画面上に表現するのが難しくなるとともに，人間がデータの内容を理解するのが難しくなる。そのため，3次元以上のデータに関しては一部の次元を固定することで, データの次元を小さくした上で作図等を行うことが多い。

例えば，i, jを経度, 緯度，kを高度とする空間3次元のデータ，x(i, j, k)に関して，**高度kを固定する**ことにより，**水平分布** x(i, j, k=一定)**を得る**ことができる。

i, j, k, mを経度，緯度, 高度, 時刻とする4次元のデータ, x(i, j, k, m)に関して, **経度i, 緯度jを固定する**ことにより，**時間‐高度断面**, x (i=一定, j=一定, k, m)**を得る**ことができる。

一部の次元を固定することでデータの次元を下げることを，**次元の縮約**と呼ぶことがある。次元の縮約は高次元のデータを解析する際に重要となる手法である。  

詳細は下記の文献を参照のこと。  

広田 勇, 1999. 気象解析学-観測データの表現論. 東京大学出版会, 184pp.



## GrADSのコントロールファイル

気象学・海洋学では，データの描画にはGrADSという操作が簡便なソフトウェアがよく使われる。

GrADSでは描画したいデータの構造を，コントロールファイル (**CTLファイル**)と呼ばれるテキストファイルを用いて表現する。ここでは，CTLファイルの書式について説明する。[BINARY_DATA_GRADS.pdf](./BINARY_DATA_GRADS.pdf)も参照のこと。



### CTLファイルのサンプル

```bash
dset ^TEST_DATA.BIN
title test Data
undef 1.0e20
xdef 3 linear  150 0.5
ydef 2 linear   30 0.5
zdef 3 levels 1000 850 500
tdef 5 linear 00Z01NOV2022 1HR
vars 2
x2D 0 99 TEST_VAR_2D UNIT_x2D
x3D 3 99 TEST_VAR_3D UNIT_x3D
endvars
```

#### 記号の説明

##### dset

`dset`: バイナリファイルの名前

`^`: カレント・ディレクトリ (自分が現在いるディレクトリ）を意味する記号  
この場合したがって，CTLファイルとデータファイルが同じ場所に保存されていることを仮定している      

^は実際にデータがあるディレクトリに書き換えることができる    

例えば，  

/work01/DATA/にファイル (TEST_DATA.BIN)がある場合，  

```
dset /work01/DATA/TEST_DATA.BIN
```

とすればよい。  

##### title

データの名称や説明。ユーザーが自由に記述してよい    

##### undef

`undef 1.e20`: 値が存在しない場所には，ダミーの値として10の20乗が入っている      

##### xdef

`xdef`: 東西方向のデータ並びに関する情報  

```
xdef 3 linear  150 0.5
```

 上の例では，東西方向のデータの数は3個で，等間隔でデータが並び (`linear`), 西端の点の経度は東経150度で，隣り合うデータの間隔は0.5度であることを意味する。西経は負の数で表す。  

##### ydef  

`ydef`: 南北方向のデータ並びに関する情報  

```
ydef 2 linear   30 0.5  
```

 上の例では，南北方向のデータの数は2個で，等間隔でデータが並び (`linear`), 南端の点の緯度は北緯30度で，隣り合うデータの間隔は0.5度であることを意味する。南緯は負の数で表す。  

##### zdef  

`zdef`: 鉛直方向のデータ数    

```
zdef 3 levels 1000 850 500    
```

上の例では，鉛直方向のデータの数は3個で，不等間隔にデータが並び (`levels`)，鉛直方向の座標は下から順に, 1000, 850, 500となっている（この場合，気圧を鉛直座標に採用している）。  

##### tdef  

`tdef`: 時間方向のデータ並びに関する情報    

```
tdef 5 linear 00Z01NOV2022 1HR
```

  上の例では，時間方向のデータの個数は5個で等間隔にデータが並び，最初のデータの時刻は00Z01NOV2022で，1時間間隔にデータがあることを意味する。  

##### vars  

`vars`: 保存されている変数の数 

`vars`に続けて，変数の情報を書く。

```
vars 2
x2D 0 99 TEST_VAR_2D UNIT_x2D
x3D 3 99 TEST_VAR_3D UNIT_x3D
```

上の例では`vars`が2と設定されているので，2種類の変数があることを意味する。

以下，各変数に関する情報が記載されている。

x2Dというデータは，2次元のデータで（鉛直層の数＝0と指定する）, 変数の正式名称はTEST_VAR_2Dで，単位は，UNIT_x2Dである。

x3Dというデータは，3次元のデータで（鉛直層の数＝3と指定する）, 変数の正式名称はTEST_VAR_3Dで，単位は，UNIT_x3Dである。

**3次元のデータの鉛直層の数は，zdefで指定した値と同じでなければならない**。

##### endvars

`endvars`: 変数に関する情報終わり（`endvars`を書くのを忘れるとエラーになる）。endvars**最後のsを忘れない**ようにする (変数の数が1個でもendvar**s**)。



## FORTRANによる入出力の例

ここでは，利用頻度の高い4次元データまで練習する。

以降，1～4次元のデータについて

- FORTRANで疑似データ作成
- GrADSのCTLファイルの例
- 作図用のGrADSスクリプトの例
- スクリプトの実行例

の順に説明する。



### 準備

#### コンパイラの準備

インテルフォートラン用の環境を準備

```bash
$ ift
```

もしくは，

```bash
$ source /opt/intel/oneapi/setvars.sh
```

#### ディレクトリ作成

作業用のディレクトリを作成して，そのディレクトリに移動する

```bash
$ mkdir -vp $HOME/2022_PROGRAM/F_05_SP01_BINARY_IO
```

```bash
$ cd $HOME/2022_PROGRAM/F_05_SP01_BINARY_IO
```

```bash
$ pwd
```



### 1次元データ

#### ディレクトリ作成

```bash
$ mkdir -vp 01.1D
```

```bash
$ cd 01.1D
```

```bash
$ pwd
```



#### データの作成と出力

##### FORTRANプログラム

$ vi 01.1D.F90

```FORTRAN
CHARACTER OUT*1000

REAL,ALLOCATABLE,DIMENSION(:)::X

REAL,PARAMETER::UNDEF=1.E20

MM=5

ALLOCATE(X(MM))

X(1)=1; X(2)=2; X(3)=3; X(4)=2; X(5)=1

OUT="01_1D.BIN"

OPEN(21,FILE=OUT,FORM='UNFORMATTED',ACCESS='DIRECT',&
RECL=4)

IREC=1
DO M=1,MM
WRITE(21,REC=IREC) X(M)
IREC=IREC+1
END DO !M

CLOSE(21)

PRINT *; PRINT '(A,A)','OUTPUT: ',TRIM(OUT); PRINT *

END
```

##### コンパイル

```bash
$ ifort -assume byterecl 01.1D.F90 -o 01.1D.EXE
```

##### 実行

```bash
$ ll 01.1D.EXE
```

```bash
$ 01.1D.EXE
```

OUTPUT: 01_1D.BIN

##### CTLファイルの例

```bash
$ cat 01_1D.CTL
```

```
dset ^01_1D.BIN
title test 1D Data
undef 1.0e20
xdef 1 linear  137 1 
ydef 1 linear   33 1
zdef 1 levels 1000
tdef 1 linear 00Z01NOV2022 1HR
vars 1
x 0 99 TEST_VAR
endvars
```

##### スクリプトの例

```bash
$ vi 01_1D.GS
```

```bash
'open 01_1D.CTL'

say; 'q ctlinfo'; say result

'set time 00Z01NOV2022 05Z01NOV2022'
#'set t 1 5'

say; 'q dims'; say result; say

'cc'
'set grads off'; 'set grid off'
'set ylint 1'

'd x'

FIG='01_1D.eps'
'gxprint 'FIG

say 'OUTPUT: 'FIG; say

'quit'
```

##### 作図例

```bash
$ grads -bcp "01_1D.GS"
```



#### データの読み込み例

##### FORTRANプログラム

```bash
$ vi 01.1D_READ.F90 
```

```fortran
CHARACTER IN*1000

REAL,ALLOCATABLE,DIMENSION(:)::X

REAL,PARAMETER::UNDEF=1.E20

MM=5

ALLOCATE(X(MM))

IN="01_1D.BIN"

OPEN(21,FILE=IN,FORM='UNFORMATTED',ACTION='READ', \
ACCESS='DIRECT',RECL=4)

IREC=1
DO M=1,MM
READ(21,REC=IREC) X(M)
IREC=IREC+1
END DO !M

CLOSE(21)

PRINT *; PRINT '(A,A)','INPUT: ',TRIM(IN); PRINT *

DO M=1,MM
WRITE(*,'(i3,f8.2)'), M,X(M)
END DO !M

END
```

##### コンパイル

```bash
$ ifort -assume byterecl 01.1D_READ.F90 -o 01.1D_READ.EXE
```

##### 実行

```bash
$ ll 01.1D_READ.EXE
```

-rwxr-xr-x. 1 am 850K 2022-11-05 11:17 01.1D_READ.EXE*

```bash
$ 01.1D_READ.EXE
```

```bash
INPUT: 01_1D.BIN

  1    1.00
  2    2.00
  3    3.00
  4    2.00
  5    1.00
```



## 2次元データ

#### ディレクトリ作成
```bash
$ cd $HOME/2022_PROGRAM/F_05_SP01_BINARY_IO
```
```bash
$ mkdir -vp 02.2D
```
```bash
$ cd 02.2D
```



#### データの作成と出力

##### FORTRANプログラム

```
$ vi 02.2D.F90
```

```fortran
CHARACTER OUT*1000

REAL,ALLOCATABLE,DIMENSION(:,:)::X

REAL,PARAMETER::UNDEF=1.E20

IM=3; JM=2

ALLOCATE(X(IM,JM))

X(1,1)=11; X(1,2)=12
X(2,1)=21; X(2,2)=22
X(3,1)=31; X(3,2)=32

OUT="02_2D.BIN"

OPEN(21,FILE=OUT,FORM='UNFORMATTED',ACCESS='DIRECT',&
RECL=4*IM*JM)

IREC=1
WRITE(21,REC=IREC) X

CLOSE(21)

PRINT *; PRINT '(A,A)','OUTPUT: ',TRIM(OUT); PRINT *

END
```



##### コンパイル

```bash
$ ifort -assume byterecl 02.2D.F90 -o 02.2D.EXE
```



##### 実行

```bash
$ 02.2D.EXE 
```

OUTPUT: 02_2D.BIN

##### CTLファイルの例

```bash
$ vi 02_2D.CTL 
```

```bash
dset ^02_2D.BIN
title test 2D Data
undef 1.0e20
xdef 3 linear  150 1
ydef 2 linear   30 1
zdef 1 levels 1000
tdef 1 linear 00Z01NOV2022 1HR
vars 1
x 0 99 TEST_VAR
endvars
```



##### スクリプトの例

```bash
$ vi 02_2D.GS
```

```bash
'open 02_2D.CTL'

say; 'q ctlinfo'; say result

'set time 00Z01NOV2022'
#'set t 1 1'

say; 'q dims'; say result; say

'cc'
'set grads off'; 'set grid off'
'set xlint 1'; 'set ylint 1'

'set gxout grid'
'set lon 149.5 152.5'; 'set lat 29.5 31.5'
'd x'

FIG='02_2D.eps'
'gxprint 'FIG

say 'OUTPUT: 'FIG; say

'quit'
```

##### 作図例

```bash
$ grads -bcp "02_2D.GS"
```

##### 注意

i=1: 南端, i=3: 北端となっていることに注意。

GrADSは南からデータが入っていることを仮定している。北からデータが入っているときは，CTLファイルの中に，

```bash
options yrev
```

という行を追加しておく。



#### データの読み込み例
##### FORTRANプログラム
```bash
$ vi 02.2D_READ.F90
```

```fortran
CHARACTER IN*1000

REAL,ALLOCATABLE,DIMENSION(:,:)::X

REAL,PARAMETER::UNDEF=1.E20

IM=3; JM=2

ALLOCATE(X(IM,JM))

IN="02_2D.BIN"

OPEN(21,FILE=IN,FORM='UNFORMATTED',ACCESS='DIRECT',&
RECL=4*IM*JM)

IREC=1
READ(21,REC=IREC) X

CLOSE(21)

PRINT *; PRINT '(A,A)','INPUT: ',TRIM(IN); PRINT *

DO I=1,IM
WRITE(*,'(2F8.1)') (X(I,J),J=1,JM)
END DO

END
```



##### コンパイル

```bash
$ ifort -assume byterecl 02.2D_READ.F90 -o 02.2D_READ.EXE
```

##### 実行

```
$ 02.2D_READ.EXE
```


INPUT: 02_2D.BIN

    11.0    12.0
    21.0    22.0
    31.0    32.0



------

### コラム レコード長の取得

READ, WRITE文のRECLで指定する数値のことをレコード長という。レコード長とは，ダイレクトアクセスでデータの読み書きをする場合，一回の操作で読み込む（書き込む）データの量を表している。

レコード長 は，一つの最小単位のデータ容量×データの数で計算できる。例えば，4バイト実数で配列要素数が5の配列a(5)をダイレクトアクセスで書き出すときのレコード長は，$4 \times 5$で20になる。

手計算ではミスが起こることもあるので，**レコード長の計算を自動化する関数**(`INQUIRE`)**が用意されている**。

**INQUIRE.F90**

```FORTRAN
REAL(KIND=4),DIMENSION(10) :: a = 3.141
INTEGER                    :: reclen

INQUIRE(iolength=reclen)a

PRINT *;PRINT *,'RECORD LENGTH OF a = ',reclen;PRINT *

OPEN(UNIT=10,FILE='INQUIRE.TEST.BIN',FORM='UNFORMATTED',&
     ACCESS='DIRECT',RECL=reclen)
WRITE(UNIT=10,REC=1)a
CLOSE(UNIT=10)
END
```

####　実行例

##### 準備

```
$ ift
```

もしくは,

```
source /opt/intel/oneapi/setvars.sh
```

##### コンパイルと実行

`-assume byterecl`オプションを付けた場合

```
$ ifort -assume byterecl -o INQUIRE.EXE INQUIRE.F90 
```

```
$ INQUIRE.EXE 

 RECORD LENGTH OF a =           40
```

Intel Fortran コンパイラのダイレクトアクセスのレコード長 の単位は, デフォルトでは「ワード(4バイト)」である. 

一般的な「バイト」単位でレコード長を指定するには, コンパイルオプション `-assume byterecl` を指定する必要がある.  

```bash
$ ls -lh INQUIRE.TEST.BIN 
-rw-r--r--. 1 am 40 11月 19 09:14 INQUIRE.TEST.BIN
```

出力ファイルのINQUIRE.TEST.BINのファイル容量が40 (バイト)になっていることに注意



オプション無しの場合

```
$ ifort -o INQUIRE.EXE INQUIRE.F90 
```

```
$ INQUIRE.EXE 

 RECORD LENGTH OF a =           10
```



#### プログラム解説

例えば，上のプログラムでは，

```FORTRAN
REAL(KIND=4),DIMENSION(5) :: a
```

で，4バイト実数で配列要素数が5の配列a(5)を用意し，

```FORTRAN
a(:)=1.0
```

で，aの全ての要素の値を1.0に設定する。`a(:)`は，すべての配列要素を意味する。

このaという配列要素のレコード長を取得するためには，

```FORTRAN
INQUIRE(iolength=reclen)a
```

とすればよい。

(コラム終わり)

------



## 3次元データ

#### ディレクトリ作成

```bash
$ cd $HOME/2022_PROGRAM/F_05_SP01_BINARY_IO
```
```bash
$ mkdir -vp 03.3D
```
```bash
$ cd 03.3D
```


#### データの作成と出力
##### FORTRANプログラム
```
$ vi 03.3D.F90
```

```fortran
CHARACTER OUT*1000

REAL,ALLOCATABLE,DIMENSION(:,:,:)::X

REAL,PARAMETER::UNDEF=1.E20

IM=3; JM=2; KM=2

ALLOCATE(X(IM,JM,KM))

X(1,1,1)=111; X(1,2,1)=121
X(2,1,1)=211; X(2,2,1)=221
X(3,1,1)=311; X(3,2,1)=321

X(1,1,2)=112; X(1,2,2)=122
X(2,1,2)=212; X(2,2,2)=222
X(3,1,2)=312; X(3,2,2)=322

OUT="03_3D.BIN"

OPEN(21,FILE=OUT,FORM='UNFORMATTED',ACCESS='DIRECT',&
RECL=4*IM*JM)

IREC=0
DO K=1,KM
IREC=IREC+1
WRITE(21,REC=IREC) X(:,:,K)
END DO !KM
CLOSE(21)

PRINT *; PRINT '(A,A)','OUTPUT: ',TRIM(OUT); PRINT *

END
```

**注**：3次元以上の次元のデータを，GrADSは**水平分布を一つの単位として扱う**。鉛直方向の複数層に渡ってデータが存在している場合，次のように各層ごとに書き出していく。

```fortran
IREC=0
DO K=1,KM
IREC=IREC+1
WRITE(21,REC=IREC) X(:,:,K)
END DO !KM
```

##### コンパイル

```bash
$ ifort -assume byterecl 03.3D.F90 -o 03.3D.EXE
```

##### 実行
```bash
$ 03.3D.EXE
```

##### CTLファイルの例

```
$ vi 03_3D.CTL
```

```bash
dset ^03_3D.BIN
title test 3D Data
undef 1.0e20
xdef 3 linear  150 1
ydef 2 linear   30 1
zdef 2 levels 1000 500
tdef 1 linear 00Z01NOV2022 1HR
vars 1
x 2 99 TEST_VAR
endvars
```



##### スクリプトの例

```
$ vi 03_3D.GS
```

```bash
'open 03_3D.CTL'

say; 'q ctlinfo'; say result

'set time 00Z01NOV2022'
#'set t 1 1'

say; 'q dims'; say result; say

'cc'

plev='1000'; say; say 'plev='plev

'mul 1 2 1 1 -xwid 2 -ywid 2 -yint 1' 
# http://kodama.fubuki.info/wiki/wiki.cgi/GrADS/script/mul.gs?lang=jp
 
'set grads off'; 'set grid off'
'set xlint 1'; 'set ylint 1'

'set gxout grid'
'set lon 149.5 152.5'; 'set lat 29.5 31.5'
'set lev 'plev
'd x'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=xl-0.7; yy=(yt+yb)/2
'set strsiz 0.14 0.16'; 'set string 1 c 3 90'
'draw string 'xx' 'yy' 'plev


plev='500'; say; say 'plev='plev

'mul 1 2 1 2 -xwid 2 -ywid 2 -yint 1' 
# http://kodama.fubuki.info/wiki/wiki.cgi/GrADS/script/mul.gs?lang=jp

'set grads off'; 'set grid off'
'set xlint 1'; 'set ylint 1'
'set lev 'plev
'd x'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=xl-0.7; yy=(yt+yb)/2
'set strsiz 0.14 0.16'; 'set string 1 c 3 90'
'draw string 'xx' 'yy' 'plev


FIG='03_3D.eps'
'gxprint 'FIG

say 'OUTPUT: 'FIG; say

'quit'
```

##### 作図例

```bash
$ grads -bcp "03_3D.GS"
```

OUTPUT: 03_3D.eps



#### データの読み込み例
##### FORTRANプログラム

```bash
$ vi 03.3D_READ.F90
```

```fortran
CHARACTER IN*1000

REAL,ALLOCATABLE,DIMENSION(:,:,:)::X

REAL,PARAMETER::UNDEF=1.E20

IM=3; JM=2; KM=2

ALLOCATE(X(IM,JM,KM))

IN="03_3D.BIN"

OPEN(21,FILE=IN,FORM='UNFORMATTED',ACCESS='DIRECT',&
RECL=4*IM*JM)

IREC=0
DO K=1,KM
IREC=IREC+1
READ(21,REC=IREC) X(:,:,K)
END DO !KM
CLOSE(21)

PRINT *; PRINT '(A,A)','OUTPUT: ',TRIM(IN); PRINT *

DO K=1,KM
WRITE(*,'(A,I3)')'K=',K
WRITE(*,'(3f8.1)') ((X(I,J,K),I=1,IM),J=1,JM)
WRITE(*,*)
END DO !KM

END
```





##### コンパイル

```bash
$ ifort -assume byterecl 03.3D_READ.F90 -o 03.3D_READ.EXE
```

##### 実行

```bash
$ 03.3D_READ.EXE
```

```bash
OUTPUT: 03_3D.BIN

K=  1
   111.0   211.0   311.0
   121.0   221.0   321.0

K=  2
   112.0   212.0   312.0
   122.0   222.0   322.0
```



## 4次元データ

#### ディレクトリ作成
```bash
$ cd $HOME/2022_PROGRAM/F_05_SP01_BINARY_IO
```
```bash
$ mkdir -vp 04.4D
```
```bash
$ cd 04.4D
```
#### データの作成と出力

##### FORTRANプログラム
```fortran
CHARACTER OUT*1000

REAL,ALLOCATABLE,DIMENSION(:,:,:,:)::X

REAL,PARAMETER::UNDEF=1.E20

IM=3; JM=2; KM=2; MM=3

ALLOCATE(X(IM,JM,KM,MM))

DO M=1,MM
DO K=1,KM
DO J=1,JM
DO I=1,IM
X(I,J,K,M) = 1000*I + 100*J + 10*K + M
END DO !I
END DO !J
END DO !K
END DO !M

OUT="04_4D.BIN"

OPEN(21,FILE=OUT,FORM='UNFORMATTED',ACCESS='DIRECT',&
RECL=4*IM*JM)

IREC=0
DO M=1,MM
DO K=1,KM
IREC=IREC+1
WRITE(21,REC=IREC) X(:,:,K,M)
END DO !KM
END DO !MM
CLOSE(21)

PRINT *; PRINT '(A,A)','OUTPUT: ',TRIM(OUT); PRINT *

END
```

##### コンパイル

```bash
$ ifort -assume byterecl 04.$D.F90 -o 04.4D.EXE
```

##### 実行

```bash
$ 04.4D.EXe
```



##### CTLファイルの例

```
$ vi 04_04.CTL
```

```
dset ^04_4D.BIN
title test 4D Data
undef 1.0e20
xdef 3 linear  150 1
ydef 2 linear   30 1
zdef 2 levels 1000 500
tdef 3 linear 00Z01NOV2022 1HR
vars 1
x 2 99 TEST_VAR
endvars
```



##### スクリプトの例

```
$ vi 04_4D.GS
```

```bash
'open 04_4D.CTL'

say; 'q ctlinfo'; say result

'set time 00Z01NOV2022'
#'set t 1 1'

say; 'q dims'; say result; say

'cc'

say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'
date='00Z01NOV2022'
say date
say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'

plev='1000'; say; say 'plev='plev
'mul 3 2 1 1 -xwid 2 -ywid 2 -yint 1' 
# http://kodama.fubuki.info/wiki/wiki.cgi/GrADS/script/mul.gs?lang=jp
 
'set grads off'; 'set grid off'
'set xlint 1'; 'set ylint 1'

'set gxout grid'
'set lon 149.5 152.5'; 'set lat 29.5 31.5'
'set lev 'plev; 'set time 'date
'd x'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=xl-0.7; yy=(yt+yb)/2
'set strsiz 0.14 0.16'; 'set string 1 c 3 90'
'draw string 'xx' 'yy' 'plev

xx=(xl+xr)/2; yy=yt+0.5
'set strsiz 0.14 0.16'; 'set string 1 c 3 0'
'draw string 'xx' 'yy' 'date

plev='500'; say; say 'plev='plev
'mul 3 2 1 2 -xwid 2 -ywid 2 -yint 1' 
'set grads off'; 'set grid off'
'set xlint 1'; 'set ylint 1'
'set lev 'plev; 'set time 'date
'd x'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=(xl+xr)/2; yy=yt+0.5
'set strsiz 0.14 0.16'; 'set string 1 c 3 0'
'draw string 'xx' 'yy' 'date

xx=xl-0.7; yy=(yt+yb)/2
'set strsiz 0.14 0.16'; 'set string 1 c 3 90'
'draw string 'xx' 'yy' 'plev

say



say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'
date='01Z01NOV2022'
say date
say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'


plev='1000'; say; say 'plev='plev
'mul 3 2 2 1 -xwid 2 -ywid 2 -yint 1' 
 
'set grads off'; 'set grid off'
'set xlint 1'; 'set ylint 1'

'set gxout grid'
'set lon 149.5 152.5'; 'set lat 29.5 31.5'
'set lev 'plev; 'set time 'date
'd x'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=(xl+xr)/2; yy=yt+0.5
'set strsiz 0.14 0.16'; 'set string 1 c 3 0'
'draw string 'xx' 'yy' 'date

plev='500'; say; say 'plev='plev
'mul 3 2 2 2 -xwid 2 -ywid 2 -yint 1' 
'set grads off'; 'set grid off'
'set xlint 1'; 'set ylint 1'
'set lev 'plev; 'set time 'date
'd x'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=(xl+xr)/2; yy=yt+0.5
'set strsiz 0.14 0.16'; 'set string 1 c 3 0'
'draw string 'xx' 'yy' 'date

say



say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'
date='02Z01NOV2022'
say date
say 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM'

plev='1000'; say; say 'plev='plev
'mul 3 2 3 1 -xwid 2 -ywid 2 -yint 1' 
 
'set grads off'; 'set grid off'
'set xlint 1'; 'set ylint 1'

'set gxout grid'
'set lon 149.5 152.5'; 'set lat 29.5 31.5'
'set lev 'plev; 'set time 'date
'd x'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=(xl+xr)/2; yy=yt+0.5
'set strsiz 0.14 0.16'; 'set string 1 c 3 0'
'draw string 'xx' 'yy' 'date

plev='500'; say; say 'plev='plev
'mul 3 2 3 2 -xwid 2 -ywid 2 -yint 1' 
'set grads off'; 'set grid off'
'set xlint 1'; 'set ylint 1'
'set lev 'plev; 'set time 'date
'd x'
'q gxinfo'
line=sublin(result,3); xl=subwrd(line,4); xr=subwrd(line,6)
line=sublin(result,4); yb=subwrd(line,4); yt=subwrd(line,6)

xx=(xl+xr)/2; yy=yt+0.5
'set strsiz 0.14 0.16'; 'set string 1 c 3 0'
'draw string 'xx' 'yy' 'date



FIG='04_4D.eps'
'gxprint 'FIG

say 'OUTPUT: 'FIG; say

'quit'
```



##### 作図例

```bash
$ grads -bcp "04_4D.GS"
```

OUTPUT: 04_4D.eps  



#### データの読み込み例
##### FORTRANプログラム

```fortran
CHARACTER IN*1000

REAL,ALLOCATABLE,DIMENSION(:,:,:,:)::X

REAL,PARAMETER::UNDEF=1.E20

IM=3; JM=2; KM=2; MM=3

ALLOCATE(X(IM,JM,KM,MM))

IN="04_4D.BIN"

OPEN(21,FILE=IN,FORM='UNFORMATTED',ACCESS='DIRECT',&
RECL=4*IM*JM)

IREC=0
DO M=1,MM
DO K=1,KM
IREC=IREC+1
READ(21,REC=IREC) X(:,:,K,M)
END DO !KM
END DO !MM
CLOSE(21)

PRINT *; PRINT '(A,A)','OUTPUT: ',TRIM(IN); PRINT *

DO M=1,MM
DO K=1,KM
WRITE(*,'(A,I3,A,I3)')'M=',M, '  K=',K
WRITE(*,'(2F8.0)') ( (X(I,J,K,M),I=1,IM), J=1,JM )
WRITE(*,*)
END DO !KM
END DO !MM
END
```

##### コンパイル

```bash
$ ifort -assume byterecl 04.4D_READ.F90 -o 04.4D_READ.EXE
```

##### 実行

```
$ 04.4D_READ.EXE 
```

```
OUTPUT: 04_4D.BIN
 
M=  1  K=  1
   1111.   2111.
   3111.   1211.
   2211.   3211.
 
M=  1  K=  2
   1121.   2121.
   3121.   1221.
   2221.   3221.
 
M=  2  K=  1
   1112.   2112.
   3112.   1212.
   2212.   3212.
 
M=  2  K=  2
   1122.   2122.
   3122.   1222.
   2222.   3222.
 
M=  3  K=  1
   1113.   2113.
   3113.   1213.
   2213.   3213.
 
M=  3  K=  2
   1123.   2123.
   3123.   1223.
   2223.   3223.
```





## 練習

### 初めに

#### FORTRANプログラムのデバッグ

##### コンパイルエラー

FORTRANのプログラムのコンパイル時にエラーが出た時は，エラー番号やエラーメッセージを検索して，エラーメッセージの意味を把握したのち，該当する行周辺に下記のようなミスがないか調べる。

- スペルミス

- 左右のカッコが対応していない

- 他の言語の関数を使用しようとしている

  

##### 実行エラー

まず，コンパイル時に，下記の例のように，次の3つのオプション (`-traceback`, `CB`, `fpe0`)を加えて，コンパイルしなおす。

```
ifort -tracback -CB -fpe0 PROG.F90 PROG.EXE
```

`-traceback`: プログラム実行時にエラーが発生した際に, ソースファイルのどこでエラーが発生したか, どのようなエラーか表示される。

`-CB` (=check bounds): 宣言した配列の要素数を上回る値の添え字を指定していないかチェックする。

例えば, `real,dimension(2):: x`とxの要素数は2であると宣言したにも関わらず, `x(3)=1`のように実際には存在しない要素にアクセスしようとした場合に、エラーであることを報告する。

`-fpe0`: 0で割り算していないかチェックする。

##### 実行エラーの読み方

一つの例として下記を示す。

$ vi SEGF.F90

```plaintext
real,dimension(2)::x

i=3
x(i)=1.0

end
```

```plaintext
$ ifort -traceback -CB -fpe0 SEGF.F90 -o SEGF.exe
```

```plaintext
$ SEGF.exe
```



forrtl: severe (**408**): fort: (2): **Subscript #1 of the array X has value 3 which is greater than the upper bound** of 2  

Image              PC                Routine            Line        Source  
 SEGF.exe           000000000040664F  Unknown               Unknown  Unknown  
 SEGF.exe           0000000000403B1F  MAIN__                      **4**  **SEGF.F90  ** 
 SEGF.exe           0000000000403AA2  Unknown               Unknown  Unknown  
 libc-2.17.so       00007F57776F7555  __libc_start_main     Unknown  Unknown  
 SEGF.exe           00000000004039A9  Unknown               Unknown  Unknown  

**408**: エラー番号 (**googleで**ifort ERROR 408で**検索する**と、エラーメッセージの詳細が分かる)    

Subscript #1 of the array X has value 3 which is greater than the upper bound of 2:  
 配列xの1番目の添え字(#1)に3が指定されているが，それは最大要素数の2を超えている  

**4 SEGF.F90**: ソースファイルSEGF.F90の4行目で該当のエラーが発生した  

##### 参考: segmentation fault

配列要素に関して誤った処理を行った場合,    

```plaintext
segmentation fault
```

という実行時エラーが表示されることがしばしばある。  

これはメモリの不正アクセスが行われたときにでるエラーで，存在しないメモリ番地を指定した際に発生することが多い。配列の番号指定を誤ると，メモリの不正アクセスとなることが多い。  



#### GrADSスクリプトのデバッグ

https://gitlab.com/infoaofd/lab/-/tree/master/DEGBUG

https://seesaawiki.jp/ykamae_grads-note/d/GrADS%20troubleshooting





### 練習1 1次元データの読み込み

(1) 下記のCTLファイル (EX01.CTL)で記述されるダイレクトアクセスのプレーンバイナリファイル   

​	NCEP1.T.MON.ANO.AAV.BIN  

の時系列を, GrADSを使って作図せよ。

#### EX01.CTL

```bash
dset /work03/am/LAB/FORTRAN/PROGRAM_2022/05_SP01_EX_BINARY_IO/11.EX01/NCEP1.T.MON.ANO.AAV.BIN
title NCEP2 GLOBAL MEAN SURFACE AIR TEMPERATURE
undef -9.96921e+36
xdef 1 linear  137 1 
ydef 1 linear   33 1
zdef 1 levels 1000 100
tdef 852 linear 00Z01JAN2019 1MO
vars 1
t 0 99 surface air temperature
endvars
```



(2) EX01.CTLに記載された情報をもとに，NCEP1.T.MON.ANO.AAV.BINを読み込み，テキストファイルとして書き出すFORTRANプログラムを作成せよ。



### 備考

NCEP1.T.MON.ANO.AAV.BINは1949年1月から2019年12月までの，全球平均した月平均地表面気温の平年値からの偏差である。

使用したデータはNCEP再解析1で，次のGrADSスクリプトを使って作成した。

```
'sdfopen /work01/DATA/NCEP1/MON.MEAN/air.mon.mean.nc'

say 'MMMMM AREA AVE'
'set t 1 898'
'set x 1';'set y 1'
'aav=tloop(aave(air,lon=0,lon=360,lat=-90,lat=90))'

say 'MMMMM ANOMALY FROM MONTHLY CLIM'
'set t 1 12'
'clim=ave(aav,t+0,t+876, 12)'
'modify clim seasonal'
'set t 1 876' ;# 1948-2021
'ano=aav-clim'

say 'MMMMM RUNNING MEAN'
'set t 6 870'
'ano1=ave(ano,t-6,t+6)'

'cc'
'set grads off'; 'set grid off'
'set xlint 30'
'set cmark 0'

'set t 13 864'
'q dims'; say result

'd ano1' ;# say result
FIG='NCEP1.T.MON.ANO.AAV.eps'
'gxprint 'FIG

say 'FIG: 'FIG

BIN='NCEP1.T.MON.ANO.AAV.BIN'
'set gxout fwrite'
'set fwrite 'BIN
'd ano1'

say 'BIN: 'BIN
'quit'
```



### 練習2 2次元データの読み込み

(1) 下記の2つのCTLファイル (EX02RAW.CTL, EX02CLM.CTL)で記述されるダイレクトアクセスのプレーンバイナリファイル

​	[1] NCEP1.T.MON.RAW_201807.BIN   (2018年7月の月平均地表面気温)

​	[2] NCEP1.T.MON.CLM_201807.BIN   (平年の7月の月平均地表面気温) 

の水平分布を, GrADSを使って作図せよ。以降[1]を生データ, [2]を平年値と呼ぶことにする。



#### EX02RAW.CTL

```
dset /work03/am/LAB/FORTRAN/PROGRAM_2022/05_SP01_EX_BINARY_IO/21.EX02/NCEP1.T.MON.RAW_201807.BIN
title 201807 monthly mean air.sig995 from the NCEP Reanalysis
undef -9.96921e+36
xdef 144 linear 0 2.5
ydef 73 linear -90 2.5
zdef 1 linear 0 1
tdef 1 linear 00Z01JUL2018 1mo
vars 1
TR   0 99  Monthly Mean Air Temperature (RAW) degC
endvars
```

#### EX02CLM.CTL

```
dset /work03/am/LAB/FORTRAN/PROGRAM_2022/05_SP01_EX_BINARY_IO/21.EX02/NCEP1.T.MON.CLM_201807.BIN
title 201807 monthly mean air.sig995 from the NCEP Reanalysis
undef -9.96921e+36
xdef 144 linear 0 2.5
ydef 73 linear -90 2.5
zdef 1 linear 0 1
tdef 1 linear 00Z01JUL2018 1mo
vars 1
TC   0 99  Monthly Mean Air Temperature (CLM) degC
endvars
```

#### 備考

2018年7月中旬から下旬にかけて，日本を含む世界各地で異常高温が発生した。



(2) 生データから平年値を引くことで，気温偏差を計算し，ダイレクトアクセスのバイナリファイルとして計算結果を出力せよ。GrADSを使って結果を描画せよ。ヒントとして，CTLファイルのサンプルを下記に記載する。

#### EX02ANO.CTL

```bash
dset 作成した気温偏差のファイル名
title 201807 monthly mean air.sig995 from the NCEP Reanalysis
undef -9.96921e+36
xdef 144 linear 0 2.5
ydef 73 linear -90 2.5
zdef 1 linear 0 1
tdef 1 linear 00Z01JUL2018 1mo
vars 1
TA   0 99  Monthly Mean Air Temperature (ANO) degC
endvars
```

#### 備考

ここで使用した入力ファイルは次のGrADSスクリプトを使って作成した。

```
'sdfopen /work01/DATA/NCEP1/MON.MEAN/air.mon.mean.nc'

#'q ctlinfo'; say result


say 'MMMMM ANOMALY FROM MONTHLY CLM'
'set t 1 12'
'CLM=ave(air,t+0,t+876, 12)'
'modify CLM seasonal'
'set t 1 876' ;# 1948-2021
'ano=air-CLM'

say 'MMMMM RUNNING MEAN'
'set t 6 870'
'ano1=ave(ano,t-6,t+6)'



say 'MMMMM PLOT CLM'
'cc'
'set grads off'; 'set grid off'
'set xlint 30'
'set cmark 0'


'set time 00Z01JUL2018'
'q dims'; say sublin(result,5)

'color -32 32 2 -kind midnightblue->deepskyblue->green->wheat->orange->red->magenta'
'd CLM(time=00Z01JUL2018)' ;# say result

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
x1=xl; x2=xr; y1=yb-0.5; y2=y1+0.1
'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.1 -fh 0.13 -fs 2 -ft 3 -line on -edge circle'

FIG='NCEP1.T.MON.CLM_201807.eps'
'gxprint 'FIG

say 'FIG: 'FIG

say 'MMMMM OUTPUT CLM'
BIN='NCEP1.T.MON.CLM_201807.BIN'
'set gxout fwrite'
'set x 1 144'; 'set y 1 73'
'set fwrite 'BIN
'd CLM(time=00Z01JUL2018)'
'disable fwrite'
say 'BIN: 'BIN



say 'MMMMM PLOT RAW'
'cc'
'set grads off'; 'set grid off'
'set xlint 30'
'set cmark 0'


'set time 00Z01JUL2018'
'q dims'; say sublin(result,5)

'color -32 32 2 -kind midnightblue->deepskyblue->green->wheat->orange->red->magenta'
'd air(time=00Z01JUL2018)' ;# say result

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
x1=xl; x2=xr; y1=yb-0.5; y2=y1+0.1
'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.1 -fh 0.13 -fs 2 -ft 3 -line on -edge circle'

FIG='NCEP1.T.MON.RAW_201807.eps'
'gxprint 'FIG

say 'FIG: 'FIG

say 'MMMMM OUTPUT RAW'
BIN='NCEP1.T.MON.RAW_201807.BIN'
'set gxout fwrite'
'set x 1 144'; 'set y 1 73'
'set fwrite 'BIN
'd air(time=00Z01JUL2018)'
'disable fwrite'
say 'BIN: 'BIN



say 'MMMMM PLOT ANO CHECK'
'cc'
'set grads off'; 'set grid off'
'set xlint 30'
'set cmark 0'


'set time 00Z01JUL2018'
'q dims'; say sublin(result,5)

'color -4 4 0.5 -kind darkblue->blue->dodgerblue->skyblue->white->gold->darkorange->red->darkred'
'd air(time=00Z01JUL2018)-CLM(time=00Z01JUL2018)' ;# say result

'q gxinfo'
line3=sublin(result,3); xl=subwrd(line3,4); xr=subwrd(line3,6)
line4=sublin(result,4); yb=subwrd(line4,4); yt=subwrd(line4,6)
x1=xl; x2=xr; y1=yb-0.5; y2=y1+0.1
'xcbar 'x1' 'x2' 'y1' 'y2 ' -fw 0.1 -fh 0.13 -fs 2 -ft 3 -line on -edge circle'

FIG='NCEP1.T.MON.ANO_CHK_201807.eps'
'gxprint 'FIG

say 'FIG: 'FIG



'quit'
```



### 練習3 3次元データの読み込み

下記のCTLファイル (EX03.CTL)で記述されるダイレクトアクセスのプレーンバイナリファイルは，1948年1月から2020年12月までの，200hPa における風の南北方向成分 (v)の月平均値である。

```
dset /work03/am/LAB/FORTRAN/PROGRAM_2022/05_SP01_EX_BINARY_IO/31.EX03/NCEP1.V200.MON.RAW_1984-2021.BIN
title monthly mean air.sig995 from the NCEP Reanalysis
undef -9.96921e+36
xdef 144 linear 0 2.5
ydef 73 linear -90 2.5
zdef 1 linear 200 1
tdef 876 linear 00Z01JAN1948 1mo
vars 1
V 0 99  Monthly Mean NORTHWARD WIND (RAW) m/s
endvars
```

(1) GrADSを使って，2018年7月のvの水平分布を描画せよ。

ヒント

1948年1月から数えて，2018年7月は847番目のデータである。

```
m = 847 Time = 00Z01JUL2018  
```

GrADSでは，

```
'set time 00Z01JUL2018'
```

もしくは，

```
'set t 847'
'q dims'; say sublin(result,5)
```

で，該当の月を指定することができる。



(2) 1948年から2020年までの期間の7月のvの平年値を計算するプログラムを作成し，さらにGrADSを使って結果を描画せよ。

i, j, mをそれぞれ，緯度，経度，月を表す添え字とすると，vはv (i, j, m)という3次元の配列で表現することができる。

ヒント: 時間方向のデータの並びは次のようになっている。

```
m = 1 Time = 00Z01JAN1948  
...
m = 7 Time = 00Z01JUL1948  
...
m = 19 Time = 00Z01JUL1949  
...
m = 876 Time = 00Z01DEC2020  
```

従って，7月の平均を求めるためにはm=7から始めて，12個とびにデータの和を取っていけばよい。

いま，VR(i,j,m)を毎月のデータ，VC(i,j)を平年値とすると，次のようなプログラムを書けばよい。

```fortran
MM=876
mjul=0; VC(:,:)=0
do m=7,MM, 12
mjul=mjul+1
VC(:,:)=VC(:,:)+vR(i,j,m)
end do !m
print *,'mjul=',mjul
VC(:,:)=VC(:,:)/float(mjul)
```

`float`は変数の型を実数型に変換する組み込み関数である。



(3) 2018年7月のvの平年値からの偏差

前問で作成したデータと入力データの2018年7月における値の差を取ればよい。

2018年7月 (m=847)のデータに関する偏差を求めればよいので，VA(i,j)を2018年7月の偏差として，

```
M18=847
VA(:,:)=VR(:,:,M18)-VC(:,:)
```

とすることで，生データVRの847番目のデータから7月の平年値VCを引くのが簡単である。

#### 補足

対流圏上層の南北風の偏差を見ることで，大規模大気波動の伝播の様子が明瞭に見えることがよくある。結果を図示すると，ユーラシア大陸の中緯度において，南北風の正負の偏差が東西な並んでいるのが分かるが，これはユーラシア大陸を東向きに伝播するロスビー波束を表していると推測される。



#### 備考

ここで使用した入力ファイルは次のGrADSスクリプトを使って作成した。

```
'sdfopen /work01/DATA/NCEP1/MON.MEAN/vwnd.mon.mean.nc'

#'q ctlinfo'; say result

PLEV='200'

#'set t 1 876' ;# 1948-2021


say 'MMMMM OUTPUT RAW'
BIN='NCEP1.V'PLEV'.MON.RAW_1984-2021.BIN'

'set gxout fwrite'
'set undef dfile'
'set fwrite 'BIN

'set lev 'PLEV

tmax=876
t=1
while(t<=tmax)
'set t 't
'q dims'; say sublin(result,5)

'set x 1 144'; 'set y 1 73'
'd vwnd'

t=t+1
endwhile

'disable fwrite'

'quit'
```



### 練習3 WRFの出力データの読み込み

練習としてWRFの出力データ※を読んで，その値を出力する。

※元々のWRFの出力データはnetCDFファイルだが，ここでは変換プログラムを通して変換されたプレーンバイナリファイルを使用する。

#### [WRFの出力データの読み込み](./F_05_SP02_READ_WRF.md)



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
