# WRFの出力データの読み込み
[[_TOC_]]

練習として気象モデルWRF（ワーフ）の出力データ※を読んで，その値をテキスト形式で出力してみる。

※元々のWRFの出力データはnetCDFファイルだが，ここではARWpostと呼ばれる変換プログラムを通して変換されたプレーンバイナリファイルを使用する。

下記の用途のための準備となる。

- **GrADSで計算できない量を計算したい**

- **GrADSで計算すると時間がかかるので，時間を短縮したい**


[BINARY_DATA_GRADS.pdf](./BINARY_DATA_GRADS.pdf)も参照のこと。



## 準備

### データの所在確認

```bash
$ ls -d /work03/2021/sakagami/WRF.RW3A.00.03.05.05/ARWpost_RW3A
.00.03.05.05.0000.01
```

/work03/2021/sakagami/WRF.RW3A.00.03.05.05/ARWpost_RW3A.00.03.05.05.0000.01/

```bash
$ ls -d /work03/2021/sakagami/WRF.RW3A.00.03.05.05/ARWpost_RW3A.00.03.05.05.0000.01/*ctl
```

/work03/2021/sakagami/WRF.RW3A.00.03.05.05/ARWpost_RW3A.00.03.05.05.0000.01/RW3A.00.03.05.05.0000.01.d01.basic_p.01HR.**ctl**

ファイル名がctlで終わるファイルを**コントロールファイル**と呼んでいる。



### 作業用ディレクトリの作成

```bash
$ cd $HOME
```

```bash
$ cd 00.TOOLS
```

以前00.TOOL**S**ではなく，00.TOOLで作成した人は, `cd 00.TOOL`とする。

```bash
$ cd ANALYSIS
```

```bash
$ mkd 03.READ_WRF
$ cd 03.READ_WRF
```



### CTLファイル

CTL（コントロール）ファイルとは, GrADSという描画ソフトがデータの並びに関する情報を得るために使用しているファイルのことだが，これを見るとバイナリファイルのデータがどのように並んでいるか分かる。[BINARY_DATA_GRADS.pdf](./BINARY_DATA_GRADS.pdf)も参照のこと。

#### サンプル

```bash
$ cp -a /work03/2021/sakagami/WRF.RW3A.00.03.05.05/ARWpost_RW3A
.00.03.05.05.0000.01/*ctl .
```

```bash
$ ls *ctl
```

RW3A.00.03.05.05.0000.01.d01.basic_p.01HR.ctl*

```bash
$ cat RW3A.00.03.05.05.0000.01.d01.basic_p.01HR.ctl
```

```bash
dset ^RW3A.00.03.05.05.0000.01.d01.basic_p.01HR_%y4-%m2-%d2_%h2:%n2.dat
options  byteswapped template
undef 1.e30
title  OUTPUT FROM WRF V4.1.5 MODEL
pdef  599 599 lcc  27.000  130.500  300.000  300.000  32.00000  27.00000  130.50000   3000.000   3000.000
xdef 1469 linear  120.56867   0.01351351
ydef 1231 linear   18.56023   0.01351351
zdef   30 levels  
1000.00000
 990.00000
 980.00000
 970.00000
 960.00000
 950.00000
 940.00000
 930.00000
 920.00000
 910.00000
 900.00000
 880.00000
 860.00000
 840.00000
 820.00000
 800.00000
 750.00000
 700.00000
 650.00000
 600.00000
 550.00000
 500.00000
 450.00000
 400.00000
 350.00000
 300.00000
 250.00000
 200.00000
 150.00000
 100.00000
tdef   73 linear 00Z12AUG2021      60MN      
VARS   30
U             30  0  x-wind component (m s-1)
以下つづく
```

#### 概要

別途資料 (BINARY_DATA.pptx)も参照のこと

- `^`: カレント・ディレクトリ，自分が現在いるディレクトリ）を意味する記号
  この場合したがって，ctlファイルとデータファイルが同じ場所に保存されていることを仮定している
  (^を実際にデータがあるディレクトリに書き換えることで変更可能)
- `template`: ファイル名の指定にひな型 (template)を使う
- `byteswapped`: バイナリデータはビッグエンディアンである。
- `undef 1.e30`: 値が存在しない場所には，ダミーの値として10の30乗が入っている。
- `pdef`: (GrADSでのみ使用される)不等間隔の格子データを，緯度・経度上のデータに変換するための情報
- `xdef`: 東西方向のデータ並びに関する情報※  
- `ydef`: 東西方向のデータ並びに関する情報※	  
- `tdef`: 時間方向のデータ並びに関する情報  
- `zdef`: 鉛直方向のデータ数  
- `VARS`: 保存されている変数の数  

※ここでのxdef, ydefに記載されているデータ数はGrADSで描画するときのみ使用される値で，**実際にはpdefに記載されている数のデータがファイルに保存されている**。

#### templateの書式

- %y4 : 年 (4桁の整数)  
- %m2: 月 (2桁の整数)  
- ％d2: 日 (2桁の整数)  
- %h2: 時 (2桁の整数)  
- %n2: 分 (2桁の整数)  

例えば%y4-%m2-%d2_%h2:%n2だと，データファイルの名前の中で  

2021-08-15_00:00  

のような形式で，日時がしていされていることを意味する。  

```
RW3A.00.03.05.05.0000.01.d01.basic_p.01HR_%y4-%m2-%d2_%h2:%n2.dat
```

であれば，RW3A.00.03.05.05.0000.01.d01.basic_p.01HR2021-08-15_00:00.datのようなデータファイルが存在しているはずである。  

今回の例では，下記を実行して確かめることができる。  

```
$ ls /work03/2021/sakagami/WRF.RW3A.00.03.05.05/ARWpost_RW3A
.00.03.05.05.0000.01
```

#### pdefの書式

```
pdef XSIZE YSIZE LCCR(LCC) YLAT XLON X Y SLAT1 SLAT2 SLON DIS_X DIS_Y
```

`XSIZE`, `YSIZE`: X, Y方向のデータ数  

(詳しくは本資料末尾参照)  

#### xdef (ydef)の書式  

```
xdef データ数 データの並べ方 西の端のデータの経度  東西方向の格子間隔(単位は度)
```

- linear＝データの並びは等間隔である  

#### zdefの書式

```
zdef データ数 データの並べ方 下端のデータの座標  上端のデータの座標 (この場合の単位は気圧)
```

- levels = データの間隔は不等間隔なので，このすぐ下に座標一覧を示す  

#### tdefの書式

```
tdef データ数  データの並べ方 最初のデータの時刻 時間間隔
```

#### 変数一覧の書式

`U`: 変数の名前

```
U             30  0  x-wind component (m s-1)
```

- 鉛直方向に30個データ有  
- 変数の名前はx-wind componentで，単位は(m s-1)である。  

### 要点

- ビッグエンディアンで記録されている  

- 東西方向に599個，南北方向に599個のデータがある※  

- 鉛直方向には30個のデータがある。  
- 欠損値は1e30（10の30乗）としている  

※**pdefに記載されいている数値が実際のデータ数**で，xdef, ydefの値はGrADSで描画するときのみ使用される。



## FORTRANプログラムの作成

### 準備

```
$ ift
```

もしくは  

```bash
$ source /opt/intel/oneapi/setvars.sh
```



### ファイルを開く箇所の作成

４バイトの実数を使う

配列は「経度、緯度、気圧」の順にする

```bash
$ vi READ_WRF_01.F90
```

```FORTRAN
PROGRAM READ_WRF
CHARACTER(LEN=1000):: INDIR, INFLE
CHARACTER(LEN=2000):: IN
REAL,DIMENSION(:,:,:),ALLOCATABLE::U

INDIR="/work03/2021/sakagami/WRF.RW3A.00.03.05.05/ARWpost_RW3A.00.03.05.05.0000.01/"
INFLE="RW3A.00.03.05.05.0000.01.d01.basic_p.01HR_2021-08-15_00:00.dat"

IM=599; JM=599; KM=30

ALLOCATE(U(IM,JM,KM))

IN=TRIM(INDIR)//TRIM(INFLE)

PRINT *,"INPUT: ",TRIM(IN)

OPEN(11,FILE=IN,ACTION="READ",form="unformatted",access="direct",recl=IM*JM*4)

CLOSE(11)
END PROGRAM READ_WRF
```

```bash
$ ifort -convert big_endian -traceback -CB -assume byterecl READ_WRF_01.F90 -o READ_WRF_01.EXE
```

- -convert big_endian: ビッグエンディアンのデータを読む  

- -traceback: 実行時のエラーが発生した行を表示する  

- -CB:  配列の不正操作を行っていないかチェックする  

- -assume byterecl (下記参照)

  intel fortranで，このオプションを**付けない場合**，open文のrecl=で設定する値の単位が4バイト単位となる。

  gfortranなどの他のコンパイラでは1バイト単位が基本なので，intel fortranでも1バイト単位となるように，このオプションを付けておくとコンパイラを変えるたびにこの値を変更せずに済む。

  

```bash
$ READ_WRF_01.EXE
```

INPUT:   /work03/2021/sakagami/WRF.RW3A.00.03.05.05/ARWpost_RW3A.00.03.05.05.0000.01/RW3A.00.03.05.05.0000.01.d01.basic_p.01HR_2021-08-15_00:00.dat  



### 最初の時刻だけ読む

#### 最初の変数だけ読み込む

```
$ cp READ_WRF_01.F90 READ_WRF_02.F90
```

```
$ vi READ_WRF_02.F90 
```

```FORTRAN
PROGRAM READ_WRF
CHARACTER(LEN=1000):: INDIR, INFLE
CHARACTER(LEN=2000):: IN
REAL,DIMENSION(:,:,:),ALLOCATABLE::U

INDIR="/work03/2021/sakagami/WRF.RW3A.00.03.05.05/ARWpost_RW3A.00.03.05.05.0000.01/"
INFLE="RW3A.00.03.05.05.0000.01.d01.basic_p.01HR_2021-08-15_00:00.dat"

IM=599; JM=599; KM=30
ALLOCATE(U(IM,JM,KM))

IN=TRIM(INDIR)//TRIM(INFLE)

PRINT *,"INPUT: ",TRIM(IN)

OPEN(11,FILE=IN,ACTION="READ",form="unformatted",access="direct",recl=IM*JM*4)

IREC=0
DO K=1,KM
IREC=IREC+1
READ(11,rec=IREC)U(:,:,K)
END DO !K

WRITE(6,*)
WRITE(6,*)'IM/2=',IM/2, 'JM/2=',JM/2
WRITE(6,*)'U(IM/2,JM/2,1)=',U(IM/2,JM/2,1)

CLOSE(11)
END PROGRAM READ_WRF
```

```
$ ifort -convert big_endian -traceback -CB READ_WRF_02.F90 -o READ_WRF_02.EXE
```

```
$ READ_WRF_02.EXE
```

```
$ READ_WRF_02.EXE
```

 INPUT: 
/work03/2021/sakagami/WRF.RW3A.00.03.05.05/ARWpost_RW3A.00.03.05.05.0000.01/RW3A.00.03.05.05.0000.01.d01.basic_p.01HR_2021-08-15_00:00.dat

 IM/2=         299 JM/2=         299
 U(IM/2,JM/2,1)=   3.845331

 

#### 複数の変数を読み込む

注: 必要な変数以外の全ての変数をdummy2, dummy3としている。

```FORTRAN
PROGRAM READ_WRF
CHARACTER(LEN=1000):: INDIR, INFLE
CHARACTER(LEN=2000):: IN, OUT 

REAL,DIMENSION(:,:,:),ALLOCATABLE::dbz,dummy3
REAL,DIMENSION(:,:),ALLOCATABLE::dummy2
REAL,DIMENSION(:,:),ALLOCATABLE::B

INDIR="/work00/DATA/WRF.RW3A/HD01/RW3A.ARWpost.DAT/basic_p/ARWpost_RW3A.00.03.05.05.0000.01/"
INFLE="RW3A.00.03.05.05.0000.01.d01.basic_p.01HR_2021-08-12_00:00.dat"

IM=599; JM=599; KM=30 

ALLOCATE(dbz(IM,JM,KM),dummy3(IM,JM,KM))
ALLOCATE(B(IM,JM),dummy2(IM,JM))

IN=TRIM(INDIR)//TRIM(INFLE)

PRINT '(A)','MMMMM READ INPUT DATA'
PRINT '(A)',"INPUT: ",TRIM(IN)

OPEN(11,FILE=IN,ACTION="READ",form="unformatted",access="direct",recl=IM*JM*4)

IREC=0
DO K=1,KM
IREC=IREC+1
READ(11,rec=IREC)dummy3(:,:,K)!U
END DO !K

DO K=1,KM
IREC=IREC+1
READ(11,rec=IREC)dummy3(:,:,K)!V
END DO !K

DO K=1,KM
IREC=IREC+1
READ(11,rec=IREC)dummy3(:,:,K)!W
END DO !K

IREC=IREC+1
READ(11,rec=IREC)dummy2(:,:)!Q2

IREC=IREC+1
READ(11,rec=IREC)dummy2(:,:)!T2

WRITE(6,*)'MMMMM CHECK dummy2(IM/2,JM/2)=',dummy2(IM/2,JM/2)

!PRINT '(A)','MMMMM TEST CALCULATION OF MAX DBZ'
!DO I=1,IM
!DO J=1,JM
!B(I,J)=0

!DO K=1,KM
!if (B(I,J).lt.dbz(I,J,K)) then
!B(I,J)=dbz(I,J,K)
!end if

!END DO ! K
!END DO ! J
!END DO ! I

!WRITE(6,*)
!WRITE(6,*)'IM/2=',IM/2, 'JM/2=',JM/2
!WRITE(6,*)'dummy3(IM/2,JM/2,1)=',dummy3(IM/2,JM/2,1)
CLOSE(11)

!PRINT '(A)','MMMMM OUTPUT'
!OUT="TEST.BIN"
!OPEN(21,FILE=OUT,FORM='UNFORMATTED',ACCESS='DIRECT',&
!RECL=4*IM*JM)

!IREC=0
!IREC=IREC+1
!WRITE(21,REC=IREC) B(:,:)
!CLOSE(21)

END PROGRAM READ_WRF
```



### 最後の時刻まで読み込む

追記予定



## 参考

### pdefに関する情報

#### pdefの書式

```
pdef XSIZE YSIZE LCCR(LCC) YLAT XLON X Y SLAT1 SLAT2 SLON DIS_X DIS_Y
```

- `XSIZE`, `YSIZE`: X, Y方向のデータ数  

- `LCCR(LCC)`: 元のデータはランベルト正角円錐図法を用いている。風の成分が北を正とするものに変換済みの場合^※^はLCCRではなく，LCCを使う

  ※ 風が緯度経度座標の成分に既に変換されている場合

- `YLAT`, `XLON`: データ上のある1点の緯度・経度

- `X`, `Y`: 上記の座標に該当する格子番目。"options yrev"で南北を反転していなければ南及び西から数えた点

- `SLAT1`, `SLAT2`: 投影基準緯度

- `SLON`: 投影基準経度

- `DIS_X`, `DIS_Y`: それぞれX, Y方向の格子間隔 (m)

  

#### データの並びを調べる簡便な方法

例として下記のようなctlファイルを考える。

```
dset ^domain_1.dat
options  byteswapped
undef 1.e30
title  OUTPUT FROM WRF V3.5.1 MODEL
pdef  150 150 lcc  46.000 -131.500   75.500   75.500  60.00000  30.00000 -131.50000  18000.000  18000.000
xdef  576 linear -154.88091   0.08108108
ydef  339 linear   31.67547   0.08108108
```

この場合、x,y方向のデータ数は 150 150, 格子間隔は18000 m, 18000 m であることを表している。 

ある点が領域中の何番目の格子点にあたるか調べるには、CTLファイルの  

```
pdef 150 150 lcc ...18000.000 
```

の行を削除したのち、xdefとydefの行を  

```
xdef 150 linear  1.0 1.0 
ydef 150 linear  1.0 1.0 
```

  のように変更して，GrADSで作図してみると分かる。  

同様に、ある点が領域中の何kmの位置にあたるか調べるにはCTLファイル  

```
xdef 150 linear  0.0  18.
ydef 150 linear  0.0  18.  
```

のように変更して，GrADSで作図してみると分かる。  



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