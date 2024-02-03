# NCL Tutorial 03

[[_TOC_]]

## はじめに

## 今回の内容

入力ファイルや計算のパラメーターを変えて，同じNCLスクリプトを繰り返し利用したいことがよくある。今回はそのような目的で用いるスクリプトの典型的な例を学習する。



### 参考になるサイト

1. http://www.atmos.rcast.u-tokyo.ac.jp/shion/NCLtips/index.php

2. https://sites.google.com/site/tips4ncl/
3. https://www.ncl.ucar.edu/gallery.shtml

3. https://www.ncl.ucar.edu/Applications/


5. https://gitlab.com/infoaofd/lab/-/blob/master/NCL/NCL_QUICK_REF.md



## 予備知識

下記が必要な予備知識である。

### Linuxのシェルの基本操作

https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/01.BASH/0.LINUX_TUTORIAL_01.md

### シェルスクリプトの文法の基礎

https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/03.BASH_SCRIPT/LINUX_SCRIPT_TUTORIAL.md



### 困ったときは

#### Google等を使って下記で検索

```
NCL keyword
```

```
NCAR Command Language keyword
```

keywordのところには，例えば，'等値線'や'countour'など**自分がやりたいこと**などを入れてみる。検索でヒットしたサイトをを見ながら，keywordを絞り込んでいく。例えば, '等値線　フォント　変更'や'change contour font size'など。



#### 上記の「参考になるサイト」を調べてみる

英語の方が情報が多い。有用な情報が英語でのみ提供されていることも多い。一度で理解できなくても，有用と思われる情報は，PDFファイルなどに書き出すなどして，何度か読み込んでみること。



## 練習1: シェルスクリプトとの連携

複数の入力ファイルを連続して処理したい時に，シェルスクリプトを通してNCLスクリプトを動かすと便利なことが多い。

### スクリプト

#### GET_ARG01.sh (シェルスクリプト)

```bash
#!/bin/bash

NCL=GET_ARG.ncl #使用するNCLスクリプト名

if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCLスクリプトが存在しない場合，エラーメッセージを表示して終了

DSET=ERA5; VAR=DLR

runncl.sh $NCL $DSET $VAR
# 引数DSETとVARを渡してNCLスクリプトを実行
```



#### GET_ARG.ncl (NCLスクリプト)

```bash
script_name  = get_script_name() ;スクリプトの名前を得る

DSET  = getenv("NCL_ARG_2") ;2番目の引数を変数DSETに代入
VAR   = getenv("NCL_ARG_3") ;3番目の引数を変数VARに代入

print("SCRIPT="+script_name)  ;スクリプト名を画面表示
print("DSET="+DSET) ;変数DSETの内容を画面表示
print("VAR="+VAR)   ;変数VARの内容を画面表示
print("DONE.")
print("") ;1行改行
```



#### runncl.sh (シェルスクリプト)

NCLスクリプトとシェルスクリプトを連携させるためのスクリプト

```bash
$ which runncl.sh
~/mybin/runncl.sh
```

本研究室のサーバでは，~/mybin/に保存されていることが多い。無い場合，下記を使用すること。

```bash
$ cat runncl.sh
```

```bash
#!/bin/bash

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

シェルスクリプトの引数をNCLに渡している



#### 実行例

```bash
$ GET_ARG01.sh
```

```bash
SCRIPT=GET_ARG.ncl
DSET=ERA5
VAR=DLR
DONE.
```

変数DSETに値ERA5が, 変数VARに値DLRが確かに代入されている。



## 練習2: 引数を変更しながら繰り返す

引数を変更しながらNCLスクリプトを繰り返し実行したい場合もある。その場合の例を示す

### スクリプト

#### GET_ARG02.sh (シェルスクリプト)

```bash
#!/bin/bash

# 引数を変更しながら繰り返しNCLスクリプトを実行する

NCL=GET_ARG.ncl #NCLスクリプト名

if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCLスクリプトが存在しない場合，エラーメッセージを表示して終了

DSET=ERA5
VARLIST="DLR DSR NLR"

for VAR in $VARLIST; do
runncl.sh $NCL $DSET $VAR
done
```

#### 実行例

```bash
$ GET_ARG02.sh
```

```
SCRIPT=GET_ARG.ncl
DSET=ERA5
VAR=DLR
DONE.

SCRIPT=GET_ARG.ncl
DSET=ERA5
VAR=DSR
DONE.

SCRIPT=GET_ARG.ncl
DSET=ERA5
VAR=NLR
DONE.
```



## 練習3: 日付処理

日付ごとに分かれているファイルを次々に処理したい場合もしばしばある。そのような場合の例を示す。

### スクリプト

#### RUN_DATE01.sh (シェルスクリプト)

まずひな型として，開始日と終了日を引数として与えると, 一日ごとに日付を出力するスクリプトを下記に示す。このスクリプトは用途が広く, 色々なケースで応用が利く。

```bash
# 日付の処理
# https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/03.BASH_SCRIPT/LINUX_DATE.md

if [ $# -ne 2 ]; then
  echo ERROR IN $0 : wRONG ARGUMENTS.; echo $0 yyyymmdd1 yyyymmdd2
  # 引数が2つなかったらエラーを表示して終了
  exit 1
fi

yyyymmdd1=$1 #1番目の引数は開始日
yyyymmdd2=$2 #2番目の引数は終了日

# 年,月,日に分離
yyyy1=${yyyymmdd1:0:4}; mm1=${yyyymmdd1:4:2}; dd1=${yyyymmdd1:6:2}
yyyy2=${yyyymmdd2:0:4}; mm2=${yyyymmdd2:4:2}; dd2=${yyyymmdd2:6:2}

#dateコマンドの書式に合わせる
start=${yyyy1}/${mm1}/${dd1}; end=${yyyy2}/${mm2}/${dd2}

#日付をユリウス日で表す
jsstart=$(date -d${start} +%s); jsend=$(date -d${end} +%s)
#秒単位
jdstart=$(expr $jsstart / 86400); jdend=$(expr   $jsend / 86400)
#日単位

nday=$( expr $jdend - $jdstart)
# 総日数

i=0
while [ $i -le $nday ]; do
  date_out=$(date -d"${yyyy1}/${mm1}/${dd1} ${i}day" +%Y%m%d)
  # dateコマンドで日付を書き出す

  yyyy=${date_out:0:4}; mm=${date_out:4:2}; dd=${date_out:6:2}
  # yyyy,mm,ddにそれぞれ年,月,日を代入する

  echo ${yyyy} ${mm} ${dd}

  i=$(expr $i + 1)
done
```

#### 実行例

```bash
$ RUN_DATE01.sh 20230716 20230718
```

```bash
2023 07 16
2023 07 17
2023 07 18
```

```bash
$ RUN_DATE01.sh
```

```bash
ERROR IN ./RUN_DATE01.sh : wRONG ARGUMENTS.
./RUN_DATE01.sh yyyymmdd1 yyyymmdd2
```

引数を指定しない場合エラーメッセージを表示して終了



#### RUN_DATE02.sh (シェルスクリプト)

このシェルスクリプトは, 日付を次々に変更しながらNCLスクリプトを次々に実行する。

```bash
# 日付の処理
# https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/03.BASH_SCRIPT/LINUX_DATE.md

NCL=RUN_1INPUT_FILE.ncl #使用するNCLスクリプト名
if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCLスクリプトが存在しない場合，エラーメッセージを表示して終了

which runncl.sh &> /dev/null
if [ $? -ne 0 ]; then echo NO SUCH FILE,runncl.sh;exit 1;fi
# もしrunncl.shというファイルが見つからなかったらエラー

if [ $# -ne 2 ]; then
  echo ERROR IN $0 : wRONG ARGUMENTS.; echo $0 yyyymmdd1 yyyymmdd2
  # 引数が2つなかったらエラーを表示して終了
  exit 1
fi

yyyymmdd1=$1 #1番目の引数は開始日
yyyymmdd2=$2 #2番目の引数は終了日

# 年,月,日に分離
yyyy1=${yyyymmdd1:0:4}; mm1=${yyyymmdd1:4:2}; dd1=${yyyymmdd1:6:2}
yyyy2=${yyyymmdd2:0:4}; mm2=${yyyymmdd2:4:2}; dd2=${yyyymmdd2:6:2}

#dateコマンドの書式に合わせる
start=${yyyy1}/${mm1}/${dd1}; end=${yyyy2}/${mm2}/${dd2}

#日付をユリウス日で表す
jsstart=$(date -d${start} +%s); jsend=$(date -d${end} +%s)
#秒単位
jdstart=$(expr $jsstart / 86400); jdend=$(expr   $jsend / 86400)
#日単位

nday=$( expr $jdend - $jdstart)
# 総日数

i=0
while [ $i -le $nday ]; do
  date_out=$(date -d"${yyyy1}/${mm1}/${dd1} ${i}day" +%Y%m%d)
  # dateコマンドで日付を書き出す

  yyyy=${date_out:0:4}; mm=${date_out:4:2}; dd=${date_out:6:2}
  # yyyy,mm,ddにそれぞれ年,月,日を代入する

  INFLE=INPUT_${yyyy}${mm}${dd}.TXT
  runncl.sh $NCL $INFLE

  i=$(expr $i + 1)
done
```

#### RUN_1INPUT_FILE.ncl (NCLスクリプト)

```bash
script_name  = get_script_name() ;スクリプトの名前を得る

INFLE  = getenv("NCL_ARG_2") ;2番目の引数を変数INFLEに代入

print("SCRIPT="+script_name)  ;スクリプト名を画面表示
print("INFLE="+INFLE) ;変数INFLEの内容を画面表示
print("DONE.")
print("") ;1行改行
```

#### 実行例

```
SCRIPT=RUN_1INPUT_FILE.ncl
INFLE=INPUT_20230716.TXT
DONE.

SCRIPT=RUN_1INPUT_FILE.ncl
INFLE=INPUT_20230717.TXT
DONE.

SCRIPT=RUN_1INPUT_FILE.ncl
INFLE=INPUT_20230718.TXT
DONE.
```



## 練習4: namelistファイルの利用

引数の数が多くなりすぎる場合に便利な方法を説明する。

FORTRANというプログラミング言語 (https://www.jamstec.go.jp/es/jp/simschool/f90learning/chap1/page1.html)では，プログラム実行に必要なパラメータの値をnamelistと呼ばれるテキストファイルから読むこむことができる。

namelistの解説記事

https://zenn.dev/bluepost/articles/1b11f695368d7c

https://qiita.com/aisha/items/47cd2086d1316348b615

https://hydrocoast.jp/index.php?fortran/namelist%E3%81%AE%E4%BD%BF%E7%94%A8%E6%96%B9%E6%B3%95

NCLでも工夫すると，この機能を利用することができる。

### スクリプト

#### READ_NML01.sh (シェルスクリプト)

```bash
#!/bin/bash

# namelistファイルから情報を読むこむ

NCL=READ_NML.ncl #NCLスクリプト名

if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCLスクリプトが存在しない場合，エラーメッセージを表示して終了

which runncl.sh &> /dev/null
if [ $? -ne 0 ]; then echo NO SUCH FILE,runncl.sh;exit 1;fi
# もしrunncl.shというファイルが見つからなかったらエラー

NML=NAMELIST.TXT
if [ ! -f $NML ];then echo NO SUCH FILE,$NML;exit 1;fi
# namelist fileが存在しない場合，エラーメッセージを表示して終了

runncl.sh $NCL $NML
```

#### READ_NML.ncl (NCLスクリプト)

```bash
script_name  = get_script_name() ;スクリプトの名前を得る

NML  = getenv("NCL_ARG_2") ;2番目の引数を変数NMLに代入

; namelistファイルの読み込み
INFLE = systemfunc("grep INFLE " +NML+ "|cut -f2 -d'='|cut -f1 -d','")
; namelistファイルの中で,キーワードINFLEで始まる行を探して, 
; 等号の右の値を変数INFLEに読み込む

OFLE  = systemfunc("grep OFLE  " +NML+ "|cut -f2 -d'='|cut -f1 -d','")

A = tofloat(systemfunc("grep A  " +NML+ "|cut -f2 -d'='|cut -f1 -d','"))
; キーワードAで始まる行を探して, 等号の右の値をtofloatで
; 実数型に変換して変数Aに代入する

B = tofloat(systemfunc("grep B  " +NML+ "|cut -f2 -d'='|cut -f1 -d','"))

print("SCRIPT="+script_name)  ;スクリプト名を画面表示
print("INFLE: "+INFLE)
print(" OFLE: "+ OFLE)
print("A = " + A)
print("B = " + B)

print("DONE.")
print("") ;1行改行
```

### namelist file

```bash
$ cat NAMELIST.TXT
```

```bash
INFLE=INPUT.TXT
OFLE=OUTPUT.TXT
A=1.0
B=2.0
```

#### 実行例

```bash
$ READ_NML01.sh
```

```bash
SCRIPT=READ_NML.ncl
INFLE: INPUT.TXT
 OFLE: OUTPUT.TXT
A = 1
B = 2
```

#### namelistファイルの読み込み方法

```
systemfunc("grep INFLE " +NML+ "|cut -f2 -d'='|cut -f1 -d','")
```

NCLの関数`systemfunc`で, ダブルクォーテーション(")で囲まれた内容のLinuxコマンドを実行している。

`grep INFLE NML`で, ファイルNMLからINFLEを含む行を抜き出す。

`cut -f2 -d'='`で, 等号 (=)の右側の文字列を抜き出す。

`|`で, の左側 (`cut -f2 -d'=`')の結果を引数として右側 (`cut -f1 -d','`)に渡す。

`cut -f1 -d','`で，コンマまでの文字列を抜き出す。コンマが無い場合，文字列すべてを渡す

### コツ

namelistファイルは, 次のようにシェルスクリプトに埋め込むと便利である。

#### READ_NML02.sh (シェルスクリプト)

```bash
#!/bin/bash

# namelistファイルから情報を読むこむ

NCL=READ_NML.ncl #NCLスクリプト名

if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCLスクリプトが存在しない場合，エラーメッセージを表示して終了

which runncl.sh &> /dev/null
if [ $? -ne 0 ]; then echo NO SUCH FILE,runncl.sh;exit 1;fi
# もしrunncl.shというファイルが見つからなかったらエラー

NML=NAMELIST.TXT
# catコマンドを使って, EOF (End of file)までの内容を,
# $NML (=NAMELIST.TXT)に書き出す
cat <<EOF>$NML
INFLE=INPUT.TXT
OFLE=OUTPUT.TXT
A=1.0
B=2.0
EOF

runncl.sh $NCL $NML
```

#### 実行例

```bash
$ READ_NML02.sh
```

```bash
SCRIPT=READ_NML.ncl
INFLE: INPUT.TXT
 OFLE: OUTPUT.TXT
A = 1
B = 2
DONE.
```



## 練習5: 共通の文字列を名前にもつ複数のファイルを処理する

特定のディレクトリに似たような名前のファイルが存在したとする。いまの例では, INPUT_DIRというディレクトリに, INで始まり.TXTで終わるファイルが3つ存在したとする。

```
$ ls INPUT_DIR
```

```
0.README.TXT  IN01.TXT  IN02.TXT  IN03.TXT
```

このような例は日付を名前に含むようなファイルによくみられる。これらのファイルをすべて処理したい場合, 次のような方法がある。

### スクリプト

#### USE_ls.sh (bashスクリプト)

```bash
#!/bin/bash

# 共通の文字列を名前にもつ複数のファイルを処理する

INDIR=INPUT_DIR
if [ ! -d $INDIR ];then echo NO SUCH DIR,$INDIR;exit 1;fi
# ディレクトリINDIRが存在しない場合，エラーメッセージを表示して終了

INLIST=$(ls $INDIR/IN*.TXT)
# lsコマンドの結果をINLISTという変数に代入

NCL=USE_ls.ncl
if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCLスクリプトが存在しない場合，エラーメッセージを表示して終了

which runncl.sh &> /dev/null
if [ $? -ne 0 ]; then echo NO SUCH FILE,runncl.sh;exit 1;fi
# もしrunncl.shというファイルが見つからなかったらエラー

# 変数INLISTに含まれる要素一つ一つを順番に変数INFLEに代入した後,
# runncl.shを実行する
for INFLE in $INLIST; do
runncl.sh $NCL $INFLE
done
```

#### USE_ls.ncl (NCLスクリプト)

```bash
script_name  = get_script_name() ;スクリプトの名前を得る

INFLE  = getenv("NCL_ARG_2") ;2番目の引数を変数INFLEに代入

print("SCRIPT="+script_name)  ;スクリプト名を画面表示
print("INFLE="+INFLE)  ;変数INFLEの内容を画面表示
print("DONE.")
print("") ;1行改行
```

#### 実行例

```bash
$ USE_ls.sh
```

```bash
SCRIPT=USE_ls.ncl
INFLE=INPUT_DIR/IN01.TXT
DONE.

SCRIPT=USE_ls.ncl
INFLE=INPUT_DIR/IN02.TXT
DONE.

SCRIPT=USE_ls.ncl
INFLE=INPUT_DIR/IN03.TXT
DONE.
```

このスクリプト (USE_ls.sh)の動作を理解するには，下記のスクリプトを実行してみるとよい

#### TEST_USE_ls.sh

```bash
#!/bin/bash

# 共通の文字列を名前にもつ複数のファイルを処理する

INDIR=INPUT_DIR
if [ ! -d $INDIR ];then echo NO SUCH DIR,$INDIR;exit 1;fi
# ディレクトリINDIRが存在しない場合，エラーメッセージを表示して終了

INLIST=$(ls $INDIR/IN*.TXT)
# lsコマンドの結果をINLISTという変数に代入

# 変数INLISTに含まれる要素一つ一つを順番に変数INFLEに代入した後,
# echo INFLEを実行する
for INFLE in $INLIST; do
echo INFLE: $INFLE
done
```

#### 実行例

```bash
$ USE_ls_TEST.sh 
```

```bash
INFLE: INPUT_DIR/IN01.TXT
INFLE: INPUT_DIR/IN02.TXT
INFLE: INPUT_DIR/IN03.TXT
```



## 上達のためのポイント

**エラーが出た時の対応の仕方でプログラミングの上達の速度が大幅に変わる**。

ポイントは次の3つである。

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

