# プログラミング入門_04

[TOC]

## Linuxの準備

```
$ cd $HOME
```

```
$ cd 2022_PROGRAM
```

```
$ mkdir -vp 04
```

```
$ cd 04
```

```
$ pwd
/work03/gu1/LAB/2022_PROGRAM/04
```

```
$ source  /opt/intel/oneapi/setvars.sh --force
```



## 復習

------

## コラム: Fortranの変数
- 宣言せずに変数を使うことができる（短いプログラムを書くとき便利）
- プログラムの先頭に`implicit none`と書くことで、上の機能を無効化できる（長いプログラムを書くとき便利）
- 宣言せずに名前がi, j, k, l, m, nで始まる変数を使うと、整数型と認識される。それ以外の変数は実数型と認識される

------



## 復習: テキストファイルの書き出しと読み込み

### テキストファイルへの書き出し

viを起動する
`$ vi textout.f90`

`i`で挿入モードに移る

ブラウザ上で下記のプログラムをコピー

```fortran
program textout

real,allocatable::a(:)
character(len=100):: ofle

n=10
allocate(a(n))

ofle="random_number.txt"

call random_number(a)

open(21,file=ofle)
write(21,'(i5)')n
do i=1,n
write(21,'(f10.5)')a(i)
end do
close(21)

do i=1,n
print '(f10.5)', a(i)
end do

print *
print '(A,A)','OUTPUT FILE = ',trim(ofle)
print *

! https://www.nag-j.co.jp/fortran/tips/tips_RandomNumberInFortran.html
end program textout
```

viの画面にペースト（貼り付け）

`Esc`でノーマルモードに移る

`:wq`でviを終了して，シェルに戻る



### テキストファイルへの書きこみ方法

```
open(21,file=ofle)
write(21,'(i5)')n
do i=1,n
write(21,'(f10.5)')a(i)
end do
close(21)
```

`open`でファイルを開く

21はファイル番号であり，5, 6以外の任意の番号を使ってよい (5はキーボード, 6はモニター画面に予約されている)

`file=`の後にファイル名を書く。上の例では，ofleという変数の中身 (random_number.txt)がファイル名になる

`write(21,'(i5)')n`で変数nに記憶されている数値(上の例では10)がファイルに書き出される

`'(i5)'`は出力を行う際の書式を意味しており，この場合は5文字分を用意し，整数として書き出す (ので，実数が保存されている変数をこの書式で書き出すと値がおかしくなる)

`do`と`end do`で囲まれた部分は繰り返される（後述）

`write(21,'(f10.5)')a(i)`でa(i)に記憶されている数値(random_numberで作成された数値)がファイルに書き出される。

a(i)のことを**配列**と呼ぶ（後述）

open文の詳細は下記参照

https://www.nag-j.co.jp/fortran/FI_15.html#_15

write文の詳細は下記参照

https://www.nag-j.co.jp/fortran/FI_14.html#_14

書式指定の詳細は下記参照

https://www.nag-j.co.jp/fortran/FI_14.html

do文の詳細は下記参照

https://www.nag-j.co.jp/fortran/FI_9.html#_9

配列の詳細は下記参照（この資料の後ろにも解説有）

https://eng-entrance.com/java-array-whatis

https://www.nag-j.co.jp/fortran/FI_12.html#_12



-----

## コラム: Fotranの組み込み手続き
random_numberはあらかじめFortranに組み込まれているサブルーチンである。その他のものについては下記を参照のこと
- https://www.nag-j.co.jp/fortran/FI_10.html
-https://www.rs.kagu.tus.ac.jp/yama/f90/f-mst.html

------

### コンパイル

`$ ifort textout.f90 -o textout.exe`

### 実行ファイルの確認

`$ ll`
-rwxrwxr-x 1 am am  14K  8月 20 20:00 textout.exe
-rw-rw-r-- 1 am am  443  8月 20 20:00 textout.f90

### 実行

`$ textout.exe`
   0.95483
   0.15289
   0.49678
   0.69831
   0.07452
   0.78291
   0.90688
   0.96399
   0.81216
   0.70186

OUTPUT FILE = random_number.txt

### 出力ファイルの確認

`$ cat random_number.txt` 
   10
   0.95483
   0.15289
   0.49678
   0.69831
   0.07452
   0.78291
   0.90688
   0.96399
   0.81216
   0.70186



## テキストファイルの読み込み
viを起動する
`$ vi textread.f90`

`i`でviの挿入モードに移る

ブラウザ上で下記のプログラムをコピー

```fortran
program textread

real,allocatable::a(:)
character(len=100):: infle

infle="random_number.txt"

print *
print '(A,A)','INPUT FILE = ',trim(infle)
print *

open(21,file=infle,action="read")
read(21,'(i5)')n
allocate(a(n))

do i=1,n
read(21,*)a(i)
end do
close(21)

do i=1,n
print '(f10.5)', a(i)
end do

end program textread
```
viの画面にペースト（貼り付け）

`Esc`でノーマルモードに移る

`:wq`でviを終了して，シェルに戻る



### コンパイル

`$ ifort textread.f90 -o textread.exe`

### 実行ファイルの確認

`$ ll textread.exe`
-rwxrwxr-x 1 am am 14K  8月 20 20:08 textread.exe

### 実行

`$ textread.exe`

INPUT FILE = random_number.txt

   0.95483
   0.15289
   0.49678
   0.69831
   0.07452
   0.78291
   0.90688
   0.96399
   0.81216
   0.70186

`textout.exe`が作成したファイル (random_number.txt)が，<u>確かに`textread.exe`によって読み込まれている</u>ことが確認できた。



## テキストファイルの読み込み方法

```fortran
open(11,file=infle,action="read")
read(11,'(i5)')n
allocate(a(n))

do i=1,n
read(11,*)a(i)
end do
close(11)
```

open文で読み込みたいファイルを開く

11はファイル番号であり，5, 6以外の任意の番号を使ってよい (5はキーボード, 6はモニター画面に予約されている)。書き出し用のプログラムで使った番号 (21)と一致してなくてよい。

file=の後に開きたいファイルの名前を入れる。ここでは，infleという変数の中身（random_number.txt）がファイルの名前になる

`action="read"`で開いたファイルを読み込み専用に指定している（**誤って上書きしないようにできる**）

`read(11,'(i5)')n`で，ファイル(random_number.txt)の1行目に書き込まれている数値（ここでは10）がnという変数に読み込まれる。

`allocate(a(n))`は配列のサイズを決めるための文（後述）

`do`と`end do`で囲まれた部分は繰り返される（後述）

`read(11,'(f10.5)')a(i)`でrandom_number.txtに書き込まれている数値が，a(i)に読み込まれる

`close(11)`で開いたファイルを閉じる



## 内積の計算

### 配列

次のような数ベクトル$\boldsymbol{x}$をプログラムで表現することを考える
$$
\boldsymbol{x}=(1, 2, 3)
$$
$\boldsymbol{x}$の各成分を左からx(1), x(2), x(3)と書くことにする。このとき

x(1)=1, x(2)=2, x(3)=3と書くことができる。これをフォートランで表現するときは，次のようにする

```fortran
real,dimension(3)::x

x(1)=1
x(2)=2
x(3)=3
```



下記の行を宣言文という。

```
real,dimension(3)::x
```

意味は，

- xという変数を新規に使用する

- xは複数の要素をもつ配列として使用する

- 要素の数は3とする

配列は，一つの変数をデータを入れる箱に例えると，箱の中をいくつか小分けに区切った小箱の集まりとして扱うことに相当する

![PROGRAM_2022_02_FIG04.01](FIG_PROGRAM/PROGRAM_02_FIG04.01.png)

宣言文の書き方にはいくつか種類があり，次のような書き方も使用できる

```
real x(3)
```

**注意**：宣言文は実行文(宣言文以外の文)より上に書く約束

（変数を宣言→変数を使用という順序を守るため）

#### 参考

```
x(1)=1
x(2)=2
x(3)=3
```

上記は，次のように1行で書くことができる

```
x(1)=1; x(2)=2; x(3)=3
```

(各行をセミコロン`;`で区切る）



### 内積の計算

例えば，2つの数ベクトル
$$
\boldsymbol{x}=(x_1, x_2, x_3), \qquad \boldsymbol{y}=(y_1, y_2, y_3)
$$
の内積，$\boldsymbol{x}\cdot \boldsymbol{y}$は，
$$
\boldsymbol{x}\cdot\boldsymbol{y}=x_1y_1+x_2y_2+x_3y_3
$$
で計算できる。

### 内積を計算するプログラム

$$
\boldsymbol{x}=(1, 2, 3),  \qquad \boldsymbol{y}=(3, 2, 1)
$$

とする。このとき$\boldsymbol{x}\cdot \boldsymbol{y}$は，次のプログラムで計算することができる

viの起動

```
$ vi ip.f90
```

(ipはinner productの略)

`i`を押してviの挿入モードに入る

```fortran
real,dimension(3)::x,y
x(1)=1; x(2)=2; x(3)=3
y(1)=3; y(2)=2; y(3)=1

x_dot_y=x(1)*y(1)+x(2)*y(2)+x(3)*y(3)

print *,'x_dot_y = ',x_dot_y

end
```

`Esc`を押してviのノーマルモードに戻る

`:wq`を押してviを終了し，シェルに戻る



プログラムをコンパイル，実行して，結果を確認しよう。

```bash
$ ifort ip.f90 -o ip.exe
```

```bash
$ ip.exe
```



結果のチェックには手持ちのスマホを使ってもよいし，googleの検索窓に

```
1*3 + 2*2 + 3*1
```

と入れて, `Enter`キーを押すのでも良い（計算してくれる）。



### 計算結果の逐次代入

```fortran
x = 1.0
print *,'x=',x
x = x + 2.0
print *,'x=',x

end
```

```
$ vi seq.f90 
```

```
$ ifort seq.f90 -o seq.exe
```

```
$ seq.exe
 x=   1.000000    
 x=   3.000000    
```

注: 2回目のxにはx=1.0+2.0が代入されていることに注意

**重要**: **右辺で計算された結果が左辺に代入される** 

------

**左辺　←　右辺**

------



### 繰り返し

```fortran
x_dot_y = 0.0
do i=1,3
x_dot_y = x_dot_y+x(i)*y(i)
end do
```

#### doとend do

`do`と`end do`で挟まれた箇所は繰り返される

繰り返す回数は`do`のすぐ後の, `i=1,3`で指定される。上の例では3回繰り返される

手作業ではできない大量の作業を計算機にさせるのが，プログラム作成の目的であり，そのため大抵のプログラムでは繰り返し処理が重要になる。このためdo文は頻繁に用いられる。do文を用いることで，見やすく，簡潔なプログラムを書くことができる



------

## コラム

### 字下げ

`do`と`end do`の間の行の数が多くなるとプログラムが読みづらくなることがあるので，その場合，下のような字下げを行うことがよくある。字下げする文字数は2文字の場合が多い。

```fortran
do i=1,3
  x = ...
  y = ...
end do
```



------

## 例題

### 階乗の計算

$2!=2\times1$, $3!=3\times2\times1$,  $4!=4\times3\times2\times1$のような階乗を計算するプログラムを作成してみよう。$n!$は次の式で計算できる。
$$
n!=n\times(n-1)\cdots2\times 1
$$
### プログラム例

n=4のとき，上の式をFortranでプログラミングすると下記のようになる。

viの起動

```bash
$ vi fac.f90
```

`i`を押してviの挿入モードに入る

```
n=4
fac=n
do i=n,2,-1
fac=fac*(i-1)
end do

print '(i3,A,f8.0)',n,"!=",fac

end
```

print文のすぐ後の`'(i3,A,f8.0)'`については，下記**書式指定**を参照

do文のすぐ後の`i=n,2,-1`については，下記**do文の書式**参照

`Esc`を押してviのノーマルモードに戻る

`:wq`を押してviを終了し，シェルに戻る



#### 書式指定

print文の後の下記の箇所を書式指定という

```
'(i3,A,f8.0)'
```

i3: 整数のデータを3桁で表示

A: 文字列を表示（文字数は任意）

f8.0: 実数のデータを小数点以下は0桁表示。全体では8文字分用意する。

詳細は下記を参照

https://www.nag-j.co.jp/fortran/FI_14.html#_14



#### do文の書式

```
  do 変数=初期値,最終値[,刻み幅]
    繰り返したい処理
  end do
```

**変数**は通常整数型の変数を使う

**初期値**は変数の最初の値を指定する。ふつうは1だが，1でなくてもよい

**最終値**は変数の最後の値を指定する。

**刻み幅**は何も指定しなければ1となる。**繰り返したい処理**が一回終わるごとに**変数**の値が**刻み幅**だけ増える

**刻み幅**を負の値にすると，変数の値が**刻み幅**だけ減る

```
do i=n,2,-1
fac=fac*(i-1)
end do
```

上記の例では，**変数**iの**初期値**はnで，**最終値**2まで繰り返す。一回**繰り返したい処理**(今の場合`fac=fac*(i-1)`)が終わると，**変数**iの値が1減る。

詳細は下記参照

https://www.nag-j.co.jp/fortran/FI_9.html#_9



#### コンパイル

```
$ ifort fac.f90 -o fac.exe
```

#### 実行ファイルの確認

```
$ ls -lh fac.exe
```

#### 実行

```
$ fac.exe
4!=      24.
```



------

## コラム：プログラムの動作確認

ソースファイルを見ただけでは，動作が分かりにくい箇所があったととする。例えば，下記の箇所は少し分かりにくいかもしれない。

```fortran
do i=n,2,-1
fac=fac*(i-1)
end do
```

このような時は，変数の値を逐次表示させると動作が確認しやすくなる

### viでソースコード入力

```
$ vi fac2.f90
```

`i`でviの挿入モードに入る

```fortran
n=4
fac=n
do i=n,2,-1
print *,'fac (before)=',fac, 'i=',i,' i-1=',i-1
fac=fac*(i-1)
print *,'fac (after)=',fac
print *
end do

print '(i3,A,f8.0)',n,"!=",fac

end
```

`Esc`でviのノーマルモードに入る

`:wq`でviを終了し,シェルに戻る

### コンパイル

```bash
$ ifort fac2.f90 -o fac2.exe
```

### 実行

```bash
$ fac2.exe
```

```
 fac (before)=   4.000000     i=           4  i-1=           3
 fac (after)=   12.00000    

 fac (before)=   12.00000     i=           3  i-1=           2
 fac (after)=   24.00000    

 fac (before)=   24.00000     i=           2  i-1=           1
 fac (after)=   24.00000    

  4!=     24.
```



**上記の手法は，プログラムの誤りを発見する場合に非常に有効であるので，習得しておく必要がある**



## 組み込み関数の利用

Fortranでは，よく使う関数については，すでに作成済みのプログラムが使用できる。内積に関しては，組み込み関数`DOT_PRODUCT`が使用できる。

`DOT_PRODUCT`を使用すると，より簡潔なプログラムを書くことができる。

viの起動

```
$ vi ip2.f90
```

`i`を押してviの挿入モードに入る

```fortran
real,dimension(3)::x,y
x(1)=1; x(2)=2; x(3)=3
y(1)=3; y(2)=2; y(3)=1

x_dot_y=DOT_PRODUCT(x,y)

print *,'x_dot_y = ',x_dot_y

end
```

`Esc`を押してviのノーマルモードに戻る

`:wq`を押してviを終了し，シェルに戻る



プログラムをコンパイル，実行して，結果を確認しよう。

```bash
$ ifort ip2.f90 -o ip2.exe
```

```bash
$ ip2.exe
```



### 組み込み関数を使う利点

・プログラムが簡潔になる

・組み込み関数は計算が早く終わるように，様々な工夫がこらされている

### 参考

第 2 章 Fortran 95 組み込み関数

https://docs.oracle.com/cd/E19205-01/820-1201/aetja/index.html



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
