# プログラミング入門_03

[TOC]

## 予備知識

下記が必要な予備知識である。

### Linuxのシェルの基本操作

https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/01.BASH/0.LINUX_TUTORIAL_01.md

### シェルスクリプトの文法の基礎

https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/03.BASH_SCRIPT/LINUX_SCRIPT_TUTORIAL.md



## Linux上での準備

```
$ cd $HOME
```

```
$ cd 2022_PROGRAM
```

```
$ mkdir -vp 03
```

```
$ cd 03
```

```
$ pwd
/work03/gu1/LAB/2022_PROGRAM/03
```

```
$ source  /opt/intel/oneapi/setvars.sh --force
```



## 復習　(画面表示と四則演算のプログラム)

### 手順

viを起動 

`$ vi prog1.f90`

`i`でviの挿入モードに移る

ブラウザ上で下記のプログラムをコピー

```fortran
print *,"HELLO, WORLD!"
print *,'1+1=',1+1
print *

A=1.0
print *,'A=',A
print '(a,f5.1)','A=',A
print '(a,f5.3)','A=',A
print *

i=1
print *,'i=',i
print *

I=2
print *,'I=',I
print '(a,i3)','i=',i
print '(a,i3.3)','i=',i
print *

stop
end
```

viの画面にペースト（貼り付け）

`Esc`でノーマルモードに移る

`:wq`でvi終了



### ソースファイルの確認

`$ ll`
合計 8.0K
-rw-rw-r-- 1 am am 263  8月 20 19:35 prog1.f90

llはls -lの省略形

### コンパイル

`$ ifort prog1.f90 -o prog1.exe`

### 実行ファイルの確認

`$ ll`
合計 24K
-rwxrwxr-x 1 am am 13K  8月 20 19:36 prog1.exe
-rw-rw-r-- 1 am am 263  8月 20 19:35 prog1.f90

### 実行

`$ prog1.exe`
 HELLO, WORLD!
 1+1=           2

 A=   1.00000000    
A=  1.0
A=1.000

 i=           1

 I=           2
i=  2
i=002



------

## コラム: Fortranの変数(1)

変数は型で区別される。最も重要な型は，**整数型** (integer), **実数型** (real)の2つである。

| 型名       | 別名             | 最小値       | 最大値       | 備考                         |
| ---------- | ---------------- | ------------ | ------------ | ---------------------------- |
| integer(2) | integer          | $−2^{15}$    | $2^{15}−1$   |                              |
| integer(4) | なし             | $−2^{31}$    | $2^{31}−1$   |                              |
| integer(8) | なし             | $−2^{63}$    | $2^{63}−1$   |                              |
| real(4)    | real             | $∼10^{-38}$  | $∼10^{+38}$  | 値は絶対値, **精度は約7桁**  |
| real(8)    | double precision | $∼10^{-308}$ | $∼10^{+308}$ | 値は絶対値, **精度は約16桁** |

**型名**の括弧の中の数字は，変数一つあたりのバイト (byte)単位で表したデータ容量（バイトについては下記コラム参照）

整数型の変数に実数の値を入れることはできない。例えば，整数型の変数 iに1.3という小数の値を入れることはできない。

ほとんどのコンピューターにおいて，iが整数型のとき，プログラム中に`i=1.3`と書いた場合，`i`には1という値が代入される。

実数型の変数に整数を代入する場合，多くのコンピューターでは変数の値は整数から実数に変換される。例えば，実数型の変数aに1という値を代入すると，aには1.0という値が保存される

real(4)のことを**単精度**，real(8)のことを**倍精度**ということがある

変数の型はプログラムの一番最初で指定する

例

```
integer i
real a
```

その他の変数の型として，文字型と論理型がある（後述）

------



------

## コラム：データ容量の単位

コンピューターで用いるデータの大きさを表す一番小さな単位はビットである。

一桁の2進数で表すことのできるデータ量 (すなわち0か1のどちらか)を1ビットと呼ぶ。

次に大きな単位がバイト (byte)で，1バイト=8ビットである。

8ビットは10進数に直すと ($2^8=256$)であるから，10進数の数字に直すと，0から255までの数値を表すことができる。コンピューターの世界で頻繁に256という数値が出てくるのはこのため。

以下 10ビット倍 (10進数に直すと $2^{10}=1024$)ごとに単位が大きくなる。よく出てくる単位は以下の通り。

1KB（キロバイト）＝1024バイト
1MB（メガバイト）＝1024KB（約100万バイト）
1GB（ギガバイト）＝1024MB（約10億バイト）
1TB（テラバイト）＝1024GB（約１兆バイト）
1EB (エクサバイト) = 1024TB (約100京バイト)

参考

https://www.networld.co.jp/sp/learn_first/storage/p22_24.html

Fortranの実数型以外の変数が何バイトになるかは下記参照

https://jp.xlsoft.com/documents/intel/cvf/vf-html/pg/pg20.htm

------



------

## コラム: Fortranの変数(2)
- 宣言せずに変数を使うことができる（短いプログラムを書くとき便利）

- プログラムの先頭に`implicit none`と書くことで、上の機能を無効化できる（長いプログラムを書くとき便利）

- 宣言せずに名前がi, j, k, l, m, nで始まる変数を使うと、整数型と認識される。それ以外の変数は実数型と認識される

### 参考

Fortran 入門 4 定数と変数

https://www.nag-j.co.jp/fortran/FI_4.html

------



## テキストファイルの書き出しと読み込み

### テキストファイルへの書き出し
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

​    

`file=`の後にファイル名を書く。上の例では，ofleという変数の中身 (random_number.txt)がファイル名になる

​    

`write(21,'(i5)')n`で変数nに記憶されている数値(上の例では10)がファイルに書き出される

  

`'(i5)'`は出力を行う際の書式を意味しており，この場合は5文字分を用意し，整数として書き出す。したがって ，実数が保存されている変数をこの書式で書き出すと値がおかしくなる)

  

`do`と`end do`で囲まれた部分は**繰り返される**（後述）

  

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

### 参考

組み込み手続き  
https://www.nag-j.co.jp/fortran/FI_10.html
https://www.rs.kagu.tus.ac.jp/yama/f90/f-mst.html

------



### コンパイル

`$ ifort textout.f90 -o textout.exe`

### 実行ファイルの確認

`$ ll`
-rwxrwxr-x 1 am am  14K  8月 20 20:00 textout.exe
-rw-rw-r-- 1 am am  443  8月 20 20:00 textout.f90

### 実行

`$ textout.exe  `
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
`$ vi textread.f90  `

`i`で挿入モードに移る  

ブラウザ上で下記のプログラムをコピー  

```fortran
program textread

real,allocatable::a(:)
character(len=100):: infle

infle="random_number.txt"

print *
print '(A,A)','INPUT FILE = ',trim(infle)
print *

open(11,file=infle,action="read")
read(11,'(i5)')n
allocate(a(n))

do i=1,n
read(11,*)a(i)
end do
close(11)

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

確かに`textout.exe`が作成したファイル (random_number.txt)が，`textread.exe`によって読み込まれていることが確認できた。



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



`action="read"`でファイルを**読み込み専用**で開くように指示している（**誤って上書きしないようにできる**）  



`read(11,'(i5)')n`で，ファイル(random_number.txt)の1行目に書き込まれている数値（ここでは10）がnという変数に読み込まれる。  



`allocate(a(n))`は配列のサイズを決めるための文（後述）  



`do`と`end do`で囲まれた部分は繰り返される（後述）  



`read(11,'(f10.5)')a(i)`でrandom_number.txtに書き込まれている数値が，a(i)に読み込まれる  



`close(11)`で開いたファイルを閉じる  



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

