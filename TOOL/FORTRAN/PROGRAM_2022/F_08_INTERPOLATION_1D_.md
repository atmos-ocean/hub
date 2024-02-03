# 線形補間 (1次元)

[[_TOC_]]

## 目標

**線形補間の計算手順の確認**

**プログラム作成手順の習得**

**プログラムの基本構造の理解**



## 0. 準備

### インテル・フォートラン

```
$ grep -n ift /etc/bashrc
108:alias ift='source /opt/intel/oneapi/setvars.sh'
```

```
$ grep -n ift $HOME/.bashrc
19:alias ift='source /opt/intel/oneapi/setvars.sh'
```

```
$ ift
 
:: initializing oneAPI environment ...
   -bash: BASH_VERSION = 4.2.46(2)-release
   args: Using "$@" for setvars.sh arguments: 

:: compiler -- latest

:: oneAPI environment initialized ::
```



### コピー

### コマンド確認

```
$ which mkd
~/mybin/mkd
```

```
$ cat $(which mkd)
```

```
$ less $(which mkd)
```

```
$ which myymdh
~/mybin/myymdh
```

```
$ cat $(which myymdh)
```

```
$ less $(which myymdh)
```

### ディレクトリ作成

```
$ mkd $HOME/00.TOOLS/ANALYSIS
```

```
$ cd $HOME/00.TOOLS/ANALYSIS
```

### 使用ファイルコピー

```
$ cp -ar /work03/am/LAB/00.SKILL/ANALYSIS/2022-10-07_08_INTERPOLATION_1D .
```



## 1. 線形補間のプログラム作成

### プログラム作成

#### 設計

1. 設定

2. データ読み込み

3. 計算

4. 計算結果表示



### 1. 設定

```
$ vi INT1D.F90
```

骨格

```
PROGRAM INT1D

END PROGRAM INT1D
```

変数宣言

```
PROGRAM INT1D
REAL X1,Y1, X2, Y2, X
REAL Y

END PROGRAM INT1D
```



### 2. データ読み込み

```
PROGRAM INT1D
REAL X1,Y1, X2, Y2, X
REAL Y

READ(5,*)X1,Y1, X2, Y2, X

END PROGRAM INT1D
```



### 3. 計算

```
PROGRAM INT1D
REAL X1,Y1, X2, Y2, X
REAL Y

READ(5,*)X1,Y1, X2, Y2, X

Y = (Y2-Y1)/(X2-X1)*(X-X1)+Y1

END PROGRAM INT1D
```



### 4. 計算結果表示

```
PROGRAM INT1D
REAL X1,Y1, X2, Y2, X
REAL Y

READ(5,*)X1,Y1, X2, Y2, X

Y = (Y2-Y1)/(X2-X1)*(X-X1)+Y1

WRITE(6,*)'X1, Y1, X2, Y2=',X1, Y1, X2, Y2
WRITE(6,*)'X, Y=',X,Y

END PROGRAM INT1D
```



## 2. 入力データ作成

```
$ vi INPUT.TXT 
```

75.0 15.16 118.0 14.48  100.0



## 3. コンパイル

```
$ ifort INT1D.F90 -o INT1D.exe

$ ll INT1D.exe
```

 

## 4. 実行

```
$ INT1D.exe
```

CTL+C

```
$ INT1D.exe < INPUT.TXT
```

```
 X1, Y1, X2, Y2=   75.00000       15.16000       118.0000       14.48000    
 X, Y=   100.0000       14.76465 
```



## サブルーチン化

主プログラム: INT1D_MAIN.F90

```fortran
X1=75.0; Y1=15.16; X2=118.0; Y2=14.48
X=100.0 
WRITE(6,*)'X1, Y1, X2, Y2=',X1, Y1, X2, Y2

CALL INT1D(X1,Y1,X2,Y2,X,Y) !サブルーチンを呼ぶ

WRITE(6,*)'X, Y=',X,Y

END
```

副プログラム: INT1D.F90 (サブルーチンINT1Dを記述)

```fortran
SUBROUTINE INT1D(X1,Y1,X2,Y2,X,Y)
REAL,INTENT(IN)::X1,Y1,X2,Y2,X   !入力用の変数
REAL,INTENT(OUT)::Y              !出力用の変数

Y = (Y2-Y1)/(X2-X1)*(X-X1)+Y1    !線形補間

END SUBROUTINE
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
