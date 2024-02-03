# バイリニア補間 (2次元)

[[_TOC_]]

## 目標

**バイリニア補間の計算手順の確認**

**プログラム作成手順の復習**

**計算結果の確認方法の習得**



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
$ ll $HOME/00.TOOLS/ANALYSIS
```

```
$ mkd $HOME/00.TOOLS/ANALYSIS
```

```
$ cd $HOME/00.TOOLS/ANALYSIS
```

### 使用ファイルコピー

```
$ cp -ar /work03/am/LAB/00.SKILL/ANALYSIS/2022-10-07_09_2022-10-07_09_BILINEAR .
```



## 1. バイリニア補間のプログラム作成

### プログラム作成

1. 設計

2. 設定

3. データ読み込み

4. 計算

5. 計算結果表示

   

### 1. 設計

#### 各データの位置関係を決める

```
! Subscript, A indicates the input.

! For given XA(i),YA(i),ZA(i),X & Y, Z is estimated.
!
! (XA(4),YA(4),ZA(4))   (XA(3),YA(3),ZA(3))
!        +------------------+
!        |                  |
!        |    X             |
!        | (X,Y,Z)          |
!        |                  |
!        |                  |
!        |                  |
!        +------------------+
! (XA(1),YA(1),ZA(1))   (XA(2),YA(2),ZA(2))
```



#### 計算式の確認

##### 内分点を決める定数, T, U

```FORTRAN
T=(X-XA(1))/(XA(2)-XA(1))
U=(Y-YA(1))/(YA(4)-YA(1))
```

##### バイリニア補間

```
 Z =  (1.-T)*(1.-U)*ZA(1) &
&  + T*(1.-U)*ZA(2) &
&  + T*U*ZA(3) &
&  + (1.-T)*U*ZA(4)
```



### 2. 設定

```
$ vi BI.F90
```

骨格

```
PROGRAM BI

END PROGRAM BI
```

変数宣言

```
PROGRAM BI
REAL,DIMENSION(4)::XA,YA,ZA
REAL X, Y
REAL Z

END PROGRAM BI
```



### 2. データ読み込み



```
PROGRAM BI
REAL,DIMENSION(4)::XA,YA,ZA
REAL X, Y
REAL Z

DO I=1,4
READ(5,*)XA(I),YA(I),ZA(I)
END DO !I
READ(5,*)X, Y

END PROGRAM BI
```

```
ESC wq
```

もしくは

```
jk wq
```

で一旦終了

### 3. 計算

計算の主要部を独立させる

サブルーチンと呼ばれる機能を用いる

サブルーチン = SUB + ROUTINE

```
$ vi BILINEAR.F90
```



```
SUBROUTINE BILINEAR(XA,YA,ZA, X,Y, Z)
REAL, INTENT(IN) :: XA(4), YA(4), ZA(4)
REAL, INTENT(IN) :: X,Y
REAL, INTENT(OUT) :: Z

REAL T, U
T=(X-XA(1))/(XA(2)-XA(1))
U=(Y-YA(1))/(YA(4)-YA(1))

 Z =  (1.-T)*(1.-U)*ZA(1) &
&  + T*(1.-U)*ZA(2) &
&  + T*U*ZA(3) &
&  + (1.-T)*U*ZA(4)

END SUBROUTINE BILINEAR
```



### 4. サブルーチン呼び出しと計算結果表示

```
PROGRAM BI
REAL,DIMENSION(4)::XA,YA,ZA
REAL X, Y
REAL Z

DO I=1,4
READ(5,*)XA(I),YA(I),ZA(I)
END DO !I
READ(5,*)X, Y

CALL BILINEAR(XA,YA,ZA, X,Y, Z)

DO I=1,4
WRITE(6,*)XA(I),YA(I),ZA(I)
END DO !I
WRITE(6,*)X, Y, Z

END PROGRAM BI
```

ESC wq

jk wq

で終了



## 2. 入力データ作成

```
$ vi INPUT.TXT 
```





## 3. コンパイル

```
$ ifort BI.F90 BILINEAR.F90 -o BI.exe

$ ll BI.exe
```

 

## 4. 実行

```
$ BI.exe < INPUT.TXT
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
