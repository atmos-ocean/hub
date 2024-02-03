# プログラミング入門_07

[TOC]

## 概要

ここでは，本格的なプログラミングへの準備として，

- 複数のソースファイルから，一つの実行ファイルを作る方法 (makeコマンドの使用法)

- よく使うサブルーチンをひとまとめにする方法 (ライブラリの作成法)

について学ぶ




## 準備

```
$ cd $HOME
```

```
$ cd 2022_PROGRAM
```

```
$ mkdir -vp 07
```

```
$ cd 07
```

```
$ pwd
/work03/gu1/LAB/2022_PROGRAM/07
```

```
$ ift
```

もしくは

```
$ source  /opt/intel/oneapi/setvars.sh --force
```



## ソースファイルの分割

`$ vi main.f90`

```fortran
program main

a=10.0
b=2.0

print *,'a=',a
print *,'b=',b

call sub(c,a,b)

print *,'c=',c

end program main
```


`vi sub.f90`

```fortran
subroutine sub(c,a,b)

real,intent(in)::a,b
real,intent(out)::c

c=a+b

end subroutine sub
```
`$ ifort main.f90 sub.f90 -o test_sub.exe`     

`$ ll test_sub.exe`  
-rwxrwxr-x 1 am am 8.6K  8月 22 13:55 test_sub.exe       

`$ test_sub.exe`   
 a=   10.0000000    
 b=   2.00000000    
 c=   12.0000000      



## 分割コンパイルとリンク

### コンパイルの手順

コンパイル手順を詳しく書くと，  

ソースファイル →オブジェクトファイル→実行ファイル

となる。普段はこの一連の作業を人間に見せないようにしているが，ここでは，このような手順を踏んでいることを理解しておく必要がある。



### 分割コンパイル

```
$ ifort -c main.f90

$ ifort -c sub.f90

$ ll main.o sub.o
```

main.oとsub.oのようなファイルをオブジェクトファイルと呼ぶ。

`-c`はオブジェクトファイルを作るところで作業を止め，実行ファイルは作らないことを意味する。



### リンク

複数のオブジェクトファイルを結合させて実行ファイルを作成することをリンクと呼ぶ。

```
$ ifort main.o sub.o -o test_sub2.exe
```

```
$ ll test_sub.exe test_sub2.exe
```



### 実行

```
$ test_sub2.exe
```



## Makefileとmakeコマンド 

### makeコマンドとは

プログラムの規模が大きくなりソースファイルの数が増えてくると，コンパイルに時間がかかるようになる。

そのため，更新されたソースファイル**だけ**をコンパイルするようにすると時間が短縮できる。  

この用途で使うコマンドにmakeコマンドがある。    

makeコマンドはmakefileという名称のファイルから情報を読み取り，コンパイルに必要な作業を行う。

名称は頭文字のみ大文字のMakefileの場合もある。



### 簡単な例

`$ cat makefile`

```makefile
test_sub.exe: main.o sub.o
        ifort -o test_sub.exe main.o sub.o
main.o: main.f90
        ifort -c main.f90
sub.o: sub.f90
        ifort -c sub.f90
clean:
        rm -vf main.o sub.o test_sub.exe
```



### 実行例

```
$ make clean
```

rm -vf main.o sub.o test_sub.exe  
'main.o' を削除しました  
'sub.o' を削除しました  
'test_sub.exe' を削除しました  

`rm -vf main.o sub.o test_sub.exe`が実行され，各ファイルが削除される。   

makeの後に書かれた文字列（ここではclean）のことを**ターゲット**と呼ぶ。 



```
$ date -R
```

Thu, 22 Aug 2019 13:56:37 +0900

```
$ make
```

ifort -c main.f90  
ifort -c sub.f90  
ifort -o test_sub.exe main.o sub.o  

オブジェクトファイルmain.o, sub.oと実行ファイル test_sub.exeが削除されているので，ソースファイルのコンパイルとオブジェクトファイルのリンクが行われる。



```
$ ll test_sub.exe
```

-rwxrwxr-x 1 am am 8.6K  8月 22 13:56 test_sub.exe     



```
$ date -R
```

Thu, 22 Aug 2019 13:57:33 +0900  



```
$ touch sub.f90
$ ll main.f90 sub.f90
```

-rw-rw-r-- 1 am am 122  8月 21 19:44 main.f90  
-rw-rw-r-- 1 am am  99  <u>8月 22 13:57</u> sub.f90  

  

```
$ make
```

ifort -c sub.f90   
ifort -o test_sub.exe main.o sub.o      

更新されたのはsub.f90だけなので，sub.f90だけをコンパイルする。  

main.f90のコンパイル済みなので，コンパイル結果のmain.oをそのままリンクする。  



### makefileの書式

#### makefileの例

```makefile
test_sub.exe: main.o sub.o
        ifort -o test_sub.exe main.o sub.o
main.o: main.f90
        ifort -c main.f90
sub.o: sub.f90
        ifort -c sub.f90
clean:
        rm -vf main.o sub.o test_sub.exe
```



#### 書式

日常の言葉でmakefileの書式を説明すると，次のようになる。  

```
作りたいもの: 材料
	作り方
```

- 「作りたいもの」のことをターゲットと呼ぶ。

- 「材料」のことを依存関係行と呼ぶ

- 「作り方」には通常コマンドとそのオプション，コマンドの引数が書かれる



以下ひとまとまりを**ルール**と呼ぶことがある。

```
ターゲット: 依存関係行
	コマンド（ターゲットを作成するために必要な作業）
```

**コマンドは タブ(TAB)の後に書く必要がある**。スペース（空白文字）は**不可**。  

人間にはどちらも同じ空白文字に見えるが，コンピュータの内部ではタブとスペースは区別されている。



#### ターゲット

##### makeコマンドでターゲットを指定する場合

makeコマンド実行の際に,  

```
$ make main.o
```

のように**ターゲットを指定した場合**，makeコマンドはmakefileの**該当のターゲットが記述された部分をまず参照する**。今の例では，makefileの   

```makefile
main.o: main.f90
        ifort -c main.f90
```

の部分を参照する。上の部分に従って，コマンド

```
ifort -c main.f90
```

を実行し，ターゲットとなっているmain.oを作成する。



##### makeコマンドでターゲットが指定されていない場合

makeコマンド実行時に**ターゲットが指定されていない場合**，まずmakefileの**一番上のルール**  

```makefile
test_sub.exe: main.o sub.o
        ifort -o test_sub.exe main.o sub.o
```

に従って，ターゲットtest_sub.exeの作成のために必要な作業を行う。

もし，依存関係行に記載されたmain.oやsub.oに更新が必要な場合，下記のルールに従って，main.oやsub.oを作成する。

```makefile
main.o: main.f90
        ifort -c main.f90
```

```makefile
sub.o: sub.f90
        ifort -c sub.f90
```



##### 依存関係行がないターゲット

cleanというターゲットには依存関係行がない。ターゲットとしてcleanを指定した場合，無条件にコマンドが実行される。このようなターゲットをphonyターゲットと呼ぶ。



### 詳細な情報

**下記リンクも読んでおくこと**

- http://omilab.naist.jp/~mukaigawa/misc/Makefile.html
- https://ie.u-ryukyu.ac.jp/~e085739/c.makefile.tuts.html





## ライブラリとは

- よく使う複数のサブルーチンをコンパイルしてまとめたもの

- コンパイル済みなので，毎回コンパイルする必要がない

- 既存のライブラリを活用することで，新規にプログラムする量を減らすことができる

**現在使われている様々なソフトウェアの大半でライブラリが使われている**。ここで述べることはごく基本的なことだけだが，理解しておくとコンピューターの扱いが格段に上達する。



## ライブラリの例

linux自体が数多くのライブラリを使用している。  

```
ls -R /usr/lib |grep .so
```

libopamgt.**so**@
libopamgt.**so**.0@
libopamgt.**so**.0.4.0*
.....



ファイル名の末尾付近が**so**となっているファイルはライブラリ（下記の動的ライブラリ）である。soはshared objectの略とされる ([IT用語辞典](https://e-words.jp/w/.so%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB.html))。   

```
$ ls /usr/local/lib
```

`/usr/lib`や`/usr/local/lib`に数多くのライブラリが収納されている。  

Windows, Mac, iphoneなども内部で数多くのライブラリを使用している。

Windowsでは.dllで終わるファイルがライブラリである。エクスプローラーを開いて, C:¥Windowsディレクトリに移動して, dllをキーワードで検索すると数多くのライブラリの存在が確認できる。

### 重要な注意

ライブラリが無くなるとソフト(アプリ)が動かなくなるので，不用意にライブラリを削除したり，移動させたりしない。



## ライブラリの種類

### 静的ライブラリ

*Static link library*  

**リンク時**にライブラリを実行ファイルに追加する。実行ファイルにはライブラリも取り込まれている。  

簡便だが実行ファイルの容量が大きくなる  



### 動的ライブラリ

*Dynamic link library* (*DLL*)  

実行ファイルにはライブラリは含まれず，**実行時**にライブラリを読み込む 。 

多数のプログラムに利用されるライブラリは動的ライブラリにしておくと利便性が高くなる。

また，実行ファイルの容量を小さくできる。

## 静的ライブラリ

### 準備

サンプルのサブルーチンをもう一つ用意しておく  

```
$ vi sub2.f90
```

```FORTRAN
subroutine sub2(c,a,b)

real,intent(in)::a,b
real,intent(out)::c

c=a-b

end subroutine sub2
```

main.f90をmain2.f90という名前でコピーして，**sub2を呼び出すようにを書き換える**。

```
$ cp main.f90 main2.f90 
```

```
$ vi main2.f90  
```

```FORTRAN
program main2

a=10.0
b=2.0

print *,'a=',a
print *,'b=',b

call sub(c,a,b)
print *,'c=',c

call sub2(d,a,b)
print *,'d=',d

end program main2
```



### ライブラリの作成

#### サブルーチンのコンパイル

```bash
$ ifort -c sub.f90 sub2.f90
```

```bash
$ ll sub.o sub2.o
```

-rw-r--r--. 1 am 1.1K 2022-10-20 15:37 sub.o  
-rw-r--r--. 1 am 1.1K 2022-10-20 15:37 sub2.o  



### ライブラリの作成

ar (archiver)というコマンドを使って，オブジェクトファイルsub.oとsub2.oからslib_sample.aという名称のライブラリファイルを作成する  

```bash
$ ar rc slib_sample.a sub.o sub2.o
```

```bash
$ ll slib_sample.a
```

-rw-r--r--. 1 am 2.4K 2022-10-20 15:38 slib_sample.a  



#### arコマンドについて

##### ライブラリを作成する  

```bash
$ ar rc slib_sample.a sub.o sub2.o
```

オブジェクトファイルsub.oとsub2.oから，ライブラリslib_sample.aを作成する  

##### ライブラリ中のファイル名を表示する  

```bash
$ ar t slib_sample.a
```



#### メインルーチンのコンパイル

```bash
$ ifort -c main2.f90
```

```bash
$ ll main.o
```



#### ライブラリのリンク

メインルーチンmain2.oとライブラリslib_sample.aを結合する  

```bash
$ ifort  main2.o -o slib_test.exe -L. slib_sample.a
```

`-L.`はリンクしたいライブラリのファイルがカレントディレクトリ`.`にあることを意味する。  



```bash
$ ll slib_test.exe 
```

-rwxr-xr-x. 1 am 829K 2022-10-20 15:47 slib_test.exe*



#### 実行

```
$ slib_test.exe
```

 a=   10.00000    
 b=   2.000000    
 c=   12.00000    
 d=   8.000000  



## 動的ライブラリ

### 準備

サブルーチンのオブジェクトファイルを削除しておく  

```bash
$ rm -v sub.o sub2.o
```



### ライブラリの作成

```bash
$ ifort -shared -fPIC sub.f90 sub2.f90 -o dlib_sample.so
```

```bash
$ ll dlib_sample.so
```



### ライブラリのリンク

```
$ ifort main2.o -o test_dlib.exe -L. dlib_sample.so
```

### 実行

$ test_dlib.exe  
 a=   10.00000    
 b=   2.000000    
 c=   12.00000    
 d=   8.000000    

**リンクに失敗して実行できない場合，末尾の補足を参照のこと**。



### 実行時にリンクされているライブラリの確認

```
$ ldd test_dlib.exe 
```

​        linux-vdso.so.1 =>  (0x00007ffd25d80000)  
​        **dlib_sample.so** (0x00007fc615f26000)  



### リンク時の-lオプション

-l (小文字のエル)オプションを指定すると、ライブラリ名がlibで始まるファイルであることが仮定される。 リンク時に

```
-labc
```

というオプションを指定すると、libabc.so* (*は任意の文字列)というライブラリをリンクしようとする。



## makeコマンドによるライブラリの作成

以下のmakefileは下記を行うためのものである。

- sub.f90, sub2.f90からlibtest.soという動的ライブラリを作成する

- main2.f90からmain2.oというオブジェクトファイルを作成する

- main2.oにlibtest.soをリンクし, test_dlib2.exeという実行ファイルを作成する

- `make clean`で，main.o, sub.o, sub2.o, libtest.soを削除する
```
$ cat makefile
```
```makefile
test_dlib2.exe:main2.o libtest.so
        ifort -traceback -CB main2.o -o test_dlib2.exe -L. -ltest
libtest.so:sub.f90 sub2.f90
        ifort -shared -fPIC sub.f90 sub2.f90 -o libtest.so
main2.o: main2.f90
        ifort -traceback -CB -c main2.f90
clean:
        rm -fv main.o sub.o sub2.o libtest.so
```

使用例

```
$ make 
```

```bash
ifort -shared -fPIC sub.f90 sub2.f90 -o libtest.so
ifort -traceback -CB main2.o -o test_dlib2.exe -L. -ltest 
```



### 練習1

wvflux.f90, q_by_wind.f90, qvapor.f90という3つのソースファイルが与えられたとする (問題文の最後に記載)。

- wvflux.f90: メインルーチン 
  - 入力データを与える
  - サブルーチンを呼び出す,
  - 計算結果を出力する
  
- qvapor.90
  - 水蒸気混合比を計算する
  
-　q_by_wind.f90
  - 水蒸気混合比と風速の積を計算する
  
    

(1) 上記のmakefileをもとに次の作業を行うのmakefileを作成せよ

- qvapor.980, q_by_wind.f90からlibflux.soという動的ライブラリを作成する
- wvflux.f90からwvflux.oというオブジェクトファイルを作成する

- wvflux.oにlibflux.soをリンクし, wvflux.exeという実行ファイルを作成する

- `make clean`で，wvflux.o,  qvapor.o, q_by_wind.o, libflux.soを削除する

#### 重要な注意

作業を開始する前に, cpコマンドで元の**makefileのバックアップを取っておく**

```bash
$ cp -a makefile makefile.ORG
```



(2) makeコマンドの実行結果をファイルに出力せよ。下記のコマンドで，実行結果がMAKE.LOGというファイルに書き出される。

```bash
make &> MAKE.LOG
```

 (3) wvflux.exeの実行結果をファイルに出力せよ。



#### 使用するソースファイル

wvflux.f90

```fortran
u=10.0   !m/s
v=5.0    !m/s
tc=27.0  !degC
rh=80.0  !%
p=1000.0 !hPa
rho=1.2  !kg/m3

print '(A)','INPUT:'
print '(A,f7.2)','u   =',u 
print '(A,f7.2)','tc  =',tc
print '(A,f7.2)','rh  =',rh
print '(A,f7.2)','p   =',p 
print '(A,f7.2)','rho =',rho
print *

call qvapor(tc,rh,p,q)
print '(A,f7.4)','q =',q 

call q_by_wind(q,u,qu)
print '(A,f7.3)','qu =',qu 

call q_by_wind(q,v,qv)
print '(A,f7.3)','qv =',qv

wvx=rho*qu; wvy=rho*qv
wvm=sqrt(wvx**2+wvy**2)

print '(A,f7.3,A)','wvx =',wvx,' [kg/m2/s]'
print '(A,f7.3,A)','wvy =',wvy,' [kg/m2/s]'
print '(A,f7.3,A)','wvm =',wvm,' [kg/m2/s]'

end
```

qvapor.f90

```fortran
subroutine qvapor(tc,rh,p,q)
real,intent(in)::tc,rh,p
real,intent(inout)::q
real,parameter::eps=0.622

es=6.112*exp(17.67*tc/(tc+243.5)) !hPa !BOLTON 1980
 e=es*rh/100.0
 q=eps*e/(p-e)

end
```

[水蒸気混合比の計算式](./QVAPOR.md)




q_by_wind.f90

```fortran
subroutine q_by_wind(q,v,qv)
real,intent(in)::q,v
real,intent(inout)::qv

qv=q*v

end
```



## 補足: 動的ライブラリのリンク失敗例と対応策

### 要旨

動的ライブラリを探しに行くディレクトリに,カレントディレクトリ `.` を追加する。

```bash
$ export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:.
```

**LD_LIBRARY_PATH**: 共用ライブラリが格納されているパスを指定する。



### 説明に使用するプログラム

**main2.f90**

```
program main2
a=10.0
b=2.0
print *,'a=',a
print *,'b=',b
call sub(c,a,b)
print *,'c=',c
call sub2(d,a,b)
print *,'d=',d
end program main2
```

**sub.f90**

```
subroutine sub(c,a,b)
real,intent(in)::a,b
real,intent(out)::c
c=a+b
end subroutine sub
```

**sub2.f90**

```
subroutine sub2(d,a,b)
real,intent(in)::a,b
real,intent(out)::d
d=a-b
end subroutine sub2
```



### エラーの再現

```
$ ifort -shared -fPIC sub.f90 sub2.f90 -o dlib_sample.so
```

```
$ ifort main2.o -o test_dlib.exe -L. dlib_sample.so 
```

```
$ ldd test_dlib.exe 
```

linux-vdso.so.1 => (0x00007ffe74b92000)  
dlib_sample.so => **not found**  
dlib_sample.soをリンクしようとしても見つからない,というメッセージ。  



### 対応

#### リンク先ディレクトリ追加

動的ライブラリを探しに行くディレクトリに,カレントディレクトリ `.` を追加する。

```
$ export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:.
```

**LD_LIBRARY_PATH**: 共用ライブラリが格納されているパスを指定する。  



#### 再度リンク

```
$ ifort main2.o -o test_dlib.exe -L. dlib_sample.so
```

```
$ ldd test_dlib.exe
```

linux-vdso.so.1 => (0x00007ffc399a7000)  
dlib_sample.so => ./dlib_sample.so (0x00007f9080013000)  
**今度はdlib_sample.soが見つかった **(カレントディレクトリ `.` にあると認識されている)。



#### テスト

プログラムを実行させてみる。

```
$ test_dlib.exe
```

a= 10.00000  
b= 2.000000  
c= 12.00000  
d= 8.000000   

**実行できた**。



### 恒久的な対応

$HOME/.bashrcに

```
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:.
```

を追加し，

```
source $HOME/.bashrc
```

で，bashのユーザー設定ファイル（.bashrc）を再度読み込む。

#### 注意

LD_LIBRARY_PATHの設定を以下のファイルに記載しても反映されない。

- $HOME/.profile (.bashrcはOK)
- /etc/profile
- /etc/environment

#### 参考資料

CentOS7でldconfigを使って共有ライブラリを追加する
https://qiita.com/Esfahan/items/0064d845ca6faf7f3d47



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
