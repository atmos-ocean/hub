# NCL Tutorial 02

[[_TOC_]]

## はじめに

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



## 練習1: 海面水温の分布図の作図

### スクリプト

#### EX02-01.NCL

```bash
print("READ INPUT DATA.")
INDIR="/work01/DATA/ERA5/EASIA/6HR/SFC/SST/07BP02/"
;入力ファイルの存在するディレクトリ名

INFLE="ERA5_EASIA_SST_6HR_202107_02.grib"
;入力ファイル名

IN=INDIR+INFLE
;INDIRとINFLEをつないで一つの変数INを作る

print("IN="+IN)
; 変数INの内容を画面に表示する

a=addfile(IN,"r")
; 読み込み(r)モードで変数INに記憶されたファイルを開く
; aはファイルを指すポインター

SST=a->SSTK_GDS0_SFC
; ポインターaで指定された入力ファイルに含まれるSSTK_GDS0_SFCという
; 名称の変数に記憶されたデータを読み込む。
; 読み込み先の変数の名称はSSTとする


print("MMMMM PLOTTING")
; 作図開始

FIG="ERA5_SST_EX02-01"
TYP="pdf"
; 図のファイル名とファイルの種類(pdf)を指定

wks = gsn_open_wks(TYP, FIG)
;作図するファイルを開く

res=True
; 作図用の設定に関する情報を記載する変数(res)を用意する
res@cnFillOn = True
; Trueの場合, 色で塗分けする
res@cnLinesOn = False
; Falseの場合, 等値線を書かない

res@gsnAddCyclic = False
;地球一周分のデータでない場合Falseにする

plot1=gsn_csm_contour_map(wks,SST(0,:,:),res)
; カラーシェード図と地図の作図

print("MMMMM IN : "+IN)
print("MMMMM FIG: "+FIG+"."+TYP)
```

#### 図のファイル

ERA5_SST_EX02-01.pdf



## 練習2: 地図の範囲指定

先ほどの例だと，データが存在しているのが東アジア域だけであるにも関わらず，地図が世界地図となっていまっていた。ここでは，地図の範囲を指定する方法を練習する。

具体的には，下記のような文を追加する。

```bash
resmp=True
; 地図の描画の設定に関する情報を記載する変数(resmp)を用意する

resmp@mpMinLatF = 0.
; 地図の南の端
resmp@mpMaxLatF = 50.
; 地図の北の端
resmp@mpMinLonF = 80.
; 地図の西の端
resmp@mpMaxLonF = 150.
; 地図の東の端

res=resmp
; resmpの情報をresに引き継ぐ
```

### スクリプト

#### EX02-02.NCL

```bash
print("MMMMM READ INPUT DATA.")
INDIR="/work01/DATA/ERA5/EASIA/6HR/SFC/SST/07BP02/"
;入力ファイルの存在するディレクトリ名

INFLE="ERA5_EASIA_SST_6HR_202107_02.grib"
;入力ファイル名

IN=INDIR+INFLE
;INDIRとINFLEをつないで一つの変数INを作る

print("IN="+IN)
; 変数INの内容を画面に表示する

a=addfile(IN,"r")
; 読み込み(r)モードで変数INに記憶されたファイルを開く
; aはファイルを指すポインター

SST=a->SSTK_GDS0_SFC
; ポインターaで指定された入力ファイルに含まれるSSTK_GDS0_SFCという
; 名称の変数に記憶されたデータを読み込む。
; 読み込み先の変数の名称はSSTとする


print("MMMMM PLOTTING")
; 作図開始

FIG="ERA5_SST_EX02-02"
TYP="pdf"
; 図のファイル名とファイルの種類(pdf)を指定

wks = gsn_open_wks(TYP, FIG)
;作図するファイルを開く

resmp=True
; 地図の描画の設定に関する情報を記載する変数(resmp)を用意する

resmp@mpMinLatF = 0.
; 地図の南の端
resmp@mpMaxLatF = 50.
; 地図の北の端
resmp@mpMinLonF = 80.
; 地図の西の端
resmp@mpMaxLonF = 150.
; 地図の東の端

res=resmp
; resmpの情報をresに引き継ぐ
res@cnFillOn = True
; Trueの場合, 色で塗分けする
res@cnLinesOn = False
; Falseの場合, 等値線を書かない

res@gsnAddCyclic = False
;地球一周分のデータでない場合Falseにする

plot1=gsn_csm_contour_map(wks,SST(0,:,:),res)
; カラーシェード図と地図の作図

print("MMMMM IN : "+IN)
print("MMMMM FIG: "+FIG+"."+TYP)
```

#### 図のファイル

ERA5_SST_EX02-02.pdf



## 練習3: 緯度・経度情報の付加

新しい変数を用意した場合，緯度・経度情報を付加しないといけない場合がある。この箇所はNCLを使う上で躓きやすいところなので，少し詳しく説明する。

例として，SSTCという名称の変数を用意して，単位を摂氏とした海面水温の値を代入することを考える。

```bash
SSTC=SST(0,:,:)-273.15
; 絶対温度から摂氏への換算
```

上記を追加した，次のスクリプトを実行するとエラーとなるが，まずこのことを確認する。

### スクリプト

#### EX02-03A.NCL

```bash
print("MMMMM READ INPUT DATA.")
INDIR="/work01/DATA/ERA5/EASIA/6HR/SFC/SST/07BP02/"
;入力ファイルの存在するディレクトリ名

INFLE="ERA5_EASIA_SST_6HR_202107_02.grib"
;入力ファイル名

IN=INDIR+INFLE
;INDIRとINFLEをつないで一つの変数INを作る

print("IN="+IN)
; 変数INの内容を画面に表示する

a=addfile(IN,"r")
; 読み込み(r)モードで変数INに記憶されたファイルを開く
; aはファイルを指すポインター

SST=a->SSTK_GDS0_SFC
; ポインターaで指定された入力ファイルに含まれるSSTK_GDS0_SFCという
; 名称の変数に記憶されたデータを読み込む。
; 読み込み先の変数の名称はSSTとする

print("MMMMM PLOTTING")
; 作図開始

FIG="ERA5_SST_EX02-03A"
TYP="pdf"
; 図のファイル名とファイルの種類(pdf)を指定

wks = gsn_open_wks(TYP, FIG)
;作図するファイルを開く

resmp=True
; 地図の描画の設定に関する情報を記載する変数(resmp)を用意する

resmp@mpMinLatF = 0.
; 地図の南の端
resmp@mpMaxLatF = 50.
; 地図の北の端
resmp@mpMinLonF = 80.
; 地図の西の端
resmp@mpMaxLonF = 150.
; 地図の東の端

res=resmp
; resmpの情報をresに引き継ぐ
res@cnFillOn = True
; Trueの場合, 色で塗分けする
res@cnLinesOn = False
; Falseの場合, 等値線を書かない

res@gsnAddCyclic = False
;地球一周分のデータでない場合Falseにする

SSTC=SST(0,:,:)-273.15
; 絶対温度から摂氏への換算

plot1=gsn_csm_contour_map(wks,SSTC,res)
; カラーシェード図と地図の作図

print("MMMMM IN : "+IN)
print("MMMMM FIG: "+FIG+"."+TYP)
```

#### 実行例

```bash
$ ncl EX02-03A.NCL
```



#### エラーメッセージ

```bash
check_for_y_lat_coord: Warning: Data either does not contain a valid latitude coordinate array or doesn't contain one at all.

A valid latitude coordinate array should have a 'units' attribute equal to one of the following values: 'degrees_north' 'degrees-north' 'degree_north' 'degrees north' 'degrees_N' 'Degrees_north' 'degree_N' 'degreeN' 'degreesN' 'deg north'

check_for_lon_coord: Warning: Data either does not contain a valid longitude coordinate array or doesn't contain one at all.
A valid longitude coordinate array should have a 'units' attribute equal to one of the following values: 'degrees_east' 'degrees-east' 'degree_east' 'degrees east' 'degrees_E' 'Degrees_east' 'degree_E' 'degreeE' 'degreesE' 'deg east'
```

詳しい意味は分からないかもしれないが，1行目の

```bash
check_for_y_lat_coord: Warning: Data either does not contain a valid latitude coordinate array
```

や中段付近の

```bash
check_for_lon_coord: Warning: Data either does not contain a valid longitude coordinate array
```

を見ると，**緯度や経度の値が欠落してしまっている**可能性があることが推察される。

エラーが出た時は，エラーメッセージを検索窓にペーストして，**似たような事例がないか検索してみる**とよい。

googleでヒットしたサイトをいくつか見ると次のような解説を見つけることができた

https://www.ncl.ucar.edu/Support/talk_archives/2009/2746.html

要点を日本語で書くと，「図を書きたい変数に緯度・経度の情報が入っていない可能性がある」ということである。

また，緯度・経度の情報が入っているかどうかは，

```bash
printVarSummary
```

で確認できる，とある。そこで元のスクリプトに下記を追加して変数SSTとSSTCの情報を書き出してみる。

```bash
printVarSummary(SST)
; SSTという変数に関する情報を画面に表示させる

printVarSummary(SSTC)
; SSTCという変数に関する情報を画面に表示させる
```

#### EX02-03B.NCL

```bash
print("MMMMM READ INPUT DATA.")
INDIR="/work01/DATA/ERA5/EASIA/6HR/SFC/SST/07BP02/"
;入力ファイルの存在するディレクトリ名

INFLE="ERA5_EASIA_SST_6HR_202107_02.grib"
;入力ファイル名

IN=INDIR+INFLE
;INDIRとINFLEをつないで一つの変数INを作る

print("IN="+IN)
; 変数INの内容を画面に表示する

a=addfile(IN,"r")
; 読み込み(r)モードで変数INに記憶されたファイルを開く
; aはファイルを指すポインター

SST=a->SSTK_GDS0_SFC
; ポインターaで指定された入力ファイルに含まれるSSTK_GDS0_SFCという
; 名称の変数に記憶されたデータを読み込む。
; 読み込み先の変数の名称はSSTとする

print("MMMMM PLOTTING")
; 作図開始

FIG="ERA5_SST_EX02-03A"
TYP="pdf"
; 図のファイル名とファイルの種類(pdf)を指定

wks = gsn_open_wks(TYP, FIG)
;作図するファイルを開く

resmp=True
; 地図の描画の設定に関する情報を記載する変数(resmp)を用意する

resmp@mpMinLatF = 0.
; 地図の南の端
resmp@mpMaxLatF = 50.
; 地図の北の端
resmp@mpMinLonF = 80.
; 地図の西の端
resmp@mpMaxLonF = 150.
; 地図の東の端

res=resmp
; resmpの情報をresに引き継ぐ
res@cnFillOn = True
; Trueの場合, 色で塗分けする
res@cnLinesOn = False
; Falseの場合, 等値線を書かない

res@gsnAddCyclic = False
;地球一周分のデータでない場合Falseにする

SSTC=SST(0,:,:)-273.15
; 絶対温度から摂氏への換算

printVarSummary(SST)
; SSTという変数に関する情報を画面に表示させる

printVarSummary(SSTC)
; SSTCという変数に関する情報を画面に表示させる

plot1=gsn_csm_contour_map(wks,SSTC,res)
; カラーシェード図と地図の作図

print("MMMMM IN : "+IN)
print("MMMMM FIG: "+FIG+"."+TYP)
```

#### 実行例

```bash
$ ncl EX02-03B.NCL
```

#### 画面表示

##### 変数SST

```
Variable: SST
Type: float
Number of Dimensions: 3
Dimensions and sizes:   [initial_time0_hours | 40] x [g0_lat_1 | 201] x [g0_lon_2 | 281]
Coordinates:
            initial_time0_hours: [1941840..1942074]
            g0_lat_1: [50.. 0]
            g0_lon_2: [80..150]
```

##### 変数SSTC

```
Variable: SSTC
Type: float
Total Size: 225924 bytes
            56481 values
Number of Dimensions: 2
Dimensions and sizes:   [201] x [281]
Coordinates:
```

SSTとSSTCを比較すると，SSTには，

```
Dimensions and sizes:   [initial_time0_hours | 40] x [g0_lat_1 | 201] x [g0_lon_2 | 281]
Coordinates:
            initial_time0_hours: [1941840..1942074]
            g0_lat_1: [50.. 0]
            g0_lon_2: [80..150]
```

とあり，座標に関する情報が付加されているのに対し，SSTCは，

```
Dimensions and sizes:   [201] x [281]
Coordinates:
```

とあり，座標に関する情報が無いことが分かる。

上述の検索でヒットしたサイトによると，解決策は下記の3つがある，とのことである。

**方法1：すでにある変数をコピーした後，値を上書きする**

下記の例では，すでにある変数uをmagにコピーした後，magの値を上書きしている。

```bash
mag = u
mag = sqrt(u^2+v^2)
```



**方法2: 緯度・経度の情報を自分で付加する**

```bash
mag = sqrt(u^2+v^2)
mag!0 = "lat"
mag!1 = "lon"
mag&lat= lat
mag&lon= lon
```



**方法3: copy_VarCoordsを使う**

copy_VarCoordsという関数を使って，座標に関する情報をコピーする

```
mag = sqrt(u^2+v^2)
copy_VarCoords(u,mag)
```



詳細は後の機会にするが，方法1と方法３ははコピー元とコピー先（今の例ではSSTC）とで，配列の次元が同じでないと使えない（今の場合，SSTは3次元で，SSTCは2次元）。

今用いているスクリプトの場合，SSTCと同じ2次元の配列で座標に関する情報を有するものがなく，方法1と3は使えない。

そこで方法２を使うことにして，以下をスクリプトに追加する。

```bash
g0_lon_2=a->g0_lon_2
; 入力ファイルから経度を読み込む
g0_lat_1=a->g0_lat_1
; 入力ファイルから緯度を読み込む
```

```bash
SSTC!0 = "lat" ;SSTCの0番目の配列要素の名称をlatにする

SSTC!1 = "lon" ;SSTCの0番目の配列要素の名称をlonにする

SSTC&lat= g0_lat_1 
; SSTCのlatという座標の値として変数g0_lat_1に収納されているものを用いる

SSTC&lon= g0_lon_2 
; SSTCのlonという座標の値として変数g0_lon_2に収納されているものを用いる
```

```bash
printVarSummary(SSTC)
; SSTCという変数に関する情報を画面に表示させる
```

`!`の後の数字は，何番目の配列要素であるかを指定している

`SSTC!0 = "lat"` で, 「SSTCの0番目の配列要素の名称をlatにする」, という意味になる。

`SSTC&lat= g0_lat_1`で, 「SSTCのlatという座標の値として変数g0_lat_1に収納されているものを用いる」という意味になる。

その他も同様である。

#### EX02-03C.NCL

```bash
print("MMMMM READ INPUT DATA.")
INDIR="/work01/DATA/ERA5/EASIA/6HR/SFC/SST/07BP02/"
;入力ファイルの存在するディレクトリ名

INFLE="ERA5_EASIA_SST_6HR_202107_02.grib"
;入力ファイル名

IN=INDIR+INFLE
;INDIRとINFLEをつないで一つの変数INを作る

print("IN="+IN)
; 変数INの内容を画面に表示する

a=addfile(IN,"r")
; 読み込み(r)モードで変数INに記憶されたファイルを開く
; aはファイルを指すポインター

SST=a->SSTK_GDS0_SFC
; ポインターaで指定された入力ファイルに含まれるSSTK_GDS0_SFCという
; 名称の変数に記憶されたデータを読み込む。
; 読み込み先の変数の名称はSSTとする

g0_lon_2=a->g0_lon_2
; 入力ファイルから経度を読み込む
g0_lat_1=a->g0_lat_1
; 入力ファイルから緯度を読み込む

print("MMMMM PLOTTING")
; 作図開始

FIG="ERA5_SST_EX02-03C"
TYP="pdf"
; 図のファイル名とファイルの種類(pdf)を指定

wks = gsn_open_wks(TYP, FIG)
;作図するファイルを開く

resmp=True
; 地図の描画の設定に関する情報を記載する変数(resmp)を用意する

resmp@mpMinLatF = 0.
; 地図の南の端
resmp@mpMaxLatF = 50.
; 地図の北の端
resmp@mpMinLonF = 80.
; 地図の西の端
resmp@mpMaxLonF = 150.
; 地図の東の端

res=resmp
; resmpの情報をresに引き継ぐ
res@cnFillOn = True
; Trueの場合, 色で塗分けする
res@cnLinesOn = False
; Falseの場合, 等値線を書かない

res@gsnAddCyclic = False
;地球一周分のデータでない場合Falseにする

SSTC=SST(0,:,:)-273.15
; 絶対温度から摂氏への換算

SSTC!0 = "lat" ;SSTCの0番目の配列要素の名称をlatにする
SSTC!1 = "lon" ;SSTCの0番目の配列要素の名称をlonにする
SSTC&lat= g0_lat_1 
; SSTCのlatという座標の値として変数g0_lat_1に収納されているものを用いる
SSTC&lon= g0_lon_2 
; SSTCのlonという座標の値として変数g0_lon_2に収納されているものを用いる

printVarSummary(SSTC)
; SSTCという変数に関する情報を画面に表示させる

plot1=gsn_csm_contour_map(wks,SSTC,res)
; カラーシェード図と地図の作図

print("MMMMM IN : "+IN)
print("MMMMM FIG: "+FIG+"."+TYP)

```

#### 実行例

```bash
$ EX02-03C.NCL
```

#### 画面表示の一部

```
Variable: SSTC
Type: float
Total Size: 225924 bytes
            56481 values
Number of Dimensions: 2
Dimensions and sizes:   [lat | 201] x [lon | 281]
Coordinates:
            lat: [50.. 0]
            lon: [80..150]
```

SSTCの座標に関する情報として，

```
Dimensions and sizes:   [lat | 201] x [lon | 281]
```

```
Coordinates:
            lat: [50.. 0]
            lon: [80..150]
```

が追加されている。

#### 図のファイル名

ERA5_SST_EX02-03C.pdf

図の下のカラーバーを見ると最大値が30程度となっており，単位が摂氏に変換されていることが分かる。



## 上達のためのポイント

**エラーが出た時の対応の仕方でプログラミングの上達の速度が大幅に変わる**。

ポイントは次の3つである。

1. エラーメッセージをよく読む
2. エラーメッセージを検索し，ヒットしたサイトをよく読む
3. 変数に関する情報を書き出して確認する

エラーメッセージは，プログラムが不正終了した直接の原因とその考えられる理由が書いてあるので，よく読むことが必要不可欠である。

記述が簡潔なため，内容が十分に理解できないことも多いが，その場合**エラーメッセージをブラウザで検索**してヒットした記事をいくつか読んでみる。エラーの原因だけでなく，**考えうる解決策**が記載されていることも良くある。

エラーを引き起こしていると思われる箇所の**変数の情報**や**変数の値そのものを書き出して**，**期待した通りにプログラムが動作しているか確認する**ことも重要である。

エラーの場所が特定できれば，エラーの修正の大部分は完了したと考えてもよいほどである。

エラーメッセージや検索してヒットするウェブサイトは英語で記載されていることも多いが，**重要な情報は英語で記載されていることが多い**ので，よく読むようにする。

重要そうに思われるが，一回で理解できないものは，PDFなどに書き出して後で繰り返し読んでみる。どうしても頭に入らないものは印刷してから読む。

