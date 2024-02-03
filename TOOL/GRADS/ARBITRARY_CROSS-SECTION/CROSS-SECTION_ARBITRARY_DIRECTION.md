# GrADS  任意方向の鉛直断面を描く

南北方向，東西方向以外の方向の断面図を描く

[[_TOC_]]

## 予備知識

下記が必要な予備知識である。

### Linuxのシェルの基本操作

https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/01.BASH/0.LINUX_TUTORIAL_01.md

### シェルスクリプトの文法の基礎

https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/03.BASH_SCRIPT/LINUX_SCRIPT_TUTORIAL.md



## サンプルスクリプト

### パラメータ

* VAR: 描画したい変数の名前
* lon1: 西端の点の経度 [度]
* lat1:  西端の点の緯度 [度]
* lon2:  東端の点の経度 [度]
* lat2: 東端の点の緯度 [度]
* dlon: サンプリング間隔 [経度] 

```bash
say "MMMMM SET PARAMETERS"
lon1 = -10; lon2 = 10; lat1 = 30; lat2 = 45
dlon = 1

'set x 1'; 'set y 1'
'set lev 1000 100'; # 'set zlog on'

say "MMMMM SAMPLING"
lon = lon1
'collect 1 free'
while (lon <= lon2)
  lat = lat1 + (lat2-lat1)*(lon-lon1) / (lon2-lon1)
  'collect 1 gr2stn(VAR,'lon','lat')'
  lon = lon + dlon
endwhile

say "MMMMM PLOTTING"
'set grads off'
'set x 14 16'
'set xaxis 'lon1' 'lon2
'set clab on'
'set gxout shaded'
'd coll2gr(1,-u)'
'set gxout contour' 
'd coll2gr(1,-u)'

say "MMMMM DONE."
```



## 解説

### 分割点の指定とサンプリング

線分を経度方向に等間隔にdlonで分割し，分割点ごとに`collect`を使って, VARの値をサンプリングしてゆく。

```bash
lon = lon1
'collect 1 free'
while (lon <= lon2)
  lat = lat1 + (lat2-lat1)*(lon-lon1) / (lon2-lon1)
  'collect 1 gr2stn(VAR,'lon','lat')'
  lon = lon + dlon
endwhile
```

線形補間を使って，分割点の緯度を決めてゆく

```bash
lat = lat1 + (lat2-lat1)*(lon-lon1) / (lon2-lon1)
```

経度は，等間隔 (dlon)ごとに増やしてゆく

```bash
lon = lon + dlon
```



#### 注意: dlonの値の設定

dlonの値が大きすぎると，データが間引かれて元のデータに含まれる細かい構造が見えなくなるので，注意する。   
dlonの値は，元のデータの格子間隔程度かそれより小さい値を何種類かテストして，結果に極端な相違が無いことを確認しておく。  



### collect

#### 使用例

```bash
'collect 1 gr2stn(VAR,'lon','lat')'
```

座標(lon, lat)において，VARの値をサンプリングし，サンプリング結果である,     

```bash
(lon, lat, VAR)の値の組    
```

を識別番号 (collection number)1として，記憶しておく。





### coll2gr

サンプリングされたデータをを鉛直断面としてまとめる (`coll2gr`)

```bash
coll2gr(cnum <,num>)
```

cnum: 識別番号    

num: 出力データの鉛直方向の層数 (default is 10)   

numを`-u`とした場合，サンプリングされた元のデータに含まれるすべての層の数を出力データの層数とする。  　(鉛直方向の層数が，各分割点ごとに異なる場合を想定している)。

#### 使用例

```bash
'd coll2gr(1,-u)'
```

