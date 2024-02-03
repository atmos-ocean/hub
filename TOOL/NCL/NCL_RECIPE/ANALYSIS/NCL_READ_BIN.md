バイナリデータの読み込み
============================
[TOC]
マニュアル
---------------------------------------
http://www.atmos.rcast.u-tokyo.ac.jp/shion/NCLtips/index.php?cmd=read&page=%E3%83%87%E3%83%BC%E3%82%BF%E3%81%AE%E8%AA%AD%E3%81%BF%E6%9B%B8%E3%81%8D#content_1_4

Fri, 24 Jul 2020 10:46:03 +0900
calypso.bosai.go.jp
/work05/manda/20.AGO_WAN/FLUX.MSM/02.FLUX.HIROSE/CHK.MSM.TOPOGRAPHY

```
srcdump.sh CHK.MSM.TERRAIN.ncl
```

### HOW TO RUN

### INFO
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB

### SOURCE FILES
- CHK.MSM.TERRAIN.ncl
  
#### CHK.MSM.TERRAIN.ncl
```
; 2020-07-24_10-24 
; manda@calypso
; /work05/manda/20.AGO_WAN/FLUX.MSM/02.FLUX.HIROSE/CHK.MSM.TOPOGRAPHY
;
indir="/work05/manda/DATA/MSM.200719/TERRAIN"
infle="TOPO.MSM_5K"

setfileoption("bin", "ReadByteOrder", "BigEndian")
TOPO= fbindirread(indir+"/"+infle,0,(/505,481/),"float")

;メソ数値予報モデルGPV
;       格子数：  東西方向   481    南北方向   505
;       格子間隔：東西方向 0.0625度  南北方向 0.05度
;       先頭の格子点：北緯47.6度  東経120度 


lat=fspan(47.6 ,22.35   ,505)
lon=fspan(120.0,150.0625,481)
;print(lat)
;print(lon)

TOPO!0="lat"
TOPO!1="lon"

TOPO&lat=lat
TOPO&lon=lon

TOPO&lat@units = "degrees_north"
TOPO&lon@units = "degrees_east"

printVarSummary(TOPO)
printVarSummary(TOPO&lat)
printVarSummary(TOPO&lon)



res=True
res@cnFillOn = True
res@gsnAddCyclic = False
res@mpMinLonF    = 136.5
res@mpMaxLonF    = 137
res@mpMinLatF    = 34
res@mpMaxLatF    = 34.5

;res@mpMinLatF    = min(TOPO&lat)
;res@mpMaxLatF    = max(TOPO&lat)
;res@mpMinLonF    = min(TOPO&lon)
;res@mpMaxLonF    = max(TOPO&lon)

res@mpDataBaseVersion="HighRes"
res@mpGridLonSpacingF = 0.1
res@mpGridLatSpacingF = 0.1

res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF = 0.
res@cnMaxLevelValF = 400.
res@cnLevelSpacingF = 20.

wks = gsn_open_wks("eps", "TOPO.MSM_5K")

gsn_define_colormap(wks,"GMT_topo")

plot = gsn_csm_contour_map_ce(wks, TOPO, res)

```

  





README（支援センター）_201406.txt
-------------------------------
http://database.rish.kyoto-u.ac.jp/arch/jmadata/data/gpv/original/etc/README%ef%bc%88%e6%94%af%e6%8f%b4%e3%82%bb%e3%83%b3%e3%82%bf%e3%83%bc%ef%bc%89_201406.txt



/work05/manda/DATA/MSM.200719/TERRAIN



### 1 資料を構成するファイル


  この資料は15個のファイルにより構成されています。

####  地表ジオポテンシャル高度データ

    (a) "TOPO.LFM_2K"              局地数値予報モデルGPV用
    (b) "TOPO.MSM_5K"              メソ数値予報モデルGPV用
    (c) "TOPO.TL959RGG_0.5"        全球数値予報モデル（全球域）GPV用
    (d) "TOPO.TL959RGG_0.2_JP"     全球数値予報モデル（日本域）GPV用
    (e) "TOPO.TL479RGG_2.5"        週間アンサンブル数値予報モデルGPV用(全球)
    (f) "TOPO.TL479RGG_1.25_JP"    週間アンサンブル数値予報モデルGPV用(日本域)
    (g) "TOPO.TL319RGG_2.5"        １か月アンサンブル数値予報モデルGPV用(全球)



 ###  海陸分布データ

    (a) "LANDSEA.LFM_2K"           局地数値予報モデルGPV用
    (b) "LANDSEA.MSM_5K"           メソ数値予報モデルGPV用
    (c) "LANDSEA.TL959RGG_0.5"     全球数値予報モデル（全球域）GPV用
    (d) "LANDSEA.TL959RGG_0.2_JP"  全球数値予報モデル（日本域）GPV用
    (e) "LANDSEA.TL479RGG_2.5"     週間アンサンブル数値予報モデルGPV用(全球)
    (f) "LANDSEA.TL479RGG_1.25_JP" 週間アンサンブル数値予報モデルGPV用(日本域)
    (g) "LANDSEA.TL319RGG_2.5"     １か月アンサンブル数値予報モデルGPV用(全球)



####    このファイル

        "README（支援センター）_201406.txt"  このファイル

（注）数値予報モデルで使用する高度データを「地表ジオポテンシャル高度データ」と呼び，国土交通省及び国土地理院が公開している地図や標高データそのものと混同しないようにしています。ただし，以下の文中では煩雑さを避けるため「高度データ」と呼びます。



### 2 前回(2011年6月)提供した資料との相違


2014年3月に局地数値予報モデルGPVの提供開始に伴い、高度データおよび海陸分布データを新規に追加しています。(a)

メソ数値予報モデルおよび全球数値予報モデルはデータの作成環境による小さな値の変化はありますが、基本的に変更ありません。(b)(c)(d)

2014年2月に週間アンサンブル数値予報モデルの高解像度化に伴い、高度データおよび海陸分布データを更新しています。(e)(f)

2014年3月の１か月アンサンブル数値予報モデルの変更に伴い、高度データおよび海陸分布データを更新しています。(g)

同モデルの日本域についてはGPVデータの提供を行わないため、地形データの提供についても行わないことと致しました。



### 3 データの値


高度データは格子点の高度。単位は m。

海陸分布データは、水域を０，陸域を１とした海陸の割合。

なお、局地数値予報モデルGPVでは領域範囲外となる格子点があります。
　
高度データについては,数値予報モデルの領域範囲外となる格子点については高度をｰ999.0mに設定してあります。
　
海陸分布データについては,数値予報モデルの領域範囲外となる格子点については－１が設定してあります。

気象業務支援センターからユーザに提供されるGPVデータの座標系は、各数値予報モデルの数値計算で使用されている座標系と異なっているため、数値予報モデルが使用している値を内挿しています。



### 4 ファイル形式

####  (1) データ形式

4バイト実数(IEEE754)の2次元配列です。

バイトの並びはbig endianです。

####   (2) 座標系

等緯度経度格子座標系です。

####     (a) 局地数値予報モデルGPV

格子数：  東西方向   1201    南北方向   1261

格子間隔：東西方向 0.025度  南北方向 0.02度

先頭の格子点：北緯47.6度  東経120度 

####     (b) メソ数値予報モデルGPV

格子数：  東西方向   481    南北方向   505

格子間隔：東西方向 0.0625度  南北方向 0.05度

先頭の格子点：北緯47.6度  東経120度 

####     (c) 全球数値予報モデル（全球域）GPV

格子数：  東西方向   720    南北方向   361

格子間隔：0.5度（東西，南北両方向とも）

先頭の格子点：北緯90度  経度0度 

####     (d) 全球数値予報モデル（日本域）GPV

格子数：  東西方向  121    南北方向  151

格子間隔：東西方向 0.25度  南北方向 0.2度

先頭の格子点：北緯50度  東経120度

####     (e) 週間アンサンブル数値予報モデルGPV（全球）

格子数：  東西方向  144    南北方向   73

格子間隔：2.5度（東西，南北両方向とも）

先頭の格子点：北緯90度  経度0度

####     (f) 週間アンサンブル数値予報モデルGPV（日本域）

格子数：  東西方向   73    南北方向   40

格子間隔：1.25度（東西，南北両方向とも）

先頭の格子点：北緯71.25度  東経90度

####     (g) １か月アンサンブル数値予報モデルGPV（全球）

格子数：  東西方向  144    南北方向   73

格子間隔：2.5度（東西，南北両方向とも）

先頭の格子点：北緯90度  経度0度

####   (3) データ格納順序

先頭の格子点から緯度の同じ格子点を経度方向東向きに格納し，

そのすぐ南の緯度で同様に繰り返し格納しています。