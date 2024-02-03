GRIBファイルをGrADSで開く
============================-
[TOC]

## 予備知識

下記が必要な予備知識である。

### Linuxのシェルの基本操作

https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/01.BASH/0.LINUX_TUTORIAL_01.md

### シェルスクリプトの文法の基礎

https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/03.BASH_SCRIPT/LINUX_SCRIPT_TUTORIAL.md



## GribファイルをGrADSで読む手順

### 1. grib2ctl.plでCTLファイルを作成

grib2ctl.plというPerlスクリプトを用いると, gribファイルからCTLファイルのひな型を作成することができる。

#### GRIB2CTL.sh
```bash
CTL1=ERA5_SST_ICE_2010.CTL
CTL2=ERA5_SST_ICE_2015.CTL

grib2ctl.pl \
/work05/manda/DATA/ERA5/BARENTS2011_WHOLE_ARCTIC/GRIB/SST.ICE\
/2010/ERA5_20101120_SST_ICE.grib \
> $CTL1

grib2ctl.pl \
/work05/manda/DATA/ERA5/BARENTS2011_WHOLE_ARCTIC/GRIB/SST.ICE\
/2015/ERA5_20151120_SST_ICE.grib \
> $CTL2
```


#### ERA5_SST_ICE_2010.CTL
```
dset /work05/manda/DATA/ERA5/BARENTS2011_WHOLE_ARCTIC/GRIB/SST.ICE/ERA5_%y4%m2%d2_SST_ICE.grib
index ^ERA5_2010SST_ICE.grib.idx
undef 9.999E+20
title /work05/manda/DATA/ERA5/BARENTS2011_WHOLE_ARCTIC/GRIB/SST.ICE/2010/ERA5_20101120_SST_ICE.grib
*  produced by grib2ctl v0.9.12.6
dtype grib 255
options yrev template
ydef 161 linear 50.000000 0.25
xdef 1440 linear 0.000000 0.250000
tdef 380 linear 00Z20nov2010 6hr
zdef 1 linear 1 1
vars 3
CIsfc  0 31,1,0  ** surface Sea-ice cover [(0 - 1)]
SKTsfc  0 235,1,0  ** surface Skin temperature [K]
SSTKsfc  0 34,1,0  ** surface Sea surface temperature [K]
ENDVARS
```



### 2. CTLファイルの書き換え

複数ファイルを取り扱う場合, 下記のように時刻を可変にする設定を良く行う

- dsetの時刻に関するところを変更 (例: %y4%m2%d2_%h2%n2)

- options templateを追加

  

### 3. gribmapでindexファイルを作成する

gribmapというツールを使って, CTLファイルとgribファイルの連携を取るためのindexファイルと呼ばれるファイルを作成する。

#### GRIBMAP.sh

```bash
CTL=ERA5_SST_ICE_2015.CTL
gribmap -v -i $CTL

```

 

## "grib file format error" となるとき
 `-e` を用いるとエラーが回避できることがある。

```
gribmap -e -i 2001.ctl
```



## 注意: 2つの変数に同じParameter IDがあてがわれている場合

### 例

| Parameter ID | Table ID | 変数名                        |
| ------------ | -------- | ----------------------------- |
| 34           | 235      | Mean surface latent heat flux |
| 34           | 128      | sea_surface_temperature       |

このような場合上手くいかない。

参考：ERA5のParameter IDの一覧 (Parameter database)  
https://codes.ecmwf.int/grib/param-db/  



### 対処法

#### 1. CDOでnetCDFに変換する

```bash
cdo -f nc4 IN.grib OUT.nc
```

#### 2. CDOで変数ごとに分割する

```bash
PREFIX=$(basename $IN .grib)_
cdo splitparam  $IN $PREFIX 
```

PREFIXは分割されたファイルの名前の先頭につける文字列
