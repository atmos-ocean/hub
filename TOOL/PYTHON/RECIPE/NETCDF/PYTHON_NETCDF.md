---
title: PythonでnetCDFを扱う
tags: Python NetCDF Weather
slide: false
---
ECMWF（ヨーロッパ中期予報センター）からnetCDF形式の気象データを取得したのでその読み込み方をまとめました。


# netCDFとは
配列指向データアクセスのためのインターフェース、あるいはそのようなファイル形式を指します。

気象データを例に挙げます。

ある時、ある場所での気温や風速をデータに記録するにはどのような形式にすればよいでしょうか。

netCDFでは緯度、経度、時刻を軸とした３次元空間に対象データ（気温、風速、波高など）の値を格納します。
![image.png](https://qiita-image-store.s3.amazonaws.com/0/167609/d8e65c24-6ceb-db90-cbd1-5681fe79d496.png)
出典: [netCDF データ格納形式の基礎 - ArcGIS for Desktop](http://desktop.arcgis.com/ja/arcmap/10.3/manage-data/netcdf/fundamentals-of-netcdf-data-storage.htm)



# 扱うデータ

今回はECMWF（ヨーロッパ中期予報センター）のERA-Interimからダウンロードした気象データを用います。
具体的には、指定した緯度経度区間における数時間おきの波・風などの観測値あるいは予測値が取得できます。

[ERA-Interim](https://apps.ecmwf.int/datasets/data/interim-full-daily/levtype=sfc/)からのダウンロード方法は割愛します。
ユーザ登録すれば無料でダウンロードできます。
ブラウザ操作で簡単ですがダウンロードに時間がかかる点は注意。

スクリプトでのダウンロードについてはQiitaに記事があったので参考にしてください。
- [ECMWFのデータをpythonスクリプトでダウンロードする準備 - Qiita](https://qiita.com/H1r0ak1Y0sh10ka/items/681f00ca722917205e5c)



# PythonでnetCDFファイルを読み込む

それでは早速ファイルを読み込みます。
まずは公式に従い[netCDF4](https://github.com/Unidata/netcdf4-python)をインストールします。
（pipじゃなかった記憶はあるけど詳しいことは忘れた）

[公式ドキュメント](http://unidata.github.io/netcdf4-python/)をザーッと読みます。
ファイルのロードには[Datasetクラス](http://unidata.github.io/netcdf4-python/#netCDF4.Dataset)を用いるみたいです。

```python
nc = Dataset(file_path, 'r')
nc
<class 'netCDF4._netCDF4.Dataset'>
root group (NETCDF3_64BIT data model, file format NETCDF3):
    Conventions: CF-1.6
    history: 2018-12-17 07:55:12 GMT by grib_to_netcdf-2.9.2: grib_to_netcdf /data/data04/scratch/bf/30/_mars-atls02-a82bacafb5c306db76464bc7e824bb75-uux64n.grib -o /data/data04/scratch/e2/9f/_grib2netcdf-atls01-a562cefde8a29a7288fa0b8b7f9413f7-EPK_mO.nc -utime
    dimensions(sizes): longitude(480), latitude(241), time(249)
    variables(dimensions): float32 longitude(longitude), float32 latitude(latitude), int32 time(time), int16 siconc(time,latitude,longitude), int16 sst(time,latitude,longitude), int16 u10(time,latitude,longitude), int16 v10(time,latitude,longitude), int16 t2m(time,latitude,longitude), int16 d2m(time,latitude,longitude), int16 lspf(time,latitude,longitude), int16 lsp(time,latitude,longitude), int16 cp(time,latitude,longitude), int16 sf(time,latitude,longitude), int16 tp(time,latitude,longitude), int16 csf(time,latitude,longitude), int16 mwd(time,latitude,longitude), int16 mwp(time,latitude,longitude)
    groups: 
```



netCDFではgroupの中にdimension, variable, attributeが格納されます。
Datasetクラスは読み込んだファイルから特殊なgroupであるroot groupを生成します。
従って先ほどファイルから生成したDatasetインスタンスの中に格納されたdimension, variable, attributeを以下で読み取っていきます。
すでにdimensions, variablesでどんな値が格納されているか見えていますね。



## Dimension

緯度、経度、時間、標高などで表現されます。

Datasetのクラス変数dimensionsを用いてOrderedDict形式で取得できます。
以下の例ではlongitude（経度）、latitude（緯度）、time（時間）の3軸でデータが記録されていることを意味します。
これに標高を加えれば4軸になります。

```python
nc.dimensions
OrderedDict([('longitude',
              <class 'netCDF4._netCDF4.Dimension'>: name = 'longitude', size = 480),
             ('latitude',
              <class 'netCDF4._netCDF4.Dimension'>: name = 'latitude', size = 241),
             ('time',
              <class 'netCDF4._netCDF4.Dimension'> (unlimited): name = 'time', size = 249)])
```




## Variable
実際に格納されている値です。
データのラベル名で取得可能です。
今回ERA-Interimからは以下の値などを取得しました。

- total precipiation (tp): 総降水量
- 10 metre eastward wind component (u10): 地上10m東向き風
- 2 metre temperature (t2m): 地上2m気温

加えて先述のdimensionsの緯度・経度・時間もvariableとしてアクセス可能です。
variableの種類はvariablesでOrderedDict形式で取得できるのでkeys()でキーを取得できます。

```python
nc.variables.keys()
odict_keys(['longitude', 'latitude', 'time', 'siconc', 'sst', 'u10', 'v10', 't2m', 'd2m', 'lspf', 'lsp', 'cp', 'sf', 'tp', 'csf', 'mwd', 'mwp'])
```

Datasetからkey指定でvariableを取得できます。

longitudeはdimension（観測空間）の軸の１つなので要素数480の単純な配列として扱えます(current shapeに注目)。

```python
nc['longitude']
<class 'netCDF4._netCDF4.Variable'>
float32 longitude(longitude)
    units: degrees_east
    long_name: longitude
unlimited dimensions: 
current shape = (480,)
filling on, default _FillValue of 9.969209968386869e+36 used
```



一方、dimensions以外の観測データ（降水量や風速など）は249×241×480の3次元であることがわかります。
単位はunitsで確認できます。どうやらケルビンらしいです。

```python
nc['t2m']
<class 'netCDF4._netCDF4.Variable'>
int16 t2m(time, latitude, longitude)
    scale_factor: 0.001664895641571446
    add_offset: 263.02950965422997
    _FillValue: -32767
    missing_value: -32767
    units: K
    long_name: 2 metre temperature
unlimited dimensions: time
current shape = (249, 241, 480)
filling on
```

再び冒頭のnetCDFのデータ形式を振り返りましょう。
確かに、項目毎に以下のような3次元配列に値が格納されているようです。
![image.png](https://qiita-image-store.s3.amazonaws.com/0/167609/d8e65c24-6ceb-db90-cbd1-5681fe79d496.png)
出典: [netCDF データ格納形式の基礎 - ArcGIS for Desktop](http://desktop.arcgis.com/ja/arcmap/10.3/manage-data/netcdf/fundamentals-of-netcdf-data-storage.htm)

では各点の値を取得してみます。
このように、[time][lat][lon]の順に指定することで任意の点の値が取得できました。

```python
nc['t2m'][0][0][0]
247.53432591812452
```



247K（ = -26.15℃）と極寒であるようです。
念のため、日時と位置を確認しておきます。

dimensionsのvariablesはインデックス指定するとmasked_arrayが返ってきました。

```python
nc['longitude'][0]
masked_array(data=0.,
             mask=False,
       fill_value=1e+20,
            dtype=float32)
```

数値だけ取得したければ以下のようにすれば良いです。

```python
float(nc['longitude'][0]) # 0.0
float(nc['latitude'][0]) # 90
float(nc['time'][0]) # 1035792.0 (hours since 1900-01-01 00:00:00.0) = 2018-03-01 00:00:00
```

時間が1900-01-01 00:00:00.0からの経過時間であることに注意してtimedeltaで変換しました。
というわけで、2018-03-01 00:00:00の経度0°、緯度90°（北極点）の気温は-26℃だったそうです。



## Attribute

メタデータのことです。
例えばデータの説明（description）や作成日時（history）などが記載されていれば取得できます。
ERA-Interimではhistoryが取得できました。

```python
nc.history
'2018-12-17 07:55:12 GMT by grib_to_netcdf-2.9.2: grib_to_netcdf /data/data04/scratch/bf/30/_mars-atls02-a82bacafb5c306db76464bc7e824bb75-uux64n.grib -o /data/data04/scratch/e2/9f/_grib2netcdf-atls01-a562cefde8a29a7288fa0b8b7f9413f7-EPK_mO.nc -utime'
```



# 参考
- [What Is netCDF?](https://www.unidata.ucar.edu/software/netcdf/docs/faq.html#What-Is-netCDF)
- [netCDF データ格納形式の基礎 - ArcGIS for Desktop](http://desktop.arcgis.com/ja/arcmap/10.3/manage-data/netcdf/fundamentals-of-netcdf-data-storage.htm)
- [netCDF4 API documentation](http://unidata.github.io/netcdf4-python/)
- [ECMWFのデータをpythonスクリプトでダウンロードする準備 - Qiita](https://qiita.com/H1r0ak1Y0sh10ka/items/681f00ca722917205e5c)
- [how to read date and time on ecmwf file - stackoverflow](https://stackoverflow.com/questions/37854256/how-to-read-date-and-time-on-ecmwf-file)

