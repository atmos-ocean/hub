# GrADS任意方向の断面図のサンプル



```bash
     'set x 1'
     'set y 1'
#     'set xlog on'
     'set lev 1000 100'
     lon1 = -95.0 ;#断面の両端の点の緯度経度を決める
     lon2 = -90.0
     lat1 = 55.0
     lat2 = 15.0
     
     lon = lon1 ;#西端の経度
     'collect 1 free' ;#まずデータを空にしておく
     while (lon <= lon2)　;#経度でループ
       lat = lat1 + (lat2-lat1)*(lon-lon1) / (lon2-lon1) ;#緯度を決める
       'collect 1 gr2stn(rhprs,'lon','lat')' ;#データのサンプリング
       lon = lon + 1
     endwhile

     'set lon 'lon1' 'lon2   
     'set clab on'
     'set gxout shaded'
     'color 50 100 2 -kind black->lightgreen->darkgreen->darkblue'

     'd coll2gr(1,-u)' ;#断面図
```

