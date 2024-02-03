---
title: NCL Quick Reference (早見表)
layout: post
language: yaml
---

[[_TOC_]]

## リンク

NCL Tips! http://www.atmos.rcast.u-tokyo.ac.jp/shion/NCLtips/index.php  
NCLTips https://sites.google.com/site/tips4ncl/home  
A First Course in NCL　https://sites.google.com/site/nclfirstcourse/home  



## XYプロット

#### 座標の範囲 

```ncl
res@trXMinF = -1.0  ; X軸の最小値
res@trXMaxF =  1.0  ; X軸の最大値
res@trYMinF = -1.0  ; Y軸の最小値
res@trYMaxF =  1.0  ; Y軸の最大値
```



## カラーシェード color shade

### カラーシェードを書く

```
cnFillOn = True
```



### カラーマップの指定

```
gsn_define_colormap(wks,"")
```



### カラーマップの見本　Color Table Gallery

http://www.ncl.ucar.edu/Document/Graphics/color_table_gallery.shtml  



### カラーマップを逆転させる

```
gsn_reverse_colormap(wks)
```





### 自分で設定した配色を使う

```
res@cnFillPalette = (/"white","azure1","lightskyblue","dodgerblue",\
"blue","yellow","orange","orangered","darkorchid4"/)
res@cnLevelSelectionMode = "ExplicitLevels"
res@cnLevels = (/10,20,50,100,150,200,300/)
```

Color sample  
;https://www.ncl.ucar.edu/Document/Graphics/named_colors.shtml  



### 色分けの間隔の自動設定

```
@cnLevelSelectionMode = "AutomaticLevels"
```



### 色分けの間隔の指定

```
@cnLevelSelectionMode = "ExplicitLevels"
@cnLevels = (/ -12, -10.,-8.,-6.,-4.,-2.,-1.,1.,2.,4.,6.,8.,10.,12./)
```



### 色の指定

```
@cnFillColors = (/ 3,13,23,30,36,41,45,-1,59,63,68,74,81,91,96/)
```



### 色分けの間隔の指定(最小・最大・間隔を指定)

```
@cnLevelSelectionMode = "ManualLevels"
@cnMinLevelValF = -5.
@cnMaxLevelValF = 30.
@cnLevelSpacingF = 5.
```



ラベルバー Label bar
------------------------------------------

### ラベルバーの位置を下げる

```
@pmLabelBarOrthogonalPosF = .10
```



### ラベルバーの位置を左に移動する

```
@pmLabelBarParallelPosF = -0.05
```



### ラベルバーの向きを縦にする

```
@lbOrientation = "vertical"
```



### ラベルバーの幅を変更する

```
@pmLabelBarWidthF=0.8
```



### ラベルバーの高さを変更する

```
@pmLabelBarHeightF = 0.05
```



### ラベルバーを下げて、横軸のタイトルを上げる

ラベルバーの下に横軸のタイトルが書かれてしまう場合の回避法  

```bash
@pmLabelBarHeightF = 0.04
@pmLabelBarOrthogonalPosF = 0.1
@tiXAxisOffsetYF = 0.1
```



### ラベルバーの数字を何個ごとに書くか

2なら一個飛ばし   

```bash
@lbLabelStride         = 2
```



### ラベルバーのタイトル

```
@lbTitleOn        = True
@lbTitleString = "" ; title string
@lbTitlePosition = "Right" ; title position
@lbTitleFontHeightF= .012 ;Font size
@lbTitleDirection = "Across" ; title direction
```



### ラベルバーの数字のフォントサイズを変える

```
@lbLabelFontHeightF = 0.009
```



### カラーテーブルの指定

```
gsn_define_colormap(wks,"WhiteBlueGreenYellowRed")
```



### デフォルトのパネルプロット関数を使うと、一種類のカラーテーブルしか使えない (6.4まで)

対応策: 下記を使って、手動でパネルを一枚一枚書いていく    

```
@mpShapeMode = "FreeAspect"
@vpWidthF
@vpHeightF
@vpXF
@vpYF
```





等値線 contour
------------------------------------------

### コンターの色を変える

```
@cnLineColor = "blue"
```



### コンターを太さを変える

```
@cnLineThicknessF = 2.0
```



### コンターを破線にする

```
@cnLineDashPattern = 1
```

https://www.ncl.ucar.edu/Document/Graphics/Images/dashpatterns.png



### 負の値のコンターを破線にする

```
@gsnContourNegLineDashPattern = 1
```



### コンターラベルの色を変える

```
@cnLineLabelFontColor = "blue"
```



### コンターラベルのフォントサイズを変える

```
@cnLineLabelFontHeightF = 0.012
```



### コンターラベルを書かない

```
@cnLineLabelsOn = False
```



### コンターラベルを書く間隔

```
@cnLineLabelInterval = 1
```

(1=全てのコンター線に数値を入れる)  
(2=2本に1本のコンター線に数値を入れる)     



### コンターラベルを密(粗)に書く

```
@cnLineLabelDensityF = 1.0
```

1.0より大きい値にするとデフォルトより密に書く      



### 図の下に表示されるcontour from というメッセージを消す

```
@cnInfoLabelOn = False 
```





フォント (Font)
------------------------------------------

### タイトルの文字の大きさの変更

```
opts@gsnStringFontHeightF = 0.015
```



### ラベルバー（カラーバー）の数字の大きさの変更

```
opts@lbLabelFontHeightF = 0.02
```



### ラベルバーのタイトルの文字の大きさの変更



### コンターの数字の大きさの変更

```
res3@cnLineLabelFontHeightF = 0.012
```

注：あまり値を大きくすると数字が全く書かれなくなる    



```
txres@txFontHeightF = 0.012

txres@txAngleF = 0.0

txres@txJust="CenterLeft"
```



```
opts_cs@lbTitleFontHeightF= .012
```





ベクトル　vector
------------------------------------------

```
@vcFillArrowsOn = True
@vcRefMagnitudeF = 10.
@vcRefLengthF = 0.02
@vcMinFracLengthF = 0.2
@vcFillArrowEdgeColor = "white"
@vcFillArrowFillColor = "green"
@vcRefAnnoOrthogonalPosF = -1.0 ; Moves the reference vector up.
@vcRefAnnoParallelPosF = 1.0 ;
```

### FillArrowの設定

```
@vcFillArrowWidthF = 0.1 ; 矢印の太さ
```

```
@vcFillArrowHeadYF = 0.25 ; 頭の幅。ベクトルの長さに対する比で，0～1の範囲で与える。
```



### ベクトルの凡例の設定

```BASH
@vcRefAnnoFontHeightF = 0.02             ;フォントの大きさ
@vcRefAnnoString1On   = True             ;スケールベクトルの上部の文字列を有効にする
@vcRefAnnoString1     ="$RFM$"+UAVE@units;デフォルトは"$VMG$"
@vcRefAnnoString2On   = True             ;スケールベクトルの下部の文字列を有効にする
@vcRefAnnoString2     = "wind"           ;デフォルトは"Reference Vector"
```

#### ベクトルの凡例の数値の大きさの変更

```
vecres@vcRefAnnoFontHeightF = 0.02
```

#### ベクトルの凡例の文字列"Reference vector"を表示させない

```
vecres@vcRefAnnoString2On = False
```

#### ベクトルの凡例の位置変更

```
res@vcRefAnnoSide             = "Left"
res@vcRefAnnoJust             = "TopLeft" 
res@vcRefAnnoOrthogonalPosF   = -0.142
```

より詳しくは下記参照

http://www.atmos.rcast.u-tokyo.ac.jp/shion/NCLtip



NCL 地図
------------------------------------------

```
@mpOutlineOn          = True         ; Use outlines from shapefile

@mpGeophysicalLineColor = "gray24" ;https://www.ncl.ucar.edu/Document/Graphics/named_colors.shtml

@mpGeophysicalLineThicknessF = 0.5
```



### 格子線を入れる

```
opts_cs@mpGridAndLimbOn   = True
```



### 経線の間隔

```
@mpGridLonSpacingF = 20
```



### 緯線の間隔

```
@mpGridLatSpacingF = 5
```



### 地図の外枠線を最後に書く

```
@mpPerimDrawOrder = "PostDraw"
```



### 地図の外枠線の太さ

```
opts_cs@mpPerimLineThicknessF=2
```



### 地図の解像度

```bash
@mpDataBaseVersion="LowRes"
@mpDataBaseVersion="MediumRes"
@mpDataBaseVersion="HighRes"
```



### 地図を塗りつぶす

```bash
res@mpFillOn               = True    ;地図を塗りつぶす
res@mpGeophysicalLineColor = "Green" ;地図の線の色
res@mpGeophysicalLineThicknessF  = 2 ;地図の線の太さ
res@mpGeophysicalLineDashPattern = 0 ;地図の線の線種
```

### 地図とプロットの上下関係の設定 

地図と等値線やシェードなどをどういう順番で描くかを設定する

```
res@cnFillDrawOrder = "PreDraw"
```

にすると，シェードが下になって陸地が上に来る。つまり、海だけが塗りつぶされる。

```
res@cnFillDrawOrder = "PostDraw"
```

にすると，陸地の上にシェードを書くことになって陸地が隠れる。

同様に，等値線についてはres@cnLineDrawOrderで，ベクトルについてはres@vcVectorDrawOrderで変更できる。

### 地図の塗りつぶしの色

```
mpOceanFillColor = 8  ;selects the light blue color from the colormap.
mpLandFillColor = 164 ;selects the dark orange color from the colormap.
mpInlandWaterFillColor = 54 ;selects the dark blue color from the colormap.
```



### 海岸線の線の太さ

```bash
@mpGeophysicalLineThicknessF = 2                 ;-- increase coastline thickness
```



### 地図の範囲設定 

```bash
res@mpMinLonF = 120     ; 経度の最小値
res@mpMaxLonF = 300     ; 経度の最大値
res@mpMinLatF = -60     ; 緯度の最小値
res@mpMaxLatF =  60     ; 緯度の最大値
```

緯度経度の中心値で与えることもできる

```bash
res@mpCenterLonF = 180  ; 経度の中心
res@mpCenterLatF =  30  ; 緯度の中心
```




------------------------------------------

### 図にパネル番号 (a), (b), ...などを入れる

```
; drawNDCGrid(wks) ; PRINT GRID SHOWING NDC COORDINATE
; ADD PANEL NAME
txres@txFontHeightF = 0.025
text="(a)"
gsn_text_ndc(wks,text,0.04,0.9,txres)
text="(b)"
gsn_text_ndc(wks,text,0.50,0.9,txres)
text="(c)"
gsn_text_ndc(wks,text,0.04,0.47,txres)
text="(d)"
gsn_text_ndc(wks,text,0.50,0.47,txres)


txres@txFontHeightF = 0.010

gsn_text_ndc(wks,"STRINGS",0.1,0.2,txres)

txres@txFontColor = "Purple"

txres@txBackgroundFillColor = "Salmon"

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
drawNDCGrid(wks)
```



座標軸
------------------------------------------

### 座標軸のラベル（数値）の文字の大きさを変更する

```
@tmXBLabelFontHeightF = 0.012@tmYLLabelFontHeightF = 0.012
```



### 座標軸のラベル（数値）とタイトルを消す

```
@tmYLLabelsOn = False
@tiYAxisString = ""
```



### 地図の座標軸の文字を入れる間隔

```
@gsnMajorLatSpacing = 1
@gsnMajorLonSpacing = 1
```



### y座標軸のラベルの位置を上げる

```
@tiXAxisOffsetYF = 0.13
```



### 座標軸の範囲を指定する

```
@trYMinF =  0.
@trYMaxF = 16.
```



### y軸の値0に水平線を入れる (zeroline)

```
gsnYRefLine = 0.
```



## 文字 strings

```
txres@txFontHeightF = 0.012

txres@txAngleF = 0.0

txres@txJust="CenterLeft"

txres@txFontHeightF = 0.010

gsn_text_ndc(wks,"STRINGS",0.1,0.2,txres)

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
drawNDCGrid(wks)
```



上付き文字　（例： $\mathrm{s^{-1}}$）  

```
~S~s-1~N~
```

下付き文字　(例：$\mathrm{N/m^{-1}}$)  

```
Nm~B~-1~N~ 
```

緯度・傾度の度  

```
~S~o~N~
```

ギリシャ文字  

```
~F33~helas~N~
```

改行  

```
~C~
```



文字列操作
------------------------------------------

### 部分文字列

substring.nclを下記からダウンロードする  
https://www.ncl.ucar.edu/Support/talk_archives/2013/att-2231/substring.ncl  

2020-04-18_12-03   
$ wget -nv https://www.ncl.ucar.edu/Support/talk_archives/2013/att-2231/substring.ncl
2020-04-18 12:03:11 URL:https://www.ncl.ucar.edu/Support/talk_archives/2013/att-2231/substring.ncl [1400/1400] -> "substring.ncl" [1]    

#### substring.ncl

```ncl
;-----------------------------------------------------------------------------
; substring.ncl -- Extract a substring from another string.
;
; Usage:  outstr = substring (instr, first, last)
;
; Input:  instr = input string
;	  first = starting index of substring (zero based)
;	  last = ending index of substring (zero based); if last is
;	     less than first, the entire right substring is returned
;
; Output: outstr = selected substring
;
; Range checking is not done in this version.  Specifying "first" or
; "last" out of range will result in a non-elegant subscript error.
;----------------------------------------------------------------------------
function substring (instr[1]:string, first[1]:numeric, last[1]:numeric)

local instr, first, last, main, p2

begin
   main = stringtochar (instr)		; convert input string to char array
					; terminal null character is included   
   
   if (last .ge. first) then		; check requested end position
      p2 = last				; go to last position specified
   else					; but if less than first:
      p2 = dimsizes (main) - 2		; go to last avail char in main string
   end if
   
   return (chartostring (main(first:p2)))	; extract substring
end
```



### NCLスクリプトの先頭でロードする

```
load "./substring.ncl"
```

### 0文字目から3文字目までを抜き出す

```
substring(STRING, 0, 3)
```




### 部分文字列

例：06/30/2016 (18:00)から、2016を抜き出す   

```
strc="06/30/2016 (18:00)"
yyyy=str_get_cols(strc,6,10)
print(yyyy)
```

注：yyyyの最初の文字は0文字目と数える    

  

縦横比変更 free aspcet
------------------------------------------

```
@mpShapeMode = "FreeAspect"
@vpWidthF = 0.8
@vpHeightF = 0.4
@vpXF = .15
@vpYF = .55
```



マーカー marker
------------------------------------------

```
res=True
res@gsnDraw  = False ;don't draw
res@gsnFrame = False ;don't advance frame
```

```
LOND=136.0
LONM=47.459
LATD=34.0
LATM=17.098
XO=LOND+LONM/60.0
YO=LATD+LATM/60.0
```

```
mres=True
mres@gsMarkerIndex  = 16           ; marker style (circle)
mres@gsMarkerSizeF  = 10           ; marker size
mres@gsMarkerColor  = "black"      ; maker color
id = gsn_add_polymarker(wks, plot, XO, YO, mres)
```

```
draw(plot)                                           
frame(wks)  
```



### 文字列

```
txres@txFontHeightF = 0.012txres@txAngleF = 0.0txres@txJust="CenterLeft"txres@txFontHeightF = 0.010gsn_text_ndc(wks,"STRINGS",0.1,0.2,txres)load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"drawNDCGrid(wks)
```



substring　部分文字列
------------------------------------------

```
yyyy=str_get_cols(strc,6,10)print(yyyy)
```

06/30/2016 (18:00)    

Variable: yyyy  
Type: string  
Total Size: 8 bytes  
1 values  
Number of Dimensions: 1  
Dimensions and sizes: [1]  
Coordinates:  
2016  



配列
------------------------------------------

An index can be reversed using a negative stride:

```
T(::-1,::-1)
```

An index can be subsampled using a positive stride:

```
T(::2,::2)
```



図のタイトル
------------------------------------------

```
gsnLeftString=""
gsnCenterString=""
gsnRightString=""
gsnLeftStringFontHeightF=
```

https://www.ncl.ucar.edu/Applications/title.shtml



NCL 色々 Misc
------------------------------------------

### 地図上に線分を書き加える

```
plot=gsn_csm_contour_map(wks, slp, res)  
.....  
.....  
plx1=(/lon_start, lon_end/)  
ply1=(/lat_start, lat_end/)  
lnres = True  
lnres@gsLineColor = "blue"  
ln = gsn_add_polyline(wks, plot, plx1, ply1, lnres)  
```



### 画面上に座標を表す枠線を表示させる

Draws NDC grid lines at 0.1 NDC coordinate intervals and labels them.    

```
drawNDCGrid(wks)  
```



NCL 注意事項
------------------------------------------

wrf_user_intrp3dを実行すると、実行前の気圧の単位はPaでも実行後にはhPaになってしまう   

```
a1 = addfile(infile1,"r")
p = wrf_user_getvar(a1,"p",it) ;Pa not hPa

plev_lw=500.0*100.0 ;500 hPa

print("plev_lw (before)="+ plev_lw)
lwdown1=wrf_user_intrp3d(lwdown,p,"h",plev_lw,0.0,False)
print("plev_lw (after)="+ plev_lw)
```

実行例  
plev_lw (before)=50000  
plev_lw (after)=500    




NCL 縦横比変更 free aspcet
------------------------------------------

```
@mpShapeMode = "FreeAspect"
@vpWidthF = 0.8
@vpHeightF = 0.4

@vpXF = .15
@vpYF = .55
```



図を縦書き(portrait)から横書き(landscape)に変更する
------------------------------------------

```
type="ps"
type@wkOrientation = "landscape"
wks = gsn_open_wks(type, figfile)
```



図の重ね書き
------------------------------------------

```
plot = gsn_csm_contour_map(wks,sst_plt,opts)

opt2@gsnDraw          = False

opt2@gsnFrame         = False

plot2 = gsn_csm_contour(wks,ice,opt2)

overlay(plot,plot2)
```




座標データのコピー
------------------------------------------

例    

```bash
Variable: sstk
Type: float
Total Size: 3848004 bytes
            962001 values
Number of Dimensions: 3
Dimensions and sizes:   [time | 1] x [lat | 801] x [lon | 1201]
```

```bash
sst=sstk(i,:,:)
sst!0=sstk!1
sst!1=sstk!2
sst&lon=sstk&lon
sst&lat=sstk&lat
```

