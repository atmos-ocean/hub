
List of the following files:
----------------------------
SST_TREND_COBE.ncl
TEST_READ_COBE.ncl
TEST_PLOT_COBE.ncl
TEST_ANNUAL_COBE.ncl
TEST_TSRS_COBE.ncl

Machine info
----------------------------
calypso.bosai.go.jp
/work05/manda/TEACHING/TYPHOON_ENECO_1912/SST_TREND_COBE
Wed, 25 Dec 2019 17:28:13 +0900

## SST_TREND_COBE.ncl
```

; http://www.atmos.rcast.u-tokyo.ac.jp/shion/NCLtips/index.php?Examples/SST_trend

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

indir="/work05/manda/TEACHING/TYPHOON_ENECO_1912/GRIB"

infle="sst"

f=addfile("../GRIB/sst1905.grib","r")
;print(f)
lat=f->g0_lat_1
lon=f->g0_lon_2


files = systemfunc("ls "+indir+"/"+infle+"*.grib")

fall=addfiles(files,"r")

sst=fall[:]->WTMP_GDS0_DBSL
time=fall[:]->initial_time0_hours

YYYYMM  = cd_calendar(time,-1) 
;print(YYYYMM)

sst1yr=month_to_annual_weighted(YYYYMM,sst,1)
dim=dimsizes(sst1yr)
ny=dim(0)

yyyy=new(ny,integer)


utc_date = cd_calendar(time(:), 0)

year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
day    = tointeger(utc_date(:,2))
dim=dimsizes(year)
nt=dim(0)


;print(nt)
yyyy=ispan(year(0),year(nt-1),1)

;printVarSummary(yyyy)
yyyys=sprinti("%4.4i",yyyy(0))
yyyye=sprinti("%4.4i",yyyy(ny-1))

print("yyyys=" + yyyys + " yyyye="+yyyye)


trend   = regCoef_n(yyyy,sst1yr,0,0)     ; 回帰係数の計算

printVarSummary(trend)

  ;;; t検定
  rstd    = reshape(trend@rstd,dimsizes(trend))   ; trend@rstdは1次元配列なのでtrendと同じ形に変形
  dof     = new(dimsizes(trend),integer)          ; 自由度の配列
  dof     = dimsizes(yyyy)-2                      ; ここでは簡単のためどこでも年数とした。もちろん実際の解析では検討が必要。
  cdl     = cdft_p(trend/rstd,dof)                ; t値(trend/rstd)からt分布の片側確率を計算
  cdl     = mask(cdl,ismissing(trend),False)      ; 陸地をマスキング
     ;;; cdl には，t分布を-∞から各地点のt値まで積分して得られる確率(0から1まで)が入る
  
  trend       = trend*100.                         ; 単位を変換

 ;;; 格子情報等の付加
  trend!0     = "lat" 
  trend!1     = "lon"
  trend&lat   = lat
  trend&lon   = lon
  trend@unit  = "~S~O~N~C/century"
  copy_VarCoords(trend,cdl)


FIG="SST_TREND_COBE_"+yyyys+"-"+yyyye+".eps"
wks  = gsn_open_wks("eps",FIG)

 res                 = True         ; トレンドのためのres
  res@gsnDraw         = False        ; plotを描かない
  res@gsnFrame        = False        ; WorkStationを更新しない
  res@cnLinesOn       = False
  res@cnFillOn        = True
  res@cnInfoLabelOn   = False
  res@tiMainString    = "SST trend ("+yyyys+"-"+yyyye+")"

;
res@cnFillPalette = (/\
"dodgerblue","lightskyblue","azure1","white","yellow","gold","orange","red","firebrick"/)
res@cnLevelSelectionMode = "ExplicitLevels"
res@cnLevels = (/-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0/)

;  res@cnLevelSelectionMode = "ManualLevels"
;  res@cnMinLevelValF  = -1
;  res@cnMaxLevelValF  =  1
;  res@cnLevelSpacingF =  0.2
;  res@cnFillPalette   = "BlWhRe"


  res@mpMaxLatF       =  60
  res@mpMinLatF       = -60
  res@mpCenterLonF    = 210
  res@gsnRightString  = ""
  res@pmLabelBarOrthogonalPosF = 0.2        ; カラーバーの位置を微調整
  ;;; タイトルや軸の文字の大きさを設定
  res@tmYLLabelFontHeightF = 0.016
  res@tmXBLabelFontHeightF = 0.016
  res@tiMainFontHeightF    = 0.024
  res@lbLabelFontHeightF   = 0.016
  ;;; カラーバーにタイトルをつける
  res@lbTitleOn            = True
  res@lbTitleString        = trend@unit
  res@lbTitlePosition      = "Right"
  res@lbTitleDirection     = "Across" 
  res@lbTitleFontHeightF   = 0.016
  
  plot = gsn_csm_contour_map_ce(wks,trend,res)   ; trendを描いたものを一旦plotに収める

  res2                 = True          ; 有意性のためのres
  res2@gsnDraw         = False         ; plotを描かない
  res2@gsnFrame        = False         ; WorkStationを更新しない
  ;;; あとでShadeLtGtContourを用いるため，あらゆるものをFalseにしておく
  res2@cnLinesOn       = False
  res2@cnLineLabelsOn  = False
  res2@cnFillOn        = False
  res2@cnInfoLabelOn   = False
  ;;; ShadeLtGtContourのために，等値線は指定しておく
  res2@cnLevelSelectionMode = "ExplicitLevels"
  res2@cnLevels        = (/0.024,0.025,0.975,0.976/)

  dum  = gsn_csm_contour(wks,cdl,res2)           ; とりあえずcdlを描く
  dum  = ShadeLtGtContour(dum,0.025,17,0.975,17)

;  dum  = ShadeLtGtContour(dum,0.025,6,0.975,17)
;      ;;; 0.025のコンターより下を6番のハッチ，0.975のコンターより上を点々(17番)
;      ;;; これは有意水準5％の両側検定に対応

  overlay(plot,dum)  ; 有意性を示したdumをplotに重ねる
  
  draw(plot)         ; ここでplotを描く
  frame(wks)         ; WorkStationの更新

print("")
print("FIG")
system("ls -lh "+FIG)
print("")

end

```

## TEST_READ_COBE.ncl
```
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

indir="../GRIB"

infle="sst"

f=addfile("../GRIB/sst1905.grib","r")
print(f)
lat=f->g0_lat_1
lon=f->g0_lon_2


files = systemfunc("ls "+indir+"/"+infle+"*.grib")

fall=addfiles(files,"r")

sst=fall[:]->WTMP_GDS0_DBSL
time=fall[:]->initial_time0_hours

printVarSummary (time)
printVarSummary (lat)
printVarSummary (lon)
printVarSummary (sst)



end

```

## TEST_PLOT_COBE.ncl
```
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

indir="/work05/manda/TEACHING/TYPHOON_ENECO_1912/GRIB"

infle="sst"

f=addfile("../GRIB/sst1905.grib","r")
print(f)
lat=f->g0_lat_1
lon=f->g0_lon_2


files = systemfunc("ls "+indir+"/"+infle+"*.grib")

fall=addfiles(files,"r")

sst=fall[:]->WTMP_GDS0_DBSL
time=fall[:]->initial_time0_hours

;printVarSummary (time)
;printVarSummary (lat)
;printVarSummary (lon)
;printVarSummary (sst)

idx=0

utc_date = cd_calendar(time(idx), 0)

year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
day    = tointeger(utc_date(:,2))

print("year month day "+year+" "+month+" "+day)

YYYY=sprinti("%0.4i", year)
  MM=sprinti("%0.2i", month)

FIG="TEST_PLOT_COBE_"+YYYY+MM+".eps"
print("FIG "+FIG)
print("")


res=True
wks = gsn_open_wks("eps",FIG)

  res@gsnLeftString   = indir               ; add the gsn titles
  res@gsnCenterString = ""
  res@gsnRightString  = infle
res@mpCenterLonF = 140  ; 経度の中心

plot = gsn_csm_contour_map_ce(wks,sst(idx,:,:),res)

end

```

## TEST_ANNUAL_COBE.ncl
```
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

indir="/work05/manda/TEACHING/TYPHOON_ENECO_1912/GRIB"

infle="sst"

f=addfile("../GRIB/sst1905.grib","r")
print(f)
lat=f->g0_lat_1
lon=f->g0_lon_2


files = systemfunc("ls "+indir+"/"+infle+"*.grib")

fall=addfiles(files,"r")

sst=fall[:]->WTMP_GDS0_DBSL
time=fall[:]->initial_time0_hours

YYYYMM  = cd_calendar(time,-1) 

sst1yr=month_to_annual_weighted(YYYYMM,sst,1)
;sst1yr=month_to_annual(sst,1)




;;;
YYYY=2005
;;;


idx=(YYYY-1905)*12
idx1yr=(YYYY-1905)

utc_date = cd_calendar(time(idx), 0)

year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
day    = tointeger(utc_date(:,2))


print("year "+year)

;; YYYY=sprinti("%0.4i", year)

FIG="TEST_ANNUAL_COBE_"+YYYY+".eps"

res=True
wks = gsn_open_wks("eps",FIG)

  res@gsnLeftString   = indir               ; add the gsn titles
  res@gsnCenterString = ""
  res@gsnRightString  = infle+" "+YYYY
res@mpCenterLonF = 140  ; 経度の中心

plot = gsn_csm_contour_map_ce(wks,sst1yr(idx1yr,:,:),res)

print("")
printVarSummary(sst1yr)
print("")
print("idx=" + idx + " idx1yr = " + idx1yr + " YYYY="+YYYY)
print("FIG ")
system("ls -lh "+FIG)
print("")

end

```

## TEST_TSRS_COBE.ncl
```
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

indir="/work05/manda/TEACHING/TYPHOON_ENECO_1912/GRIB"

infle="sst"

f=addfile("../GRIB/sst1905.grib","r")
;print(f)
lat=f->g0_lat_1
lon=f->g0_lon_2


files = systemfunc("ls "+indir+"/"+infle+"*.grib")

fall=addfiles(files,"r")

sst=fall[:]->WTMP_GDS0_DBSL
time=fall[:]->initial_time0_hours

YYYYMM  = cd_calendar(time,-1) 
;print(YYYYMM)

sst1yr=month_to_annual_weighted(YYYYMM,sst,1)
dim=dimsizes(sst1yr)
ny=dim(0)

yyyy=new(ny,integer)


utc_date = cd_calendar(time(:), 0)

year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
day    = tointeger(utc_date(:,2))
dim=dimsizes(year)
nt=dim(0)


;print(nt)
yyyy=ispan(year(0),year(nt-1),1)

;printVarSummary(yyyy)
print("yyyy(0)=" + yyyy(0) + " yyyy(ny-1)="+yyyy(ny-1))


LAT=26.5
LON=126.5
sst1p=sst1yr(:, { LAT},{ LON})
;sst1p=sst1yr(:, { g0_lat_1 |LAT},{ g0_lon_2 |LON})

FIG="TEST_TSRS_COBE_"+sprintf("%4.1f",LAT)+"_"+sprintf("%5.1f",LON)+".eps"

wks = gsn_open_wks("eps",FIG)

res=True


  res@gsnLeftString   = indir               ; add the gsn titles
  res@gsnCenterString = ""
  res@gsnRightString  = sprintf("%4.1f",LAT)+"N "+sprintf("%5.1f",LON)+"E"


plot  = gsn_csm_xy (wks,yyyy,sst1p,res) ; create plot

end

```

