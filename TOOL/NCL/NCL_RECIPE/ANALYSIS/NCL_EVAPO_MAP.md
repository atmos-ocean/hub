
List of the following files:
----------------------------
EVAPO_SST_MAP_JJA.ncl
TEST_LHF_MAP_1MO.ncl
TEST_LHF_MAP_JJA.ncl
TEST_EVAPO_MAP_JJA.ncl
TEST_EVAPO_COLOR_JJA.ncl

Machine info
----------------------------
calypso.bosai.go.jp
/work05/manda/TEACHING/TYPHOON_ENECO_1912/EVAPO_MAP
Wed, 25 Dec 2019 20:36:00 +0900

## EVAPO_SST_MAP_JJA.ncl
```
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

indir1="/work05/manda/DATA/J-OFURO3/CLM"
infle1="J-OFURO3_LHF_V1.0_CLM_HR_1988-2013.nc"
in1=indir1+"/"+infle1

f1=addfile(in1,"r")

lon=f1->longitude
lat=f1->latitude
LHF=f1->LHF

;printVarSummary(lon)
;printVarSummary(lat)
;printVarSummary(LHF)



indir2="/work05/manda/DATA/J-OFURO3/MONTHLY/HR/SST"
infle2="J-OFURO3_SST_V1.0_MONTHLY_HR_*.nc"
in2=indir2+"/"+infle2

in2tmp=indir2+"/J-OFURO3_SST_V1.0_MONTHLY_HR_1988.nc"
;system("ncdump -h "+in2tmp)

f2t=addfile(in2tmp,"r")
lon2=f2t->longitude
lat2=f2t->latitude

files=systemfunc("ls "+in2)
f2=addfiles(files,"r")
;print(f2)

SSTin=f2[:]->SST


SSTsea=month_to_season(SSTin,"JJA")


SST=dim_avg_n_Wrap(SSTsea,0)

printVarSummary(lon2)
printVarSummary(lat2)

SST!0     = "lat" 
SST!1     = "lon"
SST&lat   = lat2
SST&lon   = lon2
SST@unit  = "degC"
SST&lat@units="degrees_north"
SST&lon@units="degrees_east"
printVarSummary(SST)



idx=6

DAY3M=30.0+31.0+31.0
EVAPO=LHF(idx-1,:,:)*30.0/DAY3M + LHF(idx,:,:)*31.0/DAY3M+\
LHF(idx+1,:,:)*31.0/DAY3M
EVAPO!0     = "lat" 
EVAPO!1     = "lon"
EVAPO&lat   = lat
EVAPO&lon   = lon
EVAPO@unit  = "mm/d"
EVAPO&lat@units="degrees_north"
EVAPO&lon@units="degrees_east"
printVarSummary(EVAPO)

; https://sites.google.com/site/afcanalysis/home/constants/evaporation
FACT=0.001441*24.0 ; W/m2 -> mm/d

EVAPO=EVAPO*FACT



FIG="EVAPO_SST_MAP_JJA_JOFURO3.eps"

res=True
wks = gsn_open_wks("eps",FIG)

  res@vpWidthF = 0.6
  res@vpHeightF = 0.6
  res@gsnLeftString   = ""               ; add the gsn titles
  res@gsnCenterString = ""
  res@gsnRightString  = ""

  res@gsnDraw         = False        ; plotを描かない
  res@gsnFrame        = False        ; WorkStationを更新しない
  res@cnLinesOn       = False
  res@cnFillOn        = True
  res@cnInfoLabelOn   = False

  res@cnLevelSelectionMode = "ManualLevels"
  res@cnMinLevelValF  = 0
  res@cnMaxLevelValF  =  5
  res@cnLevelSpacingF =  0.5
  res@cnFillPalette   = "CBR_wet"

  res@mpMinLonF       = 100
  res@mpMaxLonF       = 160
  res@mpMinLatF       = -10
  res@mpMaxLatF       =  50
  res@mpCenterLonF = 140  ; 経度の中心

;; res@gsnMaximize        = True 

  res@gsnRightString  = ""
  res@pmLabelBarOrthogonalPosF = 0.1        ; カラーバーの位置を微調整
  ;;; タイトルや軸の文字の大きさを設定
  res@tmYLLabelFontHeightF = 0.016
  res@tmXBLabelFontHeightF = 0.016
  res@tiMainFontHeightF    = 0.024
  res@lbLabelFontHeightF   = 0.016
  ;;; カラーバーにタイトルをつける
  res@lbTitleOn            = True
  res@lbTitleString        = EVAPO@unit
  res@lbTitlePosition      = "Right"
  res@lbTitleDirection     = "Across" 
  res@lbTitleFontHeightF   = 0.016
  res@pmLabelBarHeightF = 0.05
plot = gsn_csm_contour_map_ce(wks,EVAPO(:,:),res)


  res@cnLinesOn       = True
  res@cnFillOn        = False

  delete(res@mpMinLonF);       = 100
  delete(res@mpMaxLonF);       = 160
  delete(res@mpMinLatF);       = -10
  delete(res@mpMaxLatF);       =  50
  delete(res@mpCenterLonF); = 140  ; 経度の中心

  res@cnLevelSelectionMode = "ManualLevels"
  res@cnMinLevelValF  = 0
  res@cnMaxLevelValF  =  32
  res@cnLevelSpacingF =  1

plot2 = gsn_csm_contour(wks,SST(:,:),res)

;draw(plot2)

overlay(plot,plot2)
  draw(plot)         ; ここでplotを描く

; HEADER
txres=True
txres@txFontHeightF = 0.015
txres@txJust="CenterLeft"
today = systemfunc("date -R")
gsn_text_ndc(wks,today,  0.05,0.005,txres)
cwd =systemfunc("pwd")
gsn_text_ndc(wks,"Current dir: "+cwd,      0.05,0.025,txres)
gsn_text_ndc(wks,"Script: "+indir1, 0.05,0.045,txres)
gsn_text_ndc(wks,"Script: "+indir2, 0.05,0.065,txres)

  frame(wks)         ; WorkStationの更新

print("FIG "+FIG)
print("")

end

```

## TEST_LHF_MAP_1MO.ncl
```
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

indir1="/work05/manda/DATA/J-OFURO3/CLM"
infle1="J-OFURO3_LHF_V1.0_CLM_HR_1988-2013.nc"

in1=indir1+"/"+infle1

;system("ncdump -h "+in1)

f=addfile(in1,"r")

LON=f->longitude
LAT=f->latitude
LHF=f->LHF

printVarSummary(LON)
printVarSummary(LAT)
printVarSummary(LHF)

FIG="TEST_LHF_MAP_1MO.eps"
print("FIG "+FIG)
print("")


idx=6

res=True
wks = gsn_open_wks("eps",FIG)

  res@gsnLeftString   = indir1               ; add the gsn titles
  res@gsnCenterString = ""
  res@gsnRightString  = infle1
res@mpCenterLonF = 140  ; 経度の中心

plot = gsn_csm_contour_map_ce(wks,LHF(idx,:,:),res)

end

```

## TEST_LHF_MAP_JJA.ncl
```
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

indir1="/work05/manda/DATA/J-OFURO3/CLM"
infle1="J-OFURO3_LHF_V1.0_CLM_HR_1988-2013.nc"

in1=indir1+"/"+infle1

;system("ncdump -h "+in1)

f=addfile(in1,"r")

lon=f->longitude
lat=f->latitude
LHF=f->LHF

;printVarSummary(lon)
;printVarSummary(lat)
;printVarSummary(LHF)

FIG="TEST_LHF_MAP_JJA.eps"
print("FIG "+FIG)
print("")


idx=6

DAY3M=30.0+31.0+31.0
LHF3M=LHF(idx-1,:,:)*30.0/DAY3M + LHF(idx,:,:)*31.0/DAY3M+\
LHF(idx+1,:,:)*31.0/DAY3M

 ;;; 格子情報等の付加
LHF3M!0     = "lat" 
LHF3M!1     = "lon"
LHF3M&lat   = lat
LHF3M&lon   = lon
LHF3M@unit  = "Wm-2"

res=True
wks = gsn_open_wks("eps",FIG)

  res@gsnLeftString   = indir1               ; add the gsn titles
  res@gsnCenterString = ""
  res@gsnRightString  = infle1
res@mpCenterLonF = 140  ; 経度の中心

plot = gsn_csm_contour_map_ce(wks,LHF3M(:,:),res)

end

```

## TEST_EVAPO_MAP_JJA.ncl
```
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

indir1="/work05/manda/DATA/J-OFURO3/CLM"
infle1="J-OFURO3_LHF_V1.0_CLM_HR_1988-2013.nc"

in1=indir1+"/"+infle1

;system("ncdump -h "+in1)

f=addfile(in1,"r")

lon=f->longitude
lat=f->latitude
LHF=f->LHF

;printVarSummary(lon)
;printVarSummary(lat)
;printVarSummary(LHF)

FIG="TEST_EVAPO_MAP_JJA.eps"
print("FIG "+FIG)
print("")


idx=6

DAY3M=30.0+31.0+31.0
EVAPO=LHF(idx-1,:,:)*30.0/DAY3M + LHF(idx,:,:)*31.0/DAY3M+\
LHF(idx+1,:,:)*31.0/DAY3M

; https://sites.google.com/site/afcanalysis/home/constants/evaporation
FACT=0.001441*24.0 ; W/m2 -> mm/d

EVAPO=EVAPO*FACT




 ;;; 格子情報等の付加
EVAPO!0     = "lat" 
EVAPO!1     = "lon"
EVAPO&lat   = lat
EVAPO&lon   = lon
EVAPO@unit  = "Wm-2"

res=True
wks = gsn_open_wks("eps",FIG)

  res@gsnLeftString   = indir1               ; add the gsn titles
  res@gsnCenterString = ""
  res@gsnRightString  = infle1
res@mpCenterLonF = 140  ; 経度の中心

plot = gsn_csm_contour_map_ce(wks,EVAPO(:,:),res)

end

```

## TEST_EVAPO_COLOR_JJA.ncl
```
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

indir1="/work05/manda/DATA/J-OFURO3/CLM"
infle1="J-OFURO3_LHF_V1.0_CLM_HR_1988-2013.nc"
in1=indir1+"/"+infle1


;system("ncdump -h "+in1)

f=addfile(in1,"r")

lon=f->longitude
lat=f->latitude
LHF=f->LHF

;printVarSummary(lon)
;printVarSummary(lat)
;printVarSummary(LHF)

FIG="TEST_EVAPO_COLOR_JJA.eps"
print("FIG "+FIG)
print("")


idx=6

DAY3M=30.0+31.0+31.0
EVAPO=LHF(idx-1,:,:)*30.0/DAY3M + LHF(idx,:,:)*31.0/DAY3M+\
LHF(idx+1,:,:)*31.0/DAY3M

; https://sites.google.com/site/afcanalysis/home/constants/evaporation
FACT=0.001441*24.0 ; W/m2 -> mm/d

EVAPO=EVAPO*FACT




 ;;; 格子情報等の付加
EVAPO!0     = "lat" 
EVAPO!1     = "lon"
EVAPO&lat   = lat
EVAPO&lon   = lon
EVAPO@unit  = "mm/d"

res=True
wks = gsn_open_wks("eps",FIG)

  res@gsnLeftString   = indir1               ; add the gsn titles
  res@gsnCenterString = ""
  res@gsnRightString  = infle1

  res@gsnDraw         = False        ; plotを描かない
  res@gsnFrame        = False        ; WorkStationを更新しない
  res@cnLinesOn       = False
  res@cnFillOn        = True
  res@cnInfoLabelOn   = False

  res@cnLevelSelectionMode = "ManualLevels"
  res@cnMinLevelValF  = 0
  res@cnMaxLevelValF  =  5
  res@cnLevelSpacingF =  0.5
  res@cnFillPalette   = "CBR_wet"

  res@mpMinLonF       = 100
  res@mpMaxLonF       = 160
  res@mpMinLatF       = -10
  res@mpMaxLatF       =  50
  res@mpCenterLonF = 140  ; 経度の中心

res@gsnMaximize        = True 

  res@gsnRightString  = ""
  res@pmLabelBarOrthogonalPosF = 0.1        ; カラーバーの位置を微調整
  ;;; タイトルや軸の文字の大きさを設定
  res@tmYLLabelFontHeightF = 0.016
  res@tmXBLabelFontHeightF = 0.016
  res@tiMainFontHeightF    = 0.024
  res@lbLabelFontHeightF   = 0.016
  ;;; カラーバーにタイトルをつける
  res@lbTitleOn            = True
  res@lbTitleString        = EVAPO@unit
  res@lbTitlePosition      = "Right"
  res@lbTitleDirection     = "Across" 
  res@lbTitleFontHeightF   = 0.016

plot = gsn_csm_contour_map_ce(wks,EVAPO(:,:),res)


  draw(plot)         ; ここでplotを描く
  frame(wks)         ; WorkStationの更新

end

```

