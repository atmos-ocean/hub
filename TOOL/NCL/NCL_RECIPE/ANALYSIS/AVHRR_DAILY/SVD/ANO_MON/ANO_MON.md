
List of the following files:
----------------------------
ANO_MON.sh
ANO_MON.ncl
ANO_MON.LOG

Machine info
----------------------------
calypso.bosai.go.jp
/work05/manda/NCL/ANO_MON
Sun, 05 Jan 2020 23:04:00 +0900

## ANO_MON.sh
```bash
#!/bin/bash
#
# Sun, 05 Jan 2020 20:05:41 +0900
# calypso.bosai.go.jp
# /work05/manda/NCL/SVD
# manda
#

LOG=$(basename $0 .sh).LOG
# LOG=$(basename $0 .sh)_$(date +"%y%m%d_%H%M).LOG

date -R  >$LOG
hostname >>$LOG
pwd      >>$LOG
NCL=$(basename $0 .sh).ncl
ls -lh --time-style=long-iso $(basename $0) >>$LOG
ls -lh --time-style=long-iso $(basename $NCL) >>$LOG

echo     >>$LOG


#ncl -nQ $NCL 2>&1 |tee -a $LOG
unbuffer ncl -nQ $NCL >> $LOG 2>&1

echo
echo "Done $(basename $0)"
echo
echo "LOG: $LOG"
echo
```

## ANO_MON.ncl
```
; 
; ANO_MON.ncl
; 
; Sun, 05 Jan 2020 14:59:59 +0900
; calypso.bosai.go.jp
; /work05/manda/NCL/SVD
; manda
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

script_name  = get_script_name()
script=systemfunc("basename "+script_name+ " .ncl")


  y1      = 1958                    ; 最初の年
  y2      = 2015                    ; 最後の年

  indir1="/work05/manda/DATA_PROC/JRA55/Monthly/OUT_NC_CAT/"
  indir2="/work05/manda/DATA/SST/ERSST/V4/200104/"

  infile1 = indir1+"hgt.1958-2015.nc"    ; 変数1(Z500)のデータ
  infile2 = indir2+"sst.mnmean.v4.nc"    ; 変数2(SST)のデータ
  out     = "eps"                   ; 出力形式

;print("")
;system("ncdump -c "+infile1)
;print("")
;system("ncdump -h "+infile2)
;print("")


;;; 変数1(Z500)のデータを読む
print_clock("READ Z500 RAW")

  in1     = addfile(infile1,"r")             ; 読み込むファイルを取得


  time    = in1->time                        ; 時間の配列を取得
  YYYYMM  = cd_calendar(time,-1)             ; timeからYYYYMMの配列を作成
  t1      = ind(YYYYMM.eq.y1*100+1)          ; 1958年1月に対応するインデックスを取得
  t2      = ind(YYYYMM.eq.y2*100+12)         ; 2015年12月に対応するインデックスを取得

  hgt     = in1->hgt(t1:t2:12,{50000},:,:)   ; Z500の値を取得。{500}で500hPa面を指定(高さ次元の中身をあらかじめ知っている)。
                                             ; 時間次元はt1からt2まで12おき
  lat1    = in1->lat                         ; Z500に対応する緯度の配列を取得
  lon1    = in1->lon                         ; Z500に対応する経度の配列を取得
  hgt!0   = "time"                           ; hgtに座標をつけておく
  hgt!1   = "lat"
  hgt!2   = "lon"

  hgt_ano = hgt
  hgt_ano = dtrend_msg_n(ispan(y1,y2,1),hgt,True,False,0)
                                             ; 平均とトレンドの除去

  delete([/time,YYYYMM/])

printVarSummary(hgt)
printVarSummary(hgt_ano)



print_clock("READ SST")
  ;;; 変数2(SST)のデータを上と同様にして読む
  in2     = addfile(infile2,"r")
  time    = in2->time
  YYYYMM  = cd_calendar(time,-1)
  t1      = ind(YYYYMM.eq.y1*100+1)
  t2      = ind(YYYYMM.eq.y2*100+12)

  sst     = in2->sst(t1:t2:12,:,:)

  lat2    = in2->lat
  lon2    = in2->lon
  sst!0   = "time"
  sst!1   = "lat"
  sst!2   = "lon"

  sst_ano = sst
  sst_ano = dtrend_msg_n(ispan(y1,y2,1),sst,True,False,0)
  delete([/sst_ano@valid_range, sst_ano@actual_range/])
  sst_ano@statistic="ANOMALY"
  sst_ano@long_name="SST ANOMALY FROM LONG-TERM MEAN"
  sst_ano!0   = "time"
  sst_ano!1   = "lat"
  sst_ano!2   = "lon"
  delete([/time,YYYYMM/])

printVarSummary(sst)
printVarSummary(sst_ano)


;;;
print_clock("PLOT ANOMALY")
OUTDIR="OUT_"+script+"/"
system("mkdir -vp "+OUTDIR)

dim=dimsizes(sst_ano)
nt=dim(0)
do n=0,nt-1
utc_date = ut_calendar(sst_ano&time(n), 0)
year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
date_str = sprinti("%0.4i", year)+ sprinti("%0.2i", month)
ofle=OUTDIR+"ANO_"+date_str
print("OUTPUT: "+ofle)

plot = new(2,graphic)                          ; create a plot array
wks   = gsn_open_wks("eps",ofle)

; Z
  res0 = True                                  ; Z500の描画に用いるresource
  res0@gsnDraw          = False                ; gsn_csm_contour_map_ce実行時にDrawしない
  res0@gsnFrame         = False                ; gsn_csm_contour_map_ce実行時にFrame更新しない
  res0@cnFillOn         = True                 ; シェードを使う
res0@cnLineLabelsOn             = True
res0@cnLevelSelectionMode = "ManualLevels"	; manually set the contour levels with the following 3 resources
res0@cnMinLevelValF  = -200.			; set the minimum contour level
res0@cnMaxLevelValF  =  200.			; set the maximum contour level
res0@cnLevelSpacingF = 20.			; set the interval between contours
res0@mpCenterLonF           = 180
  res0@tiMainString    = ""   ; add titles
  res0@gsnLeftString   = date_str               ; add the gsn titles
  res0@gsnCenterString = "JRA55"
  res0@gsnRightString  = "Z500 ANOM [m]"
  res0@lbOrientation = "vertical"
  res0@pmLabelBarWidthF=0.08
  res0@lbLabelFontHeightF = 0.015

plot(0) = gsn_csm_contour_map_ce(wks,hgt_ano(n,:,:),res0)


; SST
  res1 = True                                  ; Z500の描画に用いるresource
  res1@gsnDraw          = False                ; gsn_csm_contour_map_ce実行時にDrawしない
  res1@gsnFrame         = False                ; gsn_csm_contour_map_ce実行時にFrame更新しない
  res1@cnFillOn         = True                 ; シェードを使う
res1@cnLineLabelsOn             = True
res1@cnLevelSelectionMode = "ManualLevels"	; manually set the contour levels with the following 3 resources
res1@cnMinLevelValF  = -3.			; set the minimum contour level
res1@cnMaxLevelValF  =  3.			; set the maximum contour level
res1@cnLevelSpacingF = 0.2			; set the interval between contours
res1@mpCenterLonF           = 180
  res1@tiMainString    = ""   ; add titles
  res1@gsnLeftString   = date_str               ; add the gsn titles
  res1@gsnCenterString = "ERSSTv4"
  res1@gsnRightString  = "SST ANO [C]"
  res1@lbOrientation = "vertical"
  res1@pmLabelBarWidthF=0.08
  res1@lbLabelFontHeightF = 0.015

  plot(1) = gsn_csm_contour_map_ce(wks,sst_ano(n,:,:),res1)

; HEADER
txres=True
txres@txFontHeightF = 0.015
txres@txJust="CenterLeft"
today = systemfunc("date -R")
gsn_text_ndc(wks,today,  0.05,0.96,txres)
cwd =systemfunc("pwd")
gsn_text_ndc(wks,"Current dir: "+cwd,      0.05,0.94,txres)
gsn_text_ndc(wks,"INPUT: "+infile1, 0.05,0.92,txres)
gsn_text_ndc(wks,"INPUT: "+infile2, 0.05,0.90,txres)
gsn_text_ndc(wks,"OUTPUT: "+ofle, 0.05,0.88,txres)

  resP                  = True                   ; modify the panel plot
  resP@gsnFrame         = False                  ; don't advance panel plot
  resP@gsnPanelLabelBar = False                  ; add common colorbar
  resP@gsnPanelMainString = ""     ; set main title
  resP@gsnPanelTop   = 0.85                   ; add space at bottom
;  resP@gsnPanelBottom   = 0.15                   ; add space at bottom
  resP@gsnPanelFigureStrings= (/"a)","b)"/) ; add strings to panel
  resP@amJust   = "TopLeft"
  gsn_panel(wks,plot,(/2,1/),resP)             ; now draw as one plot

  frame(wks)
  delete([/plot,res0,res1/])



end do
;;;



print("")
print_clock("DONE PLOT ANOMALY")
print("Done " + script_name)
print("")
; system("ls -lh --time-style=long-iso *."+out)
print("")

end
```

## ANO_MON.LOG
```
Sun, 05 Jan 2020 22:58:55 +0900
calypso.bosai.go.jp
/work05/manda/NCL/ANO_MON
-rwxrw-r-- 1 manda manda 508 2020-01-05 20:48 ANO_MON.sh
-rwxrw-r-- 1 manda manda 6.5K 2020-01-05 22:58 ANO_MON.ncl

READ Z500 RAW                                 |  2020-01-05 22:58:55 JST

Variable: hgt
Type: float
Total Size: 9688320 bytes
            2422080 values
Number of Dimensions: 3
Dimensions and sizes:	[time | 58] x [lat | 145] x [lon | 288]
Coordinates: 
            time: [   0..499656]
            lat: [  90.. -90]
            lon: [   0..358.75]
Number Of Attributes: 3
  lev :	50000
  code :	7
  table :	200

Variable: hgt_ano
Type: float
Total Size: 9688320 bytes
            2422080 values
Number of Dimensions: 3
Dimensions and sizes:	[time | 58] x [lat | 145] x [lon | 288]
Coordinates: 
            time: [   0..499656]
            lat: [  90.. -90]
            lon: [   0..358.75]
Number Of Attributes: 4
  _FillValue :	 0
  table :	200
  code :	7
  lev :	50000
READ SST                                      |  2020-01-05 22:58:56 JST

Variable: sst
Type: float
Total Size: 3716640 bytes
            929160 values
Number of Dimensions: 3
Dimensions and sizes:	[time | 58] x [lat | 89] x [lon | 180]
Coordinates: 
            time: [57708..78527]
            lat: [88..-88]
            lon: [ 0..358]
Number Of Attributes: 11
  long_name :	Monthly Means of Sea Surface Temperature
  units :	degC
  var_desc :	Sea Surface Temperature
  level_desc :	Surface
  statistic :	Mean
  missing_value :	-9.96921e+36
  actual_range :	( -1.8, 33.95 )
  valid_range :	( -5, 40 )
  dataset :	NOAA Extended Reconstructed SST V4
  parent_stat :	Individual Values
  _FillValue :	-9.96921e+36

Variable: sst_ano
Type: float
Total Size: 3716640 bytes
            929160 values
Number of Dimensions: 3
Dimensions and sizes:	[time | 58] x [lat | 89] x [lon | 180]
Coordinates: 
            time: [57708..78527]
            lat: [88..-88]
            lon: [ 0..358]
Number Of Attributes: 9
  _FillValue :	-9.96921e+36
  parent_stat :	Individual Values
  dataset :	NOAA Extended Reconstructed SST V4
  missing_value :	-9.96921e+36
  statistic :	ANOMALY
  level_desc :	Surface
  var_desc :	Sea Surface Temperature
  units :	degC
  long_name :	SST ANOMALY FROM LONG-TERM MEAN
PLOT ANOMALY                                  |  2020-01-05 22:59:05 JST
OUTPUT: OUT_ANO_MON/ANO_195801
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_195901
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_196001
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_196101
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_196201
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_196301
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_196401
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_196501
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_196601
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_196701
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_196801
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_196901
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_197001
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_197101
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_197201
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_197301
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_197401
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_197501
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_197601
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_197701
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_197801
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_197901
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_198001
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_198101
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_198201
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_198301
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_198401
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_198501
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_198601
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_198701
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_198801
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_198901
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_199001
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_199101
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_199201
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_199301
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_199401
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_199501
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_199601
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_199701
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_199801
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_199901
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_200001
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_200101
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_200201
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_200301
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_200401
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_200501
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_200601
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_200701
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_200801
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_200901
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_201001
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_201101
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_201201
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_201301
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_201401
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

OUTPUT: OUT_ANO_MON/ANO_201501
warning:ContourPlotInitialize: 0.0 not currently supported as a missing value; expect inaccurate plot

DONE PLOT ANOMALY                             |  2020-01-05 22:59:54 JST
Done ANO_MON.ncl


```

