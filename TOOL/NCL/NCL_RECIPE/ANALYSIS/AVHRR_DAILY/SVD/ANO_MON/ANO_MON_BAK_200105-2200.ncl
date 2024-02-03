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
  nsvd    =    3                    ; 求めるモード数

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

  hgt     = in1->hgt(t1:t2:12,{50000},:,:)     ; Z500の値を取得。{500}で500hPa面を指定(高さ次元の中身をあらかじめ知っている)。
                                             ; 時間次元はt1からt2まで12おき
                                             ; これは1958年から2015年の1月の値のみ切り取ることに対応
;;;
print_clock("CHECK Z500 RAW")
OUTDIR_TMP="OUT_ANO_MON/"
system("mkdir -vp "+OUTDIR_TMP)
dim=dimsizes(hgt)
nt=dim(0)
print("nt="+nt)
;print("hgt "+hgt(nt-1,:,:))

time_tmp=in1->time(t1:t2:12)

do n=0,nt-1
utc_date = ut_calendar(time_tmp(n), 0)
year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
date_str = sprinti("%0.4i", year)+ sprinti("%0.2i", month)

ofle=OUTDIR_TMP+"Z500_"+date_str
print("OUTPUT: "+ofle)

  ;;; ここからお絵描きの設定
  res1 = True                                  ; Z500の描画に用いるresource
  res1@gsnDraw          = False                ; gsn_csm_contour_map_ce実行時にDrawしない
  res1@gsnFrame         = False                ; gsn_csm_contour_map_ce実行時にFrame更新しない
  res1@cnFillOn         = True                 ; シェードを使う
res1@cnLineLabelsOn             = True
res1@cnLevelSelectionMode = "ManualLevels"	; manually set the contour levels with the following 3 resources
res1@cnMinLevelValF  = 5000.			; set the minimum contour level
res1@cnMaxLevelValF  = 5900.			; set the maximum contour level
res1@cnLevelSpacingF = 50.			; set the interval between contours
res1@mpCenterLonF           = 180

  res1@tiMainString    = "JRA55"   ; add titles
  res1@gsnLeftString   = date_str              ; add the gsn titles
  res1@gsnCenterString = ""
  res1@gsnRightString  = "Z500 [m]"

  wks   = gsn_open_wks("eps",ofle)
  plot1 = gsn_csm_contour_map_ce(wks,hgt(n,:,:),res1)

; HEADER
txres=True
txres@txFontHeightF = 0.015
txres@txJust="CenterLeft"
today = systemfunc("date -R")
gsn_text_ndc(wks,today,  0.05,0.04,txres)
cwd =systemfunc("pwd")
gsn_text_ndc(wks,"Current dir: "+cwd,      0.05,0.06,txres)
gsn_text_ndc(wks,"INPUT: "+infile1, 0.05,0.08,txres)
gsn_text_ndc(wks,"OUTPUT: "+ofle, 0.05,0.10,txres)

  draw(plot1)
  frame(wks)
  delete([/plot1,res1/])
end do
;;;



  hgt     = dtrend_msg_n(ispan(y1,y2,1),hgt,True,False,0)
                                             ; 平均とトレンドの除去

;;;
print_clock("CHECK Z500 ANO")

;printVarSummary(hgt)
dim=dimsizes(hgt)
nt=dim(0)
print("nt="+nt)
;print("hgt "+hgt(nt-1,:,:))

time_tmp=in1->time(t1:t2:12)

do n=0,nt-1
utc_date = ut_calendar(time_tmp(n), 0)
year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
date_str = sprinti("%0.4i", year)+ sprinti("%0.2i", month)

ofle=OUTDIR_TMP+"Z500_ANO_"+date_str
print("OUTPUT: "+ofle)

  ;;; ここからお絵描きの設定
  res1 = True                                  ; Z500の描画に用いるresource
  res1@gsnDraw          = False                ; gsn_csm_contour_map_ce実行時にDrawしない
  res1@gsnFrame         = False                ; gsn_csm_contour_map_ce実行時にFrame更新しない
  res1@cnFillOn         = True                 ; シェードを使う
res1@cnLineLabelsOn             = True
res1@cnLevelSelectionMode = "ManualLevels"	; manually set the contour levels with the following 3 resources
res1@cnMinLevelValF  = -200.			; set the minimum contour level
res1@cnMaxLevelValF  =  200.			; set the maximum contour level
res1@cnLevelSpacingF = 20.			; set the interval between contours
res1@mpCenterLonF           = 180
  res1@tiMainString    = "JRA55"   ; add titles
  res1@gsnLeftString   = date_str               ; add the gsn titles
  res1@gsnCenterString = ""
  res1@gsnRightString  = "Z500 ANOM [m]"

  wks   = gsn_open_wks("eps",ofle)
  plot1 = gsn_csm_contour_map_ce(wks,hgt(n,:,:),res1)

; HEADER
txres=True
txres@txFontHeightF = 0.015
txres@txJust="CenterLeft"
today = systemfunc("date -R")
gsn_text_ndc(wks,today,  0.05,0.04,txres)
cwd =systemfunc("pwd")
gsn_text_ndc(wks,"Current dir: "+cwd,      0.05,0.06,txres)
gsn_text_ndc(wks,"INPUT: "+infile1, 0.05,0.08,txres)
gsn_text_ndc(wks,"OUTPUT: "+ofle, 0.05,0.10,txres)

  draw(plot1)
  frame(wks)
  delete([/plot1,res1/])
end do
;;;


  lat1    = in1->lat                         ; Z500に対応する緯度の配列を取得
  lon1    = in1->lon                         ; Z500に対応する経度の配列を取得
  hgt!0   = "time"                           ; hgtに座標をつけておく
  hgt!1   = "lat"
  hgt!2   = "lon"
  delete([/time,YYYYMM/])                    ; timeとYYYYMMの配列を消しておく
                                             ; (SSTデータの読み込みでも同じ変数名を使うから)
printVarSummary(hgt)



print_clock("READ SST")
  ;;; 変数2(SST)のデータを上と同様にして読む
  in2     = addfile(infile2,"r")
  time    = in2->time
  YYYYMM  = cd_calendar(time,-1)
  t1      = ind(YYYYMM.eq.y1*100+1)
  t2      = ind(YYYYMM.eq.y2*100+12)

  sst     = in2->sst(t1:t2:12,:,:)

;;;
print_clock("CHECK SST")

;printVarSummary(sst)
dim=dimsizes(sst)
nt=dim(0)
print("nt="+nt)

time_tmp=in2->time(t1:t2:12)

do n=0,nt-1

utc_date = ut_calendar(time_tmp(n), 0)
year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
date_str = sprinti("%0.4i", year)+ sprinti("%0.2i", month)

ofle=OUTDIR_TMP+"SST_"+date_str
print("OUTPUT: "+ofle)

  ;;; ここからお絵描きの設定
  res1 = True                                  ; Z500の描画に用いるresource
  res1@gsnDraw          = False                ; gsn_csm_contour_map_ce実行時にDrawしない
  res1@gsnFrame         = False                ; gsn_csm_contour_map_ce実行時にFrame更新しない
  res1@cnFillOn         = True                 ; シェードを使う
res1@cnLineLabelsOn             = True
res1@cnLevelSelectionMode = "ManualLevels"	; manually set the contour levels with the following 3 resources
res1@cnMinLevelValF  = -2.			; set the minimum contour level
res1@cnMaxLevelValF  =  32.			; set the maximum contour level
res1@cnLevelSpacingF = 2.			; set the interval between contours
res1@mpCenterLonF           = 180
  res1@tiMainString    = "ERSSTv4"   ; add titles
  res1@gsnLeftString   = date_str               ; add the gsn titles
  res1@gsnCenterString = ""
  res1@gsnRightString  = "SST [C]"

  wks   = gsn_open_wks("eps",ofle)
  plot1 = gsn_csm_contour_map_ce(wks,sst(n,:,:),res1)

; HEADER
txres=True
txres@txFontHeightF = 0.015
txres@txJust="CenterLeft"
today = systemfunc("date -R")
gsn_text_ndc(wks,today,  0.05,0.04,txres)
cwd =systemfunc("pwd")
gsn_text_ndc(wks,"Current dir: "+cwd,      0.05,0.06,txres)
gsn_text_ndc(wks,"INPUT: "+infile1, 0.05,0.08,txres)
gsn_text_ndc(wks,"OUTPUT: "+ofle, 0.05,0.10,txres)

  draw(plot1)
  frame(wks)
  delete([/plot1,res1/])
end do
;;;

  sst     = dtrend_msg_n(ispan(y1,y2,1),sst,True,False,0)

;;;
print_clock("CHECK SST ANO")

;printVarSummary(sst)
dim=dimsizes(sst)
nt=dim(0)
print("nt="+nt)
;print("sst "+sst(nt-1,:,:))

time_tmp=in2->time(t1:t2:12)

do n=0,nt-1

utc_date = ut_calendar(time_tmp(n), 0)
year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
date_str = sprinti("%0.4i", year)+ sprinti("%0.2i", month)

ofle=OUTDIR_TMP+"SST_ANO_"+date_str
print("OUTPUT: "+ofle)


  ;;; ここからお絵描きの設定
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
  res1@tiMainString    = "ERSSTv4"   ; add titles
  res1@gsnLeftString   = date_str               ; add the gsn titles
  res1@gsnCenterString = ""
  res1@gsnRightString  = "SST ANO [C]"

  wks   = gsn_open_wks("eps",ofle)
  plot1 = gsn_csm_contour_map_ce(wks,sst(n,:,:),res1)

; HEADER
txres=True
txres@txFontHeightF = 0.015
txres@txJust="CenterLeft"
today = systemfunc("date -R")
gsn_text_ndc(wks,today,  0.05,0.04,txres)
cwd =systemfunc("pwd")
gsn_text_ndc(wks,"Current dir: "+cwd,      0.05,0.06,txres)
gsn_text_ndc(wks,"INPUT: "+infile1, 0.05,0.08,txres)
gsn_text_ndc(wks,"OUTPUT: "+ofle, 0.05,0.10,txres)

  draw(plot1)
  frame(wks)
  delete([/plot1,res1/])
end do
;;;


  lat2    = in2->lat
  lon2    = in2->lon
  sst!0   = "time"
  sst!1   = "lat"
  sst!2   = "lon"
  delete([/time,YYYYMM/])
printVarSummary(sst)


print("")
print("Done " + script_name)
print("")
; system("ls -lh --time-style=long-iso *."+out)
print("")

end
