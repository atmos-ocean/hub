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

  indir="/work05/manda/DATA/SST/ERSST/V4/200104/"

  infile = indir+"sst.mnmean.v4.nc"    ; 変数2(SST)のデータ
  out     = "eps"                   ; 出力形式

;print("")
;system("ncdump -h "+infile)
;print("")

OUTDIR="OUT_"+script+"/"
system("mkdir -vp "+OUTDIR)


print_clock("READ SST")
  ;;; 変数2(SST)のデータを上と同様にして読む
  in2     = addfile(infile,"r")
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

ofle=OUTDIR+"SST_"+date_str
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
  res1@lbOrientation = "vertical"
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
gsn_text_ndc(wks,"INPUT: "+infile, 0.05,0.08,txres)
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

ofle=OUTDIR+"SST_ANO_"+date_str
print("OUTPUT: "+ofle)


  ;;; ここからお絵描きの設定
  res1 = True                                  ; Z500の描画に用いるresource
  res1@gsnDraw          = False                ; gsn_csm_contour_map_ce実行時にDrawしない
  res1@gsnFrame         = False                ; gsn_csm_contour_map_ce実行時にFrame更新しない
  res1@cnFillOn         = True                 ; シェードを使う
res1@cnLineLabelsOn             = True
res1@cnLevelSelectionMode = "ManualLevels"	; manually set the contour levels with the following 3 resources
res1@cnMinLevelValF  = -2.			; set the minimum contour level
res1@cnMaxLevelValF  =  2.			; set the maximum contour level
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
gsn_text_ndc(wks,"INPUT: "+infile, 0.05,0.08,txres)
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
