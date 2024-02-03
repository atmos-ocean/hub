scriptname_in = getenv("NCL_ARG_1")
scriptname=systemfunc("basename " + scriptname_in + " .ncl")
YYYYMM    = getenv("NCL_ARG_2")

print("script name ="+scriptname_in)
print("YYYYMM         ="+YYYYMM)
print("")

infle="OI_AVHRR_v02r01_MAVE_"+YYYYMM+".nc"

f = addfile(infle,"r")
sst=f->sst

ofle=systemfunc("basename "+infle+" .nc")+".eps"

printVarSummary(sst)
printVarSummary(sst&time)
printVarSummary(sst&lat)
printVarSummary(sst&lon)

  ;;; ここからお絵描きの設定
  res1 = True                                  ; Z500の描画に用いるresource
  res1@gsnDraw          = False                ; gsn_csm_contour_map_ce実行時にDrawしない
  res1@gsnFrame         = False                ; gsn_csm_contour_map_ce実行時にFrame更新しない
  res1@cnFillOn         = True                 ; シェードを使う
res1@cnLineLabelsOn             = True
;res1@cnLevelSelectionMode = "ManualLevels"	; manually set the contour levels with the following 3 resources
;res1@cnMinLevelValF  = -2.			; set the minimum contour level
;res1@cnMaxLevelValF  =  32.			; set the maximum contour level
;res1@cnLevelSpacingF = 2.			; set the interval between contours
res1@mpCenterLonF           = 180
  res1@tiMainString    = "OISSTv02r01"   ; add titles
  res1@gsnLeftString   = ""               ; add the gsn titles
  res1@gsnCenterString = YYYYMM
  res1@gsnRightString  = "SST [C]"
  res1@lbOrientation = "vertical"
  wks   = gsn_open_wks("eps",ofle)
  var=short2flt(sst(0,0,:,:))
  plot1 = gsn_csm_contour_map_ce(wks,var,res1)

; HEADER
txres=True
txres@txFontHeightF = 0.015
txres@txJust="CenterLeft"
today = systemfunc("date -R")
gsn_text_ndc(wks,today,  0.05,0.04,txres)
cwd =systemfunc("pwd")
gsn_text_ndc(wks,"Current dir: "+cwd,      0.05,0.06,txres)
gsn_text_ndc(wks,"INPUT: "+infle, 0.05,0.08,txres)
gsn_text_ndc(wks,"OUTPUT: "+ofle, 0.05,0.10,txres)

  draw(plot1)
  frame(wks)
  delete([/plot1,res1/])

print("----------------------------")
system("ls --time-style=long-iso -lh "+ofle)
print("----------------------------")
