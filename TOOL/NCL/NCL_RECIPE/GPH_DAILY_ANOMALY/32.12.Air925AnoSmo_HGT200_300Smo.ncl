begin

setfileoption("nc","Format","LargeFile")

VAR="Tano925_HGTsmo200_300"

yearI=2018
monthI=07
dayI=23
LEV=925

INFLE1="AIR_2018_"+LEV+"_DAILY_SMO5DAY.nc"
INFLE2="AIR_1981-2017_ClmSmo"+LEV+".nc"
INFLE3="hgt_2018_300_DAILY_SMO5DAY.nc"
INFLE4="HGT_1981-2017_ClmSmo300.nc"
INFLE5="hgt_2018_200_DAILY_SMO5DAY.nc"
INFLE6="HGT_1981-2017_ClmSmo200.nc"


a1=addfile(INFLE1,"r")
air1=a1->airSmo
lon=a1->lon
lat=a1->lat
time1= a1->time                     

air1&lon=lon
air1&lat=lat
air1&time=time1



a2=addfile(INFLE2,"r")
air2=a2->airClmSmo

air2&lon=lon
air2&lat=lat



a3=addfile(INFLE3,"r")
hgt3=a3->hgtSmo

a4=addfile(INFLE4,"r")
hgt4=a4->hClmSmo

a5=addfile(INFLE5,"r")
hgt5=a5->hgtSmo

a6=addfile(INFLE6,"r")
hgt6=a6->hClmSmo

print("MMMMM AIR ANOMALY")
doyI=day_of_year(yearI,monthI,dayI)-1
doyI@units="days since "+tostring(yearI)+"-1-1"

VOUT=air1(doyI,:,:)-air2(doyI,:,:)
VOUT!0="lat"
VOUT!1="lon"
VOUT&lat=lat
VOUT&lon=lon
printVarSummary(VOUT)


print("MMMMM PLOT")

CLON=120 ;220 ;270.0       ; choose center lon
CLAT=45 ;45 

date_str = sprinti("%0.4i", yearI)+"-"+sprinti("%0.2i", monthI)+"-"+sprinti("%0.2i", dayI)

print("PLOT DATE="+date_str) 

VOUT!0="lat"
VOUT!1="lon"
VOUT&lat=lat
VOUT&lon=lon
printVarSummary(VOUT)

LONLAT=tostring(CLON)+"_"+tostring(CLAT)

FIG=VAR+"_"+date_str+"_"+LONLAT
TYP="pdf"
wks = gsn_open_wks(TYP,FIG)

opt = True       ; plot mods desired
opt@gsnDraw      =  False 
opt@gsnFrame     =  False
opt@gsnFrame                = False   
;opt@gsnMaximize = True



print("MMMMM AIR POSITIVE VALUE")
res=opt
res@mpProjection               = "Satellite"
res@mpCenterLonF               = CLON 
res@mpCenterLatF               = CLAT
res@mpSatelliteDistF           = 3.0 
res@mpPerimOn = False
res@mpOutlineOn  = True
res@mpGridLineDashPattern= 2 
res@mpGeophysicalLineColor       = "black"  ; ínê}ÇÃê¸ÇÃêF
res@mpGeophysicalLineThicknessF  = 2        ; ínê}ÇÃê¸ÇÃëæÇ≥
res@mpOceanFillColor       = "gray97"            ; array index in color map
res@mpLandFillColor        = "tan"          ; for land, ocean, and inland h20
res@mpInlandWaterFillColor = "gray97"

res@tiMainString    = date_str
res@gsnLeftString    = ""
res@gsnCenterString    = ""
res@gsnRightString    = ""

;res@mpLandFillColor = 
res@cnFillOn             = False
res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF = 2.; ;12330.
res@cnMaxLevelValF = 10.; 12410.
res@cnLevelSpacingF =  1
res@cnLineThicknessF = 2
res@cnLabelDrawOrder           = "PostDraw"
res@cnLineLabelFontColor="red"
res@cnLineLabelBackgroundColor="white"
res@cnLineColor = "red"
res@gsnLeftString   = ""
res@gsnCenterString = ""
res@gsnRightString  = ""
res@lbOrientation = "vertical"
res@pmLabelBarWidthF=0.05
plot = gsn_csm_contour_map(wks,VOUT,res)
delete(res)


print("MMMMM AIR NEGATIVE VALUE")
res=opt
res@cnFillOn             = False
res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF = -10; ;12330.
res@cnMaxLevelValF =  -2.; 12410.
res@cnLevelSpacingF =  1
res@cnLineThicknessF = 2
res@cnLineThicknessF = 2
res@cnLabelDrawOrder           = "PostDraw" 
res@cnLineLabelFontColor="blue"
res@cnLineColor = "blue"
res@gsnLeftString   = ""
res@gsnCenterString = ""
res@gsnRightString  = ""
res@lbOrientation = "vertical"
res@pmLabelBarWidthF=0.05
plot2 = gsn_csm_contour(wks,VOUT,res)
overlay(plot,plot2)



print("MMMMM hgtSmo 300")
delete([/VOUT,res/])
VOUT=hgt3(doyI,:,:)
VOUT!0="lat"
VOUT!1="lon"
VOUT&lat=lat
VOUT&lon=lon
printVarSummary(VOUT)
res=opt
res@cnFillOn             = False
res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF = 9250.
res@cnMaxLevelValF = 9330.
res@cnLevelSpacingF =  40.
res@cnLineLabelsOn = False
res@cnLineThicknessF = 3
res@cnLabelDrawOrder           = "PostDraw" 
res@cnLineColor = "green"
res@gsnLeftString   = ""
res@gsnCenterString = ""
res@gsnRightString  = ""
res@lbOrientation = "vertical"
res@pmLabelBarWidthF=0.05
plot3 = gsn_csm_contour(wks,VOUT,res)
overlay(plot,plot3)






print("MMMMM hgtClmSmo 300")
delete([/VOUT,res/])
VOUT=hgt4(doyI,:,:)
VOUT!0="lat"
VOUT!1="lon"
VOUT&lat=lat
VOUT&lon=lon
printVarSummary(VOUT)
res=opt
res@cnFillOn             = False
res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF = 9250.
res@cnMaxLevelValF = 9330.
res@cnLevelSpacingF =  40.
res@cnLineLabelsOn = False
res@cnLineThicknessF = 2
res@cnLabelDrawOrder           = "PostDraw" 
res@cnLineColor = "black"
res@gsnLeftString   = ""
res@gsnCenterString = ""
res@gsnRightString  = ""
res@lbOrientation = "vertical"
res@pmLabelBarWidthF=0.05
plot4 = gsn_csm_contour(wks,VOUT,res)
overlay(plot,plot4)



print("MMMMM hgtSmo 200")
delete([/VOUT,res/])
VOUT=hgt5(doyI,:,:)
VOUT!0="lat"
VOUT!1="lon"
VOUT&lat=lat
VOUT&lon=lon
printVarSummary(VOUT)
res=opt
res@cnFillOn             = False
res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF = 12330.
res@cnMaxLevelValF = 12410.
res@cnLevelSpacingF =  40.
res@cnLineLabelsOn = False
res@cnLineThicknessF = 3
res@cnLabelDrawOrder           = "PostDraw" 
res@cnLineColor = "orange"
res@gsnLeftString   = ""
res@gsnCenterString = ""
res@gsnRightString  = ""
res@lbOrientation = "vertical"
res@pmLabelBarWidthF=0.05
plot3 = gsn_csm_contour(wks,VOUT,res)
overlay(plot,plot3)



print("MMMMM hgtClmSmo 200")
delete([/VOUT,res/])
VOUT=hgt6(doyI,:,:)
VOUT!0="lat"
VOUT!1="lon"
VOUT&lat=lat
VOUT&lon=lon
printVarSummary(VOUT)
res=opt
res@cnFillOn             = False
res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF = 12330.
res@cnMaxLevelValF = 12410.
res@cnLevelSpacingF =  40.
res@cnLineLabelsOn = False
res@cnLineThicknessF = 2
res@cnLabelDrawOrder           = "PostDraw" 
res@cnLineColor = "black"
res@gsnLeftString   = ""
res@gsnCenterString = ""
res@gsnRightString  = ""
res@lbOrientation = "vertical"
res@pmLabelBarWidthF=0.05
plot6 = gsn_csm_contour(wks,VOUT,res)
overlay(plot,plot6)

draw(plot)


frame(wks)

print("MMMMM FIG:    "+FIG)

end

