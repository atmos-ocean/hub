begin

setfileoption("nc","Format","LargeFile")

VAR="air"
LEV=925
print("MMMMM AIR "+LEV)

yearI=2018
monthI=07
dayI=23

INFLE1="AIR_2018_"+LEV+"_DAILY_SMO5DAY.nc"
INFLE2="AIR_1981-2017_ClmSmo"+LEV+".nc"

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

print("MMMMM ANOMALY")
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

FIG=VAR+"_AnoSmo_"+LEV+"_"+date_str+"_"+LONLAT
TYP="pdf"
wks = gsn_open_wks(TYP,FIG)

opt = True       ; plot mods desired
opt@gsnDraw      =  False 
opt@gsnFrame     =  False
opt@gsnFrame                = False   
;opt@gsnMaximize = True

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
res@tiMainString    = LEV+"hPa "+date_str+" Daily Anom"
res@gsnLeftString    = ""
res@gsnCenterString    = ""
res@gsnRightString    = ""

;sres@mpLandFillColor = 1
res@cnFillOn             = True
res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF = -5.; ;12330.
res@cnMaxLevelValF = 5.; 12410.
res@cnLevelSpacingF =  0.5
res@cnLabelDrawOrder           = "PostDraw" 
res@cnLineColor = "white"
res@gsnLeftString   = ""
res@gsnCenterString = ""
res@gsnRightString  = ""
res@lbOrientation = "vertical"
res@pmLabelBarWidthF=0.05
plot = gsn_csm_contour_map(wks,VOUT,res)

;overlay(plot,plot1)
draw(plot)

frame(wks)

print("MMMMM FIG:    "+FIG)

end

