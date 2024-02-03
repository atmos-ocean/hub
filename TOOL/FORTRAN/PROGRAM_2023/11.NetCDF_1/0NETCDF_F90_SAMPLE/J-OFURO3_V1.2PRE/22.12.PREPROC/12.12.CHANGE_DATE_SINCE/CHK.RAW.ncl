begin
YYYY=2007; 1988; 2017
M=7
MM=sprinti("%0.2i", M)
idx=M-1

MMM=(/"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep", \
                    "Oct","Nov","Dec"/)
DSET="JOFURO3"
ABB="LHF"
FLXDIR="+ UPWARD"
;VAR=""
UNIT="W/m2"
YMIN=-180.
YMAX=180.

lonw = 115.
lone = 145.
lats = 15.
latn = 50.

prefix=DSET+".BUGFIX."+ABB+".MAP."
FIG=prefix+YYYY+"."+MM
TYP="png"

indir="/work01/DATA/JOFURO3/J-OFURO3/V1.1/MONTHLY/HR/LHF.BUG.FIX.DAYS.SINCE/"
;indir="/work01/DATA/"+DSET+"/J-OFURO3/V1.1/MONTHLY/HR/LHF"
infle="J-OFURO3_LHF_V1.1_MONTHLY_HR_"+YYYY+".nc"

f=indir+"/"+infle

a=addfile(f,"r")
;print(a)


print("READ DATA")
xMON=a->LHF
;xMON=-xMON; POS UPWARD

lon=a->longitude
lat=a->latitude

rad    = 4.0*atan(1.0)/180.0
wgty=cos(lat*rad)
wgty!0="latitude"
wgty&latitude=lat


TIME=a->time
printVarSummary(TIME)

xMON!0="time"
xMON!1="lat"
xMON!2="lon"
xMON&time=TIME
xMON&lat=lat
xMON&lon=lon
printVarSummary(xMON)



print("### PLOT")

wks = gsn_open_wks(TYP, FIG)

gsn_define_colormap(wks,"BlueDarkRed18") ;ncl_default")

opt=True

opt@gsnDraw       = False     
opt@gsnFrame      = False      

opt@gsnLeftString=DSET+" "+ABB
opt@gsnCenterString=""
opt@gsnRightString=MMM(M)+YYYY


print("### COLOR")
opt@gsnCenterStringOrthogonalPosF=0.05
opt@gsnLeftStringOrthogonalPosF=0.05
opt@gsnRightStringOrthogonalPosF=0.05
opt@gsnLeftStringFontHeightF=0.025
opt@gsnCenterStringFontHeightF=0.025
opt@gsnRightStringFontHeightF=0.025
opt@lbTitleFontHeightF= .015 ;Font size
opt@cnLevelSelectionMode = "ManualLevels"
opt@cnMinLevelValF = YMIN
opt@cnMaxLevelValF = YMAX
opt@cnLevelSpacingF = 30.
opt@pmLabelBarHeightF = 0.08



res=opt
res@cnFillOn     = True   ; turn on color fill
res@cnLinesOn    = False    ; turn off contour lines

res@gsnAddCyclic = False
res@mpGeophysicalLineThicknessF=4
res@mpGeophysicalLineColor="saddlebrown"

;res@mpProjection = "Mercator"
res@mpDataBaseVersion = "MediumRes" ;-- better map resolution
res@mpMinLonF = 105. ;-- min longitude
res@mpMaxLonF = 145. ;-- max longitude
res@mpMinLatF =  0. ;-- min latitude
res@mpMaxLatF =  50. ;-- max latitude

plot1=gsn_csm_contour_map_ce(wks,xMON(idx,{0:50},{100:145}),res)


draw(plot1)

;printVarSummary(xMON!lon)



txres=True
txres@txFontHeightF = 0.015
txres@txJust="CenterLeft"
today = systemfunc("date -R")
gsn_text_ndc(wks,today,  0.05,0.90,txres)
cwd =systemfunc("pwd")
gsn_text_ndc(wks,"Current dir: "+cwd,      0.05,0.925,txres)
scriptname  = get_script_name()
gsn_text_ndc(wks,"Script: "+scriptname, 0.05,0.950,txres)

frame(wks)

print("FIG="+FIG+"."+TYP)
end
