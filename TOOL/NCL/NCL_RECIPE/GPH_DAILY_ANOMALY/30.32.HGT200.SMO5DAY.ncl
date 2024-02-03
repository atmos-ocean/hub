begin

setfileoption("nc","Format","LargeFile")

VAR="hgt"
LEV=200
print("MMMMM HGT "+LEV)

INFLE="hgt_2018_"+LEV+"_DAILY.nc"
OFLE="hgt_2018_"+LEV+"_DAILY_SMO5DAY.nc"

a=addfile(INFLE,"r")
hgt=a->hgt
lon=a->lon
lat=a->lat
time= a->time                     
 
hgt&lon=lon
hgt&lat=lat
hgt&time=time

TIME    = cd_calendar(time, 0)          ; type float 
year    = toint( TIME(:,0) )            ; toint strips meta data
month   = toint( TIME(:,1) )
day     = toint( TIME(:,2) ) 
                                        ; check for calendar attribute
if (isatt(TIME,"calendar")) then        ; default is gregorian
  year@calendar = TIME@calendar       
end if

ddd = day_of_year(year, month, day) 
if (isatt(year,"calendar")) then        ; default is gregorian
  ddd@calendar = year@calendar  
end if

yyyyddd = year*1000 + ddd               ; needed for input
if (isatt(ddd,"calendar")) then         ; default is gregorian
  yyyyddd@calendar = ddd@calendar  
end if

run=5
print("MMMMM "+run+"-DAY RUNNING MEAN")
opt_runave_n=0
dim=0
print("MMMMM run="+run+" opt_runave_n="+opt_runave_n+" dim="+dim)
hgtSmo=runave_n(hgt, run, opt_runave_n, dim)
copy_VarCoords(hgt,hgtSmo)
printVarSummary(hgtSmo)   

system("rm -vf "+OFLE)
f=addfile(OFLE,"c")
f->hgtSmo=hgtSmo



print("MMMMM PLOT")

CLON=120 ;220 ;270.0       ; choose center lon
CLAT=45 ;45 

yearI=2018
monthI=07
dayI=23
doyI=day_of_year(yearI,monthI,dayI)-1
doyI@units="days since "+tostring(yearI)+"-1-1"

utc_date = cd_calendar(doyI, 0)
yearC   = tointeger(utc_date(:,0))    ; Convert to integer for
monthC  = tointeger(utc_date(:,1))    ; use sprinti 
dayC    = tointeger(utc_date(:,2))
hourC   = tointeger(utc_date(:,3))

date_str = sprinti("%0.4i", yearC)+"-"+sprinti("%0.2i", monthC)+"-"+sprinti("%0.2i", dayC)

print("PLOT DATE="+date_str) 

VOUT=hgtSmo(doyI,:,:)
VOUT!0="lat"
VOUT!1="lon"
VOUT&lat=lat
VOUT&lon=lon
printVarSummary(VOUT)

LONLAT=tostring(CLON)+"_"+tostring(CLAT)

FIG=VAR+"_Smoothd_Daily_"+LEV+"_"+date_str+"_"+LONLAT
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
res@tiMainString    = LEV+"hPa "+date_str+" Smoothed Daily"
res@gsnLeftString    = ""
res@gsnCenterString    = ""
res@gsnRightString    = ""

;sres@mpLandFillColor = 1
res@cnFillOn             = True
res@cnLevelSelectionMode = "ManualLevels"
res@cnMinLevelValF = 11500.; ;12330.
res@cnMaxLevelValF = 13000.; 12410.
res@cnLevelSpacingF =  100.
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



print("MMMMM OUTPUT: "+OFLE)
print("MMMMM FIG:    "+FIG)

delete([/INFLE,OFLE,a,f/])


end

