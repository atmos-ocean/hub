; 
; PLT.TMP.ncl
; 
; Wed, 20 Jul 2022 13:40:20 +0900
;
script_name  = get_script_name()
script=systemfunc("basename "+script_name+ " .ncl")
;LOG=script+".LOG"

;NOW=systemfunc("date '+%y%m%d_%H%M' ")
NOW=systemfunc("date -R")
HOST=systemfunc("hostname")
CWD=systemfunc("pwd")

print("script="+script_name)

VAROUT="UV"
TYP="eps"
yyyymmddhh=2022061900
ymdh_str=tostring(yyyymmddhh)

INDIR    = "INDIR/"
LEV=850

INFLE1 = "anl_p125.033_ugrd.2022060100_2022063018"
INFLE2 = "anl_p125.034_vgrd.2022060100_2022063018"

IN1    = addfile(INDIR+INFLE1,"r")    
IN2    = addfile(INDIR+INFLE2,"r")    

time=IN1->initial_time0_hours
utc_date=cd_calendar(time, 0)
year  = tointeger(utc_date(:,0))
month = tointeger(utc_date(:,1))
day   = tointeger(utc_date(:,2))
hour  = tointeger(utc_date(:,3))
dim=dimsizes(year) 

do n=0,dim-1
date_str = sprinti("%0.4i", year)+sprinti("%0.2i",month)+\
sprinti("%0.2i",day)+sprinti("%0.2i",hour)

if ( date_str(n) .eq. ymdh_str) then
print(date_str(n) +" "+ ymdh_str)
NDX=n
end if
end do ;n

U    = IN1->UGRD_GDS0_ISBL(NDX,{LEV},:,:)  
V    = IN2->VGRD_GDS0_ISBL(NDX,{LEV},:,:)  

ODIR="FIG/"
system("mkdir -vp "+ODIR)
OFLE=VAROUT+"_"+LEV+"_"+ymdh_str
wks  = gsn_open_wks(TYP,ODIR+OFLE)

res  = True
res@gsnFrame = False
res@gsnDraw =  False

res@vcMinDistanceF = 0.02
res@vcRefMagnitudeF = 50.0
res@vcRefAnnoString1       = "$RFM$"+"m/s"
res@vcGlyphStyle = "FillArrow"
res@vcLineArrowThicknessF   = 2
res@vcRefLengthF    = 0.04 
res@vcRefAnnoArrowAngleF      = 0.0        
res@vcRefAnnoSide             = "Bottom"   
res@vcRefAnnoJust             = "TopRight" 
res@vcRefAnnoOrthogonalPosF   = 0.15
res@vcRefAnnoSide = "Bottom"
res@vcRefAnnoParallelPosF     = 1.0 
res@vcRefAnnoSide = "Bottom" 
res@vcRefAnnoPerimOn          = False
res@vcMonoLineArrowColor     = False
res@vcMonoFillArrowFillColor = False
res@vcLevelPalette           = "BlAqGrYeOrRe"

res@pmLabelBarWidthF   = 0.85           
res@pmLabelBarOrthogonalPosF = 0.2      

res@lbLabelFontHeightF = 0.016          
res@lbBoxEndCapStyle   = "TriangleBothEnds"  
res@lbTitleOn          = True                
res@lbTitleString      = ""
res@lbTitleFontHeightF = 0.018               
res@lbTitlePosition    = "Right"             
res@lbTitleDirection   = "Across"            

res@tmBorderThicknessF     = 1.5          
res@tmXBMajorThicknessF    = 1.5          
res@tmYLMajorThicknessF    = 1.5          
res@mpGeophysicalLineColor = "gray20"     
res@mpGeophysicalLineThicknessF = 1.5     

res@pmTickMarkDisplayMode    = "Always"   
res@tmXBLabelFontHeightF     = 0.018      
res@tmYLLabelFontHeightF     = 0.018      
res@gsnLeftStringFontHeightF = 0.03       
res@gsnLeftString  = VAROUT+" "+LEV+" "+yyyymmddhh
res@gsnRightString = ""                             
plot = gsn_csm_vector_map_ce(wks, U, V, res)
draw(plot)

txres=True

txres@txJust="CenterLeft"
txres@txFontHeightF = 0.015
gsn_text_ndc(wks,"Current dir: "+CWD,   0.05,0.875,txres)
gsn_text_ndc(wks,"Script: "+script_name,0.05,0.850,txres)
gsn_text_ndc(wks, NOW,                  0.05,0.825,txres)
gsn_text_ndc(wks,"FIG: "+ODIR+OFLE,     0.05,0.800,txres)

frame(wks)

print("")
print("OFLE: " + OFLE+"."+TYP) 
print("")
print("")
