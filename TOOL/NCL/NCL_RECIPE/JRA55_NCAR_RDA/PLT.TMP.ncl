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

VAROUT="TMP"
TYP="png"
yyyymmddhh=2022061900
ymdh_str=tostring(yyyymmddhh)

indir    = "INDIR/"
LEV=850

infile = "anl_p125.011_tmp.2022060100_2022063018"

in     = addfile(indir+infile,"r")    

tmp    = in->TMP_GDS0_ISBL(0,{LEV},:,:)  

time=in->initial_time0_hours
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
end if
 
end do ;n


ODIR="FIG/"
system("mkdir -vp "+ODIR)
OFLE=VAROUT+"_"+LEV+"_"+ymdh_str
wks  = gsn_open_wks(TYP,ODIR+OFLE)

res  = True
res@gsnFrame = False
res@gsnDraw =  False
res@cnFillOn         = True             
res@cnLinesOn        = True             
res@cnInfoLabelOn    = False            
res@cnLineLabelsOn   = False            
res@cnLineThicknessF = 0.8              
res@cnLineColor      = "white"          
res@cnFillPalette    = "NCV_bright"     
res@pmLabelBarWidthF   = 0.85           
res@lbLabelFontHeightF = 0.016          
res@pmLabelBarOrthogonalPosF = 0.2      
res@lbBoxEndCapStyle   = "TriangleBothEnds"  
res@lbTitleOn          = True                
res@lbTitleString      = tmp@units           
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
res@cnLevelSelectionMode = "ManualLevels"  
res@cnLevelSpacingF      = 3               
res@cnMaxLevelValF       = 303             
res@cnMinLevelValF       = 243             
plot = gsn_csm_contour_map_ce(wks,tmp,res) 
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
