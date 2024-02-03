load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

external wrfdiv2 "./wrfdiv2.so"

begin

FAC=1.E4
FACI=1.0/FAC
CINT_CNV=30. ;
Z0 = 400.   ; m

lonw=130.
lone=132.
lats=33.
latn=34.

alonw=130.4
alone=131.2
alats=33.2
alatn=33.5

plx1=(/alonw, alone, alone ,alonw, alonw/)
ply1=(/alats, alats, alatn ,alatn, alats/)

wallClock1 = systemfunc("date") ; retrieve wall clock time

scriptname_in = getenv("NCL_ARG_1")
scriptname=systemfunc("basename " + scriptname_in + " .ncl")
nml    = getenv("NCL_ARG_2")

print("script name ="+scriptname_in)
print("namelist    ="+nml)
print("")

print("READ NAMELIST FILE")

runname=systemfunc("grep runname "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
indir=systemfunc("grep indir "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
domain=systemfunc("grep domain "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
wrfout=systemfunc("grep wrfout "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
sdatetime=systemfunc("grep sdatetime "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
edatetime=systemfunc("grep edatetime "+nml+ "|cut -f2 -d'='|cut -f1 -d','")

print("DONE.")
print("")



print("CHECK NAMELIST FILE")

print("runname: "+runname)
print("indir:   "+indir)
print("wrfout:")
print("sdatetime: "+sdatetime)
print("edatetime: "+edatetime)

print("DONE.")
print("")



print("CREATE OUTPUT DIRECTORY")

outdir="OUT_"+scriptname+"/"
print("outdir="+outdir)
command="mkdir -p " + outdir
system(command)

print("DONE.")
print("")



print("OPEN INPUT FILE")

infle=indir+"/"+wrfout
print(systemfunc("ls -lh "+infle))
a = addfile(infle,"r")
; printVarSummary(a)

print("DONE.")
print("")



print("CHECK TIME STEPS")
times=wrf_user_getvar(a,"times",-1)
;printVarSummary(times)

dim=dimsizes(times)
nt=dim(0)
;print(nt)
delete(dim)

do n=0,nt-1
if(times(n).eq.sdatetime)then
ns=n
print(ns+" "+times(n)+" "+sdatetime)
end if
if(times(n).eq.edatetime)then
ne=n
print(ne+" "+times(n)+" "+edatetime)
end if
end do ; n
NN=ne-ns+1
print(NN+"="+NN)



print("READ TIME INVARIANT DATA")
xlong=wrf_user_getvar(a,"XLONG",0)
xlat =wrf_user_getvar(a,"XLAT",0)

ter=wrf_user_getvar(a,"ter",0)
DX_IN=a@DX
DY_IN=a@DY

RAIN0 = wrf_user_getvar(a, "RAINNC",ns)
RAIN1 = wrf_user_getvar(a, "RAINNC",ne)
RAINR = RAIN1-RAIN0
RAINR@_FillValue=-999.

dim=dimsizes(RAINR)
;print(dim)
NY=dim(0)
NX=dim(1)

print(" DX_IN="+DX_IN+" DY_IN="+DY_IN)
print(" NX="+NX+" NY="+NY)


print("CREATE VAR FOR TIME AVERAGE")
CNV_T=new((/NN,NY,NX/),"double")


print("------------------------")
print(" PLOT FIGURE")
print("------------------------")

type="ps"

figdir=outdir
figfile=figdir+"/"+runname+"_"+tostring(toint(Z0))+"_"+times(ns)+"-"+times(ne)

wks = gsn_open_wks(type,figfile)

type="ps"

res = True
res@gsnAddCyclic = False
res@sfXArray = xlong
res@sfYArray = xlat
res@gsnDraw          = False                ; turn off draw and frame
res@gsnFrame         = False                ; b/c this is an overlay plot


opts = res

opts@mpMinLonF            = lonw
opts@mpMaxLonF            = lone
opts@mpMinLatF            = lats
opts@mpMaxLatF            = latn

opts@mpDataBaseVersion    = "HighRes" ;"MediumRes" ; fill land in gray and ocean in transparent
opts@mpFillOn = False; True
opts@mpFillColors = (/"background","transparent","gray","transparent"/)
opts@mpFillDrawOrder = "PreDraw"

opts@gsnMajorLatSpacing = 1              ; change maj lat tm spacing
opts@gsnMajorLonSpacing = 1              ; change maj lon tm spacing


print("=============================================")
print(" CONVERGENCE")
print(" FAC="+FAC)
print("")

n=0
do it=ns,ne

print(" "+it+" "+times(it))

U_IN  = wrf_user_getvar(a,"ua",it)   ; u on mass points
V_IN  = wrf_user_getvar(a,"va",it)   ; v on mass points
Z_IN  = wrf_user_getvar(a, "z",it)


; Horizontally interpolate to height coordinates ("Z")
;https://www2.mmm.ucar.edu/wrf/OnLineTutorial/Graphics/NCL/Examples/LEVELS_INTERP/wrf_Height2.ncl
U2 = wrf_user_intrp3d(U_IN,Z_IN,"h",Z0,0.,False)
V2 = wrf_user_intrp3d(V_IN,Z_IN,"h",Z0,0.,False)

MSFT_IN =wrf_user_getvar(a,"MAPFAC_M",it)


U=todouble(U2)
V=todouble(V2)

FVAL=U@_FillValue
;print("U@_FillValue="+U@_FillValue)
;print("V@_FillValue="+V@_FillValue)

MSFT=todouble(MSFT_IN)
DX=todouble(DX_IN)
DY=todouble(DY_IN)

DIV=new(dim,double)


wrfdiv2::DIVD2(DIV,U,V,MSFT,DX,DY,NX,NY,FVAL)
CNV=-DIV*FAC

CNV_T(n,:,:)=CNV
delete([/DIV,CNV/])

n=n+1
end do ; it

CNV_A= dim_max_n_Wrap(CNV_T,0)
CNV_A@_FillValue=-999.
sub= where((xlat.lt.alats.or.xlat.gt.alatn .or. \
           xlong.lt.alonw.or.xlong.gt.alone) .and. CNV_A.le.0.0, \
           CNV_A@_FillValue, CNV_A)
CNV_AA=avg(sub)

opts=res

opts@cnFillOn = True
opts@cnLinesOn  = False            ; turn off contour lines

opts@lbTitleString = "10^4 1/s"
opts@lbTitlePosition = "bottom"
opts@lbTitleFontHeightF = 0.015

opts@pmLabelBarOrthogonalPosF = 0.1
opts@pmLabelBarHeightF = 0.08

COLORMAP="precip2_17lev"
COLORMAP="StepSeq25"
COLORMAP="prcp_2"
gsn_define_colormap(wks,COLORMAP)
;gsn_reverse_colormap(wks)

opts@cnLevelSelectionMode = "ExplicitLevels"
;opts@cnLevels = (/2,4,6,8,10,12,14,16,18,20/)
opts@cnLevels = (/5,10,15,20,25,30,35,40,45,50,55,60/)

opts@tiMainString=runname
opts@gsnLeftString=times(ns)+"-"+times(ne)+" "+tostring(toint(Z0))+"m"
opts@gsnCenterString="" ; add titles
opts@gsnRightString="["+tostring(FACI)+" 1/s]" ; add titles

plot = gsn_csm_contour_map(wks,CNV_A,opts)

print("=============================================")



opts2=res
opts2@cnFillOn = False
opts2@cnLinesOn            = True
opts2@gsnLeftString=""
opts2@gsnRightString=""
opts2@cnLevelSelectionMode = "ExplicitLevels"
opts2@cnLevels = (/400.,1000./)
opts2@cnLineLabelsOn = False
opts2@cnInfoLabelOn = False
opts2@cnLineColor = "brown"
plot2=gsn_csm_contour(wks,ter,opts2)
overlay(plot,plot2)



opts3=res
opts3@cnFillOn = False
opts3@cnLinesOn            = True
opts3@gsnLeftString=""
opts3@gsnRightString=""
opts3@cnLevelSelectionMode = "ExplicitLevels"
opts3@cnLevels = (/100.,200.,300.,400.,500.,600.,700./)
opts3@cnLineLabelsOn = False
opts3@cnInfoLabelOn = False
opts3@cnLineColor = "purple"

plot3=gsn_csm_contour(wks,RAINR,opts3)
overlay(plot,plot3)

sub= where(xlat.lt.lats.or.xlat.gt.latn .or. \
           xlong.lt.lonw.or.xlong.gt.lone, \
           RAINR@_FillValue, RAINR)

av=avg(sub)

txres               = True                            
txres@txFontHeightF = 0.02             ; Set the font height
txres@txJust="CenterLeft"
text="P12="+tostring(av)+"mm"
print(" "+text)
temp = gsn_add_text(wks,plot,text,130.05, \
                                    33.97 ,txres)

text="CNV="+tostring(CNV_AA)+" ["+tostring(FACI)+" 1/s]"
print(" "+text)
temp = gsn_add_text(wks,plot,text,130.05, \
                                    33.90 ,txres)

lnres = True
lnres@gsLineColor = "blue"
ln = gsn_add_polyline(wks, plot3, plx1, ply1, lnres)



;HEADER
txres               = True
txres@txJust="CenterLeft"

txres@txFontHeightF = 0.01
txres@txJust="CenterLeft"
today = systemfunc("date -R")

xh=0.1
yh=0.9
dyh=0.02
gsn_text_ndc(wks,today,  xh,yh,txres)

cwd =systemfunc("pwd")
yh=yh-dyh
gsn_text_ndc(wks,"Current dir: "+cwd,      xh,yh,txres)
yh=yh-dyh
gsn_text_ndc(wks,"Script: "+scriptname_in, xh,yh,txres)

str1 = systemfunc("ls -lh --time-style=long-iso "+infle)
yh=yh-dyh
gsn_text_ndc(wks,str1,xh,yh,txres)
yh=yh-dyh
gsn_text_ndc(wks,"Output: "+figfile+"."+type, xh,yh,txres)
yh=yh-dyh
text=tostring(plx1(0))+"-"+tostring(plx1(1))+"_"+tostring(ply1(0))+"-"+tostring(ply1(2))
gsn_text_ndc(wks,text, xh,yh,txres)


draw(plot)
frame(wks)

print("")
print("OUTPUT: ")
print(systemfunc("ls -lh --time-style=long-iso " + figfile+"."+type))

print("------------------------")
print(" DONE PLOT FIGURE")
print("------------------------")


print("DONE.")
print("")



print("")
print("Input:  "+ infle)

print("")

end
