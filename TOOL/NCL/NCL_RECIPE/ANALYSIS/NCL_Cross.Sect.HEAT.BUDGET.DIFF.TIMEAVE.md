Cross.Sect.HEAT.BUDGET.DIFF.TIMEAVE_2020-02-12
==============================
  
Wed, 12 Feb 2020 17:37:11 +0900
localhost
/work2/am/WRF.POST/PL1101/NCL/Cross.Sect.HEAT.BUDGET.DIFF.TIMEAVE_2020-02-12
  
**Machine info**
processor	: 31
model name	:        Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:     132051292 kB
  
**List of the following files:**
- Cross.Sect.HEAT.BUDGET.DIFF.TIMEAVE.sh
- Cross.Sect.HEAT.BUDGET.DIFF.TIMEAVE.ncl
- runncl.sh
  
## Cross.Sect.HEAT.BUDGET.DIFF.TIMEAVE.sh
```
#!/bin/sh

exe=./runncl.sh

ncl=$(basename $0 .sh).ncl

if [ ! -f $ncl ]; then
  echo Error in $0 : No such file, $ncl
  exit 1
fi

indir="/work1/am/WRF/WRF.Result"
outdir="FIG_$(basename $0 .sh)"

runname1="PL1101_03.06.02"
datetime1="2011-01-20_00:00:00"
runname2="PL1101_03.06.02_NoEv_01-22"
datetime2="2011-01-22_00:00:00"
domain="d01"
outtimes="2011-01-22_01:00:00" #OUTPUT TIME
outtimee="2011-01-23_00:00:00" #OUTPUT TIME


nstep=1
level=900. ;975.

#2011-01-23_00:00:00
csctype="zonal" #"meridional"
lon_start=31.5
lat_start=76.5
lon_end=50.
lat_end=77.5

#2011-01-23_00:00:00
#csctype="zonal" #"meridional"
#lon_start=32.
#lat_start=77
#lon_end=50.
#lat_end=77

#Cross section
cnintcsc=10.  #Contour interval
csmincsc=-30. #Min. of color shade
csmaxcsc=30.  #Max.
csintcsc=10. #Int. of

mkdir -vp $outdir
dirs="$indir"
for dir in $dirs; do
if [ ! -d $dir ]; then
  echo Error in $0 : No such directory, $dir
  exit 1
fi
done



ulimit -s unlimited
echo
echo ulimit -s unlimited
echo



nml=$(basename $0 .sh).namelist.txt
cat <<EOF>$nml
&para
runname1  =${runname1}
datetime1 =${datetime1}
runname2  =${runname2}
datetime2 =${datetime2}
domain   =${domain}
indir    =${indir}
outdir   =${outdir}
outtimes =${outtimes}
outtimee =${outtimee}
nstep    =${nstep}
level    =${level}
csctype   =${csctype}
lon_start =${lon_start}
lat_start =${lat_start}
lon_end   =${lon_end}
lat_end   =${lat_end}
cnintcsc  =${cnintcsc} #Contour interval
csmincsc  =${csmincsc} #Min. of color shade
csmaxcsc  =${csmaxcsc} #Max.
csintcsc  =${csintcsc} #Int. of
&end
EOF



export LANG=C
$exe $ncl "$nml"
if [ $? -ne 0 ]; then
  echo
  echo "ERROR in $0 : ERROR while running $exe $ncl $nml"
  echo
  exit 1
fi

exit 0
```

## Cross.Sect.HEAT.BUDGET.DIFF.TIMEAVE.ncl
```
begin

wallClock1 = systemfunc("date") ; retrieve wall clock time

script = getenv("NCL_ARG_1")
nml    = getenv("NCL_ARG_2")
print("script name ="+script)
print("namelist    ="+nml)
print("")

; READ NAMELIST FILE
runname1  =systemfunc("grep runname1 " +nml+ "|cut -f2 -d'='|cut -f1 -d','")
datetime1 =systemfunc("grep datetime1 "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
runname2  =systemfunc("grep runname2 " +nml+ "|cut -f2 -d'='|cut -f1 -d','")
datetime2 =systemfunc("grep datetime2 "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
domain   =systemfunc("grep domain "  +nml+ "|cut -f2 -d'='|cut -f1 -d','")
indir    =systemfunc("grep indir "   +nml+ "|cut -f2 -d'='|cut -f1 -d','")
outdir   =systemfunc("grep outdir "  +nml+ "|cut -f2 -d'='|cut -f1 -d','")
outtimes =systemfunc("grep outtimes "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
outtimee =systemfunc("grep outtimee "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
nstep=toint(systemfunc("grep nstep "+nml+ "|cut -f2 -d'='|cut -f1 -d','"))
level    =tofloat(systemfunc("grep level "   +nml+ "|cut -f2 -d'='|cut -f1 -d','"))
csctype  =systemfunc("grep csctype " +nml+ "|cut -f2 -d'='|cut -f1 -d','")
lon_start=tofloat(systemfunc("grep lon_start "   +nml+ "|cut -f2 -d'='|cut -f1 -d','"))
lat_start=tofloat(systemfunc("grep lat_start "   +nml+ "|cut -f2 -d'='|cut -f1 -d','"))
lon_end  =tofloat(systemfunc("grep lon_end "   +nml+ "|cut -f2 -d'='|cut -f1 -d','"))
lat_end  =tofloat(systemfunc("grep lat_end "   +nml+ "|cut -f2 -d'='|cut -f1 -d','"))
cnintcsc =tofloat(systemfunc("grep cnintcsc "   +nml+ "|cut -f2 -d'='|cut -f1 -d','"))
csmincsc =tofloat(systemfunc("grep csmincsc "   +nml+ "|cut -f2 -d'='|cut -f1 -d','"))
csmaxcsc =tofloat(systemfunc("grep csmaxcsc "   +nml+ "|cut -f2 -d'='|cut -f1 -d','"))
csintcsc =tofloat(systemfunc("grep csintcsc "   +nml+ "|cut -f2 -d'='|cut -f1 -d','"))

outtime=outtimes

scriptname=systemfunc("basename "+script + " .ncl")

outdir= outdir + "/" + runname1 +"-"+runname2 + "/"
print("")
print("Creating directory, " + outdir)
command="mkdir -p " + outdir
system(command)

varlist=(/"TH_MYTEND","TH_MYTEND_ADV","RTHRATLW","RTHRATSW","RTHBLTEN",\
         "H_DIABATIC","RTHCUTEN"/)
abblist=(/"TND",      "ADV",          "LWR",     "SWR",     "PBL",     \
         "LCH",       "CUH"/)
cuplist=(/ 1,         1,               0,         0,         1,        \
           0,         1/)

dimvl=dimsizes(varlist)
nvar=dimvl(0)
print("")
print("VARIABLE LIST: ")
do n=0,nvar-1
print(varlist(n)+" "+abblist(n)+" "+cuplist(n))
end do ;n
print("")

d2r=3.14159265358979/180.0  ; degree to radian 
rearth=6378.137*1000.0           ; radius of the earth

figtype="eps"



infile1=indir+"/"+runname1+"/WRF_"+runname1+"/wrfout_"+domain+"_"+datetime1+".nc"
infile2=indir+"/"+runname2+"/WRF_"+runname2+"/wrfout_"+domain+"_"+datetime2+".nc"



a1 = addfile(infile1,"r")
a2 = addfile(infile2,"r")



it=0
oro = wrf_user_getvar(a1,"XLAND",it)
lat = wrf_user_getvar(a1,"XLAT",it)   ; latitude
lon = wrf_user_getvar(a1,"XLONG",it)  ; longitude
dim=dimsizes(oro)
jm=dim(0)
im=dim(1)

dx = a1@DX       ; m
dy = a1@DY       ; m



;SET CROSS SECTION

dimll  = dimsizes(lat)                ; get size of dimensions
nlat   = dimll(0)
mlon   = dimll(1)

; Plotting options for the longitude-pressure sections
llres           = True
llres@ReturnInt = True
;START
locij_a         = wrf_user_ll_to_ij(a1, lon_start, lat_start, llres)
locij_a         = locij_a - 1 ; To convert to NCL subscripts
locX_a          = locij_a(0)
locY_a          = locij_a(1)

;END
locij_b     = wrf_user_ll_to_ij(a1, lon_end, lat_end, llres)
locij_b     = locij_b - 1
locX_b      = locij_b(0)
locY_b      = locij_b(1)

plx1=(/lon_start, lon_end/)
ply1=(/lat_start, lat_end/)

; The requested and calculated values should be close
print("")
print("lon_start="+lon_start+" lon(locY_a,locX_a)="+\
lon(locY_a,locX_a))
print("lon_end=  "+lon_end+" lon(locY_b,locX_b)="+\
lon(locY_b,locX_b))
print("")
print("lat_start="+lat_start+" lat(locY_a,locX_a)="+\
lat(locY_a,locX_a))
print("lat_end=  "+lat_end+" lat(locY_b,locX_b)="+\
lat(locY_b,locX_b))

plane   = new(4,float)
plane = (/ locX_a,locY_a , locX_b,locY_b /)

angle   = 0.

;
; SET TRANSECT
;
if(csctype .eq. "meridional")then
x_plane  = wrf_user_intrp2d(lat,plane,angle,True)
x_plane@description	= "Latitude"
x_axis_unit=" (N)"
y_plane  = wrf_user_intrp2d(lon,plane,angle,True)
else
x_plane  = wrf_user_intrp2d(lon,plane,angle,True)
x_plane@description	= "Longitude"
x_axis_unit=" (E)"
y_plane  = wrf_user_intrp2d(lat,plane,angle,True)
end if

dimsX	   = dimsizes(x_plane)
xmin	   = x_plane(0)
xmax	   = x_plane(dimsX(0)-1)
xspan	   = dimsX(0)-1
nx	   = 5 ;floattoint((xmax-xmin)/2+1)



;SET UP VAR FOR AVE
it=0
n=0
var3din=wrf_user_getvar(a1,varlist(n),it)
p=wrf_user_getvar(a1,"pressure",it)
var2d=wrf_user_intrp3d(var3din, p, "h", level, 0., False)
dim2d=dimsizes(var2d)
jm2d=dim2d(0)
im2d=dim2d(1)
var2dsum=new((/jm2d,im2d/),typeof(var2d))
var2dsum=0.0
var2dave=var2dsum

var2dF2sum=var2dsum
var2dF2ave=var2dsum

printVarSummary(var2dsum)

varcs=wrf_user_intrp3d(var3din, p, "v", plane, 0., True)
dimcs=dimsizes(varcs)
kmcs=dimcs(0)
imcs=dimcs(1)
varcssum=new((/kmcs,imcs/),typeof(varcs))
varcssum=0.0
varcsave=varcssum

varcsF2sum=varcssum
varcsF2ave=varcssum

printVarSummary(varcssum)

delete([/var3din,var2d,varcs,dim2d,dimcs/])

; RESIDUAL TERM
res2dave=var2dave
rescsave=varcsave
res2dF2ave=var2dF2ave
rescsF2ave=varcsF2ave



; BASIC PLOT OPTIONS
opts=True
;Coordinates and domain
opts@gsnAddCyclic = False
opts@sfXArray = lon
opts@sfYArray = lat

;Plotting area
opts@vpHeightF = .27 ;1
opts@vpWidthF  = opts@vpHeightF*tofloat(mlon)/tofloat(nlat) ;.4
opts@vpYF      = .85 ;1.25

;Page control
opts@gsnFrame = False
opts@gsnDraw =  False
opts@gsnStringFontHeightF = 0.012
opts@lbLabelFontHeightF   = 0.012

; Contour line
opts_cn=opts
opts_cn@cnFillOn = False
opts_cn@cnLevelSpacingF = 1
opts_cn@cnLineThicknessF= 1.0
opts_cn@cnInfoLabelOn = False
opts_cn@cnLabelDrawOrder="PostDraw"
opts_cn@cnLineLabelFontHeightF   = 0.008

; Color shade
opts_cs=opts
opts_cs@cnFillOn = True
opts_cs@pmLabelBarOrthogonalPosF = 0.0
opts_cs@pmLabelBarHeightF = 0.04

; Color bar
opts_cs@pmLabelBarOrthogonalPosF = .10          ; move whole thing down
opts_cs@lbTitlePosition  = "Right"              ; title position
opts_cs@lbTitleFontHeightF= .012                ; make title smaller
opts_cs@lbTitleDirection = "Across"             ; title direction

; MAP
opts_cs@mpProjection          = "Stereographic"
opts_cs@mpLimitMode           = "Corners"
opts_cs@mpLeftCornerLatF      = lat(0,0)
opts_cs@mpLeftCornerLonF      = lon(0,0)
opts_cs@mpRightCornerLatF     = lat(nlat-1,mlon-1)
opts_cs@mpRightCornerLonF     = lon(nlat-1,mlon-1)
opts_cs@mpCenterLonF          = a1@CEN_LON
opts_cs@mpCenterLatF          = a1@CEN_LAT
opts_cs@mpCenterLatF          = 90.         ; This is necessary to fix the wrong value on the WRF file.
opts_cs@mpDataBaseVersion = "HighRes" ; fill land in gray and ocean in transparent
opts_cs@mpAreaMaskingOn             = "True"
opts_cs@mpDataBaseVersion = "HighRes"
opts_cs@mpFillOn = False ; True
opts_cs@mpGridAndLimbOn   = True
opts_cs@mpGridLonSpacingF = 20
opts_cs@mpGridLatSpacingF = 5
opts_cs@mpPerimDrawOrder = "PostDraw"
opts_cs@mpPerimLineThicknessF=2

;CICLES OF LONGITUDE AND LATITUDE
;---Create lat/lon arrays for location of latitude labels.
lats1 = ispan(15,90,5)
nlat1 = dimsizes(lats1)
lons1 = new(nlat1,integer)
lons1 = 62;-2
;---Resources for text strings
txres1               = True
txres1@txFontHeightF = 0.008
txres1@txPerimSpaceF=0.1
txres1@txBackgroundFillColor="white"

;---Create lat/lon arrays for location of longitude labels.
lons2 = ispan(-180,180,20)
nlon2 = dimsizes(lons2)
lats2 = new(nlon2,float)
lats2 = 82.5;-2
;---Resources for text strings
txres2               = True
txres2@txFontHeightF = 0.008
txres2@txPerimSpaceF=0.1
txres2@txBackgroundFillColor="white"



print("")

;FIND OUTPUT TIME STEP
;FILE 1
times = wrf_user_getvar(a1,"times",-1)  ; get all times in the file
ntimes = dimsizes(times)         ; number of times in the file

do n=0,ntimes-1

if(times(n) .eq. outtimes)then ;NSTART
nstart=n
end if ; NSTART

if(times(n) .eq. outtimee)then ;NEND
nend=n
end if ; NEND

end do; n
delete([/ntimes/])

;FILE 2
times2 = wrf_user_getvar(a2,"times",-1)  ; get all times in the file
ntimes = dimsizes(times2)         ; number of times in the file

do n=0,ntimes-1

if(times2(n) .eq. outtimes)then ;NSTART
nstart2=n
end if ; NSTART

if(times2(n) .eq. outtimee)then ;NEND
nend2=n
end if ; NEND

end do; n
delete([/ntimes/])


;=========================================
;LOOP FOR OUTPUT TERMS
;=========================================
do n=0,nvar-1 ;LOOP FOR OUTPUT TERMS

templete= outdir +abblist(n) + runname1 + "_" + runname2+ "." +domain
figfile = templete + "_" + times(nstart)+"-"+times(nend)

wks = gsn_open_wks(figtype, figfile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FILE1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------
;TIME LOOP
;----------------
do it=nstart,nend ;LOOP OVER TIME

print(abblist(n)+" FILE1: domain="+domain+". Working on time: " + times(it))

psfc=wrf_user_getvar(a1,"slp",it)
dp=psfc
delete(psfc@description) 
delete(psfc@units) 
p=wrf_user_getvar(a1,"pressure",it)

MU      = wrf_user_getvar(a1, "MU"   , it)
MUB     = wrf_user_getvar(a1, "MUB"  , it)
MU_d=MU+MUB
delete([/MU,MUB/])
var3din=wrf_user_getvar(a1,varlist(n),it)

var3d=var3din

if(cuplist(n).eq.1)then ;DECOUPLE 
dimua=dimsizes(var3d)

do k=0,dimua(0)-1
var3d(k,:,:)=var3din(k,:,:)/MU_d(:,:) 
end do ;k

end if ;DECOUPLE

;CHANGE UNIT
var3d=var3d*86400. ;K/d

delete(var3din)

; INTERPOLATION
var2d=wrf_user_intrp3d(var3d, p, "h", level, 0., False)
varcs=wrf_user_intrp3d(var3d, p, "v", plane, 0., True)

var2dsum(:,:)=var2dsum(:,:)+var2d(:,:)
varcssum(:,:)=varcssum(:,:)+varcs(:,:)

delete([/var3d,var2d,varcs/])

end do ;it TIME LOOP
;----------------
; TIME LOOP END
;----------------

rnt=1.0/tofloat(nend-nstart+1)
var2dave(:,:)=smth9(var2dsum(:,:)*rnt,0.5,0.25,False)
varcsave(:,:)=smth9(varcssum(:,:)*rnt,0.5,0.25,False)

varcssum=0.0
var2dsum=0.0

if(abblist(n).eq."TND")then
res2dave=var2dave
rescsave=varcsave
else
res2dave=res2dave-var2dave
rescsave=rescsave-varcsave
end if

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FILE2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;----------------
;TIME LOOP
;----------------
do it=nstart2,nend2 ;LOOP OVER TIME
print(abblist(n)+" FILE2: domain="+domain+". Working on time: " + times2(it))

psfc2=wrf_user_getvar(a2,"slp",it)
dp=psfc
delete(psfc2@description) 
delete(psfc@units) 
p=wrf_user_getvar(a2,"pressure",it)

MU      = wrf_user_getvar(a2, "MU"   , it)
MUB     = wrf_user_getvar(a2, "MUB"  , it)
MU_d=MU+MUB
delete([/MU,MUB/])
var3din=wrf_user_getvar(a2,varlist(n),it)

var3d=var3din

if(cuplist(n).eq.1)then ;DECOUPLE 
dimua=dimsizes(var3d)

do k=0,dimua(0)-1
var3d(k,:,:)=var3din(k,:,:)/MU_d(:,:) 
end do ;k

end if ;DECOUPLE

;CHANGE UNIT
var3d=var3d*86400. ;K/d

delete(var3din)

; INTERPOLATION
var2d=wrf_user_intrp3d(var3d, p, "h", level, 0., False)
varcs=wrf_user_intrp3d(var3d, p, "v", plane, 0., True)

var2dF2sum(:,:)=var2dF2sum(:,:)+var2d(:,:)
varcsF2sum(:,:)=varcsF2sum(:,:)+varcs(:,:)

delete([/var3d,var2d,varcs/])

end do ;it TIME LOOP
;----------------
;TIME LOOP END
;----------------

rnt=1.0/tofloat(nend2-nstart2+1)
var2dF2ave(:,:)=smth9(var2dF2sum(:,:)*rnt,0.5,0.25,False)
varcsF2ave(:,:)=smth9(varcsF2sum(:,:)*rnt,0.5,0.25,False)

varcsF2sum=0.0
var2dF2sum=0.0


if(abblist(n).eq."TND")then
res2dF2ave=var2dF2ave
rescsF2ave=varcsF2ave
else
res2dF2ave=res2dF2ave-var2dF2ave
rescsF2ave=rescsF2ave-varcsF2ave
end if

;DIFF
var2dDave=var2dF2ave - var2dave
varcsDave=varcsF2ave - varcsave

var2dDave(0:3,:)=0.0
var2dDave(jm-3:jm-1,:)=0.0
var2dDave(:,0:3)=0.0
var2dDave(:,im-3:im-1)=0.0



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PLOT FILE1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
res1=opts_cs
res1@vpXF = 0.1
res1@gsnLeftString   = abblist(n)+" [K/d] "+level+" [hPa]"
res1@gsnCenterString = ""
res1@gsnRightString  = "SLP [hPa]"
res1@cnLinesOn = False
res1@cnLineColor          = "white"
res1@cnLineThicknessF = 0.5
res1@cnLevelSelectionMode = "ManualLevels"
;res1@cnMinLevelValF  = 0.1
;res1@cnMaxLevelValF  = 0.6	
;res1@cnLevelSpacingF = 0.1
res1@cnLevelSelectionMode = "ExplicitLevels" ;
res1@cnLevels    = (/-15.0,-10.0,-5.0,-0.5, 0.5, 5.0,10.0,15.0/)
res1@lbLabelStride = 1

gsn_define_colormap(wks,"CBR_coldhot")

plot1=gsn_csm_contour_map(wks,var2dave,res1)

; SLP
res6=opts_cn
res6@cnLineColor          = "white"
res6@cnLineLabelsOn = False
res6@cnLineLabelsOn = False
res6@cnMinLevelValF  = 950
res6@cnMaxLevelValF  = 1040
res6@cnLevelSpacingF = 4
res6@cnLineThicknessF = 3
plotp1=gsn_csm_contour(wks,psfc,res6)
overlay(plot1,plotp1)

res7=opts_cn
res7@cnLineColor          = "black"
res7@cnLineLabelFontColor = "black"
res7@cnMinLevelValF  = 950
res7@cnMaxLevelValF  = 1040
res7@cnLevelSpacingF = 4
res7@cnLineThicknessF = 1

plotp2=gsn_csm_contour(wks,psfc,res7)
overlay(plot1,plotp2)

;LINE
lnres               = True
lnres@gsLineThicknessF=3
lnres@gsLineColor = "black"
if(csctype .eq. "meridional")then
ln = gsn_add_polyline(wks,plot1,y_plane,x_plane,lnres)
else
ln = gsn_add_polyline(wks,plot1,x_plane,y_plane,lnres)
end if

dum1 = gsn_add_text(wks,plot1,""+lats1,lons1,lats1,txres1)
dum2 = gsn_add_text(wks,plot1,""+lons2,lons2,lats2,txres2)
draw(plot1)
delete(plot1)
delete([/dum1,dum2/])



;CROSS SECTION
; CREATE NICE LEVELS FOR CROSS SECTION
zmax = 300.
zz   = wrf_user_intrp3d(p, p, "v",plane,0.,opts)
zmin = 1000.
nice_levs   = floor((zmin - zmax) / 50) * 50
zmax        = zmin - nice_levs
dims        = dimsizes(zz)
zmax_pos    = dims(0) - 1

do imax = 1, dims(0) - 1
  if(.not.ismissing(zz(imax,0)) .and. zz(imax,0) .ge. zmax) then
     zmax_pos    = imax
  end if
end do

zspan       = zmax_pos
zmax        = zz(zmax_pos,0)
nz          = floattoint((zmin - zmax) / 100 + 1)

opts_xy                = True
opts_xy@gsnDraw          = False         ; don't draw
opts_xy@gsnFrame         = False         ; don't advance frame
opts_xy@vpXF = 0.1
opts_xy@vpYF      = .50
opts_xy@vpHeightF = .30
opts_xy@vpWidthF  = opts@vpWidthF
opts_xy@gsnLeftString   = ""               ; add the gsn titles

lonStmp=lon(locY_a,locX_a)
if(abs(lonStmp) .lt. 100) then
lonS=sprintf("%4.1f",lon(locY_a,locX_a))
else
lonS=sprintf("%5.1f",lon(locY_a,locX_a))
end if

latS=sprintf("%4.1f",lat(locY_a,locX_a))

lonEtmp=lon(locY_b,locX_b)
if(abs(lonEtmp) .lt. 100)then
lonE=sprintf("%4.1f",lon(locY_b,locX_b))
else
lonE=sprintf("%5.1f",lon(locY_a,locX_a))
end if

latE=sprintf("%4.1f",lat(locY_b,locX_b))

opts_xy@gsnCenterString = ""
opts_xy@gsnRightString  = ""
opts_xy@cnInfoLabelOn = False 

opts_xy@trYLog         = False ;True
opts_xy@tiYAxisString = "log"
opts_xy@trYMinF        = 1
opts_xy@tiXAxisString  = x_plane@description + x_axis_unit
opts_xy@tiXAxisOffsetYF = 0.07
opts_xy@tiYAxisString  = "P [hPa]"
opts_xy@tmYLMode       = "Explicit"
opts_xy@tmYLValues     = fspan(1,zspan,nz)
opts_xy@tmYLLabels     = sprintf("%.0f",fspan(zmin,zmax,nz))
opts_xy@tmXBMode       = "Explicit"
opts_xy@tmXBValues     = fspan(0,xspan,nx) 
opts_xy@tmXBLabels     = sprintf("%.2f",fspan(xmin,xmax,nx))
opts_xy@gsnPaperOrientation = "landscape"

gsn_define_colormap(wks,"CBR_coldhot")

opts_z                       = opts_xy
opts_z@cnFillOn=True
opts_z@cnLevelSelectionMode = "ExplicitLevels" ;
opts_z@cnLevels    = (/-15.0,-10.0,-5.0,-0.5, 0.5, 5.0,10.0,15.0/)
opts_z@lbLabelStride = 1
;opts_z@cnLevelSelectionMode = "ManualLevels"
;opts_z@cnMinLevelValF  = csmincsc
;opts_z@cnMaxLevelValF  = csmaxcsc
;opts_z@cnLevelSpacingF = csintcsc

opts_z@cnLineColor           = "white"
opts_z@pmLabelBarOrthogonalPosF = .10; Move whole thing down
opts_z@pmLabelBarHeightF = 0.04

contourd = gsn_csm_contour(wks,varcsave(0:zmax_pos,:),opts_z)
draw(contourd)
delete(contourd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PLOT FILE2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
res1=opts_cs
res1@vpXF = 0.6
opts@vpYF = 0.85 ;1.25
res1@gsnLeftString   = abblist(n)+" [K/d] "+level+" [hPa]"
res1@gsnCenterString = ""
res1@gsnRightString  = "SLP [hPa]"
res1@cnLinesOn = False
res1@cnLineColor          = "white"
res1@cnLineThicknessF = 0.5
res1@cnLevelSelectionMode = "ManualLevels"
;res1@cnMinLevelValF  = 0.1
;res1@cnMaxLevelValF  = 0.6	
;res1@cnLevelSpacingF = 0.1
res1@cnLevelSelectionMode = "ExplicitLevels" ;
res1@cnLevels  = (/-3.0,-2.0,-1.0,-0.5, 0.5, 1.0,2.0,3.0/)

res1@lbLabelStride = 1

gsn_define_colormap(wks,"CBR_coldhot")

plot1=gsn_csm_contour_map(wks,smth9(var2dDave,0.5,0.25,False),res1)

; SLP
res6=opts_cn
res6@cnLineColor          = "white"
res6@cnLineLabelsOn = False
res6@cnLineLabelsOn = False
res6@cnMinLevelValF  = 950
res6@cnMaxLevelValF  = 1040
res6@cnLevelSpacingF = 4
res6@cnLineThicknessF = 3
plotp1=gsn_csm_contour(wks,psfc2,res6)
overlay(plot1,plotp1)

res7=opts_cn
res7@cnLineColor          = "black"
res7@cnLineLabelFontColor = "black"
res7@cnMinLevelValF  = 950
res7@cnMaxLevelValF  = 1040
res7@cnLevelSpacingF = 4
res7@cnLineThicknessF = 1

plotp2=gsn_csm_contour(wks,psfc2,res7)
overlay(plot1,plotp2)

;LINE
lnres               = True
lnres@gsLineThicknessF=3
lnres@gsLineColor = "black"
if(csctype .eq. "meridional")then
ln = gsn_add_polyline(wks,plot1,y_plane,x_plane,lnres)
else
ln = gsn_add_polyline(wks,plot1,x_plane,y_plane,lnres)
end if

dum1 = gsn_add_text(wks,plot1,""+lats1,lons1,lats1,txres1)
dum2 = gsn_add_text(wks,plot1,""+lons2,lons2,lats2,txres2)
draw(plot1)
delete(plot1)
delete([/dum1,dum2/])




;CROSS SECTION
; CREATE NICE LEVELS FOR CROSS SECTION
zmax = 300.
zz   = wrf_user_intrp3d(p, p, "v",plane,0.,opts)
zmin = 1000.
nice_levs   = floor((zmin - zmax) / 50) * 50
zmax        = zmin - nice_levs
dims        = dimsizes(zz)
zmax_pos    = dims(0) - 1

do imax = 1, dims(0) - 1
  if(.not.ismissing(zz(imax,0)) .and. zz(imax,0) .ge. zmax) then
     zmax_pos    = imax
  end if
end do

zspan       = zmax_pos
zmax        = zz(zmax_pos,0)
nz          = floattoint((zmin - zmax) / 100 + 1)

opts_xy                = True
opts_xy@gsnDraw          = False         ; don't draw
opts_xy@gsnFrame         = False         ; don't advance frame
opts_xy@vpXF = 0.6
opts_xy@vpYF = .50
opts_xy@vpHeightF = .30
opts_xy@vpWidthF  = opts@vpWidthF
opts_xy@gsnLeftString   = ""               ; add the gsn titles

lonStmp=lon(locY_a,locX_a)
if(abs(lonStmp) .lt. 100) then
lonS=sprintf("%4.1f",lon(locY_a,locX_a))
else
lonS=sprintf("%5.1f",lon(locY_a,locX_a))
end if

latS=sprintf("%4.1f",lat(locY_a,locX_a))

lonEtmp=lon(locY_b,locX_b)
if(abs(lonEtmp) .lt. 100)then
lonE=sprintf("%4.1f",lon(locY_b,locX_b))
else
lonE=sprintf("%5.1f",lon(locY_a,locX_a))
end if

latE=sprintf("%4.1f",lat(locY_b,locX_b))

opts_xy@gsnCenterString = ""
opts_xy@gsnRightString  = ""
opts_xy@cnInfoLabelOn = False 

opts_xy@trYLog         = False ;True
opts_xy@tiYAxisString = "log"
opts_xy@trYMinF        = 1
opts_xy@tiXAxisString  = x_plane@description + x_axis_unit
opts_xy@tiXAxisOffsetYF = 0.07
opts_xy@tiYAxisString  = "P [hPa]"
opts_xy@tmYLMode       = "Explicit"
opts_xy@tmYLValues     = fspan(1,zspan,nz)
opts_xy@tmYLLabels     = sprintf("%.0f",fspan(zmin,zmax,nz))
opts_xy@tmXBMode       = "Explicit"
opts_xy@tmXBValues     = fspan(0,xspan,nx) 
opts_xy@tmXBLabels     = sprintf("%.2f",fspan(xmin,xmax,nx))
opts_xy@gsnPaperOrientation = "landscape"

gsn_define_colormap(wks,"CBR_coldhot")

opts_z                       = opts_xy
opts_z@cnFillOn=True
opts_z@cnLevelSelectionMode = "ExplicitLevels" ;
opts_z@cnLevels    =  (/-3.0,-2.0,-1.0,-0.5, 0.5, 1.0,2.0,3.0/)
opts_z@lbLabelStride = 1
;opts_z@cnLevelSelectionMode = "ManualLevels"
;opts_z@cnMinLevelValF  = csmincsc
;opts_z@cnMaxLevelValF  = csmaxcsc
;opts_z@cnLevelSpacingF = csintcsc

opts_z@cnLineColor           = "white"
opts_z@pmLabelBarOrthogonalPosF = .10; Move whole thing down
opts_z@pmLabelBarHeightF = 0.04

contourd = gsn_csm_contour(wks,smth9(varcsDave(0:zmax_pos,:),0.5,0.25,False),opts_z)
draw(contourd)



;Header
txres               = True
txres@txJust="CenterLeft"
txres@txFontHeightF = 0.010
lst = systemfunc("ls -lh "+infile1)
strs = str_split(lst, " ")
str1 = "Input: "+strs(8)
str2 = strs(0)+" "+strs(1)+" "+strs(2)+" "+strs(3)+" "+strs(4)+" "+\
strs(5)+" "+strs(6)+" "+strs(7)
gsn_text_ndc(wks,str1,0.05,0.99,txres)
gsn_text_ndc(wks,str2,0.05,0.97,txres)
lst = systemfunc("ls -lh "+infile2)
strs = str_split(lst, " ")
str1 = "Input: "+strs(8)
str2 = strs(0)+" "+strs(1)+" "+strs(2)+" "+strs(3)+" "+strs(4)+" "+\
strs(5)+" "+strs(6)+" "+strs(7)
gsn_text_ndc(wks,str1,0.05,0.95,txres)
gsn_text_ndc(wks,str2,0.05,0.93,txres)

txres@txJust="CenterCenter"
txres@txFontHeightF = 0.012
text=runname1
gsn_text_ndc(wks,text,0.3,0.91,txres)
text="DIFF ("+runname2+")"
gsn_text_ndc(wks,text,0.8,0.91,txres)

txres@txFontHeightF = 0.012
text=times(nstart)+"-"+times(nend)+" "+domain
gsn_text_ndc(wks,text,0.3,0.89,txres)
gsn_text_ndc(wks,text,0.8,0.89,txres)

;Footer
txres@txFontHeightF = 0.01
txres@txJust="CenterLeft"
text = systemfunc("export LANC=C; date")
gsn_text_ndc(wks,text,  0.05,0.055,txres)

cwd =systemfunc("pwd")
gsn_text_ndc(wks,"Current dir: "+cwd,   0.05,0.035,txres)
gsn_text_ndc(wks,"Script: "+scriptname, 0.05,0.020,txres)
gsn_text_ndc(wks,"Output dir: "+outdir, 0.05,0.005,txres)

frame(wks)
print("Figure: "+figfile+"."+figtype)
print("")

end do ;n (nvar) ;END OF LOOP FOR OUTPUT TERMS
;=========================================
;END OF LOOP FOR OUTPUT TERMS
;=========================================





;=========================================
;RESIDUAL TERM
;=========================================
var2dave=res2dave
var2dave(0:5,:)=0.0
var2dave(jm-5:jm-1,:)=0.0
var2dave(:,0:5)=0.0
var2dave(:,im-5:im-1)=0.0
varcsave=rescsave
var2dDave=res2dF2ave-res2dave
varcsDave=rescsF2ave-rescsave
var2dDave(0:5,:)=0.0
var2dDave(jm-5:jm-1,:)=0.0
var2dDave(:,0:5)=0.0
var2dDave(:,im-5:im-1)=0.0

templete= outdir + runname1 + "_" + runname2+ "." +domain
figfile = templete + "_" + times(nstart)+"-"+times(nend) + "_"+"RES"

wks = gsn_open_wks(figtype, figfile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PLOT FILE1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
res1=opts_cs
res1@vpXF = 0.1
res1@gsnLeftString   = "RES [K/d] "+level+" [hPa]"
res1@gsnCenterString = ""
res1@gsnRightString  = "SLP [hPa]"
res1@cnLinesOn = False
res1@cnLineColor          = "white"
res1@cnLineThicknessF = 0.5
res1@cnLevelSelectionMode = "ManualLevels"
;res1@cnMinLevelValF  = 0.1
;res1@cnMaxLevelValF  = 0.6	
;res1@cnLevelSpacingF = 0.1
res1@cnLevelSelectionMode = "ExplicitLevels" ;
res1@cnLevels    = (/-15.0,-10.0,-5.0,-0.5, 0.5, 5.0,10.0,15.0/)
res1@lbLabelStride = 1

gsn_define_colormap(wks,"CBR_coldhot")

plot1=gsn_csm_contour_map(wks,var2dave,res1)

; SLP
res6=opts_cn
res6@cnLineColor          = "white"
res6@cnLineLabelsOn = False
res6@cnLineLabelsOn = False
res6@cnMinLevelValF  = 950
res6@cnMaxLevelValF  = 1040
res6@cnLevelSpacingF = 4
res6@cnLineThicknessF = 3
plotp1=gsn_csm_contour(wks,psfc,res6)
overlay(plot1,plotp1)

res7=opts_cn
res7@cnLineColor          = "black"
res7@cnLineLabelFontColor = "black"
res7@cnMinLevelValF  = 950
res7@cnMaxLevelValF  = 1040
res7@cnLevelSpacingF = 4
res7@cnLineThicknessF = 1

plotp2=gsn_csm_contour(wks,psfc,res7)
overlay(plot1,plotp2)

;LINE
lnres               = True
lnres@gsLineThicknessF=3
lnres@gsLineColor = "black"
if(csctype .eq. "meridional")then
ln = gsn_add_polyline(wks,plot1,y_plane,x_plane,lnres)
else
ln = gsn_add_polyline(wks,plot1,x_plane,y_plane,lnres)
end if

dum1 = gsn_add_text(wks,plot1,""+lats1,lons1,lats1,txres1)
dum2 = gsn_add_text(wks,plot1,""+lons2,lons2,lats2,txres2)
draw(plot1)
delete(plot1)
delete([/dum1,dum2/])



;CROSS SECTION
; CREATE NICE LEVELS FOR CROSS SECTION
zmax = 300.
zz   = wrf_user_intrp3d(p, p, "v",plane,0.,opts)
zmin = 1000.
nice_levs   = floor((zmin - zmax) / 50) * 50
zmax        = zmin - nice_levs
dims        = dimsizes(zz)
zmax_pos    = dims(0) - 1

do imax = 1, dims(0) - 1
  if(.not.ismissing(zz(imax,0)) .and. zz(imax,0) .ge. zmax) then
     zmax_pos    = imax
  end if
end do

zspan       = zmax_pos
zmax        = zz(zmax_pos,0)
nz          = floattoint((zmin - zmax) / 100 + 1)

opts_xy                = True
opts_xy@gsnDraw          = False         ; don't draw
opts_xy@gsnFrame         = False         ; don't advance frame
opts_xy@vpXF = 0.1
opts_xy@vpYF      = .50
opts_xy@vpHeightF = .30
opts_xy@vpWidthF  = opts@vpWidthF
opts_xy@gsnLeftString   = ""               ; add the gsn titles

lonStmp=lon(locY_a,locX_a)
if(abs(lonStmp) .lt. 100) then
lonS=sprintf("%4.1f",lon(locY_a,locX_a))
else
lonS=sprintf("%5.1f",lon(locY_a,locX_a))
end if

latS=sprintf("%4.1f",lat(locY_a,locX_a))

lonEtmp=lon(locY_b,locX_b)
if(abs(lonEtmp) .lt. 100)then
lonE=sprintf("%4.1f",lon(locY_b,locX_b))
else
lonE=sprintf("%5.1f",lon(locY_a,locX_a))
end if

latE=sprintf("%4.1f",lat(locY_b,locX_b))

opts_xy@gsnCenterString = ""
opts_xy@gsnRightString  = ""
opts_xy@cnInfoLabelOn = False 

opts_xy@trYLog         = False ;True
opts_xy@tiYAxisString = "log"
opts_xy@trYMinF        = 1
opts_xy@tiXAxisString  = x_plane@description + x_axis_unit
opts_xy@tiXAxisOffsetYF = 0.07
opts_xy@tiYAxisString  = "P [hPa]"
opts_xy@tmYLMode       = "Explicit"
opts_xy@tmYLValues     = fspan(1,zspan,nz)
opts_xy@tmYLLabels     = sprintf("%.0f",fspan(zmin,zmax,nz))
opts_xy@tmXBMode       = "Explicit"
opts_xy@tmXBValues     = fspan(0,xspan,nx) 
opts_xy@tmXBLabels     = sprintf("%.2f",fspan(xmin,xmax,nx))
opts_xy@gsnPaperOrientation = "landscape"

gsn_define_colormap(wks,"CBR_coldhot")

opts_z                       = opts_xy
opts_z@cnFillOn=True
opts_z@cnLevelSelectionMode = "ExplicitLevels" ;
opts_z@cnLevels    = (/-15.0,-10.0,-5.0,-0.5, 0.5, 5.0,10.0,15.0/)
opts_z@lbLabelStride = 1
;opts_z@cnLevelSelectionMode = "ManualLevels"
;opts_z@cnMinLevelValF  = csmincsc
;opts_z@cnMaxLevelValF  = csmaxcsc
;opts_z@cnLevelSpacingF = csintcsc

opts_z@cnLineColor           = "white"
opts_z@pmLabelBarOrthogonalPosF = .10; Move whole thing down
opts_z@pmLabelBarHeightF = 0.04

contourd = gsn_csm_contour(wks,varcsave(0:zmax_pos,:),opts_z)
draw(contourd)
delete(contourd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PLOT FILE2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
res1=opts_cs
res1@vpXF = 0.6
opts@vpYF = 0.85 ;1.25
res1@gsnLeftString   = "RES [K/d] "+level+" [hPa]"
res1@gsnCenterString = ""
res1@gsnRightString  = "SLP [hPa]"
res1@cnLinesOn = False
res1@cnLineColor          = "white"
res1@cnLineThicknessF = 0.5
res1@cnLevelSelectionMode = "ManualLevels"
;res1@cnMinLevelValF  = 0.1
;res1@cnMaxLevelValF  = 0.6	
;res1@cnLevelSpacingF = 0.1
res1@cnLevelSelectionMode = "ExplicitLevels" ;
res1@cnLevels  = (/-3.0,-2.0,-1.0,-0.5, 0.5, 1.0,2.0,3.0/)

res1@lbLabelStride = 1

gsn_define_colormap(wks,"CBR_coldhot")

plot1=gsn_csm_contour_map(wks,smth9(var2dDave,0.5,0.25,False),res1)

; SLP
res6=opts_cn
res6@cnLineColor          = "white"
res6@cnLineLabelsOn = False
res6@cnLineLabelsOn = False
res6@cnMinLevelValF  = 950
res6@cnMaxLevelValF  = 1040
res6@cnLevelSpacingF = 4
res6@cnLineThicknessF = 3
plotp1=gsn_csm_contour(wks,psfc2,res6)
overlay(plot1,plotp1)

res7=opts_cn
res7@cnLineColor          = "black"
res7@cnLineLabelFontColor = "black"
res7@cnMinLevelValF  = 950
res7@cnMaxLevelValF  = 1040
res7@cnLevelSpacingF = 4
res7@cnLineThicknessF = 1

plotp2=gsn_csm_contour(wks,psfc2,res7)
overlay(plot1,plotp2)

;LINE
lnres               = True
lnres@gsLineThicknessF=3
lnres@gsLineColor = "black"
if(csctype .eq. "meridional")then
ln = gsn_add_polyline(wks,plot1,y_plane,x_plane,lnres)
else
ln = gsn_add_polyline(wks,plot1,x_plane,y_plane,lnres)
end if

dum1 = gsn_add_text(wks,plot1,""+lats1,lons1,lats1,txres1)
dum2 = gsn_add_text(wks,plot1,""+lons2,lons2,lats2,txres2)
draw(plot1)
delete(plot1)
delete([/dum1,dum2/])




;CROSS SECTION
; CREATE NICE LEVELS FOR CROSS SECTION
zmax = 300.
zz   = wrf_user_intrp3d(p, p, "v",plane,0.,opts)
zmin = 1000.
nice_levs   = floor((zmin - zmax) / 50) * 50
zmax        = zmin - nice_levs
dims        = dimsizes(zz)
zmax_pos    = dims(0) - 1

do imax = 1, dims(0) - 1
  if(.not.ismissing(zz(imax,0)) .and. zz(imax,0) .ge. zmax) then
     zmax_pos    = imax
  end if
end do

zspan       = zmax_pos
zmax        = zz(zmax_pos,0)
nz          = floattoint((zmin - zmax) / 100 + 1)

opts_xy                = True
opts_xy@gsnDraw          = False         ; don't draw
opts_xy@gsnFrame         = False         ; don't advance frame
opts_xy@vpXF = 0.6
opts_xy@vpYF = .50
opts_xy@vpHeightF = .30
opts_xy@vpWidthF  = opts@vpWidthF
opts_xy@gsnLeftString   = ""               ; add the gsn titles

lonStmp=lon(locY_a,locX_a)
if(abs(lonStmp) .lt. 100) then
lonS=sprintf("%4.1f",lon(locY_a,locX_a))
else
lonS=sprintf("%5.1f",lon(locY_a,locX_a))
end if

latS=sprintf("%4.1f",lat(locY_a,locX_a))

lonEtmp=lon(locY_b,locX_b)
if(abs(lonEtmp) .lt. 100)then
lonE=sprintf("%4.1f",lon(locY_b,locX_b))
else
lonE=sprintf("%5.1f",lon(locY_a,locX_a))
end if

latE=sprintf("%4.1f",lat(locY_b,locX_b))

opts_xy@gsnCenterString = ""
opts_xy@gsnRightString  = ""
opts_xy@cnInfoLabelOn = False 

opts_xy@trYLog         = False ;True
opts_xy@tiYAxisString = "log"
opts_xy@trYMinF        = 1
opts_xy@tiXAxisString  = x_plane@description + x_axis_unit
opts_xy@tiXAxisOffsetYF = 0.07
opts_xy@tiYAxisString  = "P [hPa]"
opts_xy@tmYLMode       = "Explicit"
opts_xy@tmYLValues     = fspan(1,zspan,nz)
opts_xy@tmYLLabels     = sprintf("%.0f",fspan(zmin,zmax,nz))
opts_xy@tmXBMode       = "Explicit"
opts_xy@tmXBValues     = fspan(0,xspan,nx) 
opts_xy@tmXBLabels     = sprintf("%.2f",fspan(xmin,xmax,nx))
opts_xy@gsnPaperOrientation = "landscape"

gsn_define_colormap(wks,"CBR_coldhot")

opts_z                       = opts_xy
opts_z@cnFillOn=True
opts_z@cnLevelSelectionMode = "ExplicitLevels" ;
opts_z@cnLevels    =  (/-3.0,-2.0,-1.0,-0.5, 0.5, 1.0,2.0,3.0/)
opts_z@lbLabelStride = 1
;opts_z@cnLevelSelectionMode = "ManualLevels"
;opts_z@cnMinLevelValF  = csmincsc
;opts_z@cnMaxLevelValF  = csmaxcsc
;opts_z@cnLevelSpacingF = csintcsc

opts_z@cnLineColor           = "white"
opts_z@pmLabelBarOrthogonalPosF = .10; Move whole thing down
opts_z@pmLabelBarHeightF = 0.04

contourd = gsn_csm_contour(wks,smth9(varcsDave(0:zmax_pos,:),0.5,0.25,False),opts_z)
draw(contourd)



;Header
txres               = True
txres@txJust="CenterLeft"
txres@txFontHeightF = 0.010
lst = systemfunc("ls -lh "+infile1)
strs = str_split(lst, " ")
str1 = "Input: "+strs(8)
str2 = strs(0)+" "+strs(1)+" "+strs(2)+" "+strs(3)+" "+strs(4)+" "+\
strs(5)+" "+strs(6)+" "+strs(7)
gsn_text_ndc(wks,str1,0.05,0.99,txres)
gsn_text_ndc(wks,str2,0.05,0.97,txres)
lst = systemfunc("ls -lh "+infile2)
strs = str_split(lst, " ")
str1 = "Input: "+strs(8)
str2 = strs(0)+" "+strs(1)+" "+strs(2)+" "+strs(3)+" "+strs(4)+" "+\
strs(5)+" "+strs(6)+" "+strs(7)
gsn_text_ndc(wks,str1,0.05,0.95,txres)
gsn_text_ndc(wks,str2,0.05,0.93,txres)

txres@txJust="CenterCenter"
txres@txFontHeightF = 0.012
text=runname1
gsn_text_ndc(wks,text,0.3,0.91,txres)
text="DIFF ("+runname2+")"
gsn_text_ndc(wks,text,0.8,0.91,txres)

txres@txFontHeightF = 0.012
text=times(nstart)+"-"+times(nend)+" "+domain
gsn_text_ndc(wks,text,0.3,0.89,txres)
gsn_text_ndc(wks,text,0.8,0.89,txres)

;Footer
txres@txFontHeightF = 0.01
txres@txJust="CenterLeft"
text = systemfunc("export LANC=C; date")
gsn_text_ndc(wks,text,  0.05,0.055,txres)

cwd =systemfunc("pwd")
gsn_text_ndc(wks,"Current dir: "+cwd,   0.05,0.035,txres)
gsn_text_ndc(wks,"Script: "+scriptname, 0.05,0.020,txres)
gsn_text_ndc(wks,"Output dir: "+outdir, 0.05,0.005,txres)

frame(wks)
print("Figure: "+figfile+"."+figtype)
print("")


print("")
print("Input file: "+infile1)
print("Input file: "+infile2)
print("")
print("Output files: "+templete+"xxx."+figtype)
print("")
print("Done " + script + ".")

end
```

## runncl.sh
```
#!/bin/bash
#
# Universal wrapper script for ncl. 
# Pass arguments from the command line to environment variables
#
# version 0.1, Thierry Corti, C2SM ETH Zurich
# 

E_BADARGS=65

if [ ! -n "$1" ]
then
  echo "Usage: `basename $0` script.ncl argument1 argument2 etc."
  exit $E_BADARGS
fi  

# save number of arguments to environment variable NCL_N_ARG
export NCL_N_ARGS=$#

# save command line arguments to environment variable NCL_ARG_#
for ((index=1; index<=$#; index++))
do
  eval export NCL_ARG_$index=\$$index
done   



command="ulimit -s unlimited"
echo ==========================================================
echo
echo  $command
echo
echo  Be careful! The above command will use lots of memory.
echo
echo ==========================================================


$command


# run ncl
ncl -Q -n $1
```

