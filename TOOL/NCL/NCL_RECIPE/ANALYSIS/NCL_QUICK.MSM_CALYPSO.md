MSM_QUICK_LOOK
==============================

Sat, 01 Feb 2020 21:23:58 +0900
calypso.bosai.go.jp
/work05/manda/TEACHING/LOCAL_MET/CYCLONE120403/MSM_QUICK_LOOK

**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB

**List of the following files:**
- quick.msm.run.sh
- quick.msm.ncl
- runncl.sh
  
## quick.msm.run.sh
```
#!/bin/sh

exe=./runncl.sh

ncl=$(basename $0 .run.sh).ncl

if [ ! -f $ncl ]; then
  echo Error in $0 : No such file, $ncl
  exit 1
fi

if [ $# -ne 1 ]; then
  echo ERROR in $0: Wrong argument.
  echo
  echo Usage: $0 yyyymmdd
  echo
fi

yyyymmdd0=$1
yyyy0=${yyyymmdd0:0:4}
  mm0=${yyyymmdd0:4:2}
  dd0=${yyyymmdd0:6:2}

indir=/work05/manda/DATA/MSM

outdir="FIG"

mkdir -vp $outdir

indirp0=$indir/MSM-P/${yyyy0}
indirs0=$indir/MSM-S/${yyyy0}

yyyymmdd1=$(date -d"$yyyymmdd0"-1days '+%Y%m%d')
yyyy1=${yyyymmdd1:0:4}
  mm1=${yyyymmdd1:4:2}
  dd1=${yyyymmdd1:6:2}

indirp1=$indir/MSM-P/${yyyy1}
indirs1=$indir/MSM-S/${yyyy1}

dirs="$indirs0 $indirp0 $indirs1 $indirp1 $outdir"

for dir in $dirs; do

if [ ! -d $dir ]; then
  echo Error in $0 : No such directory, $dir
  exit 1
fi

done

inlist="\
${indirs0}/${mm0}${dd0}.nc  ${indirp0}/${mm0}${dd0}.nc \
${indirs1}/${mm1}${dd1}.nc  ${indirp1}/${mm1}${dd1}.nc"

for input in $inlist; do
if [ ! -f $input ]; then
  echo No such file, $input
  exit 1
fi
done

export LANG=C

$exe $ncl "$yyyymmdd0" "$yyyymmdd1" "$indir" "$outdir"

exit 0
```

## quick.msm.ncl
```
begin

wallClock1 = systemfunc("date") ; retrieve wall clock time

scriptname_i = getenv("NCL_ARG_1")
yyyymmdd0    = getenv("NCL_ARG_2")
yyyymmdd1    = getenv("NCL_ARG_3")
indir_i      = getenv("NCL_ARG_4")
outdir_i     = getenv("NCL_ARG_5")

print("")

print(scriptname_i + " is running ...")
scriptname=systemfunc("basename " + scriptname_i + " .ncl")

if(yyyymmdd0 .eq. "")then
  print("Error : yyyymmdd0 is not specified. Abort.")
  exit()
end if

if(yyyymmdd1 .eq. "")then
  print("Error : yyyymmdd1 is not specified. Abort.")
  exit()
end if

if(indir_i .eq. "")then
  print("Error : indir_i is not specified. Abort.")
  exit()
end if

if(outdir_i .eq. "")then
  print("Error : outdir_i is not specified. Abort.")
  exit()
end if

outdir= outdir_i
;TODAY=>0

yyyymmdd0c=stringtochar(yyyymmdd0)
yyyy0=tostring(yyyymmdd0c( 0: 3))
  mm0=tostring(yyyymmdd0c( 4: 5))
  dd0=tostring(yyyymmdd0c( 6: 7))

infls0=indir_i+"/"+ "/MSM-S/" + yyyy0 + "/" + mm0+dd0+ ".nc"
inflp0=indir_i+"/"+ "/MSM-P/" +yyyy0 + "/"  + mm0+dd0+ ".nc"

as0 = addfile(infls0,"r")
ap0 = addfile(inflp0,"r")

if   (mm0 .eq. "01" )then; mmm="JAN"
elif (mm0 .eq. "02" )then; mmm="FEB"
elif (mm0 .eq. "03" )then; mmm="MAR"
elif (mm0 .eq. "04" )then; mmm="APR"
elif (mm0 .eq. "05" )then; mmm="MAY"
elif (mm0 .eq. "06" )then; mmm="JUN"
elif (mm0 .eq. "07" )then; mmm="JUL"
elif (mm0 .eq. "08" )then; mmm="AUG"
elif (mm0 .eq. "09" )then; mmm="SEP"
elif (mm0 .eq. "11" )then; mmm="NOV"
elif (mm0 .eq. "12" )then; mmm="DEC"

;YESTERDAY=>1

yyyymmdd1c=stringtochar(yyyymmdd1)
yyyy1=tostring(yyyymmdd1c( 0: 3))
  mm1=tostring(yyyymmdd1c( 4: 5))
  dd1=tostring(yyyymmdd1c( 6: 7))
infls1=indir_i+ "/MSM-S/"+ yyyy1 + "/" + mm1+dd1+ ".nc"
inflp1=indir_i+ "/MSM-P/"+ yyyy1 + "/" + mm1+dd1+ ".nc"
as1 = addfile(infls1,"r")
ap1 = addfile(inflp1,"r")

;print(as0)
;print(as1)
;print(ap0)
;print(ap1)

times = as0->time
nts=dimsizes(times)
lons  = as0->lon
nxs=dimsizes(lons)
lats  = as0->lat
nys=dimsizes(lats)
print("")
print("nts="+nts+ " nys="+nys+" nxs="+nxs)
timep = ap0->time
ntp=dimsizes(timep)
    p = ap0->p
np=dimsizes(p)
lonp  = ap0->lon
nxp=dimsizes(lonp)
latp  = ap0->lat
nyp=dimsizes(latp)

print("ntp="+ntp+" np="+np+" nyp="+nyp+" nxp="+nxp)

print("")

figtype="eps"

opts=True

;Coordinates and domain
opts@gsnAddCyclic = False
opts@sfXArray = lons
opts@sfYArray = lats

;Plotting area
opts@vpHeightF = .3 ;1
opts@vpWidthF  = .3 ;.4
opts@vpYF      = .85 ;1.25

;Page control
opts@gsnFrame = False
opts@gsnDraw =  False

; Contour line
opts_cn=opts
opts_cn@cnFillOn = False
opts_cn@cnLevelSpacingF = 1
opts_cn@cnLineThicknessF= 1.0
opts_cn@cnInfoLabelOn = False

; Color shade
opts_cs=opts
opts_cs@cnFillOn = True
opts_cs@cnLinesOn= False                   ; turn off contour lines
opts_cs@pmLabelBarOrthogonalPosF = 0.1
opts_cs@pmLabelBarHeightF = 0.025

; Color bar
opts_cs@pmLabelBarOrthogonalPosF = .10          ; move whole thing down
opts_cs@lbTitlePosition  = "Right"              ; title position
opts_cs@lbTitleFontHeightF= .012                ; make title smaller
opts_cs@lbTitleDirection = "Across"             ; title direction

; MAP
opts_cs@mpGridAndLimbOn             = "False"
opts_cs@mpAreaMaskingOn             = "True"
opts_cs@mpDataBaseVersion = "HighRes"
opts_cs@mpFillOn = False ; True

opts_cs@mpFillDrawOrder       = "PostDraw"     ; draw map fill last
opts_cs@mpGeophysicalLineThicknessF = 1.0
opts_cs@mpShapeMode = "FreeAspect"
opts_cs@mpMinLonF = min(lonp)
opts_cs@mpMaxLonF = max(lonp)
opts_cs@mpMinLatF = min(latp)
opts_cs@mpMaxLatF = max(latp)

; VECTOR

vskp=20
vecres                  = True            ; vector only resources
vecres@gsnAddCyclic = False
vecres@gsnDraw          = False           ; don't draw
vecres@gsnFrame         = False           ; don't advance frame
vecres@vcGlyphStyle     = "LineArrow" ;FillArrow"
vecres@vcLineArrowThicknessF    = 1               ; change vector thickness
vecres@vcRefLengthF     = 0.05            ; define length of vec ref
vecres@vcRefAnnoOrthogonalPosF=-1.0
vecres@vcRefAnnoParallelPosF  =1.2
;vecres@vcLabelFontHeightF = 0.08
vecres@gsnRightString   = " "             ; turn off right string
vecres@gsnLeftString    = " "             ; turn off left string
vecres@tiXAxisString    = " "             ; turn off axis label

dthr=3

do itp=0,ntp-1

  templete=outdir+"/"+scriptname+"."+yyyymmdd0

  hh=sprinti("%0.2i",itp*3)

  figfile = templete + "_" + hh

  print("")
  print("Working on time: " + yyyymmdd0 + "_" + hh )

  wks = gsn_open_wks(figtype, figfile)

  print("Output: "+figfile+"."+figtype)

; 降水量は、いつ降った雨の量ですか？
; http://www.jma.go.jp/jma/kishou/know/faq/faq1.html

  r3h=new((/nys,nxs/),float) ;3-hr precipitation
  r3h=0.0

  do its=0,2
    idx=(itp-1)*3+its+1
    iday=0
    if(idx .lt. 0)then
      idx=idx+24
      iday=-1
    end if
    if(iday .eq. 0)then ;Today
      ;print("itp="+itp+" timep="+timep(itp)+" iday="+iday+" idx="+idx)
      r1h=short2flt( as0->r1h(idx,:,:) )
    end if
    if(iday .eq. -1)then ;Yesterday
      ;print("itp="+itp+" timep="+timep(itp)+" iday="+iday+" idx="+idx)
      r1h=short2flt( as1->r1h(idx,:,:) )
    end if
    r3h=r3h+r1h
  end do ; its

  res1=opts_cs

  res1@vpXF = 0.1
  res1@gsnLeftString   = "R [mm/"+ dthr +"hr]"               ; add the gsn titles
  res1@gsnCenterString = "U~B~10~N~ [m/s]" ;SST [~S~o~N~C]"
  res1@gsnRightString  = "SFC"
  res1@cnLevelSelectionMode = "ManualLevels"

  r3max=max(r3h)

  ;print(r3max)

  if (r3max .le. 50.0                    ) then
  res1@cnMinLevelValF       = 5
  res1@cnMaxLevelValF       = 50
  res1@cnLevelSpacingF = 5
  end if

  if (r3max .gt. 50.0 .and. r3max .le. 100.0 ) then
  res1@cnMinLevelValF       = 10
  res1@cnMaxLevelValF       = 100
  res1@cnLevelSpacingF = 10
  end if

  if (r3max .gt. 100.0 .and. r3max .le. 150.0 ) then
  res1@cnMinLevelValF       = 15
  res1@cnMaxLevelValF       = 150
  res1@cnLevelSpacingF = 15
  end if

  if (r3max .gt. 150.0 .and. r3max .le. 200.0 ) then
  res1@cnMinLevelValF       = 20
  res1@cnMaxLevelValF       = 200
  res1@cnLevelSpacingF = 20
  end if

  if (r3max .gt. 200.0                    ) then
  res1@cnMinLevelValF       = 40.
  res1@cnMaxLevelValF       = 400.
  res1@cnLevelSpacingF = 40.
  end if

  res5=opts_cn
  gsn_define_colormap(wks,"precip3_16lev")
  plot1=gsn_csm_contour_map(wks,r3h,res1)

  ;res5@cnLineColor     = "Red"
  ;plots=gsn_csm_contour(wks,mask(sst,oro,2),res5)
  ;overlay(plot1,plots)
  idx=itp*3
  u10 = short2flt( as0->u(idx,:,:) ) ; surface wind at every 3hr
  v10 = short2flt( as0->v(idx,:,:) )
  ;printVarSummary(u10)
  ;printVarSummary(v10)
  vskp=20
  vecres@vcRefMagnitudeF  = 30               ; define vector ref mag
  vecres@vcRefAnnoOrthogonalPosF =  -0.36    ; move ref vector into plot
  plotv=gsn_csm_vector (wks,u10(::vskp,::vskp),v10(::vskp,::vskp),vecres)
  overlay(plot1,plotv)

  draw(plot1)
  delete([/r3h/])

  plev=950.0
   u = short2flt( ap0->u(itp,{plev},:,:)   )
   v = short2flt( ap0->v(itp,{plev},:,:)   )
  tp = short2flt( ap0->temp(itp,{plev},:,:))
  rh = short2flt( ap0->rh(itp,{plev},:,:)  )

  ;printVarSummary(u)
  ;printVarSummary(v)
  ;printVarSummary(tp)
  ;printVarSummary(rh)

; Water vapor mixing ratio

  tc=tp-273.15
  es=611.0/100.*10^((7.5*tc)/(237.3+tc)) ;[hPa] saturated water vapor pressure by Tetens (1930)'s formulae

  e=rh/100*es
  qv=0.622*e/(plev-e)*1000.0 ;g/kg
  qv!0   = tp!0
  qv!1   = tp!1
  qv&lat = tp&lat
  qv&lon = tp&lon

  ; printVarSummary(qv)
  res2=opts_cs
  delete([/res2@sfXArray, res2@sfYArray/])
  res2@sfXArray = lonp
  res2@sfYArray = latp
  res2@vpXF = 0.57
  res2@gsnLeftString   = "W~B~mix~N~ [g/kg]"               ; add the gsn titles
  res2@gsnCenterString = "WVF [g/kg m/s]"
  res2@gsnRightString  = floattointeger(plev) + " [hPa]"
  plot2=gsn_csm_contour_map(wks,qv,res2)

  vskp=10
  wvfu = u*qv   ; water vapor flux [g/kg m/s]
  wvfv = v*qv
  wvfu!0   = u!0
  wvfu!1   = u!1
  wvfu&lat = u&lat
  wvfu&lon = u&lon
  wvfv!0   = v!0
  wvfv!1   = v!1
  wvfv&lat = v&lat
  wvfv&lon = v&lon
  vecres@vcRefMagnitudeF  = 600              ; define vector ref mag
  vecres@vcRefAnnoOrthogonalPosF =  -0.36    ; move ref vector into plot
  plotv=gsn_csm_vector(wks,wvfu(::vskp,::vskp),wvfv(::vskp,::vskp),vecres)
  overlay(plot2,plotv)

  draw(plot2)
  delete([/u,v,tp,rh,qv,wvfu,wvfv/])

  plev=850.0
  u = short2flt( ap0->u(itp,{plev},:,:)   )
  v = short2flt( ap0->v(itp,{plev},:,:)   )
  tp= short2flt( ap0->temp(itp,{plev},:,:))
  rh= short2flt( ap0->rh(itp,{plev},:,:)  )
  ;printVarSummary(rh)
  ; Water vapor mixing ratio
  tc=tp-273.15
  es=611.0/100.*10^((7.5*tc)/(237.3+tc)) ;[hPa] saturated water vapor pressure by Tetens (1930)'s formulae

  e=rh/100*es
  qv=0.622*e/(plev-e)   ;kg/kg
; Equivalent potential temperature

  ndim=dimsizes(tp)
  jm=ndim(0)
  im=ndim(1)
  ;print("jm="+jm+" im="+im)
  pr_plev=new( (/1,jm,im/), typeof(tp))
  qv_plev=new( (/1,jm,im/), typeof(tp))
  tk_plev=new( (/1,jm,im/), typeof(tp))
  pr_plev=plev*100.0
  qv_plev(0,:,:)=qv(:,:)
  tk_plev(0,:,:)=tp(:,:)
  ;printVarSummary(tk_plev)
  eth=wrf_eth(qv_plev, tk_plev, pr_plev)
  ept=new( (/jm,im/), typeof(eth))
  ept(:,:)=eth(0,:,:)
  ;printVarSummary(ept)

  res3=opts_cs
  res3@vpXF = 0.1
  res3@vpYF = 0.43
  delete([/res3@sfXArray, res3@sfYArray/])
  res3@sfXArray = lonp
  res3@sfYArray = latp
  res3@gsnLeftString   = "EPT [K]"               ; add the gsn titles
  res3@gsnCenterString = "U [m/s]"
  res3@gsnRightString  = floattointeger(plev) + " [hPa]"
  plot3=gsn_csm_contour_map(wks,ept,res3)
  vecres@vcRefMagnitudeF  = 50               ; define vector ref mag
  vecres@vcRefAnnoOrthogonalPosF =  -0.36    ; move ref vector into plot
  plotv=gsn_csm_vector(wks,u(::vskp,::vskp),v(::vskp,::vskp),vecres)
  overlay(plot3,plotv)

  draw(plot3)

  delete([/u,v,rh,tp,es,e,ept,qv,pr_plev,qv_plev,tk_plev/])
  plev=500.0
  u  = short2flt( ap0->u(itp,{plev},:,:)   )
  v  = short2flt( ap0->v(itp,{plev},:,:)   )
  tp = short2flt( ap0->temp(itp,{plev},:,:))
  rh = short2flt( ap0->rh(itp,{plev},:,:)  )
  gph= doubletofloat(ap0->z(itp,{plev},:,:))
  gph=gph/10.0 ; m->decameters(dam)
  idx=itp*3
  psea=short2flt( as0->psea(idx,:,:)  )
  psea=psea/100.0 ; hPa
  gph!0   = tp!0
  gph!1   = tp!1
  gph&lat = tp&lat
  gph&lon = tp&lon
  gph&lon@units="degrees_east"
  gph&lat@units="degrees_north"
  tc=tp-273.15
  tc!0   = tp!0
  tc!1   = tp!1
  tc&lat = tp&lat
  tc&lon = tp&lon
  tc&lon@units="degrees_east"
  tc&lat@units="degrees_north"
  ;printVarSummary(tc)
  ;printVarSummary(gph)
  res4=opts_cs
  res4@vpXF = 0.57
  res4@vpYF = 0.43
  delete([/res4@sfXArray, res4@sfYArray/])

  res4@gsnLeftString   = "SLP [hPa] T [~S~o~N~C] GPH [dam] "+\
  floattointeger(plev) + " [hPa]"
  res4@gsnCenterString = ""
  res4@gsnRightString  = ""
  plot4=gsn_csm_contour_map(wks,tc,res4)

  ;Sea level pressure
  res7=opts_cn
  res7@cnLineColor          = "black"
  res7@cnLineLabelFontColor = "black"
  res7@cnLevelSpacingF = 5
  res7@cnLineLabelFontHeightF   = 0.008
  res7@gsnLeftString   = ""
  res7@gsnCenterString = ""
  res7@gsnRightString  = ""
  delete([/res7@sfXArray, res7@sfYArray/])

  plot7=gsn_csm_contour(wks,psea,res7)
  overlay(plot4,plot7)

  res6=opts_cn
  res6@cnLineColor          = "red"
  res6@cnLineLabelFontColor = "red"
  res6@cnLevelSpacingF = 5
  res6@cnLineLabelFontHeightF   = 0.008

  delete([/res6@sfXArray, res6@sfYArray/])

  plot6=gsn_csm_contour(wks,gph,res6)

  overlay(plot4,plot6)
  draw(plot4)

  delete([/u,v,gph,tp,rh,tc,psea/])

  delete(plot4)

  txres               = True
  txres@txFontHeightF = 0.018
  txres@txJust="CenterCenter"
  text="JMA Mesoanalysis"
  gsn_text_ndc(wks,text,0.5,0.97,txres)
  txres@txFontHeightF = 0.014
  text="http://database.rish.kyoto-u.ac.jp/arch/jmadata/gpv-netcdf.html"
  gsn_text_ndc(wks,text,0.5,0.95,txres)
  txres@txFontHeightF = 0.018
  text=hh + ":00" + "UTC" + dd0+mmm+yyyy0
  gsn_text_ndc(wks,text,0.5,0.91,txres)
  txres@txFontHeightF = 0.01
  txres@txJust="CenterLeft"
  text = systemfunc("export LANC=C; date")
  gsn_text_ndc(wks,text,  0.05,0.010,txres)
  cwd =systemfunc("pwd")
  gsn_text_ndc(wks,cwd,   0.05,0.025,txres)
  gsn_text_ndc(wks,outdir,0.05,0.040,txres)
  frame(wks)

  print("Done.")
end do ;it

templete=outdir+"/"+scriptname+"."+yyyymmdd0

ofle_example=templete+"_xx."+figtype

print("")

print("Input file: "+infls0)
print("Input file: "+infls1)
print("Input file: "+inflp0)
print("Input file: "+inflp1)
print("")

print("Output files: "+ ofle_example)
print("")

print("Done " + scriptname_i + ".")

end
```

## runncl.sh
```bash
#!/bin/bash
#

# Universal wrapper script for ncl.
# Pass arguments from the command line to environment variables
#
# version 0.1, Thierry Corti, C2SM ETH Zurich
#

E_BADARGS=65

if [ ! -n "$1" ]; then
  echo "Usage: `basename $0` script.ncl argument1 argument2 etc."
  exit $E_BADARGS
fi

# save number of arguments to environment variable NCL_N_ARG

export NCL_N_ARGS=$#

# save command line arguments to environment variable NCL_ARG_#

for ((index=1; index<=$#; index++)); do
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

