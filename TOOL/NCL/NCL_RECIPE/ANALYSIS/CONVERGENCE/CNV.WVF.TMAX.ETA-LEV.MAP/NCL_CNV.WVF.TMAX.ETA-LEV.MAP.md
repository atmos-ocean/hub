CNV.MAP
------------------------
[TOC]

Fri, 10 Apr 2020 17:27:09 +0900
calypso.bosai.go.jp
/work05/manda/WRF.POST/K17/NCL/CNV.MAP

**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB

**How to Run**
```
$ WRAPIT wrfdiv2.f
WRAPIT Version: 120209
COMPILING wrfdiv2.f
LINKING
END WRAPIT

$ CNV.MAP.sh
script name =CNV.MAP.ncl
namelist    =CNV.MAP.nml

READ NAMELIST FILE
DONE.

CHECK NAMELIST FILE
runname: K17.R11.00.00
indir:   /work05/manda/WRF.RESULT2//K17.R11_OUT/K17.R11.00.00
wrfout:
sdatetime: 2017-07-05_00:00:00
edatetime: 2017-07-05_12:00:00
DONE.

CREATE OUTPUT DIRECTORY
outdir=OUT_K17.R11.00.00/
DONE.

OPEN INPUT FILE
-r--r----- 1 manda manda 16G Jun 23  2019 /work05/manda/WRF.RESULT2//K17.R11_OUT/K17.R11.00.00/wrfout_d03_2017-07-03_12:00:00
DONE.

CHECK TIME STEPS
36 2017-07-05_00:00:00 2017-07-05_00:00:00
48 2017-07-05_12:00:00 2017-07-05_12:00:00
DONE.

=============================================
 START 40 2017-07-05_04:00:00

 INPUT & COMPUTE CONVERGENCE
 FAC=10000
 DX_IN=1000 DY_IN=1000
 NX=300 NY=300
U@_FillValue=9.969209968386869e+36
V@_FillValue=9.969209968386869e+36
------------------------
 PLOT FIGURE
------------------------
------------------------
 DONE PLOT FIGURE
------------------------
 DONE 40 2017-07-05_04:00:00
=============================================
DONE.


Input:  /work05/manda/WRF.RESULT2//K17.R11_OUT/K17.R11.00.00/wrfout_d03_2017-07-03_12:00:00

OUTPUT FILE NAME: 
-rw-rw-r-- 1 manda manda 0 2020-04-10 17:28 OUT_K17.R11.00.00//K17.R11.00.00_2017-07-05_04:00:00_400.ps
```



**List of the following files:**

- CNV.MAP.sh
- CNV.MAP.ncl
- runncl.sh
- wrfdiv2.f
  
### CNV.MAP.sh
```
#!/bin/sh
export LANG=C

exe=runncl.sh
nml=$(basename $0 .sh).nml
ncl=$(basename $0 .sh).ncl

wrfout_time="2017-07-03_12:00:00"
domain=d03
wrfout=wrfout_${domain}_${wrfout_time}
sdatetime="2017-07-05_00:00:00"
edatetime="2017-07-05_12:00:00"

CASE=K17
RUN=R11

runname_list="\
${CASE}.${RUN}.00.00 \
"

for runname in $runname_list; do

indir_root=/work05/manda/WRF.RESULT2/
indir=${indir_root}/${CASE}.${RUN}_OUT/${runname}
if [ ! -d $indir ]; then
echo
echo ERROR in $0 : NO SUCH DIR, $indir
echo
exit 1
fi
infle=${indir}/${wrfout}
if [ ! -f $infle ]; then
echo
echo ERROR in $0 : NO SUCH FILE, $infle
echo
exit 1
fi

cat<<EOF>$nml
runname =${runname}
indir =${indir}
domain =${domain}
wrfout =${wrfout}
sdatetime =${sdatetime}
edatetime =${edatetime}
EOF

$exe $ncl "$nml"

done #for runname

exit 0

# NCL WRAPIT (FORTRAN)
# If you have a Fortran function or procedure that you'd like to 
# call from NCL, you can do it by "wrapping" this function using 
# the WRAPIT script.
#
# https://www.ncl.ucar.edu/Document/Tools/WRAPIT.shtml#Step_1
# https://www.ncl.ucar.edu/Document/Tools/WRAPIT.shtml#Step_2
```

### CNV.MAP.ncl
```
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

external wrfdiv2 "./wrfdiv2.so"

begin

FAC=1.E4

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

outdir="OUT_"+runname+"/"
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

print("DONE.")
print("")



xlong=wrf_user_getvar(a,"XLONG",0)
xlat =wrf_user_getvar(a,"XLAT",0)
;lmask=wrf_user_getvar(a,"LANDMASK",0)



datetime="2017-07-05_04:00:00"
do n=0,nt-1
if(times(n).eq.datetime)then
it=n
end if
end do ; n


print("=============================================")
print(" START "+it+" "+times(it))
print("")
print(" INPUT & COMPUTE CONVERGENCE")
print(" FAC="+FAC)

U_IN  = wrf_user_getvar(a,"ua",it)   ; u on mass points
V_IN  = wrf_user_getvar(a,"va",it)   ; v on mass points
Z_IN  = wrf_user_getvar(a, "z",it)

; Horizontally interpolate to height coordinates ("Z")
;https://www2.mmm.ucar.edu/wrf/OnLineTutorial/Graphics/NCL/Examples/LEVELS_INTERP/wrf_Height2.ncl
Z0 = 400.   ; m
U2 = wrf_user_intrp3d(U_IN,Z_IN,"h",Z0,0.,False)
V2 = wrf_user_intrp3d(V_IN,Z_IN,"h",Z0,0.,False)

MSFT_IN =wrf_user_getvar(a,"MAPFAC_M",it)
DX_IN=a@DX
DY_IN=a@DY

dim=dimsizes(U2)
;print(dim)
NY=dim(0)
NX=dim(1)

print(" DX_IN="+DX_IN+" DY_IN="+DY_IN)
print(" NX="+NX+" NY="+NY)

U=todouble(U2)
V=todouble(V2)

FVAL=U@_FillValue
print("U@_FillValue="+U@_FillValue)
print("V@_FillValue="+V@_FillValue)

MSFT=todouble(MSFT_IN)
DX=todouble(DX_IN)
DY=todouble(DY_IN)

DIV=new(dim,double)


wrfdiv2::DIVD2(DIV,U,V,MSFT,DX,DY,NX,NY,FVAL)
CNV=-DIV*FAC
;printVarSummary(DIV)



print("------------------------")
print(" PLOT FIGURE")
print("------------------------")

type="ps"

figdir=outdir
figfile=figdir+"/"+runname+"_"+datetime+"_"+tostring(toint(Z0))
;figfile=runname+"_"+datetime+"_"+tostring(toint(Z0))

wks = gsn_open_wks(type,figfile)

type="ps"

res = True
res@gsnAddCyclic = False
res@sfXArray = xlong
res@sfYArray = xlat
res@gsnDraw          = False                ; turn off draw and frame
res@gsnFrame         = False                ; b/c this is an overlay plot
res@tiMainString=runname
res@gsnLeftString=datetime ; add titles
res@gsnRightString=tostring(toint(Z0))+"m" ; add titles

opts = res


opts@mpMinLonF            = min(xlong)               ; select a subregion
opts@mpMaxLonF            = max(xlong)
opts@mpMinLatF            = min(xlat)
opts@mpMaxLatF            = max(xlat)
opts@mpDataBaseVersion    = "HighRes" ;"MediumRes" ; fill land in gray and ocean in transparent
opts@mpFillOn = False; True
opts@mpFillColors = (/"background","transparent","gray","transparent"/)
opts@mpFillDrawOrder = "PreDraw"

opts@gsnMajorLatSpacing = 1              ; change maj lat tm spacing
opts@gsnMajorLonSpacing = 1              ; change maj lon tm spacing

opts@cnFillOn = True
opts@cnLinesOn            = False            ; turn off contour lines

opts@lbTitleString = "10^4 1/s"
opts@lbTitlePosition = "bottom"
opts@lbTitleFontHeightF = 0.015

opts@pmLabelBarOrthogonalPosF = 0.1
opts@pmLabelBarHeightF = 0.08

COLORMAP="precip2_17lev"
;COLORMAP="sunshine_9lev"
gsn_define_colormap(wks,COLORMAP)

opts@cnLevelSelectionMode = "ExplicitLevels"
opts@cnLevels = (/5,10,15,20,25,30,35,40,45,50/)

plot = gsn_csm_contour_map(wks,CNV,opts)


ter=wrf_user_getvar(a,"ter",it)
opts2=res
opts2@cnFillOn = False
opts2@cnLinesOn            = True
opts2@gsnLeftString="" ;datetime ; add titles
opts2@gsnRightString="" ;tostring(toint(Z0))+"m" ; add titles
opts2@cnLevelSelectionMode = "ExplicitLevels"
opts2@cnLevels = (/400.,1000./)
opts2@cnLineLabelsOn = False
opts2@cnInfoLabelOn = False

plot2=gsn_csm_contour(wks,ter,opts2)
overlay(plot,plot2)

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

;FOOTER

draw(plot)

frame(wks)

print("------------------------")
print(" DONE PLOT FIGURE")
print("------------------------")

delete([/DIV,CNV/])

print(" DONE "+it+" "+times(it))
print("=============================================")

print("DONE.")
print("")



print("")
print("Input:  "+ infle)
print("")
print("OUTPUT FILE NAME: ")
print(systemfunc("ls -lh --time-style=long-iso " + figfile+"."+type))
print("")

end
```

### runncl.sh
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

# run ncl
ncl -Q -n $1
```

### wrfdiv2.f
```
C NCLFORTSTART
      SUBROUTINE DIVD2(DIV,U,V,MSFT,DX,DY,NX,NY,FVAL)
      IMPLICIT NONE
      INTEGER NX,NY
      DOUBLE PRECISION U(NX,NY),V(NX,NY)
      DOUBLE PRECISION DIV(NX,NY),MSFT(NX,NY)
      DOUBLE PRECISION DX,DY
      DOUBLE PRECISION FVAL
C NCLEND

      INTEGER JP1,JM1,IP1,IM1,I,J
      DOUBLE PRECISION DSY,DSX,DUDX,DVDY
      DOUBLE PRECISION MM

C Note all data must be on T-pts
      DO J = 1,NY
      JP1 = MIN(J+1,NY)
      JM1 = MAX(J-1,1)

      DO I = 1,NX
      IP1 = MIN(I+1,NX)
      IM1 = MAX(I-1,1)

      DSX = (IP1-IM1)*DX
      DSY = (JP1-JM1)*DY

C Careful with map factors...
      MM = MSFT(I,J)*MSFT(I,J)

      DUDX = ( U(IP1,J)/MSFT(IP1,J) -
     & U(IM1,J)/MSFT(IM1,J) )/DSX*MM

      DVDY = ( V(I,JP1)/MSFT(I,JP1) -
     & V(I,JM1)/MSFT(I,JM1) )/DSY*MM

      DIV(I,J) = DUDX + DVDY

      IF(U(I,J).GE.FVAL   .OR. V(I,J).GE.FVAL .OR. 
     &   U(IP1,J).GE.FVAL .OR. U(IM1,J).GE.FVAL .OR. 
     &   V(I,JP1).GE.FVAL .OR. V(I,JM1).GE.FVAL)THEN
        DIV(I,J)=FVAL
      END IF

      END DO
      END DO



      RETURN
      END

C
C http://forum.wrfforum.com/viewtopic.php?f=32&t=2267
C Re: NCL: need help to alculate Relative Vorticity & Divergence
C 
C Postby tomk Fri Jun 17, 2011 10:48 am
C wrf.div.f

c--------------------------------------------------------
```

