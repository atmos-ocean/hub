
------------------------------
## List of the following files:
------------------------------
PT.EMAGRAM.sh
PT.EMAGRAM.ncl
PL.VPROF.EPT.SEPT.sh

------------------------------
## Machine info
------------------------------
calypso.bosai.go.jp
/work05/manda/WRF.POST/K17/NCL/PT.EMAGRAM
Thu, 18 Jul 2019 18:30:28 +0900

----------------------
## PT.EMAGRAM.sh
```
#!/bin/bash

NCL=$(basename $0 .sh).ncl
NML=$(basename $0 .sh).nml

runname1=K17.R08.00.00
domain=d03
indir_root=/work04/manda/WRF.RESULT.K17/
indir1=${indir_root}/${runname1}
dtwrfout=1.0                             #WRF OUTPUT INTERVAL [HR]
datetime1=2017-07-03_12:00:00
outdir=OUT_$(basename $0 .sh)/${runname1}
mkdir -vp $outdir
lonp=129.5
latp=33.3
datetimep=2017-07-05_00:00:00

cat <<EOF>$NML
! $(date -R)
! $(pwd)
! $(echo $0 $@)
&para
runname1  =${runname1}
domain    =${domain}
datetime1 =${datetime1}
indir1    =${indir1}
dtwrfout  =${dtwrfout}
outdir    =${outdir}
lonp      =${lonp}
latp      =${latp}
datetimep =${datetimep}
&end
EOF

runncl.sh $NCL "$NML"

```

End of PT.EMAGRAM.sh
----------------------



----------------------
## PT.EMAGRAM.ncl
```
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"      ; These four libraries are automatically
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"       ; loaded from NCL V6.4.0 onward.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"   ; No need for user to explicitly load.
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

function theta_e(tc,td,p,r)

local p0, tk, tl, es, rs, pow, pt, arg21, arg22

begin

p0=1000.0     ;hPa

tk=tc+273.15 ;K

; Eq.(10) of B80 (p.1047) 
es=6.112*exp((17.67*tc)/(tc+243.5)) ;hPa

; EPT
; https://sites.google.com/site/afcanalysis/home/formulae/saturated-ept
; https://journals.ametsoc.org/doi/pdf/10.1175/1520-0493%281980%29108%3C1046%3ATCOEPT%3E2.0.CO%3B2

denom=1.0/(td - 56.0) - log(tk/td)/800.0
tl = 1.0/denom + 56.0

; Eq.(43) of B80. (p.1052)    
pow=0.2854*(1.0 - 0.28*0.001*r)
pt=tk*(p0/p)^pow

arg11 = 3.376/tl - 0.00254
arg12 = r*(1.0 + 0.81 * 1.0E-3*r)

ept=where(p .gt. 1.0 .or. p .le. 1100.0, pt*exp( arg11 * arg12 ), tc@_FillValue)

print(ept)

return(ept)

end


function theta_e_star(tc,p)

local p0, tk, es, rs, pow, pt, arg21, arg22

begin

p0=1000.
tk=tc+273.15 ;K

; Eq.(10) of B80 (p.1047) 
es=6.112*exp((17.67*tc)/(tc+243.5)) ;hPa

; SATURATED EPT
; https://sites.google.com/site/afcanalysis/home/formulae/saturated-ept
;
; https://unidata.github.io/MetPy/latest/api/generated/
; metpy.calc.saturation_equivalent_potential_temperature.html

rs=(0.622 * es/ (p - es))*1.E3       ;g/kg
pow=0.2854*(1.0 - 0.28*0.001*rs)
pt=tk*(p0/p)^pow
arg21=3.376/tk - 0.00254
arg22 = rs*(1.0 + 0.81 * 1.0E-3*rs)

sept=pt*exp(arg21 * arg22)

return(sept)

sept=where(p .lt. 1.0 .or. p .gt. 1500.0,sept,tc@_FillValue)

end



begin

wallClock1 = systemfunc("date") ; retrieve wall clock time

script = getenv("NCL_ARG_1")
nml    = getenv("NCL_ARG_2")
print("script name ="+script)
print("namelist    ="+nml)
print("")

; READ NAMELIST FILE
runname1 =systemfunc("grep runname1 " +nml+ "|cut -f2 -d'='|cut -f1 -d','")
domain   =systemfunc("grep domain "  +nml+ "|cut -f2 -d'='|cut -f1 -d','")
datetime1=systemfunc("grep datetime1 "+nml+ "|cut -f2 -d'='|cut -f1 -d','")
indir1   =systemfunc("grep indir1 "   +nml+ "|cut -f2 -d'='|cut -f1 -d','")
dtwrfout =tofloat(systemfunc("grep dtwrfout "   +nml+ "|cut -f2 -d'='|cut -f1 -d','"))
outdir   =systemfunc("grep outdir "  +nml+ "|cut -f2 -d'='|cut -f1 -d','")
lonp     =tofloat(systemfunc("grep lonp "  +nml+ "|cut -f2 -d'='|cut -f1 -d','"))
latp     =tofloat(systemfunc("grep latp "  +nml+ "|cut -f2 -d'='|cut -f1 -d','"))
datetimep=systemfunc("grep datetimep "+nml+ "|cut -f2 -d'='|cut -f1 -d','")

print("")
print(script + " is running ...")

scriptname=systemfunc("basename " + script + " .ncl")

print("runname1 ="+runname1)
print("domain   ="+domain)
print("datetime1="+datetime1)
print("indir1   ="+indir1)
print("dtwrfout ="+dtwrfout)
print("lonp   ="+lonp)
print("latp   ="+latp)
print("datetimep   ="+datetimep)


;READ WRF OUTPUT FILE
infle1=indir1+"/wrfout_"+domain+"_"+datetime1+".nc"
a1 = addfile(infle1,"r")
times1 = wrf_user_getvar(a1,"times",-1)  ; get all times in the file
ntimes1 = dimsizes(times1)         ; number of times in the file



it=0
oro = wrf_user_getvar(a1,"XLAND",it)
lat = wrf_user_getvar(a1,"lat",it)   ; latitude
lon = wrf_user_getvar(a1,"lon",it)  ; longitude

; get size of dimensions
dim=dimsizes(oro)
jm=dim(0)
im=dim(1)

dimll  = dimsizes(lat)                
nlat   = dimll(0)
mlon   = dimll(1)

llres           = True
llres@ReturnInt = True



do it=0,ntimes1-1 ;LOOP OVER WRF OUTPUT INTERVAL


if (datetimep .eq. times1(it))then

;FIND NEAREST WRF GRID POINT
locij_a         = wrf_user_ll_to_ij(a1, lonp, latp, llres)
locij_a         = locij_a - 1 ; To convert to NCL subscripts
locX_a          = locij_a(0)
locY_a          = locij_a(1)
print(lon(locY_a,locX_a)+" "+lat(locY_a,locX_a))

ph3d=wrf_user_getvar(a1,"pressure",it); Full model pressure [hPa]
tc3d=wrf_user_getvar(a1,"tc",it)    ; Temperature [C]
td3d=wrf_user_getvar(a1,"td",it)  ; Dew point [C]
rm3d=wrf_user_getvar(a1,"QVAPOR",it)  ;

t2m=wrf_user_getvar(a1,"T2",it)


tc1=tc3d(:,locY_a,locX_a)
td1=td3d(:,locY_a,locX_a)+273.15
ph1=ph3d(:,locY_a,locX_a)
rm1=rm3d(:,locY_a,locX_a)*1000.0 ;g/kg

tc1@_FillValue=-999.9

tk3d=wrf_user_getvar(a1,"tk",it) ; Temperature [K]
pa3d=wrf_user_getvar(a1,"p",it)  ; P[Pa]

po1=fspan(1010, 5, 202)
;print(po1)

;ept_chk=wrf_eth(rm3d,tk3d,pa3d)
;ept_chk1=ept_chk(:,locY_a,locX_a)
;ept_chko=linint1(ph1,ept_chk1,False,po1,0)
;ept_chko@FillValue=-999.9

ept=theta_e(tc1,td1,ph1,rm1)
sept=theta_e_star(tc1,ph1)



tco=linint1(ph1,tc1,False,po1,0)
tko=tco+273.15
epto=linint1(ph1,ept,False,po1,0)
septo=linint1(ph1,sept,False,po1,0)

t2m1=t2m(locY_a,locX_a)

;printVarSummary(ph3d)
;printVarSummary(tc1)

;print(ph1)

;do k=0,202-1
;print (po1(k)+" "+to1(k))
;end do ;k

end if ;IF_01

end do ;it


;; Output file
outfle= outdir + "/" + runname1 + "_" + domain + "_" + datetime1 +"_" +lonp+"_"+latp+".txt"
;
;Header line
header = (/\
"# Input: "+ infle1, \
"# datetime = "+datetime1, \
"# lon = "+lonp + " lon = "+ lon(locY_a,locX_a),\
"# lat = "+latp + " lat = "+ lat(locY_a,locX_a)/)
hlist=[/header/]
;alist= [/po1,tko,epto,ept_chko,septo/]
alist= [/po1,tko,epto,septo/]
write_table(outfle, "w", hlist, "%s")
write_table(outfle, "a", alist, "%8.2f %7.3f %7.3f %7.3f %7.3f")

print("")
print("Input: "+infle1)
print("")
print("Output: "+outfle)
print("")

end
```

End of PT.EMAGRAM.ncl
----------------------



----------------------
## PL.VPROF.EPT.SEPT.sh
```
#!/bin/bash
# Description:
#
# Author: manda
#
# Host: calypso.bosai.go.jp
# Directory: /work05/manda/WRF.POST/K17/NCL/PT.EMAGRAM

. ./gmtpar.sh
echo "Bash script $0 starts."

range=330/380/100/1000
size=JX4/-4
xanot=a10f10
yanot=a100f100
anot=${xanot}/${yanot}WSne

indir=/work05/manda/WRF.POST/K17/NCL/PT.EMAGRAM/OUT_PT.EMAGRAM/K17.R08.00.00
in=$indir/K17.R08.00.00_d03_2017-07-03_12:00:00_129.5_33.3.txt
if [ ! -f $in ]; then
  echo Error in $0 : No such file, $in
  exit 1
fi
out=$(basename $in .txt).ps

awk '{if($1!="#" && $3>0)print $3,$1}' $in |\
psxy -R$range -${size} -W2/0/0/255 \
-X1.5 -Y4 -P -K >$out

awk '{if($1!="#" && $2>0)print $4,$1}' $in |\
psxy -R$range -${size} -W2/0/255/0 \
-K -O >>$out

psbasemap -R$range -${size} -B$anot -O -K >>$out

xoffset=
yoffset=4.5

export LANG=C

curdir1=$(pwd)
now=$(date)
host=$(hostname)

time=$(ls -l ${in} | awk '{print $6, $7, $8}')
#time1=$(ls -l ${in1} | awk '{print $6, $7, $8}')
timeo=$(ls -l ${out} | awk '{print $6, $7, $8}')

pstext <<EOF -JX6/1.5 -R0/1/0/1.5 -N -X${xoffset:-0} -Y${yoffset:-0} -O >> $out
0 1.50  9 0 1 LM $0 $@
0 1.35  9 0 1 LM ${now}
0 1.20  9 0 1 LM ${host}
0 1.05  9 0 1 LM ${curdir1}
0 0.90  9 0 1 LM INPUT: ${in} (${time})
0 0.75  9 0 1 LM INPUT:
0 0.60  9 0 1 LM INPUT:
0 0.45  9 0 1 LM OUTPUT: ${out} (${timeo})
EOF

echo
echo "INPUT : "
ls -lh --time-style=long-iso $in
echo "OUTPUT : "
ls -lh --time-style=long-iso $out
echo

echo "Done $0"
```

End of PL.VPROF.EPT.SEPT.sh
----------------------



