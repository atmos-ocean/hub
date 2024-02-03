;
; Plot SST data in met_em file 
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
begin

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
min_lat=tofloat(\
systemfunc("grep min_lat "+nml+ "|cut -f2 -d'='|cut -f1 -d','") )
max_lat=tofloat(\
systemfunc("grep max_lat "+nml+ "|cut -f2 -d'='|cut -f1 -d','") )
min_lon=tofloat(\
systemfunc("grep min_lon "+nml+ "|cut -f2 -d'='|cut -f1 -d','") )
max_lon=tofloat(\
systemfunc("grep max_lon "+nml+ "|cut -f2 -d'='|cut -f1 -d','") )

print("DONE.")
print("")



print("CHECK NAMELIST FILE")

print("runname: "+runname)
print("indir:   "+indir)
print("wrfout:")
print("sdatetime: "+sdatetime)
print("edatetime: "+edatetime)
print("min_lat: "+min_lat)
print("max_lat: "+max_lat)
print("min_lon: "+min_lon)
print("max_lon: "+max_lon)

print("DONE.")
print("")



print("CREATE OUTPUT DIRECTOREY")

outdir="FIG_"+scriptname+"/"+runname+"/"+\
min_lon+"-"+max_lon+"-"+min_lat+"-"+max_lat
print("outdir="+outdir)
command="mkdir -p " + outdir
system(command)

print("DONE.")
print("")



print("OPEN INPUT FILE")

infle=indir+"/"+wrfout
print(systemfunc("ls -lh "+infle))
f1 = addfile(infle,"r")
; printVarSummary(f1)

print("DONE.")
print("")



print("CHECK TIME STEPS")

times=wrf_user_getvar(f1,"times",-1)
;printVarSummary(times)

dim=dimsizes(times)
nt=dim(0)
;print(nt)

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



print("TIME LOOP STARTS")

do n=ns,ne
print(n+" "+times(n))

pw=wrf_user_getvar(f1,"pw",n)

end do; n

print("TIME LOOP DONE.")
print("")


print("")
print("Input:  "+ infle)
print("")
print("min_lon="+min_lon+" max_lon="+max_lon)
print("min_lat="+min_lat+" max_lat="+max_lat)

end
