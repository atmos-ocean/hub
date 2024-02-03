print("MMMMM air")
VAR="air"
LEV=925

INDIR="/work01/DATA/NCEP2/6HR/1981-2023/1981-2017"
INLST=systemfunc("ls "+INDIR+"/"+VAR+"*.nc")  

a=addfiles(INLST,"r")
print(a)

air=a[:]->air(:,{LEV},:,:)
;printVarSummary(hgt)

OFLE="AIR_1981-2017_"+LEV+".nc"
system("rm -v "+OFLE)

f=addfile(OFLE,"c")
f->air=air

air&lon=lon
air&lat=lat

print("MMMMM OUTPUT: "+OFLE)

delete([/INLST,OFLE,a,f/])

