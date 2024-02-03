print("MMMMM air")
VAR="air"
LEV=925

TYEAR=2018
INDIR="/work01/DATA/NCEP2/6HR/1981-2023/2018-2023"
INLST=systemfunc("ls "+INDIR+"/"+VAR+"*"+TYEAR+"*.nc")  

a=addfiles(INLST,"r")
print(a)

air=a[:]->air(:,{LEV},:,:)
lon=a[0]->lon
lat=a[0]->lat
;printVarSummary(hgt)

OFLE="AIR_"+TYEAR+"_"+LEV+".nc"
system("rm -v "+OFLE)

f=addfile(OFLE,"c")
f->air=air

air&lon=lon
air&lat=lat

print("MMMMM OUTPUT: "+OFLE)

delete([/INLST,OFLE,a,f/])

