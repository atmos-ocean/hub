VAR="hgt"
LEV=300
print("MMMMM HGT "+LEV)

TYEAR=2018
INDIR="/work01/DATA/NCEP2/6HR/1981-2023/2018-2023"
INLST=systemfunc("ls "+INDIR+"/"+VAR+"*"+TYEAR+"*.nc")  

a=addfiles(INLST,"r")
print(a)

hgt=a[:]->hgt(:,{LEV},:,:)
lon=a[0]->lon
lat=a[0]->lat
;printVarSummary(hgt)

OFLE=VAR+"_"+TYEAR+"_"+LEV+".nc"
system("rm -vf "+OFLE)

f=addfile(OFLE,"c")
f->hgt=hgt

hgt&lon=lon
hgt&lat=lat

print("MMMMM OUTPUT: "+OFLE)

delete([/INLST,OFLE,a,f/])

