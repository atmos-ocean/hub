begin

setfileoption("nc","Format","LargeFile")

print("MMMMM HGT")
VAR="hgt"
LEV=300

INDIR="/work01/DATA/NCEP2/6HR/1981-2023/1981-2017"
INLST=systemfunc("ls "+INDIR+"/"+VAR+"*.nc")  

a=addfiles(INLST,"r")
hgt=a[:]->hgt(:,{LEV},:,:)
lon=a[0]->lon
lat=a[0]->lat

printVarSummary(hgt)

OFLE="HGT_1981-2017_"+LEV+".nc"
system("rm -v "+OFLE)
f=addfile(OFLE,"c")
f->hgt=hgt

hgt&lon=lon
hgt&lat=lat

print("MMMMM OUTPUT: "+OFLE)

delete([/INLST,OFLE,a,f/])





end

