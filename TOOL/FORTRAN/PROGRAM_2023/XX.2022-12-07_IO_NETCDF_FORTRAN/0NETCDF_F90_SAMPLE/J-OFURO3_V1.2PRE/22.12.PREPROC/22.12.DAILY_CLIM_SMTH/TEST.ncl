begin

VAR="QA"
vName="QA"

nhar = 2  ;NUMER OF HARMONICS FOR SMOOTHING

INDIR="/work01/DATA/J-OFURO3/V1.2_PRE/HOURS.SINCE.1800-01-01/"+VAR+"/"
INLIST=systemfunc("ls "+INDIR+"*"+VAR+"*.nc")


a=addfiles(INLIST,"r")
;print(a)

ys=1998
ye=2022

ymdStrt = ys*10000+0101                         ; start yyyymmdd
ymdLast = ye*10000+1231                         ; last  
print("ymdStrt="+ymdStrt)
print("ymdLast="+ymdLast)

yrStrt  = ymdStrt/10000
yrLast  = ymdLast/10000

diro="/work01/DATA/J-OFURO3/V1.2_PRE/DAILY_CLIM/"
system("mkdir -vp "+diro)
filo="DAILY_CLIM_SMO_"+ys+"-"+ye+"_"+VAR+".nc"
print("MMMMM filo="+filo)
end
