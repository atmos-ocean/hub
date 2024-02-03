begin

VAR="TA10"
vName=VAR

nhar = 2  ;NUMER OF HARMONICS FOR SMOOTHING

INDIR="/work01/DATA/J-OFURO3/V1.2_PRE/HOURS.SINCE.1800-01-01/"+VAR+"/"
INLIST=systemfunc("ls "+INDIR+"*"+VAR+"*.nc")


a=addfiles(INLIST,"r")
;print(a)

ys=1988
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

time    = a[:]->time                       ; time:units = "hours since"
ymd     = cd_calendar(time, -2)            ; yyyymmdd
iStrt   = ind(ymd.eq.ymdStrt)              ; index start
iLast   = ind(ymd.eq.ymdLast)              ; index last 
delete(time)
delete(ymd)


time    = a[:]->time(iStrt:iLast)       ; time:units = "hours since"
TIME    = cd_calendar(time, 0)          ; type float 
year    = floattointeger( TIME(:,0) )
month   = floattointeger( TIME(:,1) )
day     = floattointeger( TIME(:,2) ) 
ddd     = day_of_year(year, month, day) 
yyyyddd = year*1000 + ddd               ; needed for input
print("yyyyddd(iStrt)="+yyyyddd(iStrt))
print("yyyyddd(iLast)="+yyyyddd(iLast))

longitude = a[0]->longitude
latitude  = a[0]->latitude

print("MMMMM READ "+VAR)
system("date -R")
x=a[:]->TA10(iStrt:iLast,:,:)

print("MMMMM DAILY CLIMATOLOGY AT EACH GRID POINT.")
system("date -R")
xClmDay = clmDayTLL(x, yyyyddd)     ; 
;printVarSummary(xClmDay)  

print("MMMMM SMOOTHING USING "+nhar+" HARMONICS.")
system("date -R")
xClmDay_sm = smthClmDayTLL(xClmDay, nhar)
printVarSummary(xClmDay_sm)




print("MMMMM CREATE NETCDF: CONVENIENCE USE SIMPLE METHOD.")
dimx   = dimsizes(xClmDay_sm)
ntim   = dimx(0)
nlat   = dimx(1)
mlon   = dimx(2)
year_day=xClmDay_sm&year_day

system("/bin/rm -f "+diro+filo)      ; rm any pre-exist file, if any
fnc    = addfile (diro+filo, "c")

filAtt = 0
filAtt@title         = vName+": Daily Climatology: "+yrStrt+"-"+yrLast  
filAtt@input_directory_ = INDIR
filAtt@nhar_ = "NUMBER OF HARMONICS="+nhar
filAtt@creation_date = systemfunc("date -R")
fileattdef( fnc, filAtt )         ; copy file attributes  

setfileoption(fnc,"DefineMode",True)

varNC_dc   = vName+"_dc"
varNC_sm   = vName+"_sm"

dimNames = (/"year_day", "latitude", "longitude"/)  
dimSizes = (/ ntim   ,  nlat,  mlon/) 
dimUnlim = (/ True , False, False/)   
filedimdef(fnc,dimNames,dimSizes,dimUnlim)

filevardef(fnc, "year_day"  ,typeof(year_day),getvardims(year_day)) 
filevardef(fnc, "latitude"   ,typeof(latitude) ,getvardims(latitude)) 
filevardef(fnc, "longitude"   ,typeof(longitude) ,getvardims(longitude))
filevardef(fnc, varNC_dc,typeof(xClmDay)   ,getvardims(xClmDay))    
filevardef(fnc, varNC_sm,typeof(xClmDay_sm),getvardims(xClmDay_sm))    

filevarattdef(fnc,"year_day"  ,year_day)          ; copy time attributes
filevarattdef(fnc,"latitude"   ,latitude)         ; copy lat attributes
filevarattdef(fnc,"longitude"  ,longitude)        ; copy lon attributes
filevarattdef(fnc,varNC_dc, xClmDay)                
filevarattdef(fnc,varNC_sm, xClmDay_sm)                

fnc->year_day       = (/year_day/)     
fnc->latitude        = (/latitude/)
fnc->longitude        = (/longitude/)
fnc->$varNC_dc$ = (/xClmDay/)
fnc->$varNC_sm$ = (/xClmDay_sm/)

print("MMMMM OUTPUT:")
print("OUT= "+diro+filo)

end

