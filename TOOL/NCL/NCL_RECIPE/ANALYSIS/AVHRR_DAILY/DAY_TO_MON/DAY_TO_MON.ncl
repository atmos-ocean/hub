
scriptname_in = getenv("NCL_ARG_1")
scriptname=systemfunc("basename " + scriptname_in + " .ncl")
YYYYMM    = getenv("NCL_ARG_2")

print("script name ="+scriptname_in)
print("YYYYMM         ="+YYYYMM)
print("")


INDIR="/work05/manda/DATA/SST/OISST/AVHRR-ONLY/1982-2019_1201-0301"
PREFIX="oisst-avhrr-v02r01."

print("INDIR="+INDIR)
print("PREFIX="+PREFIX)
print("YYYYMM="+YYYYMM)

PREFIXOUT="OI_AVHRR_v02r01_MAVE_"

infles = systemfunc("ls "+INDIR+"/"+PREFIX+YYYYMM+"*.nc")  
f = addfiles(infles,"r")
;dsizes = getfiledimsizes(f[0])
;print(dsizes)

sstday = f[:]->sst
time= f[:]->time
zlev=f[0]->zlev
lat=f[0]->lat
lon=f[0]->lon
printVarSummary(sstday)
printVarSummary(time)

timeave=dim_avg_n_Wrap(sstday&time,0)
printVarSummary(timeave)
utc_date = cd_calendar(timeave, 0)
 year   = tointeger(utc_date(:,0))    ; Convert to integer for
   month  = tointeger(utc_date(:,1))    ; use sprinti 
   day    = tointeger(utc_date(:,2))
   hour   = tointeger(utc_date(:,3))
   minute = tointeger(utc_date(:,4))
   second = utc_date(:,5)

date_str = sprinti("%0.2iZ ", hour) + sprinti("%0.2i ", day) + \
           sprinti("%0.2i ", month) + " "  + sprinti("%0.4i", year)
 
print(date_str) 

ntim  = 1; get dimension sizes  
nlev  = dimsizes(zlev)                                               
nlat  = dimsizes(lat)  
nlon  = dimsizes(lon)      
sst=new((/ntim,nlev,nlat,nlon/),typeof(sstday))



sst(0,:,:,:)=toshort(dim_avg_n_Wrap(sstday,0))

sst!0    = "time"                     
sst!1    = "zlev"
sst!2    = "lat"                      
sst!3    = "lon"
sst&time = timeave
sst&zlev = sstday&zlev
sst&lat  = sstday&lat                 
sst&lon  = sstday&lon 
sst@long_name = "SST MONTHLY AVE"     
sst@units     = "Celsius"

copy_VarAtts(sstday,sst)
copy_VarAtts(sstday&time,sst&time)
copy_VarAtts(sstday&lon,sst&lon)

printVarSummary(sst)


diro = "./"                     ; Output directory
filo = PREFIXOUT+YYYYMM+".nc"             ; Output file
system("/bin/rm -f " + diro + filo)    ; remove if exists
fout  = addfile (diro + filo, "c")  ; open output file

dimNames = (/"time", "lat", "lon", "zlev"/)  
dimSizes = (/ -1   ,  nlat,  nlon, nlev /) 
dimUnlim = (/ True , False, False, False/)   
filedimdef(fout,dimNames,dimSizes,dimUnlim)

filevardef(fout, "time" ,typeof(time),getvardims(time)) 
filevardef(fout, "zlev" ,typeof(zlev),getvardims(zlev) )                           
filevardef(fout, "lat"  ,typeof(lat),getvardims(lat))                          
filevardef(fout, "lon"  ,typeof(lon),getvardims(lon))                          
filevardef(fout, "sst"  ,typeof(sst)  ,getvardims(sst))

filevarattdef(fout,"time",time)
filevarattdef(fout,"zlev",zlev)
filevarattdef(fout,"lat",lat)
filevarattdef(fout,"lon",lon)
filevarattdef(fout,"sst",sstday)

setfileoption(fout,"DefineMode",False)

fout->time   = (/timeave/)     
fout->zlev    = (/zlev/)
fout->lat    = (/lat/)
fout->lon    = (/lon/) 
fout->sst    = (/sst/)


print("----------------------------")
system("ls --time-style=long-iso -lh "+diro + filo)
print("----------------------------")
