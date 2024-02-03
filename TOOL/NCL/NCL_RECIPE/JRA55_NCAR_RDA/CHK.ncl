; 
; CHK.ncl
; 
; Wed, 20 Jul 2022 13:40:20 +0900
; calypso.bosai.go.jp
; /work04/manda/2022.PV_INVERSION_TEST/22.02.2022-06_ECS_JRA/10.02.PLT_RAW
; manda
;
script_name  = get_script_name()
script=systemfunc("basename "+script_name+ " .ncl")
;LOG=script+".LOG"

NOW=systemfunc("date '+%y%m%d_%H%M' ")
;print("NOW="+NOW)
; LOG=script+NOW+".LOG"

NOW=systemfunc("date -R")
HOST=systemfunc("hostname")
CWD=systemfunc("pwd")

print("script="+script_name)

VAROUT="TMP"
TYP="png"
step=0
yyyymmddhh=2022061900
indir    = "INDIR/"
;infile = "anl_p125.011_tmp.2022060100_2022063018"
infile = "anl_p125.034_vgrd.2022060100_2022063018"
in     = addfile(indir+infile,"r")
print(in)

lev = in->lv_ISBL1   ; hPa
lev = tofloat(lev)*100  ; convert units

print(lev)

time=in->initial_time0_hours
utc_date=cd_calendar(time, 0)
year  = tointeger(utc_date(:,0))
month = tointeger(utc_date(:,1))
day   = tointeger(utc_date(:,2))
hour  = tointeger(utc_date(:,3))
dim=dimsizes(year) 

;do n=0,dim-1
;date_str = sprinti("%0.4i", year)+sprinti("%0.2i",month)+\
;sprinti("%0.2i",day)+" "+sprinti("%0.2i",hour)
;
;print(date_str) 
;end do ;n
