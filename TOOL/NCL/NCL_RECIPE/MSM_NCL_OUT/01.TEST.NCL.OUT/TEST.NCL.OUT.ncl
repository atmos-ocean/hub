; 
; TEST.NCL.OUT.ncl
; 
; Tue, 01 Nov 2022 18:47:17 +0900
; p5820.bio.mie-u.ac.jp
; /work03/am/2022.06.ECS.OBS/22.12.MSM.NCL/12.12.TEST.NCL.OUT
; am
;
script_name  = get_script_name()

NOW=systemfunc("date '+%y%m%d_%H%M' ")
HOST=systemfunc("hostname")
CWD=systemfunc("pwd")

IN  = getenv("NCL_ARG_2")
OUT = getenv("NCL_ARG_3")

if (fileexists(IN) .eq. False) then
print("ERROR: NO SUCH FILE, "+INFLE)
exit()
end if

f=addfile(IN,"r")

lon  = f->lon
lat  = f->lat
  p  = f->p
time = f->time
   z = f->z
   u = short2flt( f->u )
   v = short2flt( f->v )
temp = short2flt( f->temp )
  rh = short2flt( f->rh )



system("rm -vf "+OUT)

setfileoption("nc","Format","LargeFile")

a=addfile(OUT,"c")

a->lon  = lon 
a->lat  = lat 
a->p    = p
a->time = time
a->z    = z
a->u    = u
a->v    = v
a->temp = temp
a->rh   = rh 

print("")
print("Done " + script_name)
print("")
