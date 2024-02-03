; 
; NCL.OUT.ncl
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
;printVarSummary(temp)

p3=conform_dims(dimsizes(temp),p,1)
;printVarSummary(p3)
;print(p3(0,2,10,10))

print("mmmmm QV")
qv=mixhum_ptrh(p3,temp,rh,1)
copy_VarMeta(temp,qv)
qv@units="kg/kg"
qv = where(qv .le. 1e+36, qv, temp@_FillValue)
qv@_FillValue = temp@_FillValue
qv@standard_name = "qvapor"
qv@long_name="Water vapor mixing ratio"
;printVarSummary(qv)
;print(qv(0,1,200,10))



p3pa=p3*100.0 ;hPa -> Pa



print("mmmmm PT")
pt = pot_temp(p3pa, temp, -1, False)
copy_VarMeta(temp,pt)
pt = where(pt .le. 5000., pt, temp@_FillValue)
pt@standard_name = "PT"
pt@long_name="potential temperature"
;printVarSummary(pt)
;print(pt(0,1,200,10))



print("mmmmm VPT")
vpt=pt*(1.0+0.608*qv)
copy_VarMeta(temp,vpt)
vpt = where(pt .le. 5000., vpt, temp@_FillValue)
vpt@standard_name = "VPT"
vpt@long_name="Virtual potential temperature"
;printVarSummary(vpt)
;print(vpt(0,1,200,10))



print("mmmmm EPT")

;;;ept = wrf_eth ( qv, temp, p3pa )

tc=(temp-273.15)
es= 6.112*exp((17.67*tc)/(tc+243.5))         ;# Eq.10 of Bolton (1980)
e=0.01*rh*es                                 ;# Eq.4.1.5 (p. 108) of Emanuel (1994)
td=(243.5*log(e/6.112))/(17.67-log(e/6.112)) ;# Inverting Eq.10 of Bolton since es(Td)=e
dwpk= td+273.15
Tlcl= 1/(1/(dwpk-56)+log(temp/dwpk)/800)+56  ;#Eq.15 of Bolton (1980)
mixr= 0.62197*(e/(p3-e))*1000                ;# Eq.4.1.2 (p.108) of Emanuel(1994) 
TDL=temp*(1000/(p3-e))^0.2854*(temp/Tlcl)^(0.28*0.001*mixr)
;#Eq.24 of Bolton
ept=TDL*exp((3.036/Tlcl-0.00178)*mixr*(1.0+0.000448*mixr)) ;#Eq.39 of Bolton


copy_VarMeta(temp,ept)
ept = where(ept .le. 5000., ept, temp@_FillValue)
ept@_FillValue = temp@_FillValue
ept@standard_name = "EPT"
ept@long_name="Equivalent potential temperature"
delete(ept@description)
;printVarSummary(ept)
;print(ept(0,1,200,10))



print("mmmmm OUTPUT")
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
a->qv   = qv 
a->pt   = pt
a->vpt  = vpt 
a->ept  = ept 

print("")
print("Done " + script_name)
print("")
