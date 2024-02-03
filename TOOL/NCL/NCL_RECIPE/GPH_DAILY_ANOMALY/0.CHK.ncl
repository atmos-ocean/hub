
;************************************************
; This file is loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
;************************************************
begin
INROOT="/work01/DATA/"
DSET="NCEP2"
YYYY="2022"
MM="09"
DD="30"
HH="12"
Y=toint(YYYY)
M=toint(MM)
D=toint(DD)
H=toint(HH)
;CLON=300; 130 ;220 ;270.0       ; choose center lon
CLON=130 ;220 ;270.0       ; choose center lon
CLAT=45 

INDIR="./" ;INROOT+"/"+DSET+"/6HR/"+YYYY+"/"
NC1="uwnd_"+YYYY+MM+DD+"_"+HH+".nc"
NC2="vwnd_"+YYYY+MM+DD+"_"+HH+".nc"

f1 = addfile(INDIR+NC1,"r") 
f2 = addfile(INDIR+NC2,"r") 

V1 = f1->uwnd(0,{300},:,:)
V2 = f2->vwnd(0,{300},:,:)
printVarSummary(V1)

end

