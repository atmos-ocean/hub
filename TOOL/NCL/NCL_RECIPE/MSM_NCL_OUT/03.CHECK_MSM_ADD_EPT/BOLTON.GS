function BOLTON ( temp, rh, lev )
'tc=(temp-273.15)'
'es= 6.112*exp((17.67*tc)/(tc+243.5))'         ;# Eq.10 of Bolton (1980)
'e=0.01*rh*es'                                 ;# Eq.4.1.5 (p. 108) of Emanuel (1994)
'td=(243.5*log(e/6.112))/(17.67-log(e/6.112))' ;# Inverting Eq.10 of Bolton since es(Td)=e
'dwpk= td+273.15'
'Tlcl= 1/(1/(dwpk-56)+log(temp/dwpk)/800)+56' ;#Eq.15 of Bolton (1980)
'mixr= 0.62197*(e/(lev-e))*1000'              ;# Eq.4.1.2 (p.108) of Emanuel(1994) 
'TDL=temp*pow(1000/(lev-e),0.2854)*pow(temp/Tlcl, 0.28*0.001*mixr)'
;#Eq.24 of Bolton
'ept=TDL*exp((3.036/Tlcl-0.00178)*mixr*(1.0+0.000448*mixr))' ;#Eq.39 of Bolton
return ept
