FUNCTION EPT_B80(TC,RH,P)

! EPT BY BOLTON (1980)

IMPLICIT NONE
REAL,INTENT(IN)::TC,RH,P
REAL EPT_B80

REAL es,e,td,tk,dwpk,Tlcl,mixr, TDL

!INPUT: tc [degC]; p [hPa]; rh [%]
!OUTPUT: EPT_B80 [K]

tk=tc+273.15

es=6.112*EXP(17.67*TC/(TC+243.5)) !hPa !BOLTON 1980
e=es*RH/100.0

td=(243.5*log(e/6.112))/(17.67-log(e/6.112))!Inverting Eq.10 of Bolton since es(Td)=e
dwpk= td+273.15

Tlcl= 1/(1/(dwpk-56)+log(tk/dwpk)/800)+56   !Eq.15 of Bolton (1980)

mixr= 0.62197*(e/(p-e))*1000                !Eq.4.1.2 (p.108) of Emanuel(1994) 

TDL=tk*((1000/(p-e))**0.2854)*(tk/Tlcl)**(0.28*0.001*mixr) !Eq.24 of Bolton

EPT_B80=TDL*exp((3.036/Tlcl-0.00178)*mixr*(1.0+0.000448*mixr))   !Eq.39 of Bolton

RETURN
END
