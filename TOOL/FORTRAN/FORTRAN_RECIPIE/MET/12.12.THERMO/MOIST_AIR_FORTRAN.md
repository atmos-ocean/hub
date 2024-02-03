# 湿潤大気に関する諸量の計算   FORTRAN90

https://gitlab.com/infoaofd/lab/-/blob/master/GFD/AOFD/40.00.THERMODYNAMICS/XX01_MOIST_AIR.md

[[_TOC_]]

## 相当温位

気温, tc [C], 相対湿度, rh [%], 気圧, p [hPa] → 相当温位 [K]

```FORTRAN
REAL es,e,td,tk,dwpk,Tlcl,mixr, TDL
!INPUT: tc [degC]; p [hPa]; rh [%]
!OUTPUT: EPT [K]
```

```fortran
tk=tc+273.15
es=6.112*EXP(17.67*TC/(TC+243.5)) !hPa !BOLTON 1980
e=es*RH/100.0

td=(243.5*log(e/6.112))/(17.67-log(e/6.112))!Inverting Eq.10 of Bolton since es(Td)=e
dwpk= td+273.15

Tlcl= 1/(1/(dwpk-56)+log(tk/dwpk)/800)+56   !Eq.15 of Bolton (1980)

mixr= 0.62197*(e/(p-e))*1000                !Eq.4.1.2 (p.108) of Emanuel(1994) 

TDL=tk*((1000/(p-e))**0.2854)*(tk/Tlcl)**(0.28*0.001*mixr) !Eq.24 of Bolton

EPT=TDL*exp((3.036/Tlcl-0.00178)*mixr*(1.0+0.000448*mixr))   !Eq.39 of Bolton
```



## 温位

気温, `T` [K], 気圧, `p` [hPa] → 温位, `PT` [K]

```fortran
PT=T*(1000.0/p)**0.286
```



## 仮温位

温位, `PT` [K], 水蒸気混合比, `QV` [kg/kg] → 仮温位, `VPT` [K]

```fortran
VPT=PT*(1.0+0.608*QV)
```



## 仮温度

気温, `T` [K], 水蒸気混合比, `QV` [kg/kg] → 仮温度, `VT` [K]

```fortran
VT=T*(1.0+0.608*QV)
```



## 水蒸気混合比

気温, `tc` [C], 相対湿度, `RH` [%], 気圧, `p` [hPa] → 水蒸気混合比, `QV` [g/kg]

```fortran
tk=tc+273.15
es=6.112*EXP(17.67*TC/(TC+243.5)) !hPa !BOLTON 1980
e=es*RH/100.0
QV= 0.62197*(e/(p-e))*1000.0
```



## 比湿

 気圧, `p` [hPa], 水蒸気圧, `e` [hPa] → 比湿, `Q` [g/kg]

```fortran
real,parameter::eps=0.622
```

```fortran
Q=eps*e/(p-e*(1-eps))
```

水蒸気混合比, `QV` [g/kg] → 比湿, `Q` [g/kg]

```fortran
Q=QV/(1.0-QV)
```



## 飽和水蒸気圧

気温, `tc` [C] → 飽和水蒸気圧, `es` [hPa]

```fortran
tk=tc+273.15
es=6.112*EXP(17.67*TC/(TC+243.5)) !hPa !BOLTON 1980
```



## 水蒸気圧

気温, `tc` [C], 相対湿度, `RH` [%] → 水蒸気圧, `e` [hPa]

```fortran
tk=tc+273.15
es=6.112*EXP(17.67*TC/(TC+243.5)) !hPa !BOLTON 1980
e=es*RH/100.0
```

水蒸気混合比, `QV` [kg/kg], 気圧, `PhPa` [hPa] → 水蒸気圧, `e` [hPa]

```fortran
e=QV*PhPa/(QV+0.622)
```



## 気圧

密度, `rho` [kg/m3], 仮温度, `TV` [K] → 気圧, `P` [Pa]

```fortran
REAL,PARAMETER::Rd=287.04 !Gas constant of dry air
```

```fortran
P=rho*Rd*TV
```

