From schlatter@profsc.fsl.noaa.gov Wed Jun 12 16:25:46 1991
General information:
--------------------
This is an index of the thermodynamic subprograms located in THERMO.OLB.  They
are listed according to the general type of calculation that is desired,
divided into the following categories: 

        (a) moisture parameters
        (b) latent heat
        (c) pressure
        (d) temperature
        (e) thickness

These algorithms were collected, edited, commented, and tested by Thomas W.
Schlatter and Donald V. Baker from August to October 1981 in the PROFS Program
Office, NOAA Environmental Research Laboratories, Boulder, Colorado.  Where
possible, credit has been given to the original author of the algorithm and a
reference provided.


The input/output units are as follows:
```
Temperature                     Celsius
Pressure                        millibars
Relative humidity               percent
Saturation specific humidity    g vapor/kg moist air
Mixing ratio                    g vapor/kg dry air
Thickness                       meters
Precipitable water              centimeters
Latent heat                     joules/kg
```

The following symbols are used in subprogram calls:
```
EW      water vapor pressure
EQPT    equivalent potential temperature
P       pressure
PC      pressure at the convective condensation level
PS      surface pressure
RH      relative humidity
T       temperature
TC      temperature at the lifting condensation level
TD      dew point
TH      potential temperature (a dry adiabat)
THW     wet bulb potential temperature (a moist adiabat)
W       mixing ratio
WBAR    mean mixing ratio from surface to pressure pm
```
Note:  all routines are function subprograms except for "PTLCL", which is a
       subroutine.


Index:  routines available for various calculations
---------------------------------------------------
(A) Moisture Parameters

     To Calculate:                      Available Routine(s):
    
     1. Saturation mixing ratio         1. a) WMR(P,T)
                                           b) W(T,P)
    
     2. Precipitable water              2. PRECPW(TD,P,N)
        (Td and P are dimensioned by N)
    
     3. Relative humidity               3. HUM(T,TD)
    
     4. Saturation specific humidity    4. SSH(P,T)


(B) Latent Heat

     To Calculate Latent Heat Of:       Available Routines:
    
     1. Evaporization/Condensation      1. HEATL(1,T)
    
     2. Melting/Freezing                2. HEATL(2,T)
    
     3. Sublimation/Deposition          3. HEATL(3,T)


 (C) Pressure

     To Calculate:                      Available Routine(s):
    
     1. Pressure at Lifting             1. a) ALCL(T,TD,P)
        Condensation Level                 b) PCON(P,T,TC)
                                           c) (subroutine) PTLCL(P,T,TD,PC,TC)
    
     2. Saturation vapor pressure       2. a) ESW(T)
        over liquid water                  b) ES(T)
                                           c) ESAT(T)
                                           d) ESGG(T)
                                           e) ESRW(T)
                                           f) ESLO(T)
    
     3. Saturation vapor pressure       3. a) ESICE(T)
        over ice                           b) ESILO(T)
    
     4. Pressure at Convective          4. PCCL(PM,P,T,TD,WBAR,N)
        Condensation Level (P, T, and
        Td are dimensioned by N)


(D) Temperature

     To Calculate:                      Available Routine(s):
    
     1. Difference in Wet Bulb Poten-   1. WOBF(T)
        tial Temperatures for two
        parcels of air at the same
        temperature - one saturated
        and the other completely dry
    
     2. Equivalent temperature          2. TE(T,TD,P)
    
     3. Equivalent potential            3. a) OE(T,TD,P)
        temperature                        b) EPT(T,TD,P)
                                           c) OS(T,P)
    
     4. Dew point                       4. a) DPT(EW)
                                           b) DEWPT(EW)
                                           c) DWPT(T,RH)
    
     5. Wet bulb potential              5. a) POWT(T,P,TD)
        temperature                        b) OW(T,TD,P)
                                           c) THM(W,P)
    
     6. Potential temperature           6. O(T,P)
    
     7. Convective temperature          7. CT(WBAR,PC,PS)
    
     8. Virtual temperature             8. TV(T,TD,P)
    
     9. Wet bulb temperature            9. TW(T,TD,P)
    
    10. Temperature at Lifting         10. a) TCON(T,TD)
        Condensation Level                 b) TLCL(T,TD)
                                           c) (subroutine) PTLCL(P,T,TD,PC,TC)
                                           d) TLCL1(T,TD)
    
    11. Temperature along moist        11. SATLFT(THW,P)
        adiabat at given pressure
    
    12. Temperature on a dry adiabat   12. TDA(TH,P)
        at given pressure
    
    13. Temperature on a moist adiabat 13. a) TSA(EQPT,P)
        corresponding to an Equivalent     b) TMLAPS(EQPT,P)
        Potential Temperature "EQPT",
        at given pressure
    
    14. Temperature on a mixing ratio  14. TMR(W,P)
        line at given pressure


 (E) Thickness

     To Calculate:                      Available Routine:
    
     1. Thickness between the surface   1. Z(PT,P,T,TD,N)
        and any other pressure level
        PT in the sounding.  (P,T, and
        Td are dimensioned by N.)


Notes prepared by D. Baker, 24 Dec 86.

General information:
---
The following tables give results of the tests performed on routines in [BAKER.THERMO]THERM.FOR. These routines calculate thermodynamic parameters that are of interest when analyzing soundings. the "Smithsonian Meterological Tables" were utilized to test the accuracy of each routine where possible...otherwise, a Skew T-Log p chart was used. In cases where more than one routine calculates a given parameter, an efficiency test is also included. For the efficiency tests, a specified number of calculations was done by each routine using identical input. After 10 trials, the average time needed by each routine was determined and is listed, thereby indicating which routine is most efficient. for further information involving any of the routines to follow, refer to documentation in [BAKER.THERMO]THERM.FOR.

Computer used:
VAX 11/780 @ NOAA/ERL/PROFS...Boulder, Colorado.

Input/output units:
```
Temperature............celsius
Pressure...............millibars
Mixing ratio...........gm vapor/kg dry air
Relative humidity......percent
Sat specific humidity..gm vapor/kg moist air
```

Note: Smith = Smithsonian table value. Where these values do not       appear, the routines have been verified using a Skew T-Log p chart due to lack of an applicable table.  The number in parentheses next to "Smith" indicates the page number (6th revised edition) of the table used for the test.

---

TEST OF FUNCTIONS ESW,ES,ESAT,ESGG,ESRW,ESLO
---
PURPOSE: CALCULATE SATURATION VAPOR PRESSURE OVER LIQUID WATER
         GIVEN TEMPERATURE.
```
TEMP  ESW     ES     ESAT    ESGG    ESRW    ESLO    SMITH.(340)
---- -----  -----   ------  ------  ------  ------   ------
-50 .06356  .06357  .06356  .06356  .06362  .06337   .06356
-30 .50878  .51036  .50878  .50880  .50843  .50877   .5088
-10 2.8627  2.8677  2.8626  2.8627  2.8625  2.8635   2.8627
  0 6.1080  6.1121  6.1076  6.1078  6.1084  6.1078   6.1078
 10 12.272  12.272  12.272  12.272  12.274  12.271   12.272
 20 23.372  23.370  23.372  23.373  23.375  23.371   23.373
 30 42.430  42.457  42.429  42.430  42.431  42.429   42.430
```
```
EFFICIENCY TEST: 5000 CALCULATIONS AT T=20C.
           FUNC.  T(SEC)
           ----   ------
           ESW     1.12
           ES      0.88
           ESAT    4.50
           ESGG    4.63
           ESRW    1.08
           ESLO    0.64
```

TEST OF FUNCTIONS WMR,W
---
PURPOSE: CALCULATE MIXING RATIO GIVEN PRESSURE AND TEMPERATURE.
```
PRES  TEMP     WMR      W       SMITH.(302)
----  ----   ------   ------    ------
1000   30    27.699   27.560    27.69
1000    0     3.840    3.822     3.839
 850   10     9.147    9.112     9.146
 700   10    11.135   11.099    11.13
 500  -20     1.568    1.564     1.568
 400  -30     0.794    0.792     0.794
```
```
EFFICIENCY TEST: 2000 CALCULATIONS AT T=-10C, P=850MB.

           FUNC.  T(SEC)
           -----  ------
           WMR     0.82
           W       1.86
```

TEST OF FUNCTIONS TCON,TLCL,TLCL1
---
PURPOSE: CALCULATE TEMPERATURE AT THE LCL GIVEN TEMPERATURE AND
         DEW POINT.
```
TEMP  DWPT    TCON    TLCL    TLCL1    SMITH.(329)
----  ----   ------  ------  ------    ------
 40    20    15.473  15.449  15.450    15.48
-20   -45   -48.703 -48.768 -48.689   -48.79
 10   -20   -25.237 -25.295 -29.401   -25.29
  0   -25   -29.277 -29.331 -29.401   -29.31
 30    10     5.708   5.682   5.679     5.72
 40    35    33.710  33.727  33.636    33.74
```
```
EFFICIENCY TEST: 5000 CALCULATIONS AT T=20C, TD= 10C.

           FUNC.  T(SEC)
           -----  ------
           TCON    0.65
           TLCL    0.96
           TLCL1   1.70
```

TEST OF FUNCTION PCON
---
PURPOSE: CALCULATE LCL PRESSURE GIVEN INITIAL TEMPERATURE, DEW
         POINT, AND LCL TEMPERATURE (TEML).
```
PRES  TEMP  TEML    PCON
----  ----  ----   ------
1000   35    20    839.59
 975   30    20    866.89
 950   15   -10    691.24
 900   10   -25    566.87
 500  -20   -20    500.00
 500  -20   -50    321.40
```

TEST OF SUBROUTINE PTLCL, AND FUNCTION ALCL
---
PURPOSE: GIVEN TEMPERATURE, DEW POINT, AND PRESSURE... PLTLCL
         ESTIMATES LCL TEMPERATURE AND PRESSURE; ALCL CALC-
         ULATES LCL PRESSURE ONLY.
```
PRES  TEMP  DWPT   PTLCL:P  PTLCL:T   ALCL
----  ----  ----   -------  -------  ------
1000   35    20     811.46   16.63   806.71
 900   25    20     837.04   18.83   840.12
1000   30    28     971.57   27.51   973.34
 850    0   -30     544.92  -34.66   528.01
 900   16    16     900.00   16.00   900.00
 700  -10   -60     333.50  -65.69   300.97
```

TEST OF FUNCTION WOBF
---
PURPOSE: CALCULATE THE DIFFERENCE WBPTS-WBPTD (SEE NOTES IN 
         [SCHLATTER]THERMO.FOR FOR EXPLANATION) GIVEN THE TEMPER-
         ATURE. IN THIS CASE, EQUIVALENT POTENTIAL TEMPERATURE
         WAS INPUT SO RESULTS COULD BE CHECKED USING THE
         SMITHSONIAN TABLES.
```
 EQPT   WOBF(=EQPT-WBPT)  SMITH.(319)
----   ---------------   ------
205.24     165.2104       165.24
161.04     125.0629       125.04
113.04      83.1266        83.04
 70.34      48.8294        48.34
 54.84      37.4419        36.84
 42.14      28.6543        28.14
  6.74       8.8176         8.74
```

TEST OF FUNCTIONS POWT,OW
---
PURPOSE: CALCULATE THE WET-BULB POTENTIAL TEMPERATURE GIVEN THE
         TEMPERATURE, DEW POINT, AND PRESSURE.
```
TEMP  DWPT  PRES   POWT    OW
----  ----  ----  -----   -----
 35    25   1000  27.55   27.61
 10   -15   1000   2.13    1.83
-10   -60    850  -4.64   -4.68
 15    10    900  16.29   16.42
 37    30   1050  29.98   30.13
 -5    -5    500  22.05   22.22
```
```
EFFICIENCY TEST: 500 CALCULATIONS AT T=10C, TD=-10C, P=850MB.

           FUNC.  T(SEC)
           -----  ------
           POWT    0.53
           OW     17.31
```

TEST OF FUNCTIONS DPT,DEWPT
---
PURPOSE: CALCULATE DEW POINT GIVEN SATURATION VAPOR PRESSURE.
```
SAT.VP.    DPT    DEWPT    SMITH.(351)
-------  ------  ------    ------
0.1891  -40.002  -40.025    -40
1.2540  -20.000  -20.032    -20
2.8627  -10.000  -10.022    -10
6.1078    0.000   -0.010      0
12.272   10.000   10.000     10
23.373   20.000   20.003     20
```
```
EFFICIENCY TEST: 3000 CALCULATIONS AT SVP=.1891MB.

           FUNC.  T(SEC)
           -----  ------
           DPT     1.92
           DEWPT   0.59
```

TEST OF FUNCTION SATLFT
---
PURPOSE: CALCULATE TEMPERATURE WHERE A MOIST ADIABAT CROSSES A GIVEN
         PRESSURE GIVEN THE VALUE OF THE MOIST ADIABAT AND THE 
         PRESSURE.
```
  PRES  THETAW   SATLFT   SMITH.
-----  ------  -------   ------
 204.3    34    -19.791    -20
 200      20    -60.356    -62
1071.4    38     40.151     40
 501      32      9.941     10
 422.7     0    -51.876    -52
```
TEST OF FUNCTION HUM
---
PURPOSE: CALCULATE RELATIVE HUMIDITY GIVEN TEMPERATURE AND 
         DEW POINT.
```
TEMP  DWPT   HUM
----  ----  -----
 35    30   75.46
 25    10   38.77
  0   -15   31.18
 20   -10   12.22
 30    28   89.09
```

TEST OF FUNCTION TW
---
PURPOSE: CALCULATE WET-BULB TEMPERATURE GIVEN TEMPERATURE, DEW
         POINT, AND PRESSURE.
```
TEMP  DWPT  PRES   TW
----  ----  ----  -----
 30    15   1000  19.99
  0   -20    700  -6.05
 10     0    850   5.11
-15   -25    500 -17.39
 25    20    900  21.57
```

TEST OF FUNCTIONS OE,EPT
---
PURPOSE: CALCULATE EQUIVALENT POTENTIAL TEMPERATURE GIVEN TEMP-
         ERATURE, DEW POINT, AND PRESSURE.
```
TEMP  DWPT  PRES    OE     EPT
----  ----  ----   -----  -----
 30    15   1000   62.24  62.49
 0    -20    700   33.10  33.01
 10     0    850   36.97  37.01
-15   -25    500   45.04  45.02
 25    20    900   85.02  84.60
```
```
EFFICIENCY TEST: 1000 CALCULATIONS AT T=20C, TD=10C, P=1000MB.

           FUNC.  T(SEC)
           -----  ------
           OE      18.50
           EPT      0.80
```
TEST OF FUNCTION TE
---
PURPOSE: CALCULATE EQUIVALENT TEMPERATURE GIVEN TEMPERATURE, DEW
         POINT, AND PRESSURE.
```
TEMP  DWPT  PRES    TE
----  ----  ----   -----
 30    15   1000   62.24
  0   -20    700    3.31
 10     0    850   22.88
-15   -25    500  -12.18
 25    20    900   74.38
```

TEST OF FUNCTION TDA
---
PURPOSE: CALCULATE THE TEMPERATURE ON A DRY ADIABAT GIVEN THE VALUE
         OF THE DRY ADIABAT AND THE PRESSURE.
```
 THETA  PRES     TDA     SMITH.(308)
-----  ----    ------   ------
132.74   250     -0.12      0
 35.44   500    -20.05    -20
-14.66  1050    -11.03    -11
 28.64   850     14.93    -15
 18.24   700    -10.02    -10
```

TEST OF FUNCTIONS TSA,TMLAPS
---
PURPOSE: CALCULATE TEMPERATURE ON A MOIST ADIABAT GIVEN 
         EQUIVALENT POTENTIAL TEMPERATURE AND PRESSURE.
```
  EQPT    PRES     TSA      TMLAPS    SMITH.(319)
------   ------   ------    -------   ------
181.64   870.9    34.170    33.940     34
113.04   577.0    12.139    12.080     12
 70.34   896.0    18.057    17.950     18
 10.24   496.0   -41.766   -41.669    -42
 54.84   309.5   -39.658   -39.600    -40
```
```
EFFICIENCY TEST: 500 CALCULATIONS AT EQPT=113.04, P=577MB

           FUNC.  T(SEC)
           -----  ------
           TSA     7.16
           TMLAPS  6.81
```

TEST OF FUNCTION O
---
PURPOSE: CALCULATE POTENTIAL TEMPERATURE GIVEN TEMPERATURE AND
         PRESSURE.
```
TEMP  PRES      O    SMITH.(309)
----  ----    -----  ------
-35   850    -23.66  -23.66
-10   500     47.74   47.74
-20   950    -16.26  -16.26
 10  1000     10.04   10.00
```

TEST OF FUNCTION OS
---
PURPOSE: CALCULATE EQUIVALENT POTENTIAL TEMPERATURE FOR AIR SAT-
         URATED AT GIVEN TEMPERATURE AND PRESSURE.
```
TEMP   PRES    OS      SMITH.(319)
----  -----  ------   -------
  0   275.3  179.88    181.64
-10   321.4  112.03    113.04
 10   501.0  126.27    127.34
 10   816.0   54.89     54.84
-50   282.7   47.63     48.14
```

TEST OF FUNCTION TMR
---
PURPOSE: CALCULATE THE TEMPERATURE ALONG A GIVEN MIXING RATIO
         LINE AT A GIVEN PRESSURE.
```
  W   PRES   TMR    SMITH.(302)
----- ----  -----   ------
9.146  850  10.07    10
1.120  700 -19.96   -20
7.710  500   0.03     0
1.960  400 -19.97   -20
14.95 1000  20.10    20
```

TEST OF FUNCTION THM
---
PURPOSE: CALCULATE WET-BULB POTENTIAL TEMPERATURE OF A PARCEL
         SATURATED AT A GIVEN TEMPERATURE AND PRESSURE.
```
TEMP    PRES     THM     SMITH.(319)
----   ------   ------   ------
 12    369.6    46.277    40
-32    188.2    41.376    32
 38    1069.8   35.751    36
 10     908.    14.205    14
-14     648.     7.841     8
-50    301.7    13.961    14
```

TEST OF FUNCTION DWPT
---
PURPOSE: CALCULATE DEW POINT GIVEN TEMPERATURE AND RELATIVE HUM-
         IDITY.
```
TEMP  REL.HUM.   TD     TD*
----  -------- ------  -----
 35    75.46   30.14    30
 25    38.77    9.93    10
  0    31.18  -15.19   -15
 20    12.22  -10.16   -10
 30    89.09   28.01    28
```
         * INPUT RELATIVE HUMIDITY FROM OUTPUT OF FUNCTION HUM.
           EXPECT DEW POINT OUTPUT TO BE APPROX. DEW POINT INPUT
           USED IN "HUM". SEE TEST OF FUNCTION HUM FOR COMPARISON.


TEST OF FUNCTION SSH
---
PURPOSE: CALCULATE SATURATION SPECIFIC HUMIDITY GIVEN THE 
         PRESSURE AND TEMPERATURE.
```
PRES  TEMP   SSH          !CHECKED ON HAND CALCULATOR.
----  ----  -----
1000   10    7.70
1000    0    3.82
 850  -10    2.11
 700    0    5.46
 500  -20    1.57
 300  -30    1.06
```

TEST OF FUNCTION TV
---
PURPOSE: CALCULATE VIRTUAL TEMPERATURE GIVEN TEMPERATURE, DEW POINT,
         AND PRESSURE.
```
TEMP  DWPT  PRES    TV        !CHECKED ON HAND CALCULATOR.
----  ----  ----   -----
 30    25   1000   33.69
 20     0   1000   20.68
 10   -10    850   10.36
  0   -10    700    0.42
-20   -30    500  -19.90
-40   -60    300  -39.99
```

TEST OF FUNCTIONS ESICE,ESILO
---
PURPOSE: CALCULATE SATURATION VAPOR PRESSURE OVER ICE GIVEN TEMP-
         ERATURE.
```
TEMP   ESICE    ESILO    SMITH.(360)
----  -------  -------   ------
-50   .03935   .03963   .03935
-40   .1283    .1283    .1283
-30   .3798    .3796    .3798
-20   1.032    1.032    1.032
-10   2.597    2.596    2.597
```
```
EFFICIENCY TEST: 2000 CALCULATIONS AT T=-10C.

           FUNC.  T(SEC)
           -----  ------
           ESICE   1.10
           ESILO   0.39
```

TEST OF FUNCTION HEATL
---
PURPOSE: GIVEN TEMPERATURE, TO CALCULATE.....
         1) LATENT HEAT OF EVAPORATION/CONDENSATION (KEY=1)
         2) LATENT HEAT OF FREEZING/MELTING         (KEY=2)
         3) LATENT HEAT OF SUBLIMATION/DEPOSITION   (KEY=3)
 (RESULTS VERIFIED USING P.343, SMITH.)
```
TEMP    KEY=1     SMITH.    KEY=2      SMITH.     KEY=3      SMITH.
----  --------    -------  --------    ------   ---------    ------
-100                                            674.2192     674.4
 -80                                            676.1257     676.3
 -60                                            677.3396     677.5
 -50  628.1667    629.3     48.7133     48.6
 -40                        56.1209     56.3    677.8610     678.0
 -30  615.5021    615.0     62.9107     63.0    
 -20                        69.0828     69.0    677.6898     677.9
 -10  603.2438    603.0     74.6372     74.5    677.3444     677.5
   0  597.2670    597.3     79.5740     79.7    676.8259     677.0
  10  591.3918    591.7
  20  585.6181    586.0
  40  574.3755    574.7
  50  568.9066    569.0
```
NOTE: HEATL IS DESIGNED TO RETURN THE LATENT HEATS IN UNITS OF     JOULES/KG.  TO MAKE RESULTS COMPATIBLE WITH VALUES IN THE      SMITHSONIAN TABLES (IT-CAL/GRAM), ANSWERS WERE MULTIPLIED BY THE CORRECTION FACTOR 2.3884E-04 TO OBTAIN THOSE SHOWN ABOVE.

Fortran90 SUBROUTINES
---
```fortran
  FUNCTION alcl(t,td,p)

!    g.s. stipanuk     1973               original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages https://apps.dtic.mil/dtic/tr/fulltext/u2/769739.pdf
!    baker, schlatter  17-may-1982

!   this function returns the pressure alcl (mb) of the lifting conden-
!   sation level (lcl) for a parcel initially at temperature t (celsius)
!   dew point td (celsius) and pressure p (millibars). alcl is computed
!   by an iterative procedure described by eqs. 8-12 in stipanuk (1973),
!   pp.13-14.
!   determine the mixing ratio line through td and p.

  aw = w(td,p)

!   determine the dry adiabat through t and p.

  ao = o(t,p)

!   iterate to locate pressure pi at the intersection of the two
!   curves. pi has been set to p for the initial guess.

  3      CONTINUE
  pi = p
  DO i= 1,10
    x= .02*(tmr(aw,pi)-tda(ao,pi))
    IF (ABS(x) < 0.01) EXIT
    pi= pi*(2.**(x))
  END DO
  alcl= pi
  RETURN

  ENTRY alclm(t,td,p)
!   for entry alclm only, t is the mean potential temperature (celsius)
!   and td is the mean mixing ratio (g/kg) of the layer containing the
!   parcel.

  aw = td
  ao = t
  GO TO 3
  END FUNCTION alcl


  FUNCTION ct(wbar,pc,ps)
!
!    g.s. stipanuk     1973            original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the convective temperature ct (celsius)
!   given the mean mixing ratio wbar (g/kg) in the surface layer,
!   the pressure pc (mb) at the convective condensation level (ccl)
!   and the surface pressure ps (mb).
!   compute the temperature (celsius) at the ccl.

  tc= tmr(wbar,pc)

!   compute the potential temperature (celsius), i.e., the dry
!   adiabat ao through the ccl.

  ao= o(tc,pc)

!   compute the surface temperature on the same dry adiabat ao.

  ct= tda(ao,ps)
  RETURN
  END FUNCTION ct


  FUNCTION dewpt(ew)
!
!   this function yields the dew point dewpt (celsius), given the
!   water vapor pressure ew (millibars).
!   the empirical formula appears in bolton, david, 1980:
!   "the computation of equivalent potential temperature,"
!   monthly weather review, vol. 108, no. 7 (july), p. 1047, eq.(11).
!   the quoted accuracy is 0.03c or less for -35 < dewpt < 35c.
!
!    baker, schlatter  17-may-1982     original version.

  enl = ALOG(ew)
  dewpt = (243.5*enl-440.8)/(19.48-enl)
  RETURN
  END FUNCTION dewpt


  FUNCTION dpt(ew)
!
!   this function returns the dew point dpt (celsius), given the
!   water vapor pressure ew (millibars).
!
!    baker, schlatter  17-may-1982     original version.

  DATA es0/6.1078/

!   es0 = saturation vapor pressure (mb) over water at 0c
!   return a flag value if the vapor pressure is out of range.

  IF (ew > .06.AND.ew < 1013.) GO TO 5
  dpt = 9999.
  RETURN
  5   CONTINUE

!   approximate dew point by means of teten's formula.
!   the formula appears as eq.(8) in bolton, david, 1980:
!   "the computation of equivalent potential temperature,"
!   monthly weather review, vol 108, no. 7 (july), p.1047.
!   the formula is ew(t) = es0*10**(7.5*t/(t+237.3))
!         or    ew(t) = es0*exp(17.269388*t/(t+237.3))
!   the inverse formula is used below.

  x = ALOG(ew/es0)
  dnm = 17.269388-x
  t = 237.3*x/dnm
  fac = 1./(ew*dnm)

!   loop for iterative improvement of the estimate of dew point

  10   CONTINUE

!   get the precise vapor pressure corresponding to t.

  edp = esw(t)

!   estimate the change in temperature corresponding to (ew-edp)
!   assume that the derivative of temperature with respect to
!   vapor pressure (dtdew) is given by the derivative of the
!   inverse teten formula.

  dtdew = (t+237.3)*fac
  dt = dtdew*(ew-edp)
  t = t+dt
  IF (ABS(dt) > 1.e-04) GO TO 10
  dpt = t
  RETURN
  END FUNCTION dpt


  FUNCTION dwpt(t,rh)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns the dew point (celsius) given the temperature
!   (celsius) and relative humidity (%). the formula is used in the
!   processing of u.s. rawinsonde data and is referenced in parry, h.
!   dean, 1969: "the semiautomatic computation of rawinsondes,"
!   technical memorandum wbtm edl 10, u.s. department of commerce,
!   environmental science services administration, weather bureau,
!   office of systems development, equipment development laboratory,
!   silver spring, md (october), page 9 and page ii-4, line 460.

  x = 1.-0.01*rh

!   compute dew point depression.

  dpd =(14.55+0.114*t)*x+((2.5+0.007*t)*x)**3+(15.9+0.117*t)*x**14
  dwpt = t-dpd
  RETURN
  END FUNCTION dwpt


  FUNCTION ept(t,td,p)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns the equivalent potential temperature ept
!   (celsius) for a parcel of air initially at temperature t (celsius),
!   dew point td (celsius) and pressure p (millibars). the formula used
!   is eq.(43) in bolton, david, 1980: "the computation of equivalent
!   potential temperature," monthly weather review, vol. 108, no. 7
!   (july), pp. 1046-1053. the maximum error in ept in 0.3c.  in most
!   cases the error is less than 0.1c.
!
!   compute the mixing ratio (grams of water vapor per kilogram of
!   dry air).

  w = wmr(p,td)

!   compute the temperature (celsius) at the lifting condensation level.

  tlcl = tcon(t,td)
  tk = t+273.15
  tl = tlcl+273.15
  pt = tk*(1000./p)**(0.2854*(1.-0.00028*w))
  eptk = pt*EXP((3.376/tl-0.00254)*w*(1.+0.00081*w))
  ept= eptk-273.15
  RETURN
  END FUNCTION ept


  FUNCTION es(t)
!
!   this function returns the saturation vapor pressure es (mb) over
!   liquid water given the temperature t (celsius). the formula appears
!   in bolton, david, 1980: "the computation of equivalent potential
!   temperature," monthly weather review, vol. 108, no. 7 (july),
!   p. 1047, eq.(10). the quoted accuracy is 0.3% or better for
!   -35 < t < 35c.
!
!    baker, schlatter  17-may-1982     original version.
!
!   es0 = saturation vapor pressure over liquid water at 0c

  DATA es0/6.1121/
  es = es0*EXP(17.67*t/(t+243.5))
  RETURN
  END FUNCTION es


  FUNCTION esat(t)
!
!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982
!
!   this function returns the saturation vapor pressure over
!   water (mb) given the temperature (celsius).
!   the algorithm is due to nordquist, w.s.,1973: "numerical approxima-
!   tions of selected meteorlolgical parameters for cloud physics prob-
!   lems," ecom-5475, atmospheric sciences laboratory, u.s. army
!   electronics command, white sands missile range, new mexico 88002.

  tk = t+273.15
  p1 = 11.344-0.0303998*tk
  p2 = 3.49149-1302.8844/tk
  c1 = 23.832241-5.02808*ALOG10(tk)
  esat = 10.**(c1-1.3816E-7*10.**p1+8.1328E-3*10.**p2-2949.076/tk)
  RETURN
  END FUNCTION esat


  FUNCTION esgg(t)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns the saturation vapor pressure over liquid
!   water esgg (millibars) given the temperature t (celsius). the
!   formula used, due to goff and gratch, appears on p. 350 of the
!   smithsonian meteorological tables, sixth revised edition, 1963,
!   by roland list.

  DATA cta,ews,ts/273.15,1013.246,373.15/

!   cta = difference between kelvin and celsius temperatures
!   ews = saturation vapor pressure (mb) over liquid water at 100c
!   ts = boiling point of water (k)

  DATA c1,      c2,      c3,      c4,       c5,       c6                &
      / 7.90298, 5.02808, 1.3816E-7, 11.344, 8.1328E-3, 3.49149 /
  tk = t+cta

!   goff-gratch formula

  rhs = -c1*(ts/tk-1.)+c2*ALOG10(ts/tk)-c3*(10.**(c4*(1.-tk/ts))        &
         -1.)+c5*(10.**(-c6*(ts/tk-1.))-1.)+ALOG10(ews)
  esw = 10.**rhs
  IF (esw < 0.) esw = 0.
  esgg = esw
  RETURN
  END FUNCTION esgg


  FUNCTION esice(t)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns the saturation vapor pressure with respect to
!   ice esice (millibars) given the temperature t (celsius).
!   the formula used is based upon the integration of the clausius-
!   clapeyron equation by goff and gratch.  the formula appears on p.350
!   of the smithsonian meteorological tables, sixth revised edition,
!   1963.

  DATA cta,eis/273.15,6.1071/

!   cta = difference between kelvin and celsius temperature
!   eis = saturation vapor pressure (mb) over a water-ice mixture at 0c

  DATA c1,c2,c3/9.09718,3.56654,0.876793/

!   c1,c2,c3 = empirical coefficients in the goff-gratch formula

  IF (t <= 0.) GO TO 5
  esice = 99999.
  WRITE(6,3)esice
  3   FORMAT(' saturation vapor pressure for ice cannot be computed',   &
              /' for temperature > 0c. esice =',f7.0)
  RETURN
  5   CONTINUE

!   freezing point of water (k)

  tf = cta
  tk = t+cta

!   goff-gratch formula

  rhs = -c1*(tf/tk-1.)-c2*ALOG10(tf/tk)+c3*(1.-tk/tf)+ALOG10(eis)
  esi = 10.**rhs
  IF (esi < 0.) esi = 0.
  esice = esi
  RETURN
  END FUNCTION esice


  FUNCTION esilo(t)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns the saturation vapor pressure over ice
!   esilo (millibars) given the temperature t (celsius). the formula
!   is due to lowe, paul r., 1977: an approximating polynomial for
!   the computation of saturation vapor pressure, journal of applied
!   meteorology, vol. 16, no. 1 (january), pp. 100-103.
!   the polynomial coefficients are a0 through a6.

  DATA a0,a1,a2,a3,a4,a5,a6                                             &
      /6.109177956,     5.034698970E-01, 1.886013408E-02,               &
      4.176223716E-04, 5.824720280E-06, 4.838803174E-08,                &
      1.838826904E-10/
  IF (t <= 0.) GO TO 5
  esilo = 9999.
  WRITE(6,3)esilo
  3   FORMAT(' saturation vapor pressure over ice is undefined for',    &
       /' temperature > 0c. esilo =',f6.0)
  RETURN
  5   CONTINUE
  esilo = a0+t*(a1+t*(a2+t*(a3+t*(a4+t*(a5+a6*t)))))
  RETURN
  END FUNCTION esilo


  FUNCTION eslo(t)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns the saturation vapor pressure over liquid
!   water eslo (millibars) given the temperature t (celsius). the
!   formula is due to lowe, paul r.,1977: an approximating polynomial
!   for the computation of saturation vapor pressure, journal of applied
!   meteorology, vol 16, no. 1 (january), pp. 100-103.
!   the polynomial coefficients are a0 through a6.

  DATA a0,a1,a2,a3,a4,a5,a6                                             &
      /6.107799961,     4.436518521E-01, 1.428945805E-02,               &
      2.650648471E-04, 3.031240396E-06, 2.034080948E-08,                &
      6.136820929E-11/
  es = a0+t*(a1+t*(a2+t*(a3+t*(a4+t*(a5+a6*t)))))
  IF (es < 0.) es = 0.
  eslo = es
  RETURN
  END FUNCTION eslo


  FUNCTION esrw(t)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns the saturation vapor pressure over liquid
!   water esrw (millibars) given the temperature t (celsius). the
!   formula used is due to richards, j.m., 1971: simple expression
!   for the saturation vapour pressure of water in the range -50 to
!   140c, british journal of applied physics, vol. 4, pp.l15-l18.
!   the formula was quoted more recently by wigley, t.m.l.,1974:
!   comments on 'a simple but accurate formula for the saturation
!   vapor pressure over liquid water,' journal of applied meteorology,
!   vol. 13, no. 5 (august) p.606.

  DATA cta,ts,ews/273.15,373.15,1013.25/

!   cta = difference between kelvin and celsius temperature
!   ts = temperature of the boiling point of water (k)
!   ews = saturation vapor pressure over liquid water at 100c

  DATA c1,     c2,     c3,     c4                                       &
      / 13.3185,-1.9760,-0.6445,-0.1299 /

  tk = t+cta
  x = 1.-ts/tk
  px = x*(c1+x*(c2+x*(c3+c4*x)))
  vp = ews*EXP(px)
  IF (vp < 0) vp = 0.
  esrw = vp
  RETURN
  END FUNCTION esrw


  FUNCTION esw(t)
!
!   this function returns the saturation vapor pressure esw (millibars)
!   over liquid water given the temperature t (celsius). the polynomial
!   approximation below is due to herman wobus, a mathematician who
!   worked at the navy weather research facility, norfolk, virginia,
!   but who is now retired. the coefficients of the polynomial were
!   chosen to fit the values in table 94 on pp. 351-353 of the smith-
!   sonian meteorological tables by roland list (6th edition). the
!   approximation is valid for -50 < t < 100c.
!
!    baker, schlatter  17-may-1982    original version.
!
!   es0 = saturation vapor ressure over liquid water at 0c

  DATA es0/6.1078/
  pol = 0.99999683       + t*(-0.90826951E-02 +                         &
      t*(0.78736169E-04   + t*(-0.61117958E-06 +                        &
      t*(0.43884187E-08   + t*(-0.29883885E-10 +                        &
      t*(0.21874425E-12   + t*(-0.17892321E-14 +                        &
      t*(0.11112018E-16   + t*(-0.30994571E-19)))))))))
  esw = es0/pol**8
  RETURN
  END FUNCTION esw


  FUNCTION heatl(key,t)
!
!    baker, schlatter  17-may-1982    original version.
!
!   this function returns the latent heat of
!      evaporation/condensation         for key=1
!      melting/freezing     for key=2
!      sublimation/deposition     for key=3
!   for water. the latent heat heatl (joules per kilogram) is a
!   function of temperature t (celsius). the formulas are polynomial
!   approximations to the values in table 92, p. 343 of the smithsonian
!   meteorological tables, sixth revised edition, 1963 by roland list.
!   the approximations were developed by eric smith at colorado state
!   university.
!   polynomial coefficients

  DATA a0,a1,a2/ 3337118.5,-3642.8583, 2.1263947/
  DATA b0,b1,b2/-1161004.0, 9002.2648,-12.931292/
  DATA c0,c1,c2/ 2632536.8, 1726.9659,-3.6248111/
  hltnt = 0.
  tk = t+273.15
  IF (key == 1) hltnt=a0+a1*tk+a2*tk*tk
  IF (key == 2) hltnt=b0+b1*tk+b2*tk*tk
  IF (key == 3) hltnt=c0+c1*tk+c2*tk*tk
  heatl = hltnt
  RETURN
  END FUNCTION heatl


  FUNCTION hum(t,td)
!
!    g.s. stipanuk     1973          original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982
!
!   this function returns relative humidity (%) given the
!   temperature t and dew point td (celsius).  as calculated here,
!   relative humidity is the ratio of the actual vapor pressure to
!   the saturation vapor pressure.

  hum= 100.*(esat(td)/esat(t))
  RETURN
  END FUNCTION hum


  FUNCTION o(t,p)
!
!    g.s. stipanuk     1973          original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns potential temperature (celsius) given
!   temperature t (celsius) and pressure p (mb) by solving the poisson
!   equation.

  tk= t+273.15
  ok= tk*((1000./p)**.286)
  o= ok-273.15
  RETURN
  END FUNCTION o


  FUNCTION oe(t,td,p)
!
!    g.s. stipanuk     1973          original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns equivalent potential temperature oe (celsius)
!   of a parcel of air given its temperature t (celsius), dew point
!   td (celsius) and pressure p (millibars).
!   find the wet bulb temperature of the parcel.

  atw = tw(t,td,p)

!   find the equivalent potential temperature.

  oe = os(atw,p)
  RETURN
  END FUNCTION oe


  FUNCTION os(t,p)
!
!    g.s. stipanuk     1973          original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the equivalent potential temperature os
!   (celsius) for a parcel of air saturated at temperature t (celsius)
!   and pressure p (millibars).
  DATA b/2.6518986/
!   b is an empirical constant approximately equal to the latent heat
!   of vaporization for water divided by the specific heat at constant
!   pressure for dry air.

  tk = t+273.15
  osk= tk*((1000./p)**.286)*(EXP(b*w(t,p)/tk))
  os= osk-273.15
  RETURN
  END FUNCTION os


  FUNCTION ow(t,td,p)
!
!    g.s. stipanuk     1973          original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the wet-bulb potential temperature ow
!   (celsius) given the temperature t (celsius), dew point td
!   (celsius), and pressure p (millibars).  the calculation for ow is
!   very similar to that for wet bulb temperature. see p.13 stipanuk (1973).
!   find the wet bulb temperature of the parcel

  atw = tw(t,td,p)

!   find the equivalent potential temperature of the parcel.

  aos= os(atw,p)

!   find the wet-bulb potential temperature.

  ow= tsa(aos,1000.)
  RETURN
  END FUNCTION ow


  FUNCTION pccl(pm,p,t,td,wbar,n)
!
!    g.s. stipanuk     1973          original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982
!
!   this function returns the pressure at the convective condensation
!   level given the appropriate sounding data.
!   on input:
!    p = pressure (millibars). note that p(i).gt.p(i+1).
!    t = temperature (celsius)
!    td = dew point (celsius)
!    n = number of levels in the sounding and the dimension of
!        p, t and td
!    pm = pressure (millibars) at upper boundary of the layer for
!         computing the mean mixing ratio. p(1) is the lower
!         boundary.
!   on output:
!    pccl = pressure (millibars) at the convective condensation level
!    wbar = mean mixing ratio (g/kg) in the layer bounded by
!           pressures p(1) at the bottom and pm at the top
!   the algorithm is decribed on p.17 of stipanuk, g.s.,1973:
!   "algorithms for generating a skew-t log p diagram and computing
!   selected meteorological quantities," atmospheric sciences labora-
!   tory, u.s. army electronics command, white sands missile range, new
!   mexico 88002.

  DIMENSION t(1),p(1),td(1)
  IF (pm /= p(1)) GO TO 5
  wbar= w(td(1),p(1))
  pc= pm
  IF (ABS(t(1)-td(1)) < 0.05) GO TO 45
  GO TO 25
  5   CONTINUE
  wbar= 0.
  k= 0
  10   CONTINUE
  k = k+1
  IF (p(k) > pm) GO TO 10
  k= k-1
  j= k-1
  IF(j < 1) GO TO 20

!   compute the average mixing ratio....alog = natural log

  DO i= 1,j
    l= i+1
    wbar= (w(td(i),p(i))+w(td(l),p(l)))*ALOG(p(i)/p(l))                 &
                 +wbar
  END DO
  20   CONTINUE
  l= k+1

!   estimate the dew point at pressure pm.

  tq= td(k)+(td(l)-td(k))*(ALOG(pm/p(k)))/(ALOG(p(l)/p(k)))
  wbar= wbar+(w(td(k),p(k))+w(tq,pm))*ALOG(p(k)/pm)
  wbar= wbar/(2.*ALOG(p(1)/pm))

!   find level at which the mixing ratio line wbar crosses the
!   environmental temperature profile.

  25   CONTINUE
  DO j= 1,n
    i= n-j+1
    IF (p(i) < 300.) CYCLE

!   tmr = temperature (celsius) at pressure p (mb) along a mixing
!     ratio line given by wbar (g/kg)

    x= tmr(wbar,p(i))-t(i)
    IF (x <= 0.) GO TO 35
  END DO
  pccl= 0.0
  RETURN

!  set up bisection routine

  35   l = i
  i= i+1
  del= p(l)-p(i)
  pc= p(i)+.5*del
  a= (t(i)-t(l))/ALOG(p(l)/p(i))
  DO j= 1,10
    del= del/2.
    x= tmr(wbar,pc)-t(l)-a*(ALOG(p(l)/pc))

!   the sign function replaces the sign of the first argument
!   with that of the second.

    pc= pc+SIGN(del,x)
  END DO
  45   pccl = pc
  RETURN
  END FUNCTION pccl


  FUNCTION pcon(p,t,tc)
!
!   this function returns the pressure pcon (mb) at the lifted condensa-
!   tion level, given the initial pressure p (mb) and temperature t
!   (celsius) of the parcel and the temperature tc (celsius) at the
!   lcl. the algorithm is exact.  it makes use of the formula for the
!   potential temperatures corresponding to t at p and tc at pcon.
!   these two potential temperatures are equal.

!    baker, schlatter  17-may-1982     original version.

!
  DATA akapi/3.5037/

!   akapi = (specific heat at constant pressure for dry air) /
!        (gas constant for dry air)

!   convert t and tc to kelvin temperatures.

  tk = t+273.15
  tck = tc+273.15
  pcon = p*(tck/tk)**akapi
  RETURN
  END FUNCTION pcon


  FUNCTION powt(t,p,td)
!
!   this function yields wet-bulb potential temperature powt
!   (celsius), given the following input:
!       t = temperature (celsius)
!       p = pressure (millibars)
!       td = dew point (celsius)
!
!    baker, schlatter  17-may-1982     original version.

  DATA cta,akap/273.15,0.28541/

!   cta = difference between kelvin and celsius temperatures
!   akap = (gas constant for dry air) / (specific heat at
!       constant pressure for dry air)
!   compute the potential temperature (celsius)

  pt = (t+cta)*(1000./p)**akap-cta

!   compute the lifting condensation level (lcl).

  tc = tcon(t,td)

!   for the origin of the following approximation, see the documen-
!   tation for the wobus function.

  powt = pt-wobf(pt)+wobf(tc)
  RETURN
  END FUNCTION powt


  FUNCTION precpw(td,p,n)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function computes total precipitable water precpw (cm) in a
!   vertical column of air based upon sounding data at n levels:
!       td = dew point (celsius)
!       p = pressure (millibars)
!   calculations are done in cgs units.

  DIMENSION td(n),p(n)

!   g = acceleration due to the earth's gravity (cm/s**2)

  DATA g/980.616/

!   initialize value of precipitable water

  pw = 0.
  nl = n-1

!   calculate the mixing ratio at the lowest level.

  wbot = wmr(p(1),td(1))
  DO i=1,nl
    wtop = wmr(p(i+1),td(i+1))

!   calculate the layer-mean mixing ratio (g/kg).

    w = 0.5*(wtop+wbot)

!   make the mixing ratio dimensionless.

    wl = .001*w

!   calculate the specific humidity.

    ql = wl/(wl+1.)

!   the factor of 1000. below converts from millibars to dynes/cm**2.

    dp = 1000.*(p(i)-p(i+1))
    pw = pw+(ql/g)*dp
    wbot = wtop
  END DO
  precpw = pw
  RETURN
  END FUNCTION precpw


SUBROUTINE ptlcl(p,t,td,pc,tc) 5
!
!   this subroutine estimates the pressure pc (mb) and the temperature
!   tc (celsius) at the lifted condensation level (lcl), given the
!   initial pressure p (mb), temperature t (celsius) and dew point
!   (celsius) of the parcel.  the approximation is that lines of
!   constant potential temperature and constant mixing ratio are
!   straight on the skew t/log p chart.
!
!    baker,schlatter   17-may-1982   original version
!
!   teten's formula for saturation vapor pressure as a function of
!   pressure was used in the derivation of the formula below.  for
!   additional details, see math notes by t. schlatter dated 8 sep 81.
!   t. schlatter, noaa/erl/profs program office, boulder, colorado,
!   wrote this subroutine.
!
!   akap = (gas constant for dry air) / (specific heat at constant
!       pressure for dry air)
!   cta = difference between kelvin and celsius temperatures
!
  DATA akap,cta/0.28541,273.16/
  c1 = 4098.026/(td+237.3)**2
  c2 = 1./(akap*(t+cta))
  pc = p*EXP(c1*c2*(t-td)/(c2-c1))
  tc = t+c1*(t-td)/(c2-c1)
  RETURN
END SUBROUTINE ptlcl


  FUNCTION satlft(thw,p)
!
!    baker, schlatter  17-may-1982     original version.
!
!   input:  thw = wet-bulb potential temperature (celsius).
!         thw defines a moist adiabat.
!        p = pressure (millibars)
!   output: satlft = temperature (celsius) where the moist adiabat
!         crosses p

  DATA cta,akap/273.15,0.28541/

!   cta = difference between kelvin and celsius temperatures
!   akap = (gas constant for dry air) / (specific heat at constant
!        pressure for dry air)

!     the algorithm below can best be understood by referring to a
!   skew-t/log p chart.  it was devised by herman wobus, a mathemati-
!   cian formerly at the navy weather research facility but now retired.
!   the value returned by satlft can be checked by referring to table
!   78, pp.319-322, smithsonian meteorological tables, by roland list
!   (6th revised edition).
!

  IF (p /= 1000.) GO TO 5
  satlft = thw
  RETURN
  5   CONTINUE

!   compute tone, the temperature where the dry adiabat with value thw
!   (celsius) crosses p.

  pwrp = (p/1000.)**akap
  tone = (thw+cta)*pwrp-cta

!   consider the moist adiabat ew1 through tone at p.  using the defini-
!   tion of the wobus function (see documentation on wobf), it can be
!   shown that eone = ew1-thw.

  eone = wobf(tone)-wobf(thw)
  rate = 1.
  GO TO 15

!   in the loop below, the estimate of satlft is iteratively improved.

  10   CONTINUE

!   rate is the ratio of a change in t to the corresponding change in
!   e.  its initial value was set to 1 above.

  rate = (ttwo-tone)/(etwo-eone)
  tone = ttwo
  eone = etwo
  15   CONTINUE

!   ttwo is an improved estimate of satlft.

  ttwo = tone-eone*rate

!   pt is the potential temperature (celsius) corresponding to ttwo at p

  pt = (ttwo+cta)/pwrp-cta

!   consider the moist adiabat ew2 through ttwo at p. using the defini-
!   tion of the wobus function, it can be shown that etwo = ew2-thw.

  etwo = pt+wobf(ttwo)-wobf(pt)-thw

!   dlt is the correction to be subtracted from ttwo.

  dlt = etwo*rate
  IF (ABS(dlt) > 0.1) GO TO 10
  satlft = ttwo-dlt
  RETURN
  END FUNCTION satlft


  FUNCTION ssh(p,t)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns saturation specific humidity ssh (grams of
!   water vapor per kilogram of moist air) given the pressure p
!   (millibars) and the temperature t (celsius). the equation is given
!   in standard meteorological texts. if t is dew point (celsius), then
!   ssh returns the actual specific humidity.
!   compute the dimensionless mixing ratio.

  w = .001*wmr(p,t)

!   compute the dimensionless saturation specific humidity.

  q = w/(1.+w)
  ssh = 1000.*q
  RETURN
  END FUNCTION ssh


  FUNCTION tcon(t,d)
!
!   this function returns the temperature tcon (celsius) at the lifting
!   condensation level, given the temperature t (celsius) and the
!   dew point d (celsius).
!
!    baker, schlatter  17-may-1982     original version.
!
!   compute the dew point depression s.
  s = t-d
!   the approximation below, a third order polynomial in s and t,
!   is due to herman wobus. the source of data for fitting the
!   polynomial is unknown.

  dlt = s*(1.2185+1.278E-03*t+                                          &
        s*(-2.19E-03+1.173E-05*s-5.2E-06*t))
  tcon = t-dlt
  RETURN
  END FUNCTION tcon


  FUNCTION tda(o,p)
!
!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the temperature tda (celsius) on a dry adiabat
!   at pressure p (millibars). the dry adiabat is given by
!   potential temperature o (celsius). the computation is based on
!   poisson's equation.

  ok= o+273.15
  tdak= ok*((p*.001)**.286)
  tda= tdak-273.15
  RETURN
  END FUNCTION tda


  FUNCTION te(t,td,p)
!
!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the equivalent temperature te (celsius) of a
!   parcel of air given its temperature t (celsius), dew point (celsius)
!   and pressure p (millibars).
!   calculate equivalent potential temperature.

  aoe = oe(t,td,p)

!   use poissons's equation to calculate equivalent temperature.

  te= tda(aoe,p)
  RETURN
  END FUNCTION te


  FUNCTION thm(t,p)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns the wet-bulb potential temperature thm
!   (celsius) corresponding to a parcel of air saturated at
!   temperature t (celsius) and pressure p (millibars).

  f(x) =   1.8199427E+01+x*( 2.1640800E-01+x*( 3.0716310E-04+x*         &
          (-3.8953660E-06+x*( 1.9618200E-08+x*( 5.2935570E-11+x*        &
          ( 7.3995950E-14+x*(-4.1983500E-17)))))))
  thm = t
  IF (p == 1000.) RETURN

!   compute the potential temperature (celsius).

  thd = (t+273.15)*(1000./p)**.286-273.15
  thm = thd+6.071*(EXP(t/f(t))-EXP(thd/f(thd)))
  RETURN
  END FUNCTION thm


  FUNCTION tlcl(t,td)
!
!   this function yields the temperature tlcl (celsius) of the lifting
!   condensation level, given the temperature t (celsius) and the
!   dew point td (celsius).
!
!    baker,schlatter   17-may-1982   original version
!
!   the formula used is n bolton, david,
!   1980: "the computation of equivalent potential temperature,"
!   monthly weather review, vol. 108, no. 7 (july), p. 1048, eq.(15).
!
!   convert from celsius to kelvin degrees.
  tk = t+273.16
  tdk = td+273.16
  a = 1./(tdk-56.)
  b = ALOG(tk/tdk)/800.
  tc = 1./(a+b)+56.
  tlcl = tc-273.16
  RETURN
  END FUNCTION tlcl


  FUNCTION tlcl1(t,td)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns the temperature tlcl1 (celsius) of the lifting
!   condensation level (lcl) given the initial temperature t (celsius)
!   and dew point td (celsius) of a parcel of air.
!   eric smith at colorado state university has used the formula
!   below, but its origin is unknown.

  DATA cta/273.15/

!   cta = difference between kelvin and celsius temperature

  tk = t+cta

!   compute the parcel vapor pressure (mb).
  es = eslo(td)
  tlcl = 2840./(3.5*ALOG(tk)-ALOG(es)-4.805)+55.
  tlcl1 = tlcl-cta
  RETURN
  END FUNCTION tlcl1


  FUNCTION tmlaps(thetae,p)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns the temperature tmlaps (celsius) at pressure
!   p (millibars) along the moist adiabat corresponding to an equivalent
!   potential temperature thetae (celsius).
!   the algorithm was written by eric smith at colorado state
!   university.

  DATA crit/0.1/

!   cta = difference between kelvin and celsius temperatures.
!   crit = convergence criterion (degrees kelvin)

  eq0 = thetae

!   initial guess for solution

  tlev = 25.

!   compute the saturation equivalent potential temperature correspon-
!   ding to temperature tlev and pressure p.

  eq1 = ept(tlev,tlev,p)
  dif = ABS(eq1-eq0)
  IF (dif < crit) GO TO 3
  IF (eq1 > eq0) GO TO 1

!   dt is the initial stepping increment.

  dt = 10.
  i = -1
  GO TO 2
  1   dt = -10.
  i = 1
  2   tlev = tlev+dt
  eq1 = ept(tlev,tlev,p)
  dif = ABS(eq1-eq0)
  IF (dif < crit) GO TO 3
  j = -1
  IF (eq1 > eq0) j=1
  IF (i == j) GO TO 2

!   the solution has been passed. reverse the direction of search
!   and decrease the stepping increment.

  tlev = tlev-dt
  dt = dt/10.
  GO TO 2
  3   tmlaps = tlev
  RETURN
  END FUNCTION tmlaps


  FUNCTION tmr(w,p)
!
!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the temperature (celsius) on a mixing
!   ratio line w (g/kg) at pressure p (mb). the formula is given in
!   table 1 on page 7 of stipanuk (1973).
!
!   initialize constants

  DATA c1/.0498646455/,c2/2.4082965/,c3/7.07475/
  DATA c4/38.9114/,c5/.0915/,c6/1.2035/

  x= ALOG10(w*p/(622.+w))
  tmrk= 10.**(c1*x+c2)-c3+c4*((10.**(c5*x)-c6)**2.)
  tmr= tmrk-273.15
  RETURN
  END FUNCTION tmr


  FUNCTION tsa(os,p)
!
!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the temperature tsa (celsius) on a saturation
!   adiabat at pressure p (millibars). os is the equivalent potential
!   temperature of the parcel (celsius). sign(a,b) replaces the
!   algebraic sign of a with that of b.
!   b is an empirical constant approximately equal to 0.001 of the latent
!   heat of vaporization for water divided by the specific heat at constant
!   pressure for dry air.

  DATA b/2.6518986/
  a= os+273.15

!   tq is the first guess for tsa.

  tq= 253.15

!   d is an initial value used in the iteration below.

  d= 120.

!   iterate to obtain sufficient accuracy....see table 1, p.8
!   of stipanuk (1973) for equation used in iteration.

  DO i= 1,12
    tqk= tq-273.15
    d= d/2.
    x= a*EXP(-b*w(tqk,p)/tq)-tq*((1000./p)**.286)
    IF (ABS(x) < 1E-7) GOTO 2
    tq= tq+SIGN(d,x)
  END DO
2 tsa= tq-273.15
  RETURN
  END FUNCTION tsa


  FUNCTION tv(t,td,p)
!
!    baker, schlatter  17-may-1982     original version.
!
!   this function returns the virtual temperature tv (celsius) of
!   a parcel of air at temperature t (celsius), dew point td
!   (celsius), and pressure p (millibars). the equation appears
!   in most standard meteorological texts.

  DATA cta,eps/273.15,0.62197/

!   cta = difference between kelvin and celsius temperatures.
!   eps = ratio of the mean molecular weight of water (18.016 g/mole)
!      to that of dry air (28.966 g/mole)

  tk = t+cta

!   calculate the dimensionless mixing ratio.

  w = .001*wmr(p,td)
  tv = tk*(1.+w/eps)/(1.+w)-cta
  RETURN
  END FUNCTION tv


  FUNCTION tw(t,td,p)

!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the wet-bulb temperature tw (celsius)
!   given the temperature t (celsius), dew point td (celsius)
!   and pressure p (mb).  see p.13 in stipanuk (1973), referenced
!   above, for a description of the technique.
!
!
!   determine the mixing ratio line thru td and p.

  aw = w(td,p)
!
!   determine the dry adiabat thru t and p.

  ao = o(t,p)
  pi = p

!   iterate to locate pressure pi at the intersection of the two
!   curves .  pi has been set to p for the initial guess.

  DO i= 1,10
    x= .02*(tmr(aw,pi)-tda(ao,pi))
    IF (ABS(x) < 0.01) EXIT
    pi= pi*(2.**(x))
  END DO

!   find the temperature on the dry adiabat ao at pressure pi.

  ti= tda(ao,pi)

!   the intersection has been located...now, find a saturation
!   adiabat thru this point. function os returns the equivalent
!   potential temperature (c) of a parcel saturated at temperature
!   ti and pressure pi.

  aos= os(ti,pi)

!   function tsa returns the wet-bulb temperature (c) of a parcel at
!   pressure p whose equivalent potential temperature is aos.

  tw = tsa(aos,p)
  RETURN
  END FUNCTION tw


  FUNCTION w(t,p)
!
!    g.s. stipanuk     1973              original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982
!
!  this function returns the mixing ratio (grams of water vapor per
!  kilogram of dry air) given the dew point (celsius) and pressure
!  (millibars). if the temperture  is input instead of the
!  dew point, then saturation mixing ratio (same units) is returned.
!  the formula is found in most meteorological texts.

  x= esat(t)
  w= 622.*x/(p-x)
  RETURN
  END FUNCTION w


  FUNCTION wmr(p,t)
!
!   this function approximates the mixing ratio wmr (grams of water
!   vapor per kilogram of dry air) given the pressure p (mb) and the
!   temperature t (celsius). the formula used is given on p. 302 of the
!   smithsonian meteorological tables by roland list (6th edition).
!
!    baker, schlatter  17-may-1982   original version.
!
!   eps = ratio of the mean molecular weight of water (18.016 g/mole)
!      to that of dry air (28.966 g/mole)

  DATA eps/0.62197/

!   the next two lines contain a formula by herman wobus for the
!   correction factor wfw for the departure of the mixture of air
!   and water vapor from the ideal gas law. the formula fits values
!   in table 89, p. 340 of the smithsonian meteorological tables,
!   but only for temperatures and pressures normally encountered in
!   in the atmosphere.

  x = 0.02*(t-12.5+7500./p)
  wfw = 1.+4.5E-06*p+1.4E-03*x*x
  fwesw = wfw*esw(t)
  r = eps*fwesw/(p-fwesw)

!   convert r from a dimensionless ratio to grams/kilogram.

  wmr = 1000.*r
  RETURN
  END FUNCTION wmr


  FUNCTION wobf(t)

!   this function calculates the difference of the wet-bulb potential
!   temperatures for saturated and dry air given the temperature.
!
!    baker, schlatter  17-may-1982     original version.
!
!     let wbpts = wet-bulb potential temperature for saturated
!   air at temperature t (celsius). let wbptd = wet-bulb potential
!   temperature for completely dry air at the same temperature t.
!   the wobus function wobf (in degrees celsius) is defined by
!                   wobf(t) = wbpts-wbptd.
!   although wbpts and wbptd are functions of both pressure and
!   temperature, their difference is a function of temperature only.

!     to understand why, consider a parcel of dry air at tempera-
!   ture t and pressure p. the thermodynamic state of the parcel is
!   represented by a point on a pseudoadiabatic chart. the wet-bulb
!   potential temperature curve (moist adiabat) passing through this
!   point is wbpts. now t is the equivalent temperature for another
!   parcel saturated at some lower temperature tw, but at the same
!   pressure p.  to find tw, ascend along the dry adiabat through
!   (t,p). at a great height, the dry adiabat and some moist
!   adiabat will nearly coincide. descend along this moist adiabat
!   back to p. the parcel temperature is now tw. the wet-bulb
!   potential temperature curve (moist adiabat) through (tw,p) is wbptd.
!   the difference (wbpts-wbptd) is proportional to the heat imparted
!   to a parcel saturated at temperature tw if all its water vapor
!   were condensed. since the amount of water vapor a parcel can
!   hold depends upon temperature alone, (wbptd-wbpts) must depend
!   on temperature alone.

!     the wobus function is useful for evaluating several thermo-
!   dynamic quantities.  by definition:
!           wobf(t) = wbpts-wbptd.               (1)
!   if t is at 1000 mb, then t is a potential temperature pt and
!   wbpts = pt. thus
!           wobf(pt) = pt-wbptd.                 (2)
!   if t is at the condensation level, then t is the condensation
!   temperature tc and wbpts is the wet-bulb potential temperature
!   wbpt. thus
!           wobf(tc) = wbpt-wbptd.               (3)
!   if wbptd is eliminated from (2) and (3), there results
!           wbpt = pt-wobf(pt)+wobf(tc).
!   if wbptd is eliminated from (1) and (2), there results
!           wbpts = pt-wobf(pt)+wobf(t).

!     if t is an equivalent potential temperature ept (implying
!   that the air at 1000 mb is completely dry), then wbpts = ept
!   and wbptd = wbpt. thus
!           wobf(ept) = ept-wbpt.
!   this form is the basis for a polynomial approximation to wobf.
!   in table 78 on pp.319-322 of the smithsonian meteorological
!   tables by roland list (6th revised edition), one finds wet-bulb
!   potential temperatures and the corresponding equivalent potential
!   temperatures listed together. herman wobus, a mathematician for-
!   merly at the navy weather research facility, norfolk, virginia,
!   and now retired, computed the coefficients for the polynomial
!   approximation from numbers in this table.
!
!                                 notes by t.w. schlatter
!                                 noaa/erl/profs program office
!                                 august 1981

  x = t-20.
  IF (x > 0.) GO TO 10
  pol = 1.                 +x*(-8.8416605E-03                           &
        +x*( 1.4714143E-04  +x*(-9.6719890E-07                          &
       +x*(-3.2607217E-08  +x*(-3.8598073E-10)))))
  wobf = 15.130/pol**4
  RETURN
  10   CONTINUE
  pol = 1.                 +x*( 3.6182989E-03                           &
       +x*(-1.3603273E-05  +x*( 4.9618922E-07                           &
       +x*(-6.1059365E-09  +x*( 3.9401551E-11                           &
       +x*(-1.2588129E-13  +x*( 1.6688280E-16)))))))
  wobf = 29.930/pol**4+0.96*x-14.8
  RETURN
  END FUNCTION wobf


  FUNCTION z(pt,p,t,td,n)
!
!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982
!
!   this function returns the thickness of a layer bounded by pressure
!   p(1) at the bottom and pressure pt at the top.
!   on input:
!    p = pressure (mb).  note that p(i).gt.p(i+1).
!    t = temperature (celsius)
!    td = dew point (celsius)
!    n = number of levels in the sounding and the dimension of
!        p, t and td
!   on output:
!    z = geometric thickness of the layer (m)
!   the algorithm involves numerical integration of the hydrostatic
!   equation from p(1) to pt. it is described on p.15 of stipanuk
!   (1973).

  DIMENSION t(1),p(1),td(1),tk(100)

!    c1 = .001*(1./eps-1.) where eps = .62197 is the ratio of the
!                molecular weight of water to that of
!                dry air. the factor 1000. converts the
!                mixing ratio w from g/kg to a dimension-
!                less ratio.
!    c2 = r/(2.*g) where r is the gas constant for dry air
!             (287 kg/joule/deg k) and g is the acceleration
!             due to the earth's gravity (9.8 m/s**2). the
!             factor of 2 is used in averaging two virtual
!                  temperatures.

  DATA c1/.0006078/,c2/14.64285/
  DO i= 1,n
    tk(i)= t(i)+273.15
  END DO
  z= 0.0
  IF (pt < p(n)) GO TO 20
  i= 0
  10   i= i+1
  j= i+1
  IF (pt >= p(j)) GO TO 15
  a1= tk(j)*(1.+c1*w(td(j),p(j)))
  a2= tk(i)*(1.+c1*w(td(i),p(i)))
  z= z+c2*(a1+a2)*(ALOG(p(i)/p(j)))
  GO TO 10
  15   CONTINUE
  a1= tk(j)*(1.+c1*w(td(j),p(j)))
  a2= tk(i)*(1.+c1*w(td(i),p(i)))
  z= z+c2*(a1+a2)*(ALOG(p(i)/pt))
  RETURN
  20     z= -1.0
  RETURN
  END FUNCTION z
```

