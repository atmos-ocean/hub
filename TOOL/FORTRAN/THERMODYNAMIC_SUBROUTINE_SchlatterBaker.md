

# Algorithms, Comparisons and Source References
**by Schlatter and Baker**
https://wahiduddin.net/calc/density_algorithms.htm

```fortran
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

Temperature                     Celsius
Pressure                        millibars
Relative humidity               percent
Saturation specific humidity    g vapor/kg moist air
Mixing ratio                    g vapor/kg dry air
Thickness                       meters
Precipitable water              centimeters
Latent heat                     joules/kg


The following symbols are used in subprogram calls:

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

###############################################################################
General information:
--------------------
    The following tables give results of the tests performed on 
routines in [BAKER.THERMO]THERM.FOR. These routines calculate thermo-
dynamic parameters that are of interest when analyzing sound-
ings. the "Smithsonian Meterological Tables" were utilized to
test the accuracy of each routine where possible...otherwise, 
a Skew T-Log p chart was used. In cases where more than one
routine calculates a given parameter, an efficiency test is also
included. For the efficiency tests, a specified number of calcula-
tions was done by each routine using identical input. After 10
trials, the average time needed by each routine was determined
and is listed, thereby indicating which routine is most efficient.
for further information involving any of the routines to follow,
refer to documentation in [BAKER.THERMO]THERM.FOR.

Computer used:
VAX 11/780 @ NOAA/ERL/PROFS...Boulder, Colorado.

Input/output units:
Temperature............celsius
Pressure...............millibars
Mixing ratio...........gm vapor/kg dry air
Relative humidity......percent
Sat specific humidity..gm vapor/kg moist air

Note: Smith = Smithsonian table value. Where these values do not
      appear, the routines have been verified using a Skew T-
      Log p chart due to lack of an applicable table.  The number
      in parentheses next to "Smith" indicates the page number
      (6th revised edition) of the table used for the test.

###############################################################################

TEST OF FUNCTIONS ESW,ES,ESAT,ESGG,ESRW,ESLO
PURPOSE: CALCULATE SATURATION VAPOR PRESSURE OVER LIQUID WATER
         GIVEN TEMPERATURE.

TEMP  ESW     ES     ESAT    ESGG    ESRW    ESLO    SMITH.(340)
---- -----  -----   ------  ------  ------  ------   ------
-50 .06356  .06357  .06356  .06356  .06362  .06337   .06356
-30 .50878  .51036  .50878  .50880  .50843  .50877   .5088
-10 2.8627  2.8677  2.8626  2.8627  2.8625  2.8635   2.8627
  0 6.1080  6.1121  6.1076  6.1078  6.1084  6.1078   6.1078
 10 12.272  12.272  12.272  12.272  12.274  12.271   12.272
 20 23.372  23.370  23.372  23.373  23.375  23.371   23.373
 30 42.430  42.457  42.429  42.430  42.431  42.429   42.430

EFFICIENCY TEST: 5000 CALCULATIONS AT T=20C.

           FUNC.  T(SEC)
           ----   ------
           ESW     1.12
           ES      0.88
           ESAT    4.50
           ESGG    4.63
           ESRW    1.08
           ESLO    0.64

###############################################################################

TEST OF FUNCTIONS WMR,W
PURPOSE: CALCULATE MIXING RATIO GIVEN PRESSURE AND TEMPERATURE.

PRES  TEMP     WMR      W       SMITH.(302)
----  ----   ------   ------    ------
1000   30    27.699   27.560    27.69
1000    0     3.840    3.822     3.839
 850   10     9.147    9.112     9.146
 700   10    11.135   11.099    11.13
 500  -20     1.568    1.564     1.568
 400  -30     0.794    0.792     0.794

EFFICIENCY TEST: 2000 CALCULATIONS AT T=-10C, P=850MB.

           FUNC.  T(SEC)
           -----  ------
           WMR     0.82
           W       1.86

###############################################################################

TEST OF FUNCTIONS TCON,TLCL,TLCL1
PURPOSE: CALCULATE TEMPERATURE AT THE LCL GIVEN TEMPERATURE AND
         DEW POINT.

TEMP  DWPT    TCON    TLCL    TLCL1    SMITH.(329)
----  ----   ------  ------  ------    ------
 40    20    15.473  15.449  15.450    15.48
-20   -45   -48.703 -48.768 -48.689   -48.79
 10   -20   -25.237 -25.295 -29.401   -25.29
  0   -25   -29.277 -29.331 -29.401   -29.31
 30    10     5.708   5.682   5.679     5.72
 40    35    33.710  33.727  33.636    33.74

EFFICIENCY TEST: 5000 CALCULATIONS AT T=20C, TD= 10C.

           FUNC.  T(SEC)
           -----  ------
           TCON    0.65
           TLCL    0.96
           TLCL1   1.70

###############################################################################

TEST OF FUNCTION PCON
PURPOSE: CALCULATE LCL PRESSURE GIVEN INITIAL TEMPERATURE, DEW
         POINT, AND LCL TEMPERATURE (TEML).

PRES  TEMP  TEML    PCON
----  ----  ----   ------
1000   35    20    839.59
 975   30    20    866.89
 950   15   -10    691.24
 900   10   -25    566.87
 500  -20   -20    500.00
 500  -20   -50    321.40

###############################################################################

TEST OF SUBROUTINE PTLCL, AND FUNCTION ALCL
PURPOSE: GIVEN TEMPERATURE, DEW POINT, AND PRESSURE... PLTLCL
         ESTIMATES LCL TEMPERATURE AND PRESSURE; ALCL CALC-
         ULATES LCL PRESSURE ONLY.

PRES  TEMP  DWPT   PTLCL:P  PTLCL:T   ALCL
----  ----  ----   -------  -------  ------
1000   35    20     811.46   16.63   806.71
 900   25    20     837.04   18.83   840.12
1000   30    28     971.57   27.51   973.34
 850    0   -30     544.92  -34.66   528.01
 900   16    16     900.00   16.00   900.00
 700  -10   -60     333.50  -65.69   300.97

###############################################################################

TEST OF FUNCTION WOBF
PURPOSE: CALCULATE THE DIFFERENCE WBPTS-WBPTD (SEE NOTES IN 
         [SCHLATTER]THERMO.FOR FOR EXPLANATION) GIVEN THE TEMPER-
         ATURE. IN THIS CASE, EQUIVALENT POTENTIAL TEMPERATURE
         WAS INPUT SO RESULTS COULD BE CHECKED USING THE
         SMITHSONIAN TABLES.

 EQPT   WOBF(=EQPT-WBPT)  SMITH.(319)
 ----   ---------------   ------
205.24     165.2104       165.24
161.04     125.0629       125.04
113.04      83.1266        83.04
 70.34      48.8294        48.34
 54.84      37.4419        36.84
 42.14      28.6543        28.14
  6.74       8.8176         8.74

###############################################################################

TEST OF FUNCTIONS POWT,OW
PURPOSE: CALCULATE THE WET-BULB POTENTIAL TEMPERATURE GIVEN THE
         TEMPERATURE, DEW POINT, AND PRESSURE.

TEMP  DWPT  PRES   POWT    OW
----  ----  ----  -----   -----
 35    25   1000  27.55   27.61
 10   -15   1000   2.13    1.83
-10   -60    850  -4.64   -4.68
 15    10    900  16.29   16.42
 37    30   1050  29.98   30.13
 -5    -5    500  22.05   22.22

EFFICIENCY TEST: 500 CALCULATIONS AT T=10C, TD=-10C, P=850MB.

           FUNC.  T(SEC)
           -----  ------
           POWT    0.53
           OW     17.31

###############################################################################

TEST OF FUNCTIONS DPT,DEWPT
PURPOSE: CALCULATE DEW POINT GIVEN SATURATION VAPOR PRESSURE.

SAT.VP.    DPT    DEWPT    SMITH.(351)
-------  ------  ------    ------
0.1891  -40.002  -40.025    -40
1.2540  -20.000  -20.032    -20
2.8627  -10.000  -10.022    -10
6.1078    0.000   -0.010      0
12.272   10.000   10.000     10
23.373   20.000   20.003     20

EFFICIENCY TEST: 3000 CALCULATIONS AT SVP=.1891MB.

           FUNC.  T(SEC)
           -----  ------
           DPT     1.92
           DEWPT   0.59

###############################################################################

TEST OF FUNCTION SATLFT
PURPOSE: CALCULATE TEMPERATURE WHERE A MOIST ADIABAT CROSSES A GIVEN
         PRESSURE GIVEN THE VALUE OF THE MOIST ADIABAT AND THE 
         PRESSURE.

  PRES  THETAW   SATLFT   SMITH.
 -----  ------  -------   ------
 204.3    34    -19.791    -20
 200      20    -60.356    -62
1071.4    38     40.151     40
 501      32      9.941     10
 422.7     0    -51.876    -52

###############################################################################

TEST OF FUNCTION HUM
PURPOSE: CALCULATE RELATIVE HUMIDITY GIVEN TEMPERATURE AND 
         DEW POINT.

TEMP  DWPT   HUM
----  ----  -----
 35    30   75.46
 25    10   38.77
  0   -15   31.18
 20   -10   12.22
 30    28   89.09


TEST OF FUNCTION TW
PURPOSE: CALCULATE WET-BULB TEMPERATURE GIVEN TEMPERATURE, DEW
         POINT, AND PRESSURE.

TEMP  DWPT  PRES   TW
----  ----  ----  -----
 30    15   1000  19.99
  0   -20    700  -6.05
 10     0    850   5.11
-15   -25    500 -17.39
 25    20    900  21.57

###############################################################################

TEST OF FUNCTIONS OE,EPT
PURPOSE: CALCULATE EQUIVALENT POTENTIAL TEMPERATURE GIVEN TEMP-
         ERATURE, DEW POINT, AND PRESSURE.

TEMP  DWPT  PRES    OE     EPT
----  ----  ----   -----  -----
 30    15   1000   62.24  62.49
 0    -20    700   33.10  33.01
 10     0    850   36.97  37.01
-15   -25    500   45.04  45.02
 25    20    900   85.02  84.60

EFFICIENCY TEST: 1000 CALCULATIONS AT T=20C, TD=10C, P=1000MB.

           FUNC.  T(SEC)
           -----  ------
           OE      18.50
           EPT      0.80

###############################################################################

TEST OF FUNCTION TE
PURPOSE: CALCULATE EQUIVALENT TEMPERATURE GIVEN TEMPERATURE, DEW
         POINT, AND PRESSURE.

TEMP  DWPT  PRES    TE
----  ----  ----   -----
 30    15   1000   62.24
  0   -20    700    3.31
 10     0    850   22.88
-15   -25    500  -12.18
 25    20    900   74.38

###############################################################################

TEST OF FUNCTION TDA 
PURPOSE: CALCULATE THE TEMPERATURE ON A DRY ADIABAT GIVEN THE VALUE
         OF THE DRY ADIABAT AND THE PRESSURE.

 THETA  PRES     TDA     SMITH.(308)
 -----  ----    ------   ------
132.74   250     -0.12      0
 35.44   500    -20.05    -20
-14.66  1050    -11.03    -11
 28.64   850     14.93    -15
 18.24   700    -10.02    -10

###############################################################################

TEST OF FUNCTIONS TSA,TMLAPS
PURPOSE: CALCULATE TEMPERATURE ON A MOIST ADIABAT GIVEN 
         EQUIVALENT POTENTIAL TEMPERATURE AND PRESSURE.

  EQPT    PRES     TSA      TMLAPS    SMITH.(319)
------   ------   ------    -------   ------
181.64   870.9    34.170    33.940     34
113.04   577.0    12.139    12.080     12
 70.34   896.0    18.057    17.950     18
 10.24   496.0   -41.766   -41.669    -42
 54.84   309.5   -39.658   -39.600    -40

EFFICIENCY TEST: 500 CALCULATIONS AT EQPT=113.04, P=577MB

           FUNC.  T(SEC)
           -----  ------
           TSA     7.16
           TMLAPS  6.81

###############################################################################

TEST OF FUNCTION O
PURPOSE: CALCULATE POTENTIAL TEMPERATURE GIVEN TEMPERATURE AND
         PRESSURE.

TEMP  PRES      O    SMITH.(309)
----  ----    -----  ------
-35   850    -23.66  -23.66
-10   500     47.74   47.74
-20   950    -16.26  -16.26
 10  1000     10.04   10.00

###############################################################################

TEST OF FUNCTION OS
PURPOSE: CALCULATE EQUIVALENT POTENTIAL TEMPERATURE FOR AIR SAT-
         URATED AT GIVEN TEMPERATURE AND PRESSURE.

TEMP   PRES    OS      SMITH.(319)
----  -----  ------   -------
  0   275.3  179.88    181.64
-10   321.4  112.03    113.04
 10   501.0  126.27    127.34
 10   816.0   54.89     54.84
-50   282.7   47.63     48.14

###############################################################################

TEST OF FUNCTION TMR
PURPOSE: CALCULATE THE TEMPERATURE ALONG A GIVEN MIXING RATIO
         LINE AT A GIVEN PRESSURE.

  W   PRES   TMR    SMITH.(302)
----- ----  -----   ------
9.146  850  10.07    10
1.120  700 -19.96   -20
7.710  500   0.03     0
1.960  400 -19.97   -20
14.95 1000  20.10    20

###############################################################################

TEST OF FUNCTION THM
PURPOSE: CALCULATE WET-BULB POTENTIAL TEMPERATURE OF A PARCEL
         SATURATED AT A GIVEN TEMPERATURE AND PRESSURE.

TEMP    PRES     THM     SMITH.(319)
----   ------   ------   ------
 12    369.6    46.277    40
-32    188.2    41.376    32
 38    1069.8   35.751    36
 10     908.    14.205    14
-14     648.     7.841     8
-50    301.7    13.961    14

###############################################################################

TEST OF FUNCTION DWPT
PURPOSE: CALCULATE DEW POINT GIVEN TEMPERATURE AND RELATIVE HUM-
         IDITY.

TEMP  REL.HUM.   TD     TD*
----  -------- ------  -----
 35    75.46   30.14    30
 25    38.77    9.93    10
  0    31.18  -15.19   -15
 20    12.22  -10.16   -10
 30    89.09   28.01    28

         * INPUT RELATIVE HUMIDITY FROM OUTPUT OF FUNCTION HUM.
           EXPECT DEW POINT OUTPUT TO BE APPROX. DEW POINT INPUT
           USED IN "HUM". SEE TEST OF FUNCTION HUM FOR COMPARISON.

###############################################################################

TEST OF FUNCTION SSH
PURPOSE: CALCULATE SATURATION SPECIFIC HUMIDITY GIVEN THE 
         PRESSURE AND TEMPERATURE.

PRES  TEMP   SSH          !CHECKED ON HAND CALCULATOR.
----  ----  -----
1000   10    7.70
1000    0    3.82
 850  -10    2.11
 700    0    5.46
 500  -20    1.57
 300  -30    1.06

###############################################################################

TEST OF FUNCTION TV
PURPOSE: CALCULATE VIRTUAL TEMPERATURE GIVEN TEMPERATURE, DEW POINT,
         AND PRESSURE.

TEMP  DWPT  PRES    TV        !CHECKED ON HAND CALCULATOR.
----  ----  ----   -----
 30    25   1000   33.69
 20     0   1000   20.68
 10   -10    850   10.36
  0   -10    700    0.42
-20   -30    500  -19.90
-40   -60    300  -39.99

###############################################################################

TEST OF FUNCTIONS ESICE,ESILO
PURPOSE: CALCULATE SATURATION VAPOR PRESSURE OVER ICE GIVEN TEMP-
         ERATURE.

TEMP   ESICE    ESILO    SMITH.(360)
----  -------  -------   ------
-50   .03935   .03963   .03935
-40   .1283    .1283    .1283
-30   .3798    .3796    .3798
-20   1.032    1.032    1.032
-10   2.597    2.596    2.597

EFFICIENCY TEST: 2000 CALCULATIONS AT T=-10C.

           FUNC.  T(SEC)
           -----  ------
           ESICE   1.10
           ESILO   0.39

###############################################################################

TEST OF FUNCTION HEATL
PURPOSE: GIVEN TEMPERATURE, TO CALCULATE.....
         1) LATENT HEAT OF EVAPORATION/CONDENSATION (KEY=1)
         2) LATENT HEAT OF FREEZING/MELTING         (KEY=2)
         3) LATENT HEAT OF SUBLIMATION/DEPOSITION   (KEY=3)
        
         (RESULTS VERIFIED USING P.343, SMITH.)

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

NOTE: HEATL IS DESIGNED TO RETURN THE LATENT HEATS IN UNITS OF
      JOULES/KG.  TO MAKE RESULTS COMPATIBLE WITH VALUES IN THE
      SMITHSONIAN TABLES (IT-CAL/GRAM), ANSWERS WERE MULTIPLIED
      BY THE CORRECTION FACTOR 2.3884E-04 TO OBTAIN THOSE SHOWN
      ABOVE.

###############################################################################


USER_DEV:[GULIB.THERMOSRC]ALCL_TG.FOR;1

        FUNCTION ALCL(T,TD,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE PRESSURE ALCL (MB) OF THE LIFTING CONDEN-
C   SATION LEVEL (LCL) FOR A PARCEL INITIALLY AT TEMPERATURE T (CELSIUS)
C   DEW POINT TD (CELSIUS) AND PRESSURE P (MILLIBARS). ALCL IS COMPUTED
C   BY AN ITERATIVE PROCEDURE DESCRIBED BY EQS. 8-12 IN STIPANUK (1973),
C   PP.13-14.
C   DETERMINE THE MIXING RATIO LINE THROUGH TD AND P.

        AW = W(TD,P)

C   DETERMINE THE DRY ADIABAT THROUGH T AND P.

        AO = O(T,P)

C   ITERATE TO LOCATE PRESSURE PI AT THE INTERSECTION OF THE TWO
C   CURVES. PI HAS BEEN SET TO P FOR THE INITIAL GUESS.

 3      CONTINUE
           PI = P
           DO 4 I= 1,10
              X= .02*(TMR(AW,PI)-TDA(AO,PI))
              IF (ABS(X).LT.0.01) GO TO 5
 4            PI= PI*(2.**(X))
 5         ALCL= PI
        RETURN

        ENTRY ALCLM(T,TD,P)
C   FOR ENTRY ALCLM ONLY, T IS THE MEAN POTENTIAL TEMPERATURE (CELSIUS)
C   AND TD IS THE MEAN MIXING RATIO (G/KG) OF THE LAYER CONTAINING THE
C   PARCEL.

        AW = TD
        AO = T
        GO TO 3
        END

USER_DEV:[GULIB.THERMOSRC]CT_TG.FOR;1

        FUNCTION CT(WBAR,PC,PS)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE CONVECTIVE TEMPERATURE CT (CELSIUS)
C   GIVEN THE MEAN MIXING RATIO WBAR (G/KG) IN THE SURFACE LAYER,
C   THE PRESSURE PC (MB) AT THE CONVECTIVE CONDENSATION LEVEL (CCL)
C   AND THE SURFACE PRESSURE PS (MB).
C   COMPUTE THE TEMPERATURE (CELSIUS) AT THE CCL.

        TC= TMR(WBAR,PC)

C   COMPUTE THE POTENTIAL TEMPERATURE (CELSIUS), I.E., THE DRY
C   ADIABAT AO THROUGH THE CCL.

        AO= O(TC,PC)

C   COMPUTE THE SURFACE TEMPERATURE ON THE SAME DRY ADIABAT AO.

        CT= TDA(AO,PS)
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]DEWPT_TG.FOR;1

        FUNCTION DEWPT(EW)

C   THIS FUNCTION YIELDS THE DEW POINT DEWPT (CELSIUS), GIVEN THE
C   WATER VAPOR PRESSURE EW (MILLIBARS).
C   THE EMPIRICAL FORMULA APPEARS IN BOLTON, DAVID, 1980:
C   "THE COMPUTATION OF EQUIVALENT POTENTIAL TEMPERATURE,"
C   MONTHLY WEATHER REVIEW, VOL. 108, NO. 7 (JULY), P. 1047, EQ.(11).
C   THE QUOTED ACCURACY IS 0.03C OR LESS FOR -35 < DEWPT < 35C.

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

        ENL = ALOG(EW)
        DEWPT = (243.5*ENL-440.8)/(19.48-ENL)
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]DPT_TG.FOR;1

        FUNCTION DPT(EW)

C   THIS FUNCTION RETURNS THE DEW POINT DPT (CELSIUS), GIVEN THE
C   WATER VAPOR PRESSURE EW (MILLIBARS).

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

        DATA ES0/6.1078/

C   ES0 = SATURATION VAPOR PRESSURE (MB) OVER WATER AT 0C
C   RETURN A FLAG VALUE IF THE VAPOR PRESSURE IS OUT OF RANGE.

        IF (EW.GT..06.AND.EW.LT.1013.) GO TO 5
        DPT = 9999.
        RETURN
    5   CONTINUE

C   APPROXIMATE DEW POINT BY MEANS OF TETEN'S FORMULA.
C   THE FORMULA APPEARS AS EQ.(8) IN BOLTON, DAVID, 1980:
C   "THE COMPUTATION OF EQUIVALENT POTENTIAL TEMPERATURE,"
C   MONTHLY WEATHER REVIEW, VOL 108, NO. 7 (JULY), P.1047.
C   THE FORMULA IS EW(T) = ES0*10**(7.5*T/(T+237.3))
C            OR    EW(T) = ES0*EXP(17.269388*T/(T+237.3))
C   THE INVERSE FORMULA IS USED BELOW.

        X = ALOG(EW/ES0)
        DNM = 17.269388-X
        T = 237.3*X/DNM
        FAC = 1./(EW*DNM)

C   LOOP FOR ITERATIVE IMPROVEMENT OF THE ESTIMATE OF DEW POINT

   10   CONTINUE

C   GET THE PRECISE VAPOR PRESSURE CORRESPONDING TO T.

        EDP = ESW(T)

C   ESTIMATE THE CHANGE IN TEMPERATURE CORRESPONDING TO (EW-EDP)
C   ASSUME THAT THE DERIVATIVE OF TEMPERATURE WITH RESPECT TO 
C   VAPOR PRESSURE (DTDEW) IS GIVEN BY THE DERIVATIVE OF THE
C   INVERSE TETEN FORMULA.

        DTDEW = (T+237.3)*FAC
        DT = DTDEW*(EW-EDP)
        T = T+DT
        IF (ABS(DT).GT.1.E-04) GO TO 10
        DPT = T
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]DWPT_TG.FOR;1

        FUNCTION DWPT(T,RH)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE DEW POINT (CELSIUS) GIVEN THE TEMPERATURE
C   (CELSIUS) AND RELATIVE HUMIDITY (%). THE FORMULA IS USED IN THE
C   PROCESSING OF U.S. RAWINSONDE DATA AND IS REFERENCED IN PARRY, H.
C   DEAN, 1969: "THE SEMIAUTOMATIC COMPUTATION OF RAWINSONDES,"
C   TECHNICAL MEMORANDUM WBTM EDL 10, U.S. DEPARTMENT OF COMMERCE,
C   ENVIRONMENTAL SCIENCE SERVICES ADMINISTRATION, WEATHER BUREAU,
C   OFFICE OF SYSTEMS DEVELOPMENT, EQUIPMENT DEVELOPMENT LABORATORY,
C   SILVER SPRING, MD (OCTOBER), PAGE 9 AND PAGE II-4, LINE 460.

        X = 1.-0.01*RH

C   COMPUTE DEW POINT DEPRESSION.

        DPD =(14.55+0.114*T)*X+((2.5+0.007*T)*X)**3+(15.9+0.117*T)*X**14
        DWPT = T-DPD
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]EPT_TG.FOR;1

        FUNCTION EPT(T,TD,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE EQUIVALENT POTENTIAL TEMPERATURE EPT
C   (CELSIUS) FOR A PARCEL OF AIR INITIALLY AT TEMPERATURE T (CELSIUS),
C   DEW POINT TD (CELSIUS) AND PRESSURE P (MILLIBARS). THE FORMULA USED
C   IS EQ.(43) IN BOLTON, DAVID, 1980: "THE COMPUTATION OF EQUIVALENT
C   POTENTIAL TEMPERATURE," MONTHLY WEATHER REVIEW, VOL. 108, NO. 7
C   (JULY), PP. 1046-1053. THE MAXIMUM ERROR IN EPT IN 0.3C.  IN MOST
C   CASES THE ERROR IS LESS THAN 0.1C.
C
C   COMPUTE THE MIXING RATIO (GRAMS OF WATER VAPOR PER KILOGRAM OF
C   DRY AIR).

        W = WMR(P,TD)

C   COMPUTE THE TEMPERATURE (CELSIUS) AT THE LIFTING CONDENSATION LEVEL.

        TLCL = TCON(T,TD)
        TK = T+273.15
        TL = TLCL+273.15
        PT = TK*(1000./P)**(0.2854*(1.-0.00028*W))
        EPTK = PT*EXP((3.376/TL-0.00254)*W*(1.+0.00081*W))
        EPT= EPTK-273.15
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]ESAT_TG.FOR;1

        FUNCTION ESAT(T)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE SATURATION VAPOR PRESSURE OVER
C   WATER (MB) GIVEN THE TEMPERATURE (CELSIUS).
C   THE ALGORITHM IS DUE TO NORDQUIST, W.S.,1973: "NUMERICAL APPROXIMA-
C   TIONS OF SELECTED METEORLOLGICAL PARAMETERS FOR CLOUD PHYSICS PROB-
C   LEMS," ECOM-5475, ATMOSPHERIC SCIENCES LABORATORY, U.S. ARMY
C   ELECTRONICS COMMAND, WHITE SANDS MISSILE RANGE, NEW MEXICO 88002.

        TK = T+273.15
        P1 = 11.344-0.0303998*TK
        P2 = 3.49149-1302.8844/TK
        C1 = 23.832241-5.02808*ALOG10(TK)
        ESAT = 10.**(C1-1.3816E-7*10.**P1+8.1328E-3*10.**P2-2949.076/TK)
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]ESGG_TG.FOR;1

        FUNCTION ESGG(T)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE SATURATION VAPOR PRESSURE OVER LIQUID
C   WATER ESGG (MILLIBARS) GIVEN THE TEMPERATURE T (CELSIUS). THE
C   FORMULA USED, DUE TO GOFF AND GRATCH, APPEARS ON P. 350 OF THE
C   SMITHSONIAN METEOROLOGICAL TABLES, SIXTH REVISED EDITION, 1963,
C   BY ROLAND LIST.

        DATA CTA,EWS,TS/273.15,1013.246,373.15/

C   CTA = DIFFERENCE BETWEEN KELVIN AND CELSIUS TEMPERATURES
C   EWS = SATURATION VAPOR PRESSURE (MB) OVER LIQUID WATER AT 100C
C   TS = BOILING POINT OF WATER (K)

        DATA C1,      C2,      C3,      C4,       C5,       C6
     1  / 7.90298, 5.02808, 1.3816E-7, 11.344, 8.1328E-3, 3.49149 /
        TK = T+CTA

C   GOFF-GRATCH FORMULA

        RHS = -C1*(TS/TK-1.)+C2*ALOG10(TS/TK)-C3*(10.**(C4*(1.-TK/TS))
     1        -1.)+C5*(10.**(-C6*(TS/TK-1.))-1.)+ALOG10(EWS)
        ESW = 10.**RHS
        IF (ESW.LT.0.) ESW = 0.
        ESGG = ESW
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]ESICE_TG.FOR;2

        FUNCTION ESICE(T)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE SATURATION VAPOR PRESSURE WITH RESPECT TO
C   ICE ESICE (MILLIBARS) GIVEN THE TEMPERATURE T (CELSIUS).
C   THE FORMULA USED IS BASED UPON THE INTEGRATION OF THE CLAUSIUS-
C   CLAPEYRON EQUATION BY GOFF AND GRATCH.  THE FORMULA APPEARS ON P.350
C   OF THE SMITHSONIAN METEOROLOGICAL TABLES, SIXTH REVISED EDITION,
C   1963.

        DATA CTA,EIS/273.15,6.1071/

C   CTA = DIFFERENCE BETWEEN KELVIN AND CELSIUS TEMPERATURE
C   EIS = SATURATION VAPOR PRESSURE (MB) OVER A WATER-ICE MIXTURE AT 0C

        DATA C1,C2,C3/9.09718,3.56654,0.876793/

C   C1,C2,C3 = EMPIRICAL COEFFICIENTS IN THE GOFF-GRATCH FORMULA

        IF (T.LE.0.) GO TO 5
        ESICE = 99999.
        WRITE(6,3)ESICE
        UNLOCK (6)
    3   FORMAT(' SATURATION VAPOR PRESSURE FOR ICE CANNOT BE COMPUTED',
     1         /' FOR TEMPERATURE > 0C. ESICE =',F7.0)
        RETURN
    5   CONTINUE

C   FREEZING POINT OF WATER (K)

        TF = CTA
        TK = T+CTA

C   GOFF-GRATCH FORMULA

        RHS = -C1*(TF/TK-1.)-C2*ALOG10(TF/TK)+C3*(1.-TK/TF)+ALOG10(EIS)
        ESI = 10.**RHS
        IF (ESI.LT.0.) ESI = 0.
        ESICE = ESI
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]ESILO_TG.FOR;2

        FUNCTION ESILO(T)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE SATURATION VAPOR PRESSURE OVER ICE
C   ESILO (MILLIBARS) GIVEN THE TEMPERATURE T (CELSIUS). THE FORMULA
C   IS DUE TO LOWE, PAUL R., 1977: AN APPROXIMATING POLYNOMIAL FOR 
C   THE COMPUTATION OF SATURATION VAPOR PRESSURE, JOURNAL OF APPLIED
C   METEOROLOGY, VOL. 16, NO. 1 (JANUARY), PP. 100-103.
C   THE POLYNOMIAL COEFFICIENTS ARE A0 THROUGH A6.

        DATA A0,A1,A2,A3,A4,A5,A6
     1  /6.109177956,     5.034698970E-01, 1.886013408E-02,
     2   4.176223716E-04, 5.824720280E-06, 4.838803174E-08,
     3   1.838826904E-10/
        IF (T.LE.0.) GO TO 5
        ESILO = 9999.
        WRITE(6,3)ESILO
        UNLOCK (6)
    3   FORMAT(' SATURATION VAPOR PRESSURE OVER ICE IS UNDEFINED FOR',
     1  /' TEMPERATURE > 0C. ESILO =',F6.0)
        RETURN
    5   CONTINUE
        ESILO = A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+A6*T)))))
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]ESLO_TG.FOR;1

        FUNCTION ESLO(T)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE SATURATION VAPOR PRESSURE OVER LIQUID
C   WATER ESLO (MILLIBARS) GIVEN THE TEMPERATURE T (CELSIUS). THE
C   FORMULA IS DUE TO LOWE, PAUL R.,1977: AN APPROXIMATING POLYNOMIAL
C   FOR THE COMPUTATION OF SATURATION VAPOR PRESSURE, JOURNAL OF APPLIED
C   METEOROLOGY, VOL 16, NO. 1 (JANUARY), PP. 100-103.
C   THE POLYNOMIAL COEFFICIENTS ARE A0 THROUGH A6.

        DATA A0,A1,A2,A3,A4,A5,A6
     1  /6.107799961,     4.436518521E-01, 1.428945805E-02,
     2   2.650648471E-04, 3.031240396E-06, 2.034080948E-08,
     3   6.136820929E-11/
        ES = A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+A6*T)))))
        IF (ES.LT.0.) ES = 0.
        ESLO = ES
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]ESRW_TG.FOR;1

        FUNCTION ESRW(T)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE SATURATION VAPOR PRESSURE OVER LIQUID
C   WATER ESRW (MILLIBARS) GIVEN THE TEMPERATURE T (CELSIUS). THE
C   FORMULA USED IS DUE TO RICHARDS, J.M., 1971: SIMPLE EXPRESSION
C   FOR THE SATURATION VAPOUR PRESSURE OF WATER IN THE RANGE -50 TO
C   140C, BRITISH JOURNAL OF APPLIED PHYSICS, VOL. 4, PP.L15-L18.
C   THE FORMULA WAS QUOTED MORE RECENTLY BY WIGLEY, T.M.L.,1974:
C   COMMENTS ON 'A SIMPLE BUT ACCURATE FORMULA FOR THE SATURATION
C   VAPOR PRESSURE OVER LIQUID WATER,' JOURNAL OF APPLIED METEOROLOGY,
C   VOL. 13, NO. 5 (AUGUST) P.606.

        DATA CTA,TS,EWS/273.15,373.15,1013.25/

C   CTA = DIFFERENCE BETWEEN KELVIN AND CELSIUS TEMPERATURE
C   TS = TEMPERATURE OF THE BOILING POINT OF WATER (K)
C   EWS = SATURATION VAPOR PRESSURE OVER LIQUID WATER AT 100C

        DATA C1,     C2,     C3,     C4
     1  / 13.3185,-1.9760,-0.6445,-0.1299 /
        TK = T+CTA
        X = 1.-TS/TK
        PX = X*(C1+X*(C2+X*(C3+C4*X)))
        VP = EWS*EXP(PX)
        IF (VP.LT.0) VP = 0.
        ESRW = VP
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]ESW_TG.FOR;1

        FUNCTION ESW(T)

C   THIS FUNCTION RETURNS THE SATURATION VAPOR PRESSURE ESW (MILLIBARS)
C   OVER LIQUID WATER GIVEN THE TEMPERATURE T (CELSIUS). THE POLYNOMIAL
C   APPROXIMATION BELOW IS DUE TO HERMAN WOBUS, A MATHEMATICIAN WHO
C   WORKED AT THE NAVY WEATHER RESEARCH FACILITY, NORFOLK, VIRGINIA,
C   BUT WHO IS NOW RETIRED. THE COEFFICIENTS OF THE POLYNOMIAL WERE
C   CHOSEN TO FIT THE VALUES IN TABLE 94 ON PP. 351-353 OF THE SMITH-
C   SONIAN METEOROLOGICAL TABLES BY ROLAND LIST (6TH EDITION). THE
C   APPROXIMATION IS VALID FOR -50 < T < 100C.

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C
C   ES0 = SATURATION VAPOR RESSURE OVER LIQUID WATER AT 0C

        DATA ES0/6.1078/
        POL = 0.99999683       + T*(-0.90826951E-02 +
     1     T*(0.78736169E-04   + T*(-0.61117958E-06 +
     2     T*(0.43884187E-08   + T*(-0.29883885E-10 +
     3     T*(0.21874425E-12   + T*(-0.17892321E-14 +
     4     T*(0.11112018E-16   + T*(-0.30994571E-19)))))))))
        ESW = ES0/POL**8
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]ES_TG.FOR;1

        FUNCTION ES(T)

C   THIS FUNCTION RETURNS THE SATURATION VAPOR PRESSURE ES (MB) OVER
C   LIQUID WATER GIVEN THE TEMPERATURE T (CELSIUS). THE FORMULA APPEARS
C   IN BOLTON, DAVID, 1980: "THE COMPUTATION OF EQUIVALENT POTENTIAL
C   TEMPERATURE," MONTHLY WEATHER REVIEW, VOL. 108, NO. 7 (JULY),
C   P. 1047, EQ.(10). THE QUOTED ACCURACY IS 0.3% OR BETTER FOR
C   -35 < T < 35C.

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   ES0 = SATURATION VAPOR PRESSURE OVER LIQUID WATER AT 0C

        DATA ES0/6.1121/
        ES = ES0*EXP(17.67*T/(T+243.5))
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]HEATL_TG.FOR;1

        FUNCTION HEATL(KEY,T)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE LATENT HEAT OF
C               EVAPORATION/CONDENSATION         FOR KEY=1
C               MELTING/FREEZING                 FOR KEY=2
C               SUBLIMATION/DEPOSITION           FOR KEY=3
C   FOR WATER. THE LATENT HEAT HEATL (JOULES PER KILOGRAM) IS A 
C   FUNCTION OF TEMPERATURE T (CELSIUS). THE FORMULAS ARE POLYNOMIAL
C   APPROXIMATIONS TO THE VALUES IN TABLE 92, P. 343 OF THE SMITHSONIAN
C   METEOROLOGICAL TABLES, SIXTH REVISED EDITION, 1963 BY ROLAND LIST.
C   THE APPROXIMATIONS WERE DEVELOPED BY ERIC SMITH AT COLORADO STATE
C   UNIVERSITY.
C   POLYNOMIAL COEFFICIENTS

        DATA A0,A1,A2/ 3337118.5,-3642.8583, 2.1263947/
        DATA B0,B1,B2/-1161004.0, 9002.2648,-12.931292/
        DATA C0,C1,C2/ 2632536.8, 1726.9659,-3.6248111/
        HLTNT = 0.
        TK = T+273.15
        IF (KEY.EQ.1) HLTNT=A0+A1*TK+A2*TK*TK
        IF (KEY.EQ.2) HLTNT=B0+B1*TK+B2*TK*TK
        IF (KEY.EQ.3) HLTNT=C0+C1*TK+C2*TK*TK
        HEATL = HLTNT
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]HUM_TG.FOR;1

        FUNCTION HUM(T,TD)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS RELATIVE HUMIDITY (%) GIVEN THE
C   TEMPERATURE T AND DEW POINT TD (CELSIUS).  AS CALCULATED HERE,
C   RELATIVE HUMIDITY IS THE RATIO OF THE ACTUAL VAPOR PRESSURE TO
C   THE SATURATION VAPOR PRESSURE.

        HUM= 100.*(ESAT(TD)/ESAT(T))
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]OE_TG.FOR;1

        FUNCTION OE(T,TD,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS EQUIVALENT POTENTIAL TEMPERATURE OE (CELSIUS)
C   OF A PARCEL OF AIR GIVEN ITS TEMPERATURE T (CELSIUS), DEW POINT
C   TD (CELSIUS) AND PRESSURE P (MILLIBARS).
C   FIND THE WET BULB TEMPERATURE OF THE PARCEL.

        ATW = TW(T,TD,P)

C   FIND THE EQUIVALENT POTENTIAL TEMPERATURE.

        OE = OS(ATW,P)
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]OS_TG.FOR;1

        FUNCTION OS(T,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE EQUIVALENT POTENTIAL TEMPERATURE OS
C   (CELSIUS) FOR A PARCEL OF AIR SATURATED AT TEMPERATURE T (CELSIUS)
C   AND PRESSURE P (MILLIBARS).
        DATA B/2.6518986/
C   B IS AN EMPIRICAL CONSTANT APPROXIMATELY EQUAL TO THE LATENT HEAT
C   OF VAPORIZATION FOR WATER DIVIDED BY THE SPECIFIC HEAT AT CONSTANT
C   PRESSURE FOR DRY AIR.

        TK = T+273.15
        OSK= TK*((1000./P)**.286)*(EXP(B*W(T,P)/TK))
        OS= OSK-273.15
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]OW_TG.FOR;1

        FUNCTION OW(T,TD,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE WET-BULB POTENTIAL TEMPERATURE OW
C   (CELSIUS) GIVEN THE TEMPERATURE T (CELSIUS), DEW POINT TD
C   (CELSIUS), AND PRESSURE P (MILLIBARS).  THE CALCULATION FOR OW IS
C   VERY SIMILAR TO THAT FOR WET BULB TEMPERATURE. SEE P.13 STIPANUK (1973).
C   FIND THE WET BULB TEMPERATURE OF THE PARCEL

        ATW = TW(T,TD,P)

C   FIND THE EQUIVALENT POTENTIAL TEMPERATURE OF THE PARCEL.

        AOS= OS(ATW,P)

C   FIND THE WET-BULB POTENTIAL TEMPERATURE.

        OW= TSA(AOS,1000.)
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]O_TG.FOR;1

        FUNCTION O(T,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS POTENTIAL TEMPERATURE (CELSIUS) GIVEN
C   TEMPERATURE T (CELSIUS) AND PRESSURE P (MB) BY SOLVING THE POISSON
C   EQUATION.

        TK= T+273.15
        OK= TK*((1000./P)**.286)
        O= OK-273.15
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]PCCL_TG.FOR;1

        FUNCTION PCCL(PM,P,T,TD,WBAR,N)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE PRESSURE AT THE CONVECTIVE CONDENSATION
C   LEVEL GIVEN THE APPROPRIATE SOUNDING DATA.
C   ON INPUT:
C       P = PRESSURE (MILLIBARS). NOTE THAT P(I).GT.P(I+1).
C       T = TEMPERATURE (CELSIUS)
C       TD = DEW POINT (CELSIUS)
C       N = NUMBER OF LEVELS IN THE SOUNDING AND THE DIMENSION OF
C           P, T AND TD
C       PM = PRESSURE (MILLIBARS) AT UPPER BOUNDARY OF THE LAYER FOR
C            COMPUTING THE MEAN MIXING RATIO. P(1) IS THE LOWER 
C            BOUNDARY.
C   ON OUTPUT:
C       PCCL = PRESSURE (MILLIBARS) AT THE CONVECTIVE CONDENSATION LEVEL
C       WBAR = MEAN MIXING RATIO (G/KG) IN THE LAYER BOUNDED BY
C              PRESSURES P(1) AT THE BOTTOM AND PM AT THE TOP
C   THE ALGORITHM IS DECRIBED ON P.17 OF STIPANUK, G.S.,1973:
C   "ALGORITHMS FOR GENERATING A SKEW-T LOG P DIAGRAM AND COMPUTING
C   SELECTED METEOROLOGICAL QUANTITIES," ATMOSPHERIC SCIENCES LABORA-
C   TORY, U.S. ARMY ELECTRONICS COMMAND, WHITE SANDS MISSILE RANGE, NEW
C   MEXICO 88002.

        DIMENSION T(1),P(1),TD(1)
        IF (PM.NE.P(1)) GO TO 5
        WBAR= W(TD(1),P(1))
        PC= PM
        IF (ABS(T(1)-TD(1)).LT.0.05) GO TO 45
        GO TO 25
    5   CONTINUE
        WBAR= 0.
        K= 0
   10   CONTINUE
        K = K+1
        IF (P(K).GT.PM) GO TO 10
        K= K-1
        J= K-1
        IF(J.LT.1) GO TO 20

C   COMPUTE THE AVERAGE MIXING RATIO....ALOG = NATURAL LOG

        DO 15 I= 1,J
           L= I+1
   15      WBAR= (W(TD(I),P(I))+W(TD(L),P(L)))*ALOG(P(I)/P(L))
     *          +WBAR
   20   CONTINUE
        L= K+1

C   ESTIMATE THE DEW POINT AT PRESSURE PM.

        TQ= TD(K)+(TD(L)-TD(K))*(ALOG(PM/P(K)))/(ALOG(P(L)/P(K)))
        WBAR= WBAR+(W(TD(K),P(K))+W(TQ,PM))*ALOG(P(K)/PM)
        WBAR= WBAR/(2.*ALOG(P(1)/PM))

C   FIND LEVEL AT WHICH THE MIXING RATIO LINE WBAR CROSSES THE
C   ENVIRONMENTAL TEMPERATURE PROFILE.

   25   CONTINUE
        DO 30 J= 1,N
           I= N-J+1
           IF (P(I).LT.300.) GO TO 30

C   TMR = TEMPERATURE (CELSIUS) AT PRESSURE P (MB) ALONG A MIXING
C         RATIO LINE GIVEN BY WBAR (G/KG)

           X= TMR(WBAR,P(I))-T(I)
           IF (X.LE.0.) GO TO 35
   30   CONTINUE
        PCCL= 0.0
        RETURN

C  SET UP BISECTION ROUTINE

   35   L = I
        I= I+1
        DEL= P(L)-P(I)
        PC= P(I)+.5*DEL
        A= (T(I)-T(L))/ALOG(P(L)/P(I))
        DO 40 J= 1,10
           DEL= DEL/2.
           X= TMR(WBAR,PC)-T(L)-A*(ALOG(P(L)/PC))

C   THE SIGN FUNCTION REPLACES THE SIGN OF THE FIRST ARGUMENT
C   WITH THAT OF THE SECOND.

   40   PC= PC+SIGN(DEL,X)
   45   PCCL = PC
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]PCON_TG.FOR;1

        FUNCTION PCON(P,T,TC)

C   THIS FUNCTION RETURNS THE PRESSURE PCON (MB) AT THE LIFTED CONDENSA-
C   TION LEVEL, GIVEN THE INITIAL PRESSURE P (MB) AND TEMPERATURE T
C   (CELSIUS) OF THE PARCEL AND THE TEMPERATURE TC (CELSIUS) AT THE 
C   LCL. THE ALGORITHM IS EXACT.  IT MAKES USE OF THE FORMULA FOR THE
C   POTENTIAL TEMPERATURES CORRESPONDING TO T AT P AND TC AT PCON.
C   THESE TWO POTENTIAL TEMPERATURES ARE EQUAL.

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C
        DATA AKAPI/3.5037/

C   AKAPI = (SPECIFIC HEAT AT CONSTANT PRESSURE FOR DRY AIR) /
C           (GAS CONSTANT FOR DRY AIR)

C   CONVERT T AND TC TO KELVIN TEMPERATURES.

        TK = T+273.15
        TCK = TC+273.15
        PCON = P*(TCK/TK)**AKAPI
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]POWT_TG.FOR;1

        FUNCTION POWT(T,P,TD)

C   THIS FUNCTION YIELDS WET-BULB POTENTIAL TEMPERATURE POWT
C   (CELSIUS), GIVEN THE FOLLOWING INPUT:
C          T = TEMPERATURE (CELSIUS)
C          P = PRESSURE (MILLIBARS)
C          TD = DEW POINT (CELSIUS)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

        DATA CTA,AKAP/273.15,0.28541/

C   CTA = DIFFERENCE BETWEEN KELVIN AND CELSIUS TEMPERATURES
C   AKAP = (GAS CONSTANT FOR DRY AIR) / (SPECIFIC HEAT AT
C          CONSTANT PRESSURE FOR DRY AIR)
C   COMPUTE THE POTENTIAL TEMPERATURE (CELSIUS)

        PT = (T+CTA)*(1000./P)**AKAP-CTA

C   COMPUTE THE LIFTING CONDENSATION LEVEL (LCL).

        TC = TCON(T,TD)

C   FOR THE ORIGIN OF THE FOLLOWING APPROXIMATION, SEE THE DOCUMEN-
C   TATION FOR THE WOBUS FUNCTION.

        POWT = PT-WOBF(PT)+WOBF(TC)
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]PRECPW_TG.FOR;1

        FUNCTION PRECPW(TD,P,N)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION COMPUTES TOTAL PRECIPITABLE WATER PRECPW (CM) IN A
C   VERTICAL COLUMN OF AIR BASED UPON SOUNDING DATA AT N LEVELS:
C          TD = DEW POINT (CELSIUS)
C          P = PRESSURE (MILLIBARS)
C   CALCULATIONS ARE DONE IN CGS UNITS.

        DIMENSION TD(N),P(N)

C   G = ACCELERATION DUE TO THE EARTH'S GRAVITY (CM/S**2)

        DATA G/980.616/

C   INITIALIZE VALUE OF PRECIPITABLE WATER

        PW = 0.
        NL = N-1

C   CALCULATE THE MIXING RATIO AT THE LOWEST LEVEL.

        WBOT = WMR(P(1),TD(1))
        DO 5 I=1,NL
        WTOP = WMR(P(I+1),TD(I+1))

C   CALCULATE THE LAYER-MEAN MIXING RATIO (G/KG).

        W = 0.5*(WTOP+WBOT)

C   MAKE THE MIXING RATIO DIMENSIONLESS.

        WL = .001*W

C   CALCULATE THE SPECIFIC HUMIDITY.

        QL = WL/(WL+1.)

C   THE FACTOR OF 1000. BELOW CONVERTS FROM MILLIBARS TO DYNES/CM**2.

        DP = 1000.*(P(I)-P(I+1))
        PW = PW+(QL/G)*DP
        WBOT = WTOP
    5   CONTINUE
        PRECPW = PW
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]PTLCL_TG.FOR;1

        SUBROUTINE PTLCL(P,T,TD,PC,TC)

C   THIS SUBROUTINE ESTIMATES THE PRESSURE PC (MB) AND THE TEMPERATURE
C   TC (CELSIUS) AT THE LIFTED CONDENSATION LEVEL (LCL), GIVEN THE
C   INITIAL PRESSURE P (MB), TEMPERATURE T (CELSIUS) AND DEW POINT
C   (CELSIUS) OF THE PARCEL.  THE APPROXIMATION IS THAT LINES OF 
C   CONSTANT POTENTIAL TEMPERATURE AND CONSTANT MIXING RATIO ARE
C   STRAIGHT ON THE SKEW T/LOG P CHART.
C   TETEN'S FORMULA FOR SATURATION VAPOR PRESSURE AS A FUNCTION OF
C   PRESSURE WAS USED IN THE DERIVATION OF THE FORMULA BELOW.  FOR
C   ADDITIONAL DETAILS, SEE MATH NOTES BY T. SCHLATTER DATED 8 SEP 81.
C   T. SCHLATTER, NOAA/ERL/PROFS PROGRAM OFFICE, BOULDER, COLORADO,
C   WROTE THIS SUBROUTINE.

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   AKAP = (GAS CONSTANT FOR DRY AIR) / (SPECIFIC HEAT AT CONSTANT
C          PRESSURE FOR DRY AIR)
C   CTA = DIFFERENCE BETWEEN KELVIN AND CELSIUS TEMPERATURES

        DATA AKAP,CTA/0.28541,273.15/
        C1 = 4098.026/(TD+237.3)**2
        C2 = 1./(AKAP*(T+CTA))
        PC = P*EXP(C1*C2*(T-TD)/(C2-C1))
        TC = T+C1*(T-TD)/(C2-C1)
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]SATLFT_TG.FOR;1

        FUNCTION SATLFT(THW,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   INPUT:  THW = WET-BULB POTENTIAL TEMPERATURE (CELSIUS).
C                 THW DEFINES A MOIST ADIABAT.
C           P = PRESSURE (MILLIBARS)
C   OUTPUT: SATLFT = TEMPERATURE (CELSIUS) WHERE THE MOIST ADIABAT
C                 CROSSES P

        DATA CTA,AKAP/273.15,0.28541/

C   CTA = DIFFERENCE BETWEEN KELVIN AND CELSIUS TEMPERATURES
C   AKAP = (GAS CONSTANT FOR DRY AIR) / (SPECIFIC HEAT AT CONSTANT
C           PRESSURE FOR DRY AIR)

C        THE ALGORITHM BELOW CAN BEST BE UNDERSTOOD BY REFERRING TO A
C   SKEW-T/LOG P CHART.  IT WAS DEVISED BY HERMAN WOBUS, A MATHEMATI-
C   CIAN FORMERLY AT THE NAVY WEATHER RESEARCH FACILITY BUT NOW RETIRED.
C   THE VALUE RETURNED BY SATLFT CAN BE CHECKED BY REFERRING TO TABLE
C   78, PP.319-322, SMITHSONIAN METEOROLOGICAL TABLES, BY ROLAND LIST
C   (6TH REVISED EDITION).
C

        IF (P.NE.1000.) GO TO 5
        SATLFT = THW
        RETURN
    5   CONTINUE

C   COMPUTE TONE, THE TEMPERATURE WHERE THE DRY ADIABAT WITH VALUE THW
C   (CELSIUS) CROSSES P.

        PWRP = (P/1000.)**AKAP
        TONE = (THW+CTA)*PWRP-CTA

C   CONSIDER THE MOIST ADIABAT EW1 THROUGH TONE AT P.  USING THE DEFINI-
C   TION OF THE WOBUS FUNCTION (SEE DOCUMENTATION ON WOBF), IT CAN BE
C   SHOWN THAT EONE = EW1-THW.

        EONE = WOBF(TONE)-WOBF(THW)
        RATE = 1.
        GO TO 15

C   IN THE LOOP BELOW, THE ESTIMATE OF SATLFT IS ITERATIVELY IMPROVED.

   10   CONTINUE

C   RATE IS THE RATIO OF A CHANGE IN T TO THE CORRESPONDING CHANGE IN
C   E.  ITS INITIAL VALUE WAS SET TO 1 ABOVE.

        RATE = (TTWO-TONE)/(ETWO-EONE)
        TONE = TTWO
        EONE = ETWO
   15   CONTINUE

C   TTWO IS AN IMPROVED ESTIMATE OF SATLFT.

        TTWO = TONE-EONE*RATE

C   PT IS THE POTENTIAL TEMPERATURE (CELSIUS) CORRESPONDING TO TTWO AT P

        PT = (TTWO+CTA)/PWRP-CTA

C   CONSIDER THE MOIST ADIABAT EW2 THROUGH TTWO AT P. USING THE DEFINI-
C   TION OF THE WOBUS FUNCTION, IT CAN BE SHOWN THAT ETWO = EW2-THW.

        ETWO = PT+WOBF(TTWO)-WOBF(PT)-THW

C   DLT IS THE CORRECTION TO BE SUBTRACTED FROM TTWO.

        DLT = ETWO*RATE
        IF (ABS(DLT).GT.0.1) GO TO 10
        SATLFT = TTWO-DLT
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]SSH_TG.FOR;1

        FUNCTION SSH(P,T)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS SATURATION SPECIFIC HUMIDITY SSH (GRAMS OF 
C   WATER VAPOR PER KILOGRAM OF MOIST AIR) GIVEN THE PRESSURE P
C   (MILLIBARS) AND THE TEMPERATURE T (CELSIUS). THE EQUATION IS GIVEN
C   IN STANDARD METEOROLOGICAL TEXTS. IF T IS DEW POINT (CELSIUS), THEN
C   SSH RETURNS THE ACTUAL SPECIFIC HUMIDITY.
C   COMPUTE THE DIMENSIONLESS MIXING RATIO.

        W = .001*WMR(P,T)

C   COMPUTE THE DIMENSIONLESS SATURATION SPECIFIC HUMIDITY.

        Q = W/(1.+W)
        SSH = 1000.*Q
        RETURN
        END


USER_DEV:[GULIB.THERMOSRC]TCON_TG.FOR;1

        FUNCTION TCON(T,D)

C   THIS FUNCTION RETURNS THE TEMPERATURE TCON (CELSIUS) AT THE LIFTING
C   CONDENSATION LEVEL, GIVEN THE TEMPERATURE T (CELSIUS) AND THE
C   DEW POINT D (CELSIUS).

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C
C   COMPUTE THE DEW POINT DEPRESSION S.
        S = T-D
C   THE APPROXIMATION BELOW, A THIRD ORDER POLYNOMIAL IN S AND T,
C   IS DUE TO HERMAN WOBUS. THE SOURCE OF DATA FOR FITTING THE
C   POLYNOMIAL IS UNKNOWN.

        DLT = S*(1.2185+1.278E-03*T+
     1        S*(-2.19E-03+1.173E-05*S-5.2E-06*T))
        TCON = T-DLT
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]TDA_TG.FOR;1

        FUNCTION TDA(O,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE TEMPERATURE TDA (CELSIUS) ON A DRY ADIABAT
C   AT PRESSURE P (MILLIBARS). THE DRY ADIABAT IS GIVEN BY
C   POTENTIAL TEMPERATURE O (CELSIUS). THE COMPUTATION IS BASED ON
C   POISSON'S EQUATION.

        OK= O+273.15
        TDAK= OK*((P*.001)**.286)
        TDA= TDAK-273.15
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]TE_TG.FOR;1

        FUNCTION TE(T,TD,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE EQUIVALENT TEMPERATURE TE (CELSIUS) OF A
C   PARCEL OF AIR GIVEN ITS TEMPERATURE T (CELSIUS), DEW POINT (CELSIUS)
C   AND PRESSURE P (MILLIBARS).
C   CALCULATE EQUIVALENT POTENTIAL TEMPERATURE.

        AOE = OE(T,TD,P)

C   USE POISSONS'S EQUATION TO CALCULATE EQUIVALENT TEMPERATURE.

        TE= TDA(AOE,P)
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]THM_TG.FOR;1

        FUNCTION THM(T,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE WET-BULB POTENTIAL TEMPERATURE THM
C   (CELSIUS) CORRESPONDING TO A PARCEL OF AIR SATURATED AT 
C   TEMPERATURE T (CELSIUS) AND PRESSURE P (MILLIBARS).

        F(X) =   1.8199427E+01+X*( 2.1640800E-01+X*( 3.0716310E-04+X*
     1         (-3.8953660E-06+X*( 1.9618200E-08+X*( 5.2935570E-11+X*
     2         ( 7.3995950E-14+X*(-4.1983500E-17)))))))
        THM = T
        IF (P.EQ.1000.) RETURN

C   COMPUTE THE POTENTIAL TEMPERATURE (CELSIUS).

        THD = (T+273.15)*(1000./P)**.286-273.15
        THM = THD+6.071*(EXP(T/F(T))-EXP(THD/F(THD)))
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]TLCL1_TG.FOR;1

        FUNCTION TLCL1(T,TD)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE TEMPERATURE TLCL1 (CELSIUS) OF THE LIFTING
C   CONDENSATION LEVEL (LCL) GIVEN THE INITIAL TEMPERATURE T (CELSIUS)
C   AND DEW POINT TD (CELSIUS) OF A PARCEL OF AIR.
C   ERIC SMITH AT COLORADO STATE UNIVERSITY HAS USED THE FORMULA
C   BELOW, BUT ITS ORIGIN IS UNKNOWN.

        DATA CTA/273.15/

C   CTA = DIFFERENCE BETWEEN KELVIN AND CELSIUS TEMPERATURE

        TK = T+CTA

C   COMPUTE THE PARCEL VAPOR PRESSURE (MB).
        ES = ESLO(TD)
        TLCL = 2840./(3.5*ALOG(TK)-ALOG(ES)-4.805)+55.
        TLCL1 = TLCL-CTA
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]TLCL_TG.FOR;1

        FUNCTION TLCL(T,TD)
C   THIS FUNCTION YIELDS THE TEMPERATURE TLCL (CELSIUS) OF THE LIFTING
C   CONDENSATION LEVEL, GIVEN THE TEMPERATURE T (CELSIUS) AND THE
C   DEW POINT TD (CELSIUS).  THE FORMULA USED IS IN BOLTON, DAVID,
C   1980: "THE COMPUTATION OF EQUIVALENT POTENTIAL TEMPERATURE,"
C   MONTHLY WEATHER REVIEW, VOL. 108, NO. 7 (JULY), P. 1048, EQ.(15).

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   CONVERT FROM CELSIUS TO KELVIN DEGREES.

        TK = T+273.15
        TDK = TD+273.15
        A = 1./(TDK-56.)
        B = ALOG(TK/TDK)/800.
        TC = 1./(A+B)+56.
        TLCL = TC-273.15
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]TMLAPS_TG.FOR;1

        FUNCTION TMLAPS(THETAE,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE TEMPERATURE TMLAPS (CELSIUS) AT PRESSURE
C   P (MILLIBARS) ALONG THE MOIST ADIABAT CORRESPONDING TO AN EQUIVALENT
C   POTENTIAL TEMPERATURE THETAE (CELSIUS).
C   THE ALGORITHM WAS WRITTEN BY ERIC SMITH AT COLORADO STATE
C   UNIVERSITY.

        DATA CRIT/0.1/
C   CTA = DIFFERENCE BETWEEN KELVIN AND CELSIUS TEMPERATURES.
C   CRIT = CONVERGENCE CRITERION (DEGREES KELVIN)

        EQ0 = THETAE

C   INITIAL GUESS FOR SOLUTION

        TLEV = 25.

C   COMPUTE THE SATURATION EQUIVALENT POTENTIAL TEMPERATURE CORRESPON-
C   DING TO TEMPERATURE TLEV AND PRESSURE P.

        EQ1 = EPT(TLEV,TLEV,P)
        DIF = ABS(EQ1-EQ0)
        IF (DIF.LT.CRIT) GO TO 3
        IF (EQ1.GT.EQ0) GO TO 1

C   DT IS THE INITIAL STEPPING INCREMENT.

        DT = 10.
        I = -1
        GO TO 2
    1   DT = -10.
        I = 1
    2   TLEV = TLEV+DT
        EQ1 = EPT(TLEV,TLEV,P)
        DIF = ABS(EQ1-EQ0)
        IF (DIF.LT.CRIT) GO TO 3
        J = -1
        IF (EQ1.GT.EQ0) J=1
        IF (I.EQ.J) GO TO 2

C   THE SOLUTION HAS BEEN PASSED. REVERSE THE DIRECTION OF SEARCH
C   AND DECREASE THE STEPPING INCREMENT.

        TLEV = TLEV-DT
        DT = DT/10.
        GO TO 2
    3   TMLAPS = TLEV
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]TMR_TG.FOR;1

        FUNCTION TMR(W,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE TEMPERATURE (CELSIUS) ON A MIXING
C   RATIO LINE W (G/KG) AT PRESSURE P (MB). THE FORMULA IS GIVEN IN 
C   TABLE 1 ON PAGE 7 OF STIPANUK (1973).
C
C   INITIALIZE CONSTANTS

        DATA C1/.0498646455/,C2/2.4082965/,C3/7.07475/
        DATA C4/38.9114/,C5/.0915/,C6/1.2035/

        X= ALOG10(W*P/(622.+W))
        TMRK= 10.**(C1*X+C2)-C3+C4*((10.**(C5*X)-C6)**2.)
        TMR= TMRK-273.15
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]TSA_TG.FOR;1

        FUNCTION TSA(OS,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE TEMPERATURE TSA (CELSIUS) ON A SATURATION
C   ADIABAT AT PRESSURE P (MILLIBARS). OS IS THE EQUIVALENT POTENTIAL
C   TEMPERATURE OF THE PARCEL (CELSIUS). SIGN(A,B) REPLACES THE
C   ALGEBRAIC SIGN OF A WITH THAT OF B.
C   B IS AN EMPIRICAL CONSTANT APPROXIMATELY EQUAL TO 0.001 OF THE LATENT 
C   HEAT OF VAPORIZATION FOR WATER DIVIDED BY THE SPECIFIC HEAT AT CONSTANT
C   PRESSURE FOR DRY AIR.

        DATA B/2.6518986/
        A= OS+273.15

C   TQ IS THE FIRST GUESS FOR TSA.

        TQ= 253.15

C   D IS AN INITIAL VALUE USED IN THE ITERATION BELOW.

        D= 120.

C   ITERATE TO OBTAIN SUFFICIENT ACCURACY....SEE TABLE 1, P.8
C   OF STIPANUK (1973) FOR EQUATION USED IN ITERATION.

        DO 1 I= 1,12
           TQK= TQ-273.15
           D= D/2.
           X= A*EXP(-B*W(TQK,P)/TQ)-TQ*((1000./P)**.286)
           IF (ABS(X).LT.1E-7) GO TO 2
           TQ= TQ+SIGN(D,X)
 1      CONTINUE
 2      TSA= TQ-273.15
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]TV_TG.FOR;1

        FUNCTION TV(T,TD,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   THIS FUNCTION RETURNS THE VIRTUAL TEMPERATURE TV (CELSIUS) OF
C   A PARCEL OF AIR AT TEMPERATURE T (CELSIUS), DEW POINT TD
C   (CELSIUS), AND PRESSURE P (MILLIBARS). THE EQUATION APPEARS
C   IN MOST STANDARD METEOROLOGICAL TEXTS.

        DATA CTA,EPS/273.15,0.62197/

C   CTA = DIFFERENCE BETWEEN KELVIN AND CELSIUS TEMPERATURES.
C   EPS = RATIO OF THE MEAN MOLECULAR WEIGHT OF WATER (18.016 G/MOLE)
C         TO THAT OF DRY AIR (28.966 G/MOLE)

        TK = T+CTA

C   CALCULATE THE DIMENSIONLESS MIXING RATIO.

        W = .001*WMR(P,TD)
        TV = TK*(1.+W/EPS)/(1.+W)-CTA
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]TW_TG.FOR;1

        FUNCTION TW(T,TD,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE WET-BULB TEMPERATURE TW (CELSIUS)
C   GIVEN THE TEMPERATURE T (CELSIUS), DEW POINT TD (CELSIUS)
C   AND PRESSURE P (MB).  SEE P.13 IN STIPANUK (1973), REFERENCED
C   ABOVE, FOR A DESCRIPTION OF THE TECHNIQUE.
C
C
C   DETERMINE THE MIXING RATIO LINE THRU TD AND P.

        AW = W(TD,P)
C
C   DETERMINE THE DRY ADIABAT THRU T AND P.

        AO = O(T,P)
        PI = P

C   ITERATE TO LOCATE PRESSURE PI AT THE INTERSECTION OF THE TWO
C   CURVES .  PI HAS BEEN SET TO P FOR THE INITIAL GUESS.

        DO 4 I= 1,10
           X= .02*(TMR(AW,PI)-TDA(AO,PI))
           IF (ABS(X).LT.0.01) GO TO 5
 4         PI= PI*(2.**(X))

C   FIND THE TEMPERATURE ON THE DRY ADIABAT AO AT PRESSURE PI.

 5      TI= TDA(AO,PI)

C   THE INTERSECTION HAS BEEN LOCATED...NOW, FIND A SATURATION
C   ADIABAT THRU THIS POINT. FUNCTION OS RETURNS THE EQUIVALENT 
C   POTENTIAL TEMPERATURE (C) OF A PARCEL SATURATED AT TEMPERATURE
C   TI AND PRESSURE PI.

        AOS= OS(TI,PI)

C   FUNCTION TSA RETURNS THE WET-BULB TEMPERATURE (C) OF A PARCEL AT
C   PRESSURE P WHOSE EQUIVALENT POTENTIAL TEMPERATURE IS AOS.

        TW = TSA(AOS,P)
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]WMR_TG.FOR;1

        FUNCTION WMR(P,T)

C   THIS FUNCTION APPROXIMATES THE MIXING RATIO WMR (GRAMS OF WATER
C   VAPOR PER KILOGRAM OF DRY AIR) GIVEN THE PRESSURE P (MB) AND THE
C   TEMPERATURE T (CELSIUS). THE FORMULA USED IS GIVEN ON P. 302 OF THE
C   SMITHSONIAN METEOROLOGICAL TABLES BY ROLAND LIST (6TH EDITION).

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C   EPS = RATIO OF THE MEAN MOLECULAR WEIGHT OF WATER (18.016 G/MOLE)
C         TO THAT OF DRY AIR (28.966 G/MOLE)

        DATA EPS/0.62197/

C   THE NEXT TWO LINES CONTAIN A FORMULA BY HERMAN WOBUS FOR THE 
C   CORRECTION FACTOR WFW FOR THE DEPARTURE OF THE MIXTURE OF AIR
C   AND WATER VAPOR FROM THE IDEAL GAS LAW. THE FORMULA FITS VALUES
C   IN TABLE 89, P. 340 OF THE SMITHSONIAN METEOROLOGICAL TABLES,
C   BUT ONLY FOR TEMPERATURES AND PRESSURES NORMALLY ENCOUNTERED IN
C   IN THE ATMOSPHERE.

        X = 0.02*(T-12.5+7500./P)
        WFW = 1.+4.5E-06*P+1.4E-03*X*X
        FWESW = WFW*ESW(T)
        R = EPS*FWESW/(P-FWESW)

C   CONVERT R FROM A DIMENSIONLESS RATIO TO GRAMS/KILOGRAM.

        WMR = 1000.*R
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]WOBF_TG.FOR;1

        FUNCTION WOBF(T)

C   THIS FUNCTION CALCULATES THE DIFFERENCE OF THE WET-BULB POTENTIAL
C   TEMPERATURES FOR SATURATED AND DRY AIR GIVEN THE TEMPERATURE.

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       Baker, Schlatter  17-MAY-1982     Original version.

C        LET WBPTS = WET-BULB POTENTIAL TEMPERATURE FOR SATURATED
C   AIR AT TEMPERATURE T (CELSIUS). LET WBPTD = WET-BULB POTENTIAL
C   TEMPERATURE FOR COMPLETELY DRY AIR AT THE SAME TEMPERATURE T.
C   THE WOBUS FUNCTION WOBF (IN DEGREES CELSIUS) IS DEFINED BY
C                      WOBF(T) = WBPTS-WBPTD.
C   ALTHOUGH WBPTS AND WBPTD ARE FUNCTIONS OF BOTH PRESSURE AND
C   TEMPERATURE, THEIR DIFFERENCE IS A FUNCTION OF TEMPERATURE ONLY.

C        TO UNDERSTAND WHY, CONSIDER A PARCEL OF DRY AIR AT TEMPERA-
C   TURE T AND PRESSURE P. THE THERMODYNAMIC STATE OF THE PARCEL IS
C   REPRESENTED BY A POINT ON A PSEUDOADIABATIC CHART. THE WET-BULB
C   POTENTIAL TEMPERATURE CURVE (MOIST ADIABAT) PASSING THROUGH THIS
C   POINT IS WBPTS. NOW T IS THE EQUIVALENT TEMPERATURE FOR ANOTHER
C   PARCEL SATURATED AT SOME LOWER TEMPERATURE TW, BUT AT THE SAME
C   PRESSURE P.  TO FIND TW, ASCEND ALONG THE DRY ADIABAT THROUGH
C   (T,P). AT A GREAT HEIGHT, THE DRY ADIABAT AND SOME MOIST
C   ADIABAT WILL NEARLY COINCIDE. DESCEND ALONG THIS MOIST ADIABAT
C   BACK TO P. THE PARCEL TEMPERATURE IS NOW TW. THE WET-BULB
C   POTENTIAL TEMPERATURE CURVE (MOIST ADIABAT) THROUGH (TW,P) IS WBPTD.
C   THE DIFFERENCE (WBPTS-WBPTD) IS PROPORTIONAL TO THE HEAT IMPARTED
C   TO A PARCEL SATURATED AT TEMPERATURE TW IF ALL ITS WATER VAPOR
C   WERE CONDENSED. SINCE THE AMOUNT OF WATER VAPOR A PARCEL CAN
C   HOLD DEPENDS UPON TEMPERATURE ALONE, (WBPTD-WBPTS) MUST DEPEND
C   ON TEMPERATURE ALONE.

C        THE WOBUS FUNCTION IS USEFUL FOR EVALUATING SEVERAL THERMO-
C   DYNAMIC QUANTITIES.  BY DEFINITION:
C                   WOBF(T) = WBPTS-WBPTD.               (1)
C   IF T IS AT 1000 MB, THEN T IS A POTENTIAL TEMPERATURE PT AND
C   WBPTS = PT. THUS
C                   WOBF(PT) = PT-WBPTD.                 (2)
C   IF T IS AT THE CONDENSATION LEVEL, THEN T IS THE CONDENSATION
C   TEMPERATURE TC AND WBPTS IS THE WET-BULB POTENTIAL TEMPERATURE
C   WBPT. THUS
C                   WOBF(TC) = WBPT-WBPTD.               (3)
C   IF WBPTD IS ELIMINATED FROM (2) AND (3), THERE RESULTS
C                   WBPT = PT-WOBF(PT)+WOBF(TC).
C   IF WBPTD IS ELIMINATED FROM (1) AND (2), THERE RESULTS
C                   WBPTS = PT-WOBF(PT)+WOBF(T).

C        IF T IS AN EQUIVALENT POTENTIAL TEMPERATURE EPT (IMPLYING
C   THAT THE AIR AT 1000 MB IS COMPLETELY DRY), THEN WBPTS = EPT
C   AND WBPTD = WBPT. THUS
C                   WOBF(EPT) = EPT-WBPT.
C   THIS FORM IS THE BASIS FOR A POLYNOMIAL APPROXIMATION TO WOBF.
C   IN TABLE 78 ON PP.319-322 OF THE SMITHSONIAN METEOROLOGICAL
C   TABLES BY ROLAND LIST (6TH REVISED EDITION), ONE FINDS WET-BULB
C   POTENTIAL TEMPERATURES AND THE CORRESPONDING EQUIVALENT POTENTIAL
C   TEMPERATURES LISTED TOGETHER. HERMAN WOBUS, A MATHEMATICIAN FOR-
C   MERLY AT THE NAVY WEATHER RESEARCH FACILITY, NORFOLK, VIRGINIA,
C   AND NOW RETIRED, COMPUTED THE COEFFICIENTS FOR THE POLYNOMIAL
C   APPROXIMATION FROM NUMBERS IN THIS TABLE.
C
C                                    NOTES BY T.W. SCHLATTER
C                                    NOAA/ERL/PROFS PROGRAM OFFICE
C                                    AUGUST 1981

        X = T-20.
        IF (X.GT.0.) GO TO 10
        POL = 1.                 +X*(-8.8416605E-03
     1       +X*( 1.4714143E-04  +X*(-9.6719890E-07
     2       +X*(-3.2607217E-08  +X*(-3.8598073E-10)))))
        WOBF = 15.130/POL**4
        RETURN
   10   CONTINUE
        POL = 1.                 +X*( 3.6182989E-03
     1       +X*(-1.3603273E-05  +X*( 4.9618922E-07
     2       +X*(-6.1059365E-09  +X*( 3.9401551E-11
     3       +X*(-1.2588129E-13  +X*( 1.6688280E-16)))))))
        WOBF = 29.930/POL**4+0.96*X-14.8
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]W_TG.FOR;1

        FUNCTION W(T,P)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C  THIS FUNCTION RETURNS THE MIXING RATIO (GRAMS OF WATER VAPOR PER
C  KILOGRAM OF DRY AIR) GIVEN THE DEW POINT (CELSIUS) AND PRESSURE
C  (MILLIBARS). IF THE TEMPERTURE  IS INPUT INSTEAD OF THE
C  DEW POINT, THEN SATURATION MIXING RATIO (SAME UNITS) IS RETURNED.
C  THE FORMULA IS FOUND IN MOST METEOROLOGICAL TEXTS.

        X= ESAT(T)
        W= 622.*X/(P-X)
        RETURN
        END

USER_DEV:[GULIB.THERMOSRC]Z_TG.FOR;1

        FUNCTION Z(PT,P,T,TD,N)

C       INCLUDE 'LIB_DEV:[GUDOC]EDFVAXBOX.FOR/LIST'
C       G.S. Stipanuk     1973            Original version.
C       Reference Stipanuk paper entitled:
C            "ALGORITHMS FOR GENERATING A SKEW-T, LOG P
C            DIAGRAM AND COMPUTING SELECTED METEOROLOGICAL
C            QUANTITIES."
C            ATMOSPHERIC SCIENCES LABORATORY
C            U.S. ARMY ELECTRONICS COMMAND
C            WHITE SANDS MISSILE RANGE, NEW MEXICO 88002
C            33 PAGES
C       Baker, Schlatter  17-MAY-1982    

C   THIS FUNCTION RETURNS THE THICKNESS OF A LAYER BOUNDED BY PRESSURE
C   P(1) AT THE BOTTOM AND PRESSURE PT AT THE TOP.
C   ON INPUT:
C       P = PRESSURE (MB).  NOTE THAT P(I).GT.P(I+1).
C       T = TEMPERATURE (CELSIUS)
C       TD = DEW POINT (CELSIUS)
C       N = NUMBER OF LEVELS IN THE SOUNDING AND THE DIMENSION OF
C           P, T AND TD
C   ON OUTPUT:
C       Z = GEOMETRIC THICKNESS OF THE LAYER (M)
C   THE ALGORITHM INVOLVES NUMERICAL INTEGRATION OF THE HYDROSTATIC
C   EQUATION FROM P(1) TO PT. IT IS DESCRIBED ON P.15 OF STIPANUK
C   (1973).

        DIMENSION T(1),P(1),TD(1),TK(100)

C       C1 = .001*(1./EPS-1.) WHERE EPS = .62197 IS THE RATIO OF THE
C                             MOLECULAR WEIGHT OF WATER TO THAT OF
C                             DRY AIR. THE FACTOR 1000. CONVERTS THE
C                             MIXING RATIO W FROM G/KG TO A DIMENSION-
C                             LESS RATIO.
C       C2 = R/(2.*G) WHERE R IS THE GAS CONSTANT FOR DRY AIR
C                     (287 KG/JOULE/DEG K) AND G IS THE ACCELERATION
C                     DUE TO THE EARTH'S GRAVITY (9.8 M/S**2). THE
C                     FACTOR OF 2 IS USED IN AVERAGING TWO VIRTUAL
C                     TEMPERATURES.

        DATA C1/.0006078/,C2/14.64285/
        DO 5 I= 1,N
           TK(I)= T(I)+273.15
    5   CONTINUE
        Z= 0.0
        IF (PT.LT.P(N)) GO TO 20
        I= 0
   10   I= I+1
        J= I+1
        IF (PT.GE.P(J)) GO TO 15
        A1= TK(J)*(1.+C1*W(TD(J),P(J)))
        A2= TK(I)*(1.+C1*W(TD(I),P(I)))
        Z= Z+C2*(A1+A2)*(ALOG(P(I)/P(J)))
        GO TO 10
   15   CONTINUE
        A1= TK(J)*(1.+C1*W(TD(J),P(J)))
        A2= TK(I)*(1.+C1*W(TD(I),P(I)))
        Z= Z+C2*(A1+A2)*(ALOG(P(I)/PT))
        RETURN
 20     Z= -1.0
        RETURN
        END
```

 

 

 