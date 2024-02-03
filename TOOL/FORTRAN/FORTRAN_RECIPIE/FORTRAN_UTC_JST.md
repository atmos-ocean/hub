ADJUST TIME DIFFERENCE
================================
[TOC]

FROM LST TO UTC 
------------------------------------------
```FORTRAN
subroutine LST2UTC(lyr,lmo,ldy, lhr,  uyr, umo, udy, uhr, tdh)
! Description:
! Author: am
! Reference
! 1行で書けるうるう年判別法
! http://d.hatena.ne.jp/Kappuccino/20081025/1224906495
!
implicit none
integer,intent(in)::   lyr,lmo,ldy, lhr
integer,intent(inout)::uyr,umo,udy, uhr
integer,intent(in)::   tdh
integer y
integer,dimension(12)::last_day

data last_day/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

uyr=lyr
umo=lmo
udy=ldy
uhr=lhr
uhr=lhr-tdh

if(uhr<0)then
  uhr=uhr+24
  udy=udy-1
endif
if(udy<=0)then
  umo=umo-1
  udy=last_day(umo)
  if(umo == 2)then
    y=uyr
    udy = 28 + (1 / (y - y / 4 * 4 + 1)) * &
&          (1 - 1 / (y - y / 100 * 100 + 1)) &
&          +  (1 / (y - y / 400 * 400 + 1));
  endif
endif
if(umo<=0)then
  umo=1
  uyr=uyr-1
endif
end subroutine LST2UTC
```



FROM UTC TO LST
------------------------------------------
Wed, 22 Jul 2020 14:20:15 +0900
calypso.bosai.go.jp
/work05/manda/20.AGO_WAN/FLUX.MSM/T2.AGO.TEST/TEST.UTC2JST

```
srcdump.sh TEST.UTC2JST.sh TEST.UTC2JST.f90 UTC2LST.f90 CALENDAR.f90
```

### HOW TO RUN

### INFO
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB

### SOURCE FILES
- TEST.UTC2JST.sh
- TEST.UTC2JST.f90
- UTC2LST.f90
- CALENDAR.f90
  
#### TEST.UTC2JST.sh
```bash
#!/bin/bash
#
# Description:
#
src=$(basename $0 .sh).f90
SUB="CALENDAR.f90 UTC2LST.f90"
exe=$(basename $0 .sh).exe
nml=$(basename $0 .sh).nml

f90=ifort
opt="-CB -traceback -fpe0 " # -convert big_endian -assume byterecl"

INDIR=/work05/manda/20.AGO_WAN/DATA/MIE_MON/AGO_WANOU
INFLE=AGO_WANOU_DAY_MEAN.txt
ODIR=.
OFLE=$(basename $INFLE .txt)_SEASON.txt

cat<<EOF>$nml
&para
YYYYS=2003
MMS=01
DDS=01
YYYYE=2020
MME=12
DDE=31
TDH=9 !TIME DIFFERENCE
&end
EOF

echo
echo Created ${nml}.
echo
ls -lh --time-style=long-iso ${nml}
echo



echo
echo ${src}
echo
ls -lh --time-style=long-iso ${src}
echo

echo Compiling ${src} ...
echo
echo ${f90} ${opt} ${src} ${SUB} -o ${exe}
echo
${f90} ${opt} ${src}  ${SUB} -o ${exe}
if [ $? -ne 0 ]; then

echo
echo "=============================================="
echo
echo "   COMPILE ERROR!!!"
echo
echo "=============================================="
echo
echo TERMINATED.
echo
exit 1
fi
echo "Done Compile."
echo
ls -lh ${exe}
echo

echo
echo ${exe} is running ...
echo
#${exe}
${exe} < ${nml}
if [ $? -ne 0 ]; then
echo
echo "=============================================="
echo
echo "   ERROR in $exe: RUNTIME ERROR!!!"
echo
echo "=============================================="
echo
echo TERMINATED.
echo
exit 1
fi
echo
echo "Done ${exe}"
echo
rm -v $exe

```


#### TEST.UTC2JST.f90
```fortran
integer YYYYS, MMS, DDS, YYYYE, MME, DDE
integer YYYY,MM,DD,HH  !UTC
integer YYYYL,MML,DDL  !LOCAL TIME
integer TDH            !TIME DIFFERENCE

namelist /para/YYYYS, MMS, DDS, YYYYE, MME, DDE, TDH



read(*,nml=para)

JDO=JULDAY(YYYYS, MMS, DDS)
JD1=JULDAY(YYYYE, MME, DDE)-JULDAY(YYYYS, MMS, DDS)

do J=0,JD1

JD=J+JDO

call caldat(JD,YYYY,MM,DD)

do HH=0,23

call UTC2LST(YYYY,MM,DD,HH,TDH, YYYYL,MML,DDL,HHL)

print '(i4.4,1x,i2.2,1x,i2.2,1x,i2.2,2x,&
i4.4,1x,i2.2,1x,i2.2,1x,i2.2)',&
YYYY,MM,DD,HH, YYYYL,MML,DDL,HHL

end do !HH

end do !J

stop
end
```


#### UTC2LST.f90
```fortran
subroutine UTC2LST(YYYY,MM,DD,HH,TDH, YYYYL,MML,DDL,HHL)

! Reference
! 1行で書けるうるう年判別法
! http://d.hatena.ne.jp/Kappuccino/20081025/1224906495

integer,intent(in)::YYYY,MM,DD,HH,TDH
integer,intent(inout)::YYYYL,MML,DDL,HHL
integer y
integer NDAY(12)

data NDAY/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

YYYYL=YYYY
  MML=MM
  DDL=DD
  HHL=HH+TDH !UTC + TIME DIFFERENCE

if(HHL>=24)then
  HHL=HHL-24
  DDL=DDL+1
endif

LAST_DAY=NDAY(MML)
if(MML == 2  )then
  y=YYYYL
  LAST_DAY = 28 + (1 / (y - y / 4 * 4 + 1)) * &
&          (1 - 1 / (y - y / 100 * 100 + 1)) &
&       +  (1 / (y - y / 400 * 400 + 1));
endif


if(DDL>LAST_DAY)then
  MML=MML+1
  DDL=1
endif


if(MML>12)then
  MML=1
  YYYYL=YYYYL+1
endif

end subroutine UTC2LST
```


#### CALENDAR.f90
```fortran
INTEGER FUNCTION JULDAY(IYYY,MONTH,DD) 

      INTEGER,intent(inout):: IYYY,MONTH,DD
!                                                                       
! NAME   IN/OUT DESCRIPTION                                             
!                                                                       
! IYYY     I    YEAR                                                    
! MONTH    I    MONTH (1 TO 12)                                         
! DD       I    DAY OF MONTH                                            
!                                                                       
      INTEGER IGREG 
      PARAMETER (IGREG = 15 + 31*(10 + 12*1582)) 
      INTEGER JY,JM,JA 
!                                                                       
      IF (IYYY .LT. 0) IYYY = IYYY + 1 
      IF (MONTH .GT. 2) THEN 
        JY = IYYY 
        JM = MONTH + 1 
      ELSE 
        JY = IYYY - 1 
        JM = MONTH + 13 
      ENDIF 

      JULDAY = INT(365.25*JY) + INT(30.6001*JM) + DD + 1720995 

      IF (DD + 31*(MONTH + 12*IYYY) .GE. IGREG) THEN 

        JA = INT(0.01*JY) 
        JULDAY = JULDAY + 2 - JA + INT(0.25*JA) 
      ENDIF 
      RETURN 
end function julday

subroutine caldat(JULIAN,IYYY,MONTH,DD) 

! Description:

      INTEGER,intent(in):: JULIAN
      integer,intent(out)::IYYY,MONTH,DD 
!                                                                       
! NAME   IN/OUT DESCRIPTION                                             
!                                                                       
! JULIAN   I    THE JULIAN DAY                                          
! IYYY     O    THE YEAR                                                
! MONTH    O    THE MONTH (1 TO 12)                                     
! DD       O    THE DAY OF THE MONTH                                    
!                                                                       
      INTEGER IGREG 
      PARAMETER (IGREG=2299161) 
      INTEGER JALPHA,JA,JB,JC,JD,JE 
!                                                                       
      IF (JULIAN .GE. IGREG) THEN 
        JALPHA = INT(((JULIAN - 1867216) - 0.25)/36524.25) 
        JA = JULIAN + 1 + JALPHA - INT(0.25*JALPHA) 
      ELSE 
        JA = JULIAN 
      ENDIF 
      JB = JA + 1524 
      JC = INT(6680. + ((JB - 2439870) - 122.1)/365.25) 
      JD = 365*JC + INT(0.25*JC) 
      JE = INT((JB - JD)/30.6001) 
      DD = JB - JD - INT(30.6001*JE) 
      MONTH = JE - 1 
      IF (MONTH .GT. 12) MONTH = MONTH - 12 
      IYYY = JC - 4715 
      IF (MONTH .GT. 2) IYYY = IYYY - 1 
      IF (IYYY .LE. 0) IYYY = IYYY - 1 
      RETURN 
end subroutine caldat
```

  



