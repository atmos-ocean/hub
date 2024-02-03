TSR_SEASON
============================-

季節変動成分（気候値）を求める

[TOC]

Thu, 16 Jul 2020 19:59:59 +0900
calypso.bosai.go.jp
/work05/manda/20.AGO_WAN/MIE_MON/TSR_SEASON

```
srcdump.sh TSR_SEASON.sh TSR_SEASON.f90 CALENDAR.f90 PL_TSRS_DAY_MEAN_MUL_Z.sh
```

### HOW TO RUN

### INFO
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB

### SOURCE FILES
- TSR_SEASON.sh
- TSR_SEASON.f90
- CALENDAR.f90
- PL_TSRS_DAY_MEAN_MUL_Z.sh
  
#### TSR_SEASON.sh
```bash
#!/bin/bash
#
# Description:
#
src=$(basename $0 .sh).f90
SUB="CALENDAR.f90"
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
indir="$INDIR"
infle="$INFLE"
odir="$ODIR"
ofle="$OFLE"
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


#### TSR_SEASON.f90
```fortran
program TSR_SEASON

character(len=300)::indir,odir
character(len=300)::infle,ofle
character(len=600)::in,out
character(len=300)::strm

integer,allocatable::yyyy(:),mm(:),dd(:),hh(:)
real,allocatable::x1(:),x2(:),x3(:),x4(:)

integer,parameter::DUMY=1700
integer,dimension(365)::RMON,RDAY,JD1Y
real,dimension(365)::A1,A2,A3,A4
real,dimension(365)::N1,N2,N3,N4

real,parameter::rmiss=-999.00

namelist /para/indir,infle,odir,ofle

read(*,nml=para)

print '(A)',trim(indir)
print '(A)',trim(infle)

in=trim(indir)//'/'//trim(infle)

open(11,file=in,action="read")

n=0

count_vaild_data: do
  read(11,'(A)',iostat=ios)strm
  if(ios<0)exit
  if(strm(1:1) == "#")then
    cycle
  else
    n=n+1
  endif
enddo count_vaild_data

nt=n

rewind(11)
allocate(yyyy(nt),mm(nt),dd(nt),hh(nt))
allocate(x1(nt),x2(nt),x3(nt),x4(nt))

n=0
skip_comment: do
  read(11,'(A)',iostat=ios)strm
  if(ios<0)exit
  if(strm(1:1) == "#")then
    cycle
  else
    n=n+1
    read(strm,*)yyyy(n),mm(n),dd(n),x1(n),x2(n),x3(n),x4(n)
  endif
enddo skip_comment
close(11)
print *

!do n=1,nt
!  print '(i4.4,i2.2,i2.2,1x,i2.2,1x,3(f7.2,1x) )',&
!yyyy(n),mm(n),dd(n),hh(n),x1(n),x2(n),x3(n)
!end do

JD0=JULDAY(yyyy(1),mm(1),dd(1))
JD1=JD0+364
do i=1,365
JD=JD0+i-1
call caldat(JD,IYYY,RMON(i),RDAY(i)) 

JD1Y(i)=i
!print *,JD1Y(i),RMON(i),RDAY(i)
end do !i



do n=1,nt

JD0=JULDAY(yyyy(n),1,1)
JD1=JULDAY(yyyy(n),mm(n),dd(n))

JD=JD1-JD0+1

iy=yyyy(n)
LYP =  (1 / (iy - iy / 4 * 4 + 1)) * &
(1 - 1 / (iy - iy / 100 * 100 + 1)) &
 + (1 / (iy - iy / 400 * 400 + 1))

if(LYP == 1 .and. JD == 60)then
cycle
endif

if(LYP == 1 .and. JD > 60)then
JD=JD-1
endif

!print *,JD,LYP,yyyy(n),mm(n),dd(n)

if(x1(n) /= rmiss)then
A1(JD)=A1(JD)+x1(n)
N1(JD)=N1(JD)+1.
endif

if(x2(n) /= rmiss)then
A2(JD)=A2(JD)+x2(n)
N2(JD)=N2(JD)+1.
endif

if(x3(n) /= rmiss)then
A3(JD)=A3(JD)+x3(n)
N3(JD)=N3(JD)+1.
endif

if(x4(n) /= rmiss)then
A4(JD)=A4(JD)+x4(n)
N4(JD)=N4(JD)+1.
endif

end do !n



do i=1,365

if(N1(i) >= 5)then
A1(i)=A1(i)/N1(i)
else
A1(i)=rmiss
endif

if(N2(i) >= 5)then
A2(i)=A2(i)/N2(i)
else
A2(i)=rmiss
endif

if(N3(i) >= 5)then
A3(i)=A3(i)/N3(i)
else
A3(i)=rmiss
endif

if(N4(i) >= 5)then
A4(i)=A4(i)/N4(i)
else
A4(i)=rmiss
endif

end do !i


out=trim(odir)//'/'//trim(ofle)
open(21,file=out)
do i=1,365
write(21,'(i4.4,1x,i2.2,1x,i2.2,1x,4f8.2,4i5)')&
DUMY,RMON(i),RDAY(i),A1(i),A2(i),A3(i),A4(i),&
int(N1(i)),int(N2(i)),int(N3(i)),int(N4(i))
end do !i
close(21)


print *
print '(A,A)','OUTPUT: ',trim(out)
print *
end program TSR_SEASON



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


#### PL_TSRS_DAY_MEAN_MUL_Z.sh
```bash
. ./gmtpar.sh

gmtset PLOT_DATE_FORMAT o TIME_FORMAT_PRIMARY Character

SITE=AGO_WANOU
IN=${SITE}_DAY_MEAN_SEASON.txt

#OUTDIR=FIG_$(basename $0 .sh)_$(basename $IN .txt)
#mkdir -vp $OUTDIR
OUTDIR=.

YYYY=1700

ps=$OUTDIR/$(basename $0 .sh)_$(basename $IN .txt).ps
rm -f $ps


# グラフのサイズ
mp=X        
gx=6      
gy=3        

jopt=$mp${gx}/${gy} # -Jオプション

# グラフの範囲(range)（-Rオプション）
rw=$YYYY-01-01T00:00             # x軸の最小値(west)
re=$YYYY-12-31T00:00             # x軸の最大値(east)
rs=5                          # y軸の最小値(south)
rn=35                         # y軸の最大値(north)


xanot=pa1O               # x軸の目盛り(a=主,f=副)
yanot=a5f5:"T[C]":                # y軸の目盛り(a=主,f=副)

line01=0.3p,black               # 線：太さ,色,線種

# basemap(グリッド線のみ)
psbasemap -J$jopt -R$rw/$re/$rs/$rn -Bpa1Of1o/${yanot}WSne \
-P -K -X1.5 -Y5 > $ps
#-Bsa1Y/  \

ZLIST="0.5 2.0 5.0 8.0"

for Z in $ZLIST; do

if [ $Z = "0.5" ]; then
COL=5
COL2=4
CLR=blue
elif [ $Z = "2.0" ]; then
COL=6
COL2=5
CLR=red
elif [ $Z = "5.0"  ]; then
COL=7
COL2=6
CLR=green
elif [ $Z = "8.0"  ]; then
COL=8
COL2=7
CLR=purple
fi

echo $Z $COL2
awk '{ if($1!="#" && $'"$COL2"'>-2.0 ) {printf \
"%04d-%02d-%02dT12:00:00 %6.2f\n", $1,$2,$3,$'"$COL2"'} \
else if($1!="#") {printf ">\n"} }' $IN |\
 psxy -J -R -W0.5p,${CLR} -M -K -O >> $ps

done

xoffset=0
yoffset=2.5
export LANG=C
curdir1=$(pwd)
now=$(date -R)
host=$(hostname)

in=$IN
time=$(ls -l ${in} | awk '{print $6, $7, $8}')
#time1=$(ls -l ${in1} | awk '{print $6, $7, $8}')

yoffset=3
pstext -JX10/1.5 -R0/1/0/1.5 -X${xoffset:-0} -Y${yoffset:-0} <<EOF \
-O >> $ps
0 1.40 8 0 0 LM  $0 $@
0 1.25 8 0 0 LM  ${now}
0 1.1 8 0 0 LM  ${host}
0 0.95 8 0 0 LM  ${curdir1}
0 0.8 8 0 0 LM  Input: ${in}
EOF


if [ -f $ps ]; then
ls --time-style=long-iso -lh $ps
fi
```

  
