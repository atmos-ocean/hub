TSR_SEASON_1HR_SMOOTH
============================-
[TOC]
  
Fri, 17 Jul 2020 18:32:15 +0900
calypso.bosai.go.jp
/work05/manda/20.AGO_WAN/MIE_MON/TSR_SEASON_1HR_SMOOTH

```
srcdump.sh TSR_SEASON_1HR_SMO.sh TSR_SEASON_1HR_SMO.f90 CALENDAR.f90 PL_TSR_SEASON_1HR_SMO.sh
```
  
### HOW TO RUN
  
### INFO
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
### SOURCE FILES
- TSR_SEASON_1HR_SMO.sh
- TSR_SEASON_1HR_SMO.f90
- CALENDAR.f90
- PL_TSR_SEASON_1HR_SMO.sh
  
#### TSR_SEASON_1HR_SMO.sh
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
opt="-CB -traceback -fpe0 -convert big_endian -assume byterecl"

INDIR=/work05/manda/20.AGO_WAN/DATA/MIE_MON/AGO_WANOU
INFLE=AGO_WANOU.txt
ODIR=.
OFLE=$(basename $INFLE .txt)_SEAS_1HR_SMO.txt
BFLE=$(basename $INFLE .txt)_SEAS_1HR_SMO.bin
CTL=$(basename $INFLE .txt)_SEAS_1HR_SMO.CTL

cat<<EOF>$nml
&para
indir="$INDIR"
infle="$INFLE"
odir="$ODIR"
ofle="$OFLE"
bfle="$BFLE"
&end
EOF

echo
echo Created ${nml}.
echo
ls -lh --time-style=long-iso ${nml}
echo

cat<<EOF>$CTL
dset ^${BFLE}
title DATA_FILE
options big_endian
undef -999.00
xdef 1 levels 130
ydef 1 levels 32
zdef 1 levels 1000
tdef 157800 linear 1JAN2003 1HR
vars 4
XC1S 1 0 T0.5m CLIM SMO
XC2S 1 0 T2.0m CLIM SMO
XC3S 1 0 T5.0m CLIM SMO
XC4S 1 0 T8.0m CLIM SMO
endvars
EOF

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
ls -lh --time-style=long-iso -lh ${ODIR}/${BFLE}
echo
ls -lh --time-style=long-iso -lh ${ODIR}/${OFLE}
echo
rm -v $exe $nml

```

  
#### TSR_SEASON_1HR_SMO.f90
```fortran
program TSR_SEASON

character(len=300)::indir,odir
character(len=300)::infle,ofle,bfle
character(len=600)::in,out,bin
character(len=300)::strm

integer,allocatable::yyyy(:),mm(:),dd(:),hh(:)
real,allocatable::x1(:),x2(:),x3(:),x4(:)

integer,parameter::DUMY=1700

integer,dimension(366)::REF_JD

real,dimension(366,0:23)::A1=0.0,A2=0.0,A3=0.0,A4=0.0
real,dimension(366,0:23)::N1=0.0,N2=0.0,N3=0.0,N4=0.0
real,dimension(366,0:23)::V1=0.0,V2=0.0,V3=0.0,V4=0.0

real,allocatable::XC1(:),XC2(:),XC3(:),XC4(:)
real,allocatable::NC1(:),NC2(:),NC3(:),NC4(:)


real,parameter::rmiss=-999.00

namelist /para/indir,infle,odir,ofle,bfle

read(*,nml=para)

print '(A)',trim(indir)
print '(A)',trim(infle)
print *

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
allocate(x1(nt), x2(nt), x3(nt), x4(nt))
allocate(XC1(nt), XC2(nt), XC3(nt), XC4(nt))
allocate(NC1(nt), NC2(nt), NC3(nt), NC4(nt))



print *,'COUNT NUMBER OF DATA'
n=0
skip_comment: do
  read(11,'(A)',iostat=ios)strm
  if(ios<0)exit
  if(strm(1:1) == "#")then
    cycle
  else
    n=n+1
    read(strm,*)yyyy(n),mm(n),dd(n),hh(n),x1(n),x2(n),x3(n),x4(n)
  endif
enddo skip_comment
close(11)



print *,'SUM'
BIG_N_LOOP: do n=1,nt

JD0=JULDAY(yyyy(n),1,1)
JD1=JULDAY(yyyy(n),mm(n),dd(n))

iy=yyyy(n)
LYP =  (1 / (iy - iy / 4 * 4 + 1)) * &
(1 - 1 / (iy - iy / 100 * 100 + 1)) &
 + (1 / (iy - iy / 400 * 400 + 1))

JD=JD1-JD0+1

if(LYP == 0 .and. JD >=60)then !MAY 1ST (NOT LEAP YEAR)
JD=JD+1
endif



!call caldat(JD+JD0-1,IYYYY,IMM,IDD)

MOVE_AVE: do IS=-24*7,24*7

NS=n+IS

if(NS <= 0)then
cycle
else if(NS > nt)then
cycle
endif

IHH=hh(n)

if(x1(NS) /= rmiss)then
A1(JD,IHH)=A1(JD,IHH)+x1(NS)
N1(JD,IHH)=N1(JD,IHH)+1.0
endif

if(x2(NS) /= rmiss)then
A2(JD,IHH)=A2(JD,IHH)+x2(NS)
N2(JD,IHH)=N2(JD,IHH)+1.0
endif

if(x3(NS) /= rmiss)then
A3(JD,IHH)=A3(JD,IHH)+x3(NS)
N3(JD,IHH)=N3(JD,IHH)+1.0
endif

if(x4(NS) /= rmiss)then
A4(JD,IHH)=A4(JD,IHH)+x4(NS)
N4(JD,IHH)=N4(JD,IHH)+1.0
endif

end do MOVE_AVE

end do BIG_N_LOOP !n



IYS=yyyy(1)
IYE=yyyy(nt)


print *,'MEAN'
do JD=1,366
do IHH=0,23
if(N1(JD,IHH)>1000.)then
A1(JD,IHH)=A1(JD,IHH)/N1(JD,IHH)
else
A1(JD,IHH)=rmiss
endif

if(N2(JD,IHH)>1000.)then
A2(JD,IHH)=A2(JD,IHH)/N2(JD,IHH)
else
A2(JD,IHH)=rmiss
endif

if(N1(JD,IHH)>1000.)then
A3(JD,IHH)=A3(JD,IHH)/N3(JD,IHH)
else
A3(JD,IHH)=rmiss
endif

if(N4(JD,IHH)>1000.)then
A4(JD,IHH)=A4(JD,IHH)/N4(JD,IHH)
else
A4(JD,IHH)=rmiss
endif

enddo !IHH
enddo !JD



print *,'FEB29 (INTERPOLATION)'
do IHH=0,23
A1(60,IHH)=(A1(61,0)-A1(59,23))/25.0*(float(IHH)+1.0) + A1(59,23)
A2(60,IHH)=(A2(61,0)-A2(59,23))/25.0*(float(IHH)+1.0) + A2(59,23)
A3(60,IHH)=(A3(61,0)-A3(59,23))/25.0*(float(IHH)+1.0) + A3(59,23)
A4(60,IHH)=(A4(61,0)-A4(59,23))/25.0*(float(IHH)+1.0) + A4(59,23)
end do !IHH



print *,'CREATE CYCLIC DATA'
n=0
YEAR: do IY=IYS,IYE

LYP =  (1 / (iy - iy / 4 * 4 + 1)) * &
(1 - 1 / (iy - iy / 100 * 100 + 1)) &
 + (1 / (iy - iy / 400 * 400 + 1))

if(LYP == 0)JDMAX=365
if(LYP == 1)JDMAX=366


JULI: do JD=1,JDMAX
HOUR: do IHH=0,23

if(LYP==0 .and. JD >=60)then
JD_DUM=JD+1
else
JD_DUM=JD
endif


n=n+1
XC1(n)=A1(JD_DUM,IHH)
NC1(n)=N1(JD_DUM,IHH)

XC2(n)=A2(JD_DUM,IHH)
NC2(n)=N2(JD_DUM,IHH)

XC3(n)=A3(JD_DUM,IHH)
NC3(n)=N3(JD_DUM,IHH)

XC4(n)=A4(JD_DUM,IHH)
NC4(n)=N4(JD_DUM,IHH)

if( (mm(n)==7 .and. dd(n)==15) .and. hh(n)==4)then
!if( (mm(n)==2 .and. dd(n)==29) .and. hh(n)==4)then

print '(i7,1x,i4.4,1x,i2.2,1x,i2.2,1x,i2.2,3x,i4,1x,f8.2,1x,f5.0)', & 
n,yyyy(n),mm(n),dd(n),hh(n),JD_DUM,XC1(n),NC1(n)

endif !mm


end do HOUR
end do JULI
end do YEAR



print *,'OUTPUT TXT'
out=trim(odir)//'/'//trim(ofle)
open(21,file=out)
do n=1,nt
write(21,'(i4.4,1x,i2.2,1x,i2.2,1x,i2.2,4f8.2,4i5,1x,4f8.2)')&
yyyy(n),mm(n),dd(n),hh(n),XC1(n),XC2(n),XC3(n),XC4(n)
end do !i
close(21)



print *,'OUTPUT BIN'
bin=trim(odir)//'/'//trim(bfle)
INQUIRE (IOLENGTH=nrec) XC1(1)

open(22,file=bin,form="unformatted",access="direct",recl=nrec)
print *,'nrec   =',nrec
print *,'1*1*4=',4
irec=0
do i=1,nt
irec=irec+1
write(22,rec=irec)XC1(i)
irec=irec+1
write(22,rec=irec)XC2(i)
irec=irec+1
write(22,rec=irec)XC3(i)
irec=irec+1
write(22,rec=irec)XC4(i)

enddo !n

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

  
#### PL_TSR_SEASON_1HR_SMO.sh
```bash
HOST=$(hostname)
CWD=$(pwd)
TIMESTAMP=$(date -R)
CMD="$0 $@"
GS=$(basename $0 .sh).GS

CTL=AGO_WANOU_SEAS_1HR_SMO.CTL

YYYY=2003


FIG=$(basename $CTL .CTL)_$YYYY.eps

STN="STN O (CENTRAL AGO BAY)"
UNIT="T [\`ao\`nC]"

cat <<EOF>$GS
'open $CTL'
'cc'

'set time 00:00Z01JAN${YYYY} 23:00Z31DEC${YYYY}'

'set vpage 0.0 8.5 0 10.5'

xmax=1
ymax=1

xleft=1
ytop=9

xwid =  6/xmax
ywid =  5/ymax
xmargin=0.5
ymargin=0.5

ymap=1
xmap=1

xs = xleft + (xwid+xmargin)*(xmap-1)
xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1)
ys = ye - ywid

'set parea 'xs ' 'xe' 'ys' 'ye

'set rgb 21 0     0   255 100'
'set rgb 22 0     0   255 255'
'set rgb 23 255   0   0   100'
'set rgb 24 255   0   0   255'
'set rgb 25 0     255 0   100'
'set rgb 26 0     255 0   255'
'set rgb 27 255   0   255 100'
'set rgb 28 255   0   255 255'


#'set tlsupp year'
'set grads off'
'set grid on'
'set xlopts 1 4 0.18'
'set ylopts 1 4 0.18'
'set vrange 10 30'



'set gxout line'
'set ccolor 22'
'set cmark 0'
'set cthick 3'
'd XC1S'

'set xlab off'
'set ylab off'

'set gxout line'
'set ccolor 24'
'set cmark 0'
'set cthick 3'
'd XC2S'

'set gxout line'
'set ccolor 26'
'set cmark 0'
'set cthick 3'
'd XC3S'

'set gxout line'
'set ccolor 28'
'set cmark 0'
'set cthick 3'
'd XC4S'

'q gxinfo'
#say result
line=sublin(result,3)
xl=subwrd(line,4)
xr=subwrd(line,6)
line=sublin(result,4)
yb=subwrd(line,4)
yt=subwrd(line,6)

x=xl-0.7 ;# (xl+xr)/2
y=(yt+yb)/2
'set strsiz 0.12 0.15'
'set string 1 c 4 90'
'draw string 'x' 'y' ${UNIT}'

x=(xl+xr)/2
y=yt+0.2
'set strsiz 0.15 0.18'
'set string 1 c 4 0'
'draw string 'x' 'y' $STN'

'set strsiz 0.12 0.14'
'set string 1 l 2 0'
xx = 0.2

yy=yt+0.5
'draw string ' xx ' ' yy ' ${GS}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${CMD}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${TIMESTAMP}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${HOST}'
yy = yy+0.2
'draw string ' xx ' ' yy ' ${CWD}'

'gxprint $FIG'

'close 1'
'quit'
EOF

grads -bcp "$GS"

rm -v $GS

echo
ls --time-style=long-iso -lh $FIG
echo

```

  
