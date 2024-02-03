TEST_REGRESSION
============================-
[TOC]
  
Wed, 29 Jul 2020 19:44:04 +0900
calypso.bosai.go.jp
/work05/manda/20.AGO_WAN/FLUX.MSM/03.CALI_UKATA/TEST_REGRESSION

```
srcdump.sh REGRESSION.sh REGRESSION.f90 PL_TSR.sh
```
  
### HOW TO RUN
  
### INFO
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
### SOURCE FILES
- REGRESSION.sh
- REGRESSION.f90
- PL_TSR.sh
  
#### REGRESSION.sh
```bash
#!/bin/bash
src=$(basename $0 .sh).f90
exe=$(basename $0 .sh).exe
nml=$(basename $0 .sh).nml

f90=ifort
opt="-CB -traceback -fpe0 -convert big_endian -assume byterecl"
#opt="-convert big_endian -assume byterecl"

BIN=$(basename $0 .sh).BIN
CTL=$(basename $BIN .BIN).CTL

NT=365
cat <<EOF>$nml
&para
BIN="$BIN"
NT=${NT}
A1=1.0
P1=365
A21=1.0
P21=365
L21=0
A22=0.1
P22=10
L22=3
&end
EOF

cat <<EOF>$CTL
dset ^$BIN
title ${STN}
options big_endian
undef -999.90
xdef 1 levels 130
ydef 1 levels 32
zdef 1 levels 1000
tdef ${NT} linear 01JAN2000 1DY
vars 3
X1  1 0 INDEPEDENT (INPUT)
X2  1 0 DEPENDENT  (OUTPUT)
XR  1 0 REGRESSION
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
#ls -lh --time-style=long-iso -lh ${ODIR}/${BFLE}
#echo
#ls -lh --time-style=long-iso -lh ${ODIR}/${OFLE}
#echo
rm -v $exe $nml
```

  
#### REGRESSION.f90
```fortran

character(len=500)::BIN
CHARACTER(len=255) :: cwd
real,parameter:: PI=3.141592653589793
real,parameter::D2R=3.141592653589793/180.0
real,ALLOCATABLE,dimension(:)::X1,X2,XR
real A1,P1, A21,P21,L21, A22,P22,L22

namelist /para/BIN,NT,A1,P1, A21,P21,L21, A22,P22,L22


print *,'READ NAMELIST'
read(*,nml=para)
print *

allocate(X1(NT),X2(NT),XR(NT))


print *,'MAKE DATA'
DO I=1,NT
T=float(I-1)
X1(I)=  A1*cos(2.0*PI/P1 *T)
X2(I)= A21*cos(2.0*PI/P21*T-2.0*PI/P21*L21) &
     + A22*sin(2.0*PI/P22*T-2.0*PI/P22*L22)
END DO
print *



print *,'REGRESSION'
CALL LSQM(X1,X2,NT,A,B,R,COV,X1AVE,X2AVE,X1SD,X2SD)
print *,'A=',A
print *,'B=',B
print *,'R=',R
print *,'COV=',COV
print *,'X1AVE=',X1AVE
print *,'X2AVE=',X2AVE
print *,'X1SD=',X1SD
print *,'X2SD=',X2SD

DO I=1,NT
XR(I)=R*(X1(I)-X1AVE) + X2AVE
END DO !I

print *,'OUTPUT'
open(31,file=BIN,form="unformatted",access="direct",recl=4)
irec=0
DO I=1,NT
irec=irec+1
write(31,rec=irec)X1(I)
irec=irec+1
write(31,rec=irec)X2(I)
irec=irec+1
write(31,rec=irec)XR(I)
end do !I
print *


PRINT *
PRINT *,'OUTPUT: ',TRIM(BIN)
PRINT *
stop
end



SUBROUTINE LSQM(P,Q,N,A,B,R,COV,PM,QM,Sp,SQ)

INTEGER,INTENT(IN)::N
REAL,INTENT(IN)::P(N),Q(N)

REAL,INTENT(INOUT):: A,B,R,COV,PM,QM

TP=0.0
TQ=0.0


DO I=1,N
TP=TP+P(I)
TQ=TQ+Q(I)
END DO !I

PM=TP/FLOAT(N)
QM=TQ/FLOAT(N)
TPQ=0.0
TPP=0.0
DO I=1,N
TPQ=TPQ+(P(I)-PM)*(Q(I)-QM)
TPP=TPP+(P(I)-PM)**2
END DO !I
B=TPQ/TPP
A=QM-B*PM

!
!STANDARD DEVIATION : SP,SQ
! 
P2=0.0
Q2=0.0
DO I=1,N
  P2=P2+(P(I)-PM)**2
  Q2=q2+(Q(I)-QM)**2
END DO !N

RN=FLOAT(N)
SP=SQRT(P2/RN)
SQ=SQRT(Q2/RN)

!
! CORRELATION C0EF. : R
!
SUM=0.0
DO I=1,N
  SUM=SUM+( ((P(I)-PM)/SP)*((Q(I)-QM)/SQ) )
END DO !I
R=SUM/RN
!
! COVARIANCE
! 
SUM=0.0
DO I=1,N
SUM=SUM+( (P(I)-PM)*(Q(I)-QM) )
END DO !I
COV=SUM/RN

RETURN
END

!**********************************************************************
! LSQM.F
! 
! CODED BY A.MANDA           1997.8 
! TASK
! Linear Regression
! PARAMETERS                                                           
!   (1)P : INPUT DATA [X-COOD.]      (R)(INPUT)
!   (2)Q : INPUT DATA [Y-COOD.]      (R)(INPUT)
!   (3)N : TOTAL DATA NUMBER      (I)(INPUT)
!   (4)A : Y-SECTION OF LINE (R)(OUTPUT)
!   (5)B : GRADIENT OF LINE  (R)(OUTPUT)
! METHOD
! SLAVE SUBROUTINE(S)
! REMARK
! REFERENCE(S)                           
!***********************************************************************
```

  
#### PL_TSR.sh
```bash
HOST=$(hostname)
CWD=$(pwd)
TIMESTAMP=$(date -R)
CMD="$0 $@"
GS=$(basename $0 .sh).GS

STN=UKATA
INT=1HR
CTL=REGRESSION.CTL

TIME1=01JAN2000
TIME2=31DEC2000

FIGDIR=. #FIG_$(basename $0 .sh)
#mkdir -vp $FIGDIR
FIG=$FIGDIR/$(basename $CTL .CTL)_${TIME1}_${TIME1}.eps


MIN1=-1.5
MAX1=1.5

MIN2=-1.5
MAX2=1.5

MIN3=-1.5
MAX3=1.5


cat <<EOF>$GS
'open $CTL'
'cc'

'set time $TIME1 $TIME2'

'set vpage 0.0 8.5 0 10.5'

xmax=1
ymax=3

xleft=1
ytop=9

xwid =  5/xmax
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

#'set tlsupp year'
'set grads off'
'set grid off'
'set xlopts 1 3 0.1'
'set ylopts 1 3 0.1'

'set gxout line'
'set ccolor 3'
'set cmark 0'
'set cthick 2'
'set vrange ${MIN1} ${MAX1}'
'set ylint 0.5'
'd X1'

'set xlab off'
'set ylab off'

'zeroline'

'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

x=xl-0.7 ;# (xl+xr)/2
y=(yt+yb)/2
'set strsiz 0.12 0.15'
'set string 1 c 4 90'
'draw string 'x' 'y' X1'

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



ymap=2
xmap=1

xs = xleft + (xwid+xmargin)*(xmap-1)
xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1)
ys = ye - ywid

'set parea 'xs ' 'xe' 'ys' 'ye

#'set tlsupp year'
'set grads off'
'set grid off'
'set xlab on'
'set ylab on'
'set xlopts 1 3 0.1'
'set ylopts 1 3 0.1'

'set gxout line'
'set ccolor 3'
'set cmark 0'
'set cthick 1'
'set vrange ${MIN2} ${MAX2}'
'set ylint 0.5'
'd X2'

'set xlab off'
'set ylab off'

#'zeroline'

'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

x=xl-0.7 ;# (xl+xr)/2
y=(yt+yb)/2
'set strsiz 0.12 0.15'
'set string 1 c 4 90'
'draw string 'x' 'y' X2'



ymap=3
xmap=1

xs = xleft + (xwid+xmargin)*(xmap-1)
xe = xs + xwid
ye = ytop - (ywid+ymargin)*(ymap-1)
ys = ye - ywid

'set parea 'xs ' 'xe' 'ys' 'ye

#'set tlsupp year'
'set grads off'
'set grid off'
'set xlab on'
'set ylab on'
'set xlopts 1 3 0.1'
'set ylopts 1 3 0.1'

'set gxout line'
'set ccolor 3'
'set cmark 0'
'set cthick 1'
'set vrange ${MIN3} ${MAX3}'
'set ylint 0.5'
'd XR'

'set xlab off'
'set ylab off'



'q gxinfo'
line3=sublin(result,3); line4=sublin(result,4)
xl=subwrd(line3,4); xr=subwrd(line3,6)
yb=subwrd(line4,4); yt=subwrd(line4,6)

x=xl-0.7 ;# (xl+xr)/2
y=(yt+yb)/2
'set strsiz 0.12 0.15'
'set string 1 c 4 90'
'draw string 'x' 'y' XR'


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

  
