TEST.QUANTILE
============================-
[TOC]
  
Fri, 12 Jun 2020 21:42:51 +0900
calypso.bosai.go.jp
/work05/manda/WRF.POST/K17/RAIN.10m.QUANTILE/TEST.QUANTILE

```
srcdump.sh TEST_SORT_RANDOM.sh SORTDK.F
```
  
### HOW TO RUN
  
### INFO
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
### SOURCE FILES
- TEST_SORT_RANDOM.sh
- SORTDK.F
  
#### TEST_SORT_RANDOM.sh
```bash
#!/bin/bash

F90=ifort
SRC="$(basename $0 .sh).F90"
SUB="SORTDK.F"
EXE=$(basename $0 .sh).EXE
OPT=" -traceback -fpe0 -CB -assume byterecl -convert big_endian"
#OPT=" -O2 -assume byterecl -convert big_endian"
NML=$(basename $0 .sh).NML

cat<<EOF>$SRC
program TEST_SORT_RANDOM

integer,parameter::n=15,nu=3

real(8),dimension(:),allocatable::x0,xi,x
integer,dimension(:),allocatable::idx
logical,dimension(:),allocatable::mask
real(8),parameter::undef=999.

allocate(xi(n),x(n),idx(n))

print *

print '(A)','SET INPUT DATA ...'
do i=1,n
xi(i)=i
idx(i)=i
enddo



print '(A)','SHUFFLE ...'
call random_number(u)
idx(1)=int(u*n)+1

do i=1,n

do

call random_number(u)
itmp=int(u*n)+1

CHECK: do ii=1,i-1

if(itmp == idx(ii))THEN
exit
ENDIF

end do CHECK !ii

if(ii >= i)exit

enddo

IDX(I)=itmp

end do !i
print *


print '(A)','ADD UNDEF ...'
do i=1,nu
call random_number(u)
id=int(u*n)+1
xi(id)=undef
end do
print *


print '(A)','SORT ...'
print '(A)','BEFORE ...'
do i=1,n
x(i)=xi(idx(i))
enddo

do i=1,n
print '(2f11.1)',sngl(xi(i)),sngl(x(i))
enddo
print *


mask = (x < undef)
n2=count(mask)

IND=0
call SORTDK(N,X,IND)

print '(A)','AFTER ...'

print '(A)','# xi    x'

do i=1,n

if(xi(i) < undef .and. x(i) < undef)then
print '(2f11.1)',sngl(xi(i)),sngl(x(i))

else if(xi(i) > undef .and. x(i) < undef)then
print '(e11.4,f11.1)',sngl(xi(i)),sngl(x(i))

else if(xi(i) < undef .and. x(i) > undef)then
print '(f11.1,e11.4)',sngl(xi(i)),sngl(x(i))

else if(xi(i) > undef .and. x(i) > undef)then
print '(2e11.4)',sngl(xi(i)),sngl(x(i))

endif

end do
print *

end program TEST_SORT_RANDOM
EOF


echo "$F90 $OPT $SRC $SUB -o $EXE"
$F90 $OPT $SRC $SUB -o $EXE
if [ $? -ne 0 ]; then
echo ERROR in $0 COMPILE ERROR!
exit 1
fi

ulimit -s unlimited
$EXE
#$EXE < $NML
```

  
#### SORTDK.F
```fortran
C     ***************************************************************
C     *                                                             *
C     *  SUBROUTINE SORTDK(N,AK,IND)     CODED BY I.NINOMIYA     *
C     *                                                             *
C     ***************************************************************
C     ---------------------------------------------------------------
      SUBROUTINE SORTDK(N,AK,IND)
C     ---------------------------------------------------------------
C     ---------------------------------------------------------------
C     DECLARATIONS
C     ---------------------------------------------------------------
      INTEGER*4 RR(30),LL(30),R
      REAL*8 AK(N),BK
C     ---------------------------------------------------------------
C     PARAMETER ERROR CHECK
C     ---------------------------------------------------------------
      IF(N.LT.1) GO TO 200
      IF(N.EQ.1) GO TO 190
C     ---------------------------------------------------------------
C     INITIALIZATION
C     ---------------------------------------------------------------
      IF(IND.EQ.0) GO TO 20
      DO 10 I=1,N
   10 AK(I)=-AK(I)
   20 ISP=0
      L=1
      R=N
C     ---------------------------------------------------------------
C     ---------------------------------------------------------------
C     IF THE LENGTH OF THE CURRENT SUB-ARRAY IS LESS THAN 16,
C     SORT IT BY THE STRAIGHT INSERTION SORT. OTHERWISE PICK UP
C     THE LEFTMOST, MIDDLE AND RIGHTMOST ELEMENTS AND PUT THE
C     LARGEST IN THE RIGHT,SMALLEST IN THE MIDDLE AND THE REST
C     IN THE LEFT.
C     ---------------------------------------------------------------
   30 IF(R-L.LT.16) GO TO 120
      M=(R+L)/2
      MAX=R
      IF(AK(M).GT.AK(R)) MAX=M
      IF(AK(L).GT.AK(MAX)) MAX=L
      IF(MAX.EQ.R) GO TO 40
      BK=AK(MAX)
      AK(MAX)=AK(R)
      AK(R)=BK
   40 IF(AK(L).GE.AK(M)) GO TO 50
      BK=AK(L)
      AK(L)=AK(M)
      AK(M)=BK
C     ---------------------------------------------------------------
C     PUT THE LEFTMOST ELEMENT TO THE CORRECT POSITION,TRANSFERRING
C     SMALLER ELEMENTS TO THE LEFT AND LARGER ELEMENTS TO THE RIGHT.
C     ---------------------------------------------------------------
   50 BK=AK(L)
      I=L
      J=R
      GO TO 80
   60 AK(J)=AK(I)
   70 J=J-1
   80 IF(BK.LT.AK(J)) GO TO 70
      IF(J.LE.I) GO TO 100
      AK(I)=AK(J)
   90 I=I+1
      IF(AK(I).LT.BK) GO TO 90
      IF(J.GT.I) GO TO 60
C     ---------------------------------------------------------------
C     STORE THE END POSITIONS OF LARGER HALF SUB-ARRAY INTO STACK,
C     AND ATTACK THE SMALLER HALF.
C     ---------------------------------------------------------------
      I=J
  100 AK(I)=BK
      ISP=ISP+1
      IF(R-I.GE.I-L) GO TO 110
      LL(ISP)=L
      RR(ISP)=I-1
      L=I+1
      GO TO 30
  110 LL(ISP)=I+1
      RR(ISP)=R
      R=I-1
      GO TO 30
C     ---------------------------------------------------------------
C     STRAIGHT INSERTION SORT.
C     ---------------------------------------------------------------
  120 IF(R-L.LT.1) GO TO 160
      J=R
  130 BK=AK(J-1)
      I=J
  140 IF(AK(I).GE.BK) GO TO 150
      AK(I-1)=AK(I)
      I=I+1
      IF(I.LE.R) GO TO 140
  150 AK(I-1)=BK
      J=J-1
      IF(J.GT.L) GO TO 130
C     ---------------------------------------------------------------
C     POP UP THE LATEST INFORMATIOINS FROM THE STACK.
C     ---------------------------------------------------------------
  160 IF(ISP.EQ.0) GO TO 170
      L=LL(ISP)
      R=RR(ISP)
      ISP=ISP-1
      GO TO 30
C     ---------------------------------------------------------------
C     NORMAL EXIT.
C     ---------------------------------------------------------------
  170 IF(IND.EQ.0) RETURN
      DO 180 I=1,N
  180 AK(I)=-AK(I)
      IND=0
      RETURN
C     ---------------------------------------------------------------
C     ERROR EXITS.
C     ---------------------------------------------------------------
  190 IND=0
      RETURN
  200 IND=30000
      RETURN
      END
```

  
