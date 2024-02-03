
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
