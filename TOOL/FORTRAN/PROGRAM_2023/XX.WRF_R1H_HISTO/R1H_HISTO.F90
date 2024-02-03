PROGRAM R1H_HISTO

! /work09/am/00.WORK/2022.RW3A/0JOS.FALL2023/02.12.RAIN/22.12.R1H_HISTO

CHARACTER(LEN=200):: INDIR, PREFIX, INDIR2
CHARACTER(LEN=400):: INFLE, INFLE2
CHARACTER(LEN=600):: IN, OUT, IN2
INTEGER IM,JM,KM,NM
INTEGER YR0,MO0,DY0,HR0,MI0,SS0,DH

REAL,DIMENSION(:,:,:),ALLOCATABLE::dummy3
REAL,DIMENSION(:,:),ALLOCATABLE::R1,rlon,rlat,dummy2
REAL,DIMENSION(:,:),ALLOCATABLE::XLAT,XLON,MASK

REAL*8 julian_day
REAL RTOTAL
INTEGER NTOTAL

INTEGER year,month,day,hour,min,sec !FOR TEST

REAL::LONW,LONE,LATS,LATN

REAL,DIMENSION(:),ALLOCATABLE::RBIN_L, RBIN_R, NFRQ, RFRQ, RPCT
REAL,PARAMETER::R1MIN=1.0,R1LEFT=5.0;R1MAX=120.0; DR1=5.0


namelist /para/INDIR,PREFIX,OUT,IM,JM,KM,NM,YR0,MO0,DY0,HR0,MI0,DH
SS0=0

LONW=129;LONE=132;LATS=31;LATN=34

MB=INT((R1MAX-R1LEFT)/DR1)
ALLOCATE(RBIN_L(MB), RBIN_R(MB), NFRQ(MB), RFRQ(MB), RPCT(MB))

RTOTAL=0.0; NTOTAL=0
NFRQ=0.0; RFRQ=0.0; RPCT(MB)=0.0

RBIN_L(1)=R1MIN; RBIN_R(1)=R1LEFT
DO M=2,MB
RBIN_L(M)=R1LEFT+DR1*FLOAT(M-1)
RBIN_R(M)=R1LEFT+DR1*FLOAT(M)
END DO !M



READ(*,nml=para)

ALLOCATE(dummy3(IM,JM,KM))
ALLOCATE(R1(IM,JM),rlon(IM,JM),rlat(IM,JM),dummy2(IM,JM))
ALLOCATE(XLAT(IM,JM),XLON(IM,JM),MASK(IM,JM))

INDIR2='/work00/DATA/HD01/RW3A.ARWpost.DAT/hdiv_p/RW3A.00.03.05.05.0000.01'
INFLE2='RW3A.00.03.05.05.0000.01.d01.hdiv_p.01HR_2021-08-11_00:00.dat'
IN=TRIM(INDIR2) // '/' // TRIM(INFLE2)

OPEN(11,FILE=TRIM(IN),ACTION="READ",&
form='unformatted',access='direct',recl=IM*JM*4)
irec=0
write(*,'(A)') 'READ XLAT '
irec=irec+1
read (11,rec=irec) ((XLAT(i,j),i=1,IM),j=1,JM)

write(*,'(A)') 'READ XLON '
irec=irec+1
read (11,rec=irec) ((XLON(i,j),i=1,IM),j=1,JM)

MASK=0.0; GRIDNUM=0.0

DO J=1,JM
DO I=1,IM

IF(XLON(I,J)>=LONW .AND. XLON(I,J)<LONE .and. XLAT(I,J)>=LATS &
.and. XLAT(I,J)<LATN)THEN
MASK(I,J)=1.0
GRIDNUM=GRIDNUM+1.0
END IF !LON LAT

END DO !I
END DO !J
PRINT *,'GRIDNUM=',GRIDNUM

call date2jd(YR0,MO0,DY0,HR0,MI0,SS0,julian_day)

DO N=1,NM

call jd2date(year,month,day,hour,min,sec,julian_day)

IN=TRIM(INDIR) // '/' // TRIM(PREFIX)
IE=lnblnk(IN)

IS=IE+1;IE=IS+5; WRITE(IN(IS:IE),'(I4.4,A)')year, '-'
IS=IE  ;IE=IS+3; WRITE(IN(IS:IE),'(I2.2,A)')month,'-'
IS=IE  ;IE=IS+3; WRITE(IN(IS:IE),'(I2.2,A)')day,  '_'
IS=IE  ;IE=IS+3; WRITE(IN(IS:IE),'(I2.2,A)')hour, ':'
IS=IE  ;IE=IS+2; WRITE(IN(IS:IE),'(I2.2,A)')min
IS=IE  ;IE=IS+4; WRITE(IN(IS:IE),'(A)')'.dat'

OPEN(11,FILE=TRIM(IN),ACTION="READ",&
form='unformatted',access='direct',recl=IM*JM*4)

write(*,'(I5,5I3)',advance='yes'),&
year,month,day,hour,min

irec=0

write(*,'(A)',advance='no') 'SKIP '

write(*,'(A)',advance='no') 'U '
DO K = 1, KM; irec=irec+1; END DO !K

write(*,'(A)',advance='no') 'V '
DO K = 1, KM; irec=irec+1; END DO !K

write(*,'(A)',advance='no') 'W '
DO K = 1, KM; irec=irec+1; END DO !K

write(*,'(A)',advance='no') 'Q2 '
irec=irec+1

write(*,'(A)',advance='no') 'T2 '
irec=irec+1

write(*,'(A)',advance='no') 'U10 '
irec=irec+1

write(*,'(A)',advance='no') 'V10 '
irec=irec+1

write(*,'(A)',advance='no') 'QVAPOR '
DO K = 1, KM; irec=irec+1; END DO !K

write(*,'(A)',advance='no') 'QCLOUD '
DO K = 1, KM; irec=irec+1; END DO !K

write(*,'(A)',advance='no') 'QRAIN '
DO K = 1, KM; irec=irec+1; END DO !K

write(*,'(A)',advance='no') 'HGT '
irec=irec+1

write(*,'(A)',advance='no') 'RAINC '
irec=irec+1

write(*,'(A)',advance='no') 'RAINRC '
irec=irec+1

write(*,'(A)',advance='no') 'RAINNC '
irec=irec+1

write(*,'(A)',advance='no') 'R1=RAINRNC '
irec=irec+1
read (11,rec=irec) ((R1(i,j),i=1,IM),j=1,JM)

write(*,'(A)',advance="yes")''
CLOSE(11)

DO J=1,JM
DO I=1,IM

IF( MASK(I,J)==1.0)THEN
RTOTAL=RTOTAL+R1(I,J)
NTOTAL=NTOTAL+1
DO M=1,MB
IF(R1(I,J)>=RBIN_L(M) .and. R1(I,J)<RBIN_R(M))THEN
NFRQ(M)=NFRQ(M)+1.0
END IF !R1
END DO !M
END IF !MASK

END DO !I
END DO !J

julian_day=julian_day+dble(DH)/24.0d0 !DH [hr]

END DO !N

DO M=1,MB
RFRQ(M)=NFRQ(M)/FLOAT(NTOTAL)
END DO !M

PRINT *; PRINT '(A,A)','OUTPUT: ',TRIM(OUT)
OPEN(21,FILE=TRIM(OUT))

WRITE(21, '(A)') ,'# RBIN, RFRQ, NFRQ'
DO M=1,MB
RBIN=(RBIN_L(M)+RBIN_R(M))/2.0
WRITE(21,'(F9.2,E12.4,F12.0)'),RBIN,RFRQ(M),NFRQ(M)
END DO !M

END PROGRAM R1H_HISTO
