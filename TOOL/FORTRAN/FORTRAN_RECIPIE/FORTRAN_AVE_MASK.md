```fortran
logical,allocatable::mask(:,:)
real,allocatable::var2(:,:)
real,allocatable::xlat(:,:),xlong(:,:)
real ALONW, ALONE, ALATS, ALATN

allocate(mask(im,jm), var2(im,jm) xlat(im,jm), xlong(:,:))

var2(:,:)=0.0
ave=0.0

mask = (var2 < undef .and. xlong >= ALONW .and. &
xlong <= ALONE .and. xlat >= ALATS  .and. xlat  <= ALATN )

rnum=count(mask)
ave=sum(var2, mask)/rnum
```



CNV.BAK.200611
============================-
[TOC]

Thu, 11 Jun 2020 09:42:01 +0900
calypso.bosai.go.jp
/work05/manda/WRF.POST/K17/CNV.BAK.200611

```
srcdump.sh CNV.WRF.run.sh CNV.WRF.sh CNV.WRF.F90 HCNV3S.f90
```

### HOW TO RUN

### INFO
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB

### SOURCE FILES
- CNV.WRF.run.sh
- CNV.WRF.sh
- CNV.WRF.F90
- HCNV3S.f90
  
#### CNV.WRF.run.sh
```bash
EXE=CNV.WRF.sh

EXPLIST="\
00.00 \
"

for EXP in $EXPLIST; do
$EXE $EXP
done
```


#### CNV.WRF.sh
```bash
CASE=K17
RUN=R27
EXP=$1 #00.00
#EXP=00.06
#EXP=06.06

F90=ifort
SRC="$(basename $0 .sh).F90"
SUB="HCNV3S.f90"
EXE=$(basename $0 .sh).EXE
#OPT=" -traceback -fpe0 -CB -assume byterecl -convert big_endian"
OPT=" -O2 -assume byterecl -convert big_endian"
NML=$(basename $0 .sh).NML


HDD=work06
RUNNAME=${CASE}.${RUN}.${EXP}
DOMAIN=d03
TYPE=hdiv_p

INDIR=/work06/manda/ARWpost_${CASE}_${HDD}/ARWpost_${RUNNAME}.10m
INPFX=${RUNNAME}.10m.${DOMAIN}.${TYPE}.10m

VCOORDF="${RUNNAME}.vcoord.txt"

ODIR=${RUNNAME}
if [ -d $ODIR ]; then
rm -rfv $ODIR
fi
mkdir -vp $ODIR
OPFX=${INPFX}

OFLE2=${ODIR}/${RUNNAME}.10m.${DOMAIN}.${TYPE}_AAVE.dat

OUTCTL=$(basename $0 .sh)_${RUNNAME}.CTL

OUTCTL2=$(basename $0 .sh)_${RUNNAME}_AAVE.CTL

NUMTOUT=109
OUTDATES=00Z05JUL2017
DTOUT=10MN

cat <<EOF>$NML
&para
indir="$INDIR",
inpfx="$INPFX",
iyr1=2017
mon1=07
idy1=05
ihr1=00
imin1=00
isec1=00
iyr2=2017
mon2=07
idy2=05
ihr2=00
imin2=00
isec2=00
dt_file=600
imax=300
jmax=300
kmax=30
vcoordf="$VCOORDF"
DX=1000.
DY=1000.
odir="$ODIR",
opfx="$OPFX",
ofle2="$OFLE2"
ALONW=130.4
ALONE=131.1
ALATS=33.3
ALATN=33.55
&end
EOF

cat <<EOF>$VCOORDF
1000.00000
 990.00000
 980.00000
 970.00000
 960.00000
 950.00000
 940.00000
 930.00000
 920.00000
 910.00000
 900.00000
 880.00000
 860.00000
 840.00000
 820.00000
 800.00000
 750.00000
 700.00000
 650.00000
 600.00000
 550.00000
 500.00000
 450.00000
 400.00000
 350.00000
 300.00000
 250.00000
 200.00000
 150.00000
 100.00000
EOF

echo "$F90 $OPT $SRC $SUB -o $EXE"
$F90 $OPT $SRC $SUB -o $EXE
if [ $? -ne 0 ]; then
echo ERROR in $0 COMPILE ERROR!
exit 1
fi

ulimit -s unlimited
$EXE < $NML



cat <<EOF>$OUTCTL
dset ^${ODIR}/${RUNNAME}.10m.d03.hdiv_p.10m_%y4-%m2-%d2_%h2:%n2.dat
options  byteswapped template
undef 1.e30
title  OUTPUT FROM WRF V3.7.1 MODEL
pdef  300 300 lcc  32.948  130.856  150.500  150.500  34.00000  24.00000  128.00000   1000.000   1000.000
xdef  749 linear  129.18993   0.00450450
ydef  632 linear   31.51415   0.00450450
zdef   30 levels  
1000.00000
 990.00000
 980.00000
 970.00000
 960.00000
 950.00000
 940.00000
 930.00000
 920.00000
 910.00000
 900.00000
 880.00000
 860.00000
 840.00000
 820.00000
 800.00000
 750.00000
 700.00000
 650.00000
 600.00000
 550.00000
 500.00000
 450.00000
 400.00000
 350.00000
 300.00000
 250.00000
 200.00000
 150.00000
 100.00000
tdef  ${NUMTOUT} linear ${OUTDATES}      ${DTOUT}      
VARS    9
XLAT           1  0  LATITUDE, SOUTH IS NEGATIVE (degree_north)
XLONG          1  0  LONGITUDE, WEST IS NEGATIVE (degree_east)
U             30  0  x-wind component (m s-1)
V             30  0  y-wind component (m s-1)
QVAPOR        30  0  Water vapor mixing ratio (kg kg-1)
HGT            1  0  Terrain Height (m)
XLAND          1  0  LAND MASK (1 FOR LAND, 2 FOR WATER) (-)
DIV           30  0  DIVERGENCE (s-1)
MFDIV         30  0  MOIST FLUX DIVERGENCE (kg m-2 s-1)
ENDVARS
EOF



cat <<EOF>$OUTCTL2
dset ^${OFLE2}
* ${ALONW} ${ALONE} ${ALATS} ${ALATN}
options  byteswapped
undef 1.e30
title  OUTPUT FROM WRF V3.7.1 MODEL
xdef 1 linear  129.18993   0.00450450
ydef 1 linear   31.51415   0.00450450
zdef   30 levels  
1000.00000
 990.00000
 980.00000
 970.00000
 960.00000
 950.00000
 940.00000
 930.00000
 920.00000
 910.00000
 900.00000
 880.00000
 860.00000
 840.00000
 820.00000
 800.00000
 750.00000
 700.00000
 650.00000
 600.00000
 550.00000
 500.00000
 450.00000
 400.00000
 350.00000
 300.00000
 250.00000
 200.00000
 150.00000
 100.00000
tdef ${NUMTOUT} linear ${OUTDATES}      ${DTOUT}      
VARS    6
CNVAV         30  0  AREA AVE OF CNV (s-1)
P1SD          30  0  AREA AVE OF CNV PLUS 1 SD (s-1)
M1SD          30  0  AREA AVE OF CNV MINUS 1 SD (s-1)
MCNVAV        30  0  AAVE OF MOIST FLUX CNV (kg m-2 s-1)
MCNVP1        30  0  AAVE OF MOIST FLUX CNV P1SD (kg m-2 s-1)
MCNVM1        30  0  AAVE OF MOIST FLUX CNV M1SD (kg m-2 s-1)
ENDVARS
EOF
```


#### CNV.WRF.F90
```fortran
program CNV_WRF

character(len=500)::indir,inpfx,infle,vcoordf,odir,opfx,ofle,ofle2
real, parameter :: daysec = 86400.0 ! [sec]

real,parameter :: undef=1.e30

real*8::jd,jd_beg,jd_end

real,allocatable::plev(:)
real,allocatable::xlat(:,:),xlong(:,:),mapfac_m(:,:),HGT(:,:),xland(:,:)
real,allocatable::u(:,:,:),v(:,:,:),QVAPOR(:,:,:)
real,allocatable::mfx(:,:,:),mfy(:,:,:),mfcnv(:,:,:)

real,allocatable::cnv(:,:,:)
real,allocatable::var2(:,:)

logical :: yes

logical,allocatable::mask(:,:)
real,allocatable::cnvAV(:),P1SD(:),M1SD(:),McnvAV(:),McnvP1(:),McnvM1(:)

real,allocatable::AVECHK(:),SDCHK(:)


namelist /para/indir,inpfx,&
iyr1,mon1,idy1,ihr1,imin1,isec1, iyr2,mon2,idy2,ihr2,imin2,isec2,&
dt_file,vcoordf,imax,jmax,kmax,DX,DY,odir,opfx,ofle2,ALONW,ALONE,&
ALATS,ALATN

read(*,nml=para)

allocate(plev(kmax))
allocate(xlat(imax,jmax),xlong(imax,jmax),mapfac_m(imax,jmax),HGT(imax,jmax),xland(imax,jmax))
allocate(u(imax,jmax,kmax),v(imax,jmax,kmax),QVAPOR(imax,jmax,kmax))

allocate(cnv(imax,jmax,kmax))

allocate(mfx(imax,jmax,kmax),mfy(imax,jmax,kmax),mfcnv(imax,jmax,kmax))
allocate(var2(imax,jmax))
allocate(cnvAV(kmax),P1SD(kmax),M1SD(kmax),McnvAV(kmax),McnvP1(kmax),McnvM1(kmax))
allocate(mask(imax,jmax))

open(11,file=trim(vcoordf),action="read")


do k=1,kmax
read(11,*)plev(k)
enddo !k
print *
print '(A,f8.1,A,f8.1)','plev(1)=',plev(1),' plev(kmax)=',plev(kmax)



call date2jd(iyr1,mon1,idy1,ihr1,imin1,isec1,jd_beg)
call date2jd(iyr2,mon2,idy2,ihr2,imin2,isec2,jd_end)

jd = jd_beg



open (21,file=trim(ofle2),form='unformatted',&
         access='direct',recl=1*1*kmax*4)
!open (21,file=trim(ofle2),form='unformatted',&
!         access='direct',recl=1*1*4)
irec2=0


MAIN_TIME_LOOP: do

call jd2date(iyr,mon,idy,ihr,imin,isec,jd+0.01/daysec)


! ... infile
write(infle,'(a,a,a,a,i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)') &
trim(indir),'/',trim(inpfx),'_',&
iyr,'-',mon,'-',idy,'_',ihr,':',imin,'.dat'

print *
print '(A,A)','READ ',trim(infle)


inquire(file=trim(infle),exist=yes)
if (.not. yes) then
print *,'No such file:',trim(infle)
stop
end if
open (10,file=trim(infle),form='unformatted',action="read",&
         access='direct',recl=imax*jmax*4)

print '(A)','read latitude (degree)'

irec=1
read (10,rec=irec) ((xlat(i,j),i=1,imax),j=1,jmax)
print *,xlat(imax/2,jmax/2)

irec=irec+1
print '(A)','longitude (degree)'
read (10,rec=irec) ((xlong(i,j),i=1,imax),j=1,jmax)
print *,xlong(imax/2,jmax/2)

print '(A)','read u'
do k = 1, kmax
irec = irec + 1
read (10,rec=irec) ((u(i,j,k),i=1,imax),j=1,jmax)
end do
print *,u(imax/2,jmax/2,10)

print '(A)','read v'
do k = 1, kmax
irec = irec + 1
read (10,rec=irec) ((v(i,j,k),i=1,imax),j=1,jmax)
end do
print *,v(imax/2,jmax/2,10)

!print '(A)','read w'
!do k = 1, kmax
!irec = irec + 1
!read (10,rec=irec) ((w(i,j,k),i=1,imax),j=1,jmax)
!end do
!print *,w(imax/2,jmax/2,10)
!print '(A,i5)','irec=',irec

print '(A)','read q'
do k = 1, kmax
irec = irec + 1
read (10,rec=irec) QVAPOR(:,:,k)
end do
print *,QVAPOR(imax/2,jmax/2,10)
print '(A,i5)','irec=',irec

print '(A)','read mapfac_m'
irec = irec + 1
read (10,rec=irec) ((mapfac_m(i,j),i=1,imax),j=1,jmax)
print '(A,i5)','irec=',irec
print *,mapfac_m(imax/2,jmax/2)

print '(A)','read HGT'
irec = irec + 1
read (10,rec=irec) ((HGT(i,j),i=1,imax),j=1,jmax)
print '(A,i5)','irec=',irec
print *,HGT(imax/2,jmax/2)

print '(A)','read xland'
irec = irec + 1
read (10,rec=irec) ((xland(i,j),i=1,imax),j=1,jmax)
print '(A,i5)','irec=',irec
print *,xland(imax/2,jmax/2)

close(10)



call Hcnv3S(cnv,u,v,mapfac_m,DX,DY,imax,jmax,kmax,undef)

print *,"cnv   ",imax/2,jmax/2,plev(5),cnv(imax/2,jmax/2,5)

where (u < undef .and. QVAPOR < undef .and. v < undef )
mfx=QVAPOR*u
mfy=QVAPOR*v
else where
mfx=undef
mfy=undef
end where

call Hcnv3S(mfcnv,mfx,mfy,mapfac_m,DX,DY,imax,jmax,kmax,undef)

print *,"MFcnv ",imax/2,jmax/2,plev(5),mfcnv(imax/2,jmax/2,5)


print *,"AREA AVE"

M1SD(:)=undef
cnvAV(:)=undef
P1SD(:)=undef
McnvM1(:)=undef
McnvAV(:)=undef
McnvP1(:)=undef

do k=1,kmax

var2(:,:)=0.0
var2(:,:)=cnv(:,:,k)

mask = (var2 < undef .and. xlong >= ALONW .and. xlong <= ALONE .and. &
 xlat >= ALATS  .and. xlat  <= ALATN )

rnum=count(mask)
avetmp=sum(var2, mask)/rnum
cnvAV(k)=avetmp

sdtmp=0.0
do j=1,jmax
do i=1,imax

if(var2(i,j) < undef .and. xlong(i,j) >= ALONW .and. xlong(i,j) <= ALONE .and. &
 xlat(i,j) >= ALATS  .and. xlat(i,j)  <= ALATN)then
sdtmp=sdtmp+((var2(i,j)-avetmp)**2)
endif

enddo
enddo
sdtmp=sqrt(sdtmp/(rnum-1.0))
M1SD(k)=avetmp-sdtmp
P1SD(k)=avetmp+sdtmp


var2(:,:)=0.0
var2(:,:)=mfcnv(:,:,k)

mask = (var2 < undef .and. xlong >= ALONW .and. xlong <= ALONE .and. &
 xlat >= ALATS  .and. xlat  <= ALATN )

rnum=count(mask)
avetmp=sum(var2, mask)/rnum

McnvAV(k)=avetmp

sdtmp=0.0
do j=1,jmax
do i=1,imax

if(var2(i,j) < undef .and. xlong(i,j) >= ALONW .and. xlong(i,j) <= ALONE .and. &
 xlat(i,j) >= ALATS  .and. xlat(i,j)  <= ALATN)then
sdtmp=sdtmp+((var2(i,j)-avetmp)**2)
endif

enddo
enddo

sdtmp=sqrt(sdtmp/(rnum-1.0))
McnvM1(k)=avetmp-sdtmp
McnvP1(k)=avetmp+sdtmp

end do !k


print *,"CNV2  ",imax/2,jmax/2,plev(5),cnv(imax/2,jmax/2,5)



! CHECK
allocate(AVECHK(kmax),SDCHK(kmax))
SDCHK(:)=0.

do k=1,kmax

avetmp=0.
AVECHK(k)=0.
CNTCHK=0.0

do j=1,jmax
do i=1,imax

if(cnv(i,j,k) < undef .and. xlong(i,j) >= ALONW .and. xlong(i,j) <= ALONE .and. &
 xlat(i,j) >= ALATS  .and. xlat(i,j)  <= ALATN)then

avetmp=avetmp+cnv(i,j,k)
CNTCHK=CNTCHK+1.0

end if

end do !i
end do !j

!print *,CNTCHK
AVECHK(k)=avetmp/CNTCHK

end do !k



do k=1,kmax

SDCHK(k)=0.0
CNTCHK=0.0

do j=1,jmax
do i=1,imax

if(cnv(i,j,k) < undef .and. xlong(i,j) >= ALONW .and. xlong(i,j) <= ALONE .and. &
 xlat(i,j) >= ALATS  .and. xlat(i,j)  <= ALATN)then

SDCHK(k)=SDCHK(k)+(AVECHK(k)-cnv(i,j,k))**2
CNTCHK=CNTCHK+1.0

end if

end do !i
end do !j

SDCHK(k)=sqrt(SDCHK(k)/(CNTCHK-1.0))

end do !k

! END CHECK


print *,'CHECK AAVE'
print '(A)','   K  CNV(NEW)    CNV(LEGACY) MCNV(NEW)   MCNV(LEGACY)'
do k=1,kmax
print '(i4,6E12.4)',k,cnvAV(k),AVECHK(k),P1SD(k),AVECHK(k)+SDCHK(k)
enddo



irec2=irec2+1
write(21,rec=irec2) cnvAV

irec2=irec2+1
write(21,rec=irec2) P1SD

irec2=irec2+1
write(21,rec=irec2) M1SD

irec2=irec2+1
write(21,rec=irec2) McnvAV

irec2=irec2+1
write(21,rec=irec2) McnvP1

irec2=irec2+1
write(21,rec=irec2) McnvM1



! ... ofle
write(ofle,'(a,a,a,a,i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)') &
trim(odir),'/',trim(opfx),'_',&
iyr,'-',mon,'-',idy,'_',ihr,':',imin,'.dat'

print *
print '(A,A)','OUT ',trim(ofle)

inquire(file=trim(ofle),exist=yes)
if (yes) then
print *,'CAUTION: OVERWRITING ',trim(ofle)
print *,'TERMINATED.'
stop
end if

open (20,file=trim(ofle),form='unformatted',&
         access='direct',recl=imax*jmax*4)

irec=1
print '(A)','OUT latitude (degree)'
write (20,rec=irec) ((xlat(i,j),i=1,imax),j=1,jmax)

irec=irec+1
print '(A)','OUT longitude (degree)'
write(20,rec=irec) ((xlong(i,j),i=1,imax),j=1,jmax)

print '(A)','OUT u'
do k = 1, kmax
irec = irec + 1
write(20,rec=irec) ((u(i,j,k),i=1,imax),j=1,jmax)
end do

print '(A)','OUT v'
do k = 1, kmax
irec = irec + 1
write(20,rec=irec) ((v(i,j,k),i=1,imax),j=1,jmax)
end do

print '(A)','OUT q'
do k = 1, kmax
irec = irec + 1
write(20,rec=irec) QVAPOR(:,:,k)
end do

print '(A)','OUT HGT'
irec = irec + 1
write(20,rec=irec) ((HGT(i,j),i=1,imax),j=1,jmax)

print '(A)','OUT xland'
irec = irec + 1
write(20,rec=irec) ((xland(i,j),i=1,imax),j=1,jmax)

print '(A)','OUT cnv'
do k = 1, kmax
irec = irec + 1
write(20,rec=irec) ((cnv(i,j,k),i=1,imax),j=1,jmax)
end do

print '(A)','OUT mfcnv'
do k = 1, kmax
irec = irec + 1
write(20,rec=irec) ((mfcnv(i,j,k),i=1,imax),j=1,jmax)
end do

close(20)



jd = jd + (dt_file/daysec)

if (jd >= jd_end) exit

end do MAIN_TIME_LOOP

print *
print '(A)','DONE CNV_WRF'

end program CNV_WRF




subroutine date2jd(year,month,day,hh,mm,ss,julian_day)
!------------------------------------------------------------------
!get Julian day from Gregolian caldendar
!------------------------------------------------------------------

implicit none

integer,intent(in) :: year, month, day, hh, mm, ss
real(8),intent(out) :: julian_day

real(8), parameter :: jd0  = 1720996.5d0 ! BC4713/ 1/ 1 12:00:00
real(8), parameter :: mjd0 = 2400000.5d0 !   1858/11/17 00:00:00

integer :: y, m
if (month < 3) then
  y = year - 1
  m = month + 12
else
  y = year
  m = month
end if

julian_day = 365*(y) + int(y)/4 - int(y)/100 + int(y)/400  &
           + int((m+1)*30.6001d0) &
           + day           &
           + hh/24.0d0     &
           + mm/1440.0d0   &
           + ss/86400.0d0  &
           + jd0

! convert julian day to modified julian day
julian_day = julian_day - mjd0

end subroutine date2jd



subroutine jd2date(year,month,day,hour,min,sec,julian_day)
implicit none
!------------------------------------------------------------------
! get Gregolian caldendar from Julian day
!------------------------------------------------------------------

real(8),intent(in) :: julian_day
integer,intent(out) :: year, month, day, hour, min, sec

real(8), parameter :: mjd0 = 2400000.5d0 !  1858/11/17 00:00:00

integer :: jalpha, ia, ib, ic, id
integer :: itime
real(8) :: jday, d, xtime

! convert modified julian day to julian day
jday = julian_day + mjd0

jday = jday + 0.5d0
if (jday >= 2299161) then
  jalpha = int( (jday - 1867216.25d0)/36524.25d0 )
  d = jday + 1 + jalpha - int(0.25d0*jalpha)
else
  d = jday
end if

ia = int(d) + 1524
ib = int(6680.0d0 + ((ia-2439870) - 122.1d0)/365.25d0)
ic = 365*ib + int(0.25d0*ib)
id = int((ia-ic)/30.6001d0)
xtime = (d-int(d))*86400.d0
itime = xtime
if ( xtime-itime > 0.5d0 ) itime = itime + 1

day   = ia - ic - int(30.6001*id)

month = id - 1
if (month > 12) month = month - 12

year  = ib - 4715
if (month > 2) year = year - 1
if (year <= 0) year = year - 1

hour  = itime/3600.
min   = (itime - hour*3600)/60
sec   = itime - hour*3600 - min*60

end subroutine jd2date
```


#### HCNV3S.f90
```fortran
SUBROUTINE HCNV3S(CNVS,US,VS,MSFTS,DXS,DYS,NX,NY,NZ,FVALS)

! INPUT VARS SHOULD BE SINGLE, BUT DOUBLE PRECISION VARS ARE 
! USED INTENALLY FOR ACCURACY.
!
!============================================================
! NOTE ALL DATA MUST BE ON T-PTS (At I in the diagram below)
!
!   +--------+--------+--------+
!   |  IM1   |   I    |   IP1  |
!   |   +    |   o    |   +    |
!   |        |        |        |
!   +--------+--------+--------+
!
!============================================================

IMPLICIT NONE

INTEGER,INTENT(IN)::NX,NY,NZ

REAL,INTENT(IN)::US(NX,NY,NZ),VS(NX,NY,NZ)
REAL,INTENT(IN)::MSFTS(NX,NY)
REAL,INTENT(IN)::DXS,DYS
REAL,INTENT(IN)::FVALS

REAL,INTENT(INOUT)::CNVS(NX,NY,NZ)

INTEGER JP1,JM1,IP1,IM1,I,J,K

DOUBLE PRECISION U(NX,NY,NZ),V(NX,NY,NZ),MSFT(NX,NY),DX,DY
DOUBLE PRECISION DSY,DSX,DUDX,DVDY
DOUBLE PRECISION MM, FVAL

U=dble(US)
V=dble(VS)
MSFT=dble(MSFTS)
DX=dble(DXS)
DY=dble(DYS)
FVAL=dble(FVALS)

DO K = 1,NZ

DO J = 1,NY

JP1 = MIN(J+1,NY)
JM1 = MAX(J-1,1)

DO I = 1,NX

IP1 = MIN(I+1,NX)
IM1 = MAX(I-1,1)

DSX = dble(IP1-IM1)*DX
DSY = dble(JP1-JM1)*DY
! Careful with map factors...

MM = MSFT(I,J)*MSFT(I,J)

DUDX = ( U(IP1,J,K)/MSFT(IP1,J) - U(IM1,J,K)/MSFT(IM1,J) )/DSX*MM
DVDY = ( V(I,JP1,K)/MSFT(I,JP1) - V(I,JM1,K)/MSFT(I,JM1) )/DSY*MM

CNVS(I,J,K) = -sngl(DUDX + DVDY)

IF(U(I,J,K).GE.FVAL   .OR. V(I,J,K).GE.FVAL .OR. &
   U(IP1,J,K).GE.FVAL .OR. U(IM1,J,K).GE.FVAL .OR. &
   V(I,JP1,K).GE.FVAL .OR. V(I,JM1,K).GE.FVAL)THEN
   CNVS(I,J,K)=FVALS
END IF


END DO !I
END DO !J
END DO !K

RETURN
END

!
! C:\Users\atmos\Dropbox\MARKDOWN\WRF\CNV
!
! http://forum.wrfforum.com/viewtopic.php?f=32&t=2267
! Re: NCL: need help to alculate Relative Vorticity & Divergence
! 
! Postby tomk Fri Jun 17, 2011 10:48 am
! wrf.div.f
!
! RATATION (FOR REFERENCE)
! DVDX = (V(IP1,J,K)/MSFT(IP1,J) - V(IM1,J,K)/MSFT(IM1,J))/DSX*MM
! DUDY = (U(I,JP1,K)/MSFT(I,JP1) - U(I,JM1,K)/MSFT(I,JM1))/DSY*MM
```

  