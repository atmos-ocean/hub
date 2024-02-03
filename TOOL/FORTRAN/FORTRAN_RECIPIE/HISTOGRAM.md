# ガウス分布したノイズを作成し、そのヒストグラムを描く

2019-08-08_15-36 
/work05/manda/WRF.POST/K17/HISTOGRAM_W/TEST
manda@calypso
$ HISTO_TEST.sh

Created HISTO_TEST.nml.

-rw-rw-r-- 1 manda manda 38 2019-08-08 15:39 HISTO_TEST.nml


SOURCE FILES:

-rw-rw-r-- 1 manda manda 1.8K 2019-08-08 13:51 HISTO_TEST.f90
-rw-rw-r-- 1 manda manda 2.3K 2019-08-08 13:24 gasdev.f90
-rw-rw-r-- 1 manda manda 3.4K 2019-08-08 13:17 ran1.f90

Compiling ...

ifort -CB -traceback -fpe0 gasdev.f90 ran1.f90 HISTO_TEST.f90 -o HISTO_TEST.exe

Done Compile.

-rwxrwxr-x 1 manda manda 911K  8月  8 15:39 2019 HISTO_TEST.exe


HISTO_TEST.exe is running ...

 AVE., VAR., SD. =   1.8030121E-03   1.024235       1.012045    
nb=   50

2019-08-08_15-39 
/work05/manda/WRF.POST/K17/HISTOGRAM_W/TEST
manda@calypso
$ lgf.sh gnoise.txt
INPUT:
gnoise.txt
OUTPUT:
gnoise.ps

2019-08-08_15-40 
/work05/manda/WRF.POST/K17/HISTOGRAM_W/TEST
manda@calypso
$ HISTO_gnoise.PL.sh 
Bash script ./HISTO_gnoise.PL.sh starts.

INPUT : 
-rw-rw-r-- 1 manda manda 1.7K 2019-08-08 15:39 HISTO_gnoise.txt
OUTPUT : 
-rw-rw-r-- 1 manda manda 27K 2019-08-08 15:41 HISTO_gnoise.ps

Done ./HISTO_gnoise.PL.sh

## HISTO_TEST.sh
```
#!/bin/bash
#
# Description:
#
src=$(basename $0 .sh).f90
srclist="gasdev.f90 ran1.f90 ${src}"
exe=$(basename $0 .sh).exe
nml=$(basename $0 .sh).nml

f90=ifort
opt="-CB -traceback -fpe0 " # -convert big_endian -assume byterecl"


cat<<EOF>$nml
&para
xmin=-5
xmax=5
binsize=0.2
&end
EOF

echo
echo Created ${nml}.
echo
ls -lh --time-style=long-iso ${nml}
echo

echo
echo SOURCE FILES:
echo
ls -lh --time-style=long-iso ${srclist}
echo

echo Compiling ...
echo
echo ${f90} ${opt} ${srclist} -o ${exe}
echo
${f90} ${opt} ${srclist} -o ${exe}
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

```

End of HISTO_TEST.sh
----------------------



----------------------
## HISTO_TEST.f90
```fortran
!----------------- PARAMETERS AND COMMON VARIABLES ---------------------
parameter(MX=10000,N=10000) 
!-----------------------------------------------------------------------
!------------------------- LOCAL VARIABLES -----------------------------
double precision gasdev
integer idum
real xmin, xmax, binsize
real,allocatable::xbl(:),xbr(:)
real,allocatable::freq(:), freqper(:)
real a(MX)

namelist /para/xmin,xmax,binsize
!-----------------------------------------------------------------------
read(5,nml=para)

idum=-123
open(7,file='gnoise.txt',status='unknown')
      
do i=1,N
a(i)=sngl(gasdev(idum))
write(7,'(i5,1x,f10.5)')i,a(i) 
end do 

close(7) 



! AVE                                                                        
av=0.0
RN=1.0/float(N)

do i=1,N
av=av+a(i)*RN
enddo

!VARIANCE                                                                        
vr=0.0 
do i=1,N 
vr=vr+(a(i)-av)**2 
end do 
                                                                        
vr=vr/(float(N)-1.0) 
sd=sqrt(vr)
write(*,*)'AVE., VAR., SD. = ',av,vr,sd



! CREATE HISTOGRAM
nb=int((xmax-xmin)/binsize+0.5)
print '(A,i5)','nb=',nb
allocate(xbl(nb),xbr(nb),freq(nb),freqper(nb))

freq=0
do i=1,N
idx=(a(i)-xmin)/binsize

if(idx >= 1 .and. idx <=nb)then
freq(idx)=freq(idx)+1
else
print '(A)','ERROR'
print '(A)','I=',i,' a(i)=',a(i)
stop
endif

end do !N

freqper=0
do j=1,nb
freqper(j)=freq(j)/float(N)*100.
end do !j

do i=1,nb
xbl(i)=xmin+binsize*(i-1)
xbr(i)=xbl(i)+binsize
print '(i5,2f7.3,f7.0,f7.1)',i,xbl(i),xbr(i),freq(i),freqper(i)
enddo


open(21,file='HISTO_gnoise.txt')
do j=1,nb
write(21,'(i5,2f7.3,f7.0,f7.1)')&
,i,xbl(j),xbr(j),freq(j),freqper(j)
enddo !j

close(21)
stop 
END                                           
```

End of HISTO_TEST.f90
----------------------



----------------------
## gasdev.f90
```fortran
!***********************************************************************
! gasdev.f                                                              
! TASK                                                                  
!j NORMAL DISTRIBUTION WITH 0 AVE AND 1 SD
! USAGE                                                                 
! REMARK                                                                
! SLAVE ROUTINE                                                         
!  ran1                                                                 
! REFERENCES                                                            
!  Press, W.H., S.A.Teukolsky, W.T.Vetterling and B.P.Flannery (1986):  
!     Numerical Recipes in Fortran 77 The Art of Scientific Computing   
!     2nd ed. Cambridge University Press, Cambridge, U.K., 933pp.       
!***********************************************************************
      double precision FUNCTION gasdev(idum) 
!------------------------- PARAMETERS ----------------------------------
      implicit double precision(a-h,o-z) 
      parameter(c0=0.0, c1=1.0, c2=2.0) 
!-----------------------------------------------------------------------
!--------------------------- ARGUMENTS ---------------------------------
!-----------------------------------------------------------------------
!------------------------ COMMON VARIABLES -----------------------------
!-----------------------------------------------------------------------
!------------------------- LOCAL VARIABLES -----------------------------
!-----------------------------------------------------------------------
      SAVE iset, gset 
      DATA iset/0/ 
                                                                        
                           ! Reinitialize                               
      if(idum.lt.0) iset=0 
      if(iset.eq.0)then 
    1   v1=c2*ran1(idum)-c1 
        v2=c2*ran1(idum)-c1 
        rsq = v1**2 + v2**2 
        if(rsq.ge.c1.or.rsq.eq.c0) goto 1 
        fac=sqrt(-c2*log(rsq)/rsq) 
        gset = v1*fac 
        gasdev = v2*fac 
        iset=1 
      else 
        gasdev=gset 
        iset = 0 
      end if 
                                                                        
      return 
      END                                           
```

End of gasdev.f90
----------------------



----------------------
## ran1.f90
```fortran
!***********************************************************************
! ran1.f                                                                
! TASK                                                                  
!     "Minimal" random number generator of Park and Miller with Bays-   
!  Durham shuffle and added safeguards. Returns a uniform random dviate 
!  between 0.0 and 1.0 (exclusive of the endpoint values).              
!                                                                       
! USAGE                                                                 
!     Call with ***idum a negative integer*** to initialize; therafter, 
!  do NOT alter idum between succesive deviate in a sequence. RNMX      
!  should approximate the largest floating value that is less than 1.   
!                                                                       
! NOTE                                                                  
!     The routine, ran1, uses the Minimal Standard for its random value,
!  but it shuffles the output to remove low-order serial correlations.  
!  A random deviate derived from the j-th value in the sequence, I_j, is
!  output no on the j-th call, but rather on a randomized later call,   
!  j+32 on average. The shuffling algorithm is due to Bays and Durham as
!  described in Knuth (1981).                                           
!    The routine, ran1 passes those statistical test ran0 is known to   
!  fail. In fact, we do not know of any statistical test that ran1      
!  fails to pass, except when the number of calls starts to become on   
!  the order of the period m, say > 10^8  m/20.                       
!***********************************************************************
      double precision FUNCTION ran1(idum) 
!------------------------- PARAMETERS ----------------------------------
      implicit double precision(a-h,o-z) 
      PARAMETER(IA=16807, IM=2147483647, AM=1.d0/2147483647.d0,IQ=127773&
     &   ,IR=2836, NTAB=32, NDIV=1+(IM-1)/NTAB, EPS=1.2d-7              &
     &   ,RNMX=1.d0-EPS)                                                
!-----------------------------------------------------------------------
!--------------------------- ARGUMENTS ---------------------------------
!-----------------------------------------------------------------------
!------------------------ COMMON VARIABLES -----------------------------
!-----------------------------------------------------------------------
!------------------------- LOCAL VARIABLES -----------------------------
      dimension iv(NTAB) 
!-----------------------------------------------------------------------
      save iv, iy 
      data iv /NTAB*0/, iy/0/ 
                                                                        
      if(idum.le.0.or.iy.eq.0)then 
        idum=max(-idum,1) 
        do j=NTAB+8,1,-1 
          k=idum/IQ 
          idum=IA*(idum-k*IQ)-IR*k 
          if(idum.lt.0)idum=idum+IM 
          if(j.le.NTAB)iv(j)=idum 
        end do 
        iy=iv(1) 
      end if 
                                                                        
      k=idum/IQ 
      idum=IA*(idum-k*IQ)-IR*k 
      if(idum.lt.0)idum=idum+IM 
      j=1+iy/NDIV 
      iy=iv(j) 
      iv(j)=idum 
      ran1=min(AM*iy,RNMX) 
                                                                        
      return 
      END                                           
```

End of ran1.f90
----------------------

