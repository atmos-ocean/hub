! C:\Users\boofo\Dropbox\TOOL\ANALYSIS\NUMERICAL_RECIPE
! Numerical Recipes in Fortran 77
! The Art of Scientific Computing 
! Second Edition 
! Volume 1 of Fortran Numerical Recipes
! p.207 gammln
! p.220 betai
! p.221 betacf

FUNCTION betai(a,b,x)
! RETURNS THE INCOMPLETE BETA FUNCTION Ix(a; b).
REAL betai,a,b,x
! USES betacf,gammln
REAL bt,betacf,gammln
if(x.lt.0..or.x.gt.1.)pause "bad argument x in betai"
if(x.eq.0..or.x.eq.1.)then
bt=0.
else ! Factors in front of the continued fraction.
bt=exp(gammln(a+b)-gammln(a)-gammln(b)+a*log(x)+b*log(1.-x))
endif
if(x.lt.(a+1.)/(a+b+2.))then 
!Use continued fraction directly.
betai=bt*betacf(a,b,x)/a
return
else
betai=1.-bt*betacf(b,a,1.-x)/b 
!Use continued fraction after making the symmetry transformation. 
return
endif
END

FUNCTION betacf(a,b,x)
INTEGER MAXIT
REAL betacf,a,b,x,EPS,FPMIN
PARAMETER (MAXIT=100,EPS=3.e-7,FPMIN=1.e-30)
! Used by betai: Evaluates continued fraction for incomplete beta function by modified
! Lorentz's method (Section 5.2).
INTEGER m,m2
REAL aa,c,d,del,h,qab,qam,qap
qab=a+b 
!These qfs will be used in factors that occur in the coefficients (6.4.6). 
qap=a+1.
qam=a-1.
c=1. First step of Lentzfs method.
d=1.-qab*x/qap
if(abs(d).lt.FPMIN)d=FPMIN
d=1./d
h=d
do 11 m=1,MAXIT
m2=2*m
aa=m*(b-m)*x/((qam+m2)*(a+m2))
d=1.+aa*d 
! One step (the even one) of the recurrence.
if(abs(d).lt.FPMIN)d=FPMIN
c=1.+aa/c
if(abs(c).lt.FPMIN)c=FPMIN
d=1./d
h=h*d*c
aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
d=1.+aa*d 
! Next step of the recurrence (the odd one).
if(abs(d).lt.FPMIN)d=FPMIN
c=1.+aa/c
if(abs(c).lt.FPMIN)c=FPMIN
d=1./d
del=d*c
h=h*del
if(abs(del-1.).lt.EPS)goto 1 !Are we done?
enddo 11
pause "a or b too big, or MAXIT too small in betacf"
1 betacf=h
return
END


FUNCTION gammln(xx)
REAL gammln,xx
! Returns the value ln[ƒ¡(xx)] for xx > 0.
INTEGER j
DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
! Internal arithmetic will be done in double precision, a nicety 
! that you can omit if five-figure accuracy is good enough.
SAVE cof,stp
DATA cof,stp/76.18009172947146d0,-86.50532032941677d0, &
 24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2, &
-.5395239384953d-5,2.5066282746310005d0/
x=xx
y=x
tmp=x+5.5d0
tmp=(x+0.5d0)*log(tmp)-tmp
ser=1.000000000190015d0
do 11 j=1,6
y=y+1.d0
ser=ser+cof(j)/y
enddo 11
gammln=tmp+log(stp*ser/x)
return
END
