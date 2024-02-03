#!/bin/bash

this_script=$(basename $0)
if [ $# -ne 1 ]; then
  echo ERROR in ${this_script} : Wrong argument.
  echo "Usage: $this_script <filaneme>"
  exit 1
fi

if [ -f $1 ]; then
  echo ERROR in ${this_script}: File, $1 exists.
  exit 1
fi


cat <<END >$1
#!/bin/bash
#
# $(date -R)
# $(hostname)
# $(pwd)
#
src=\$(basename \$0 .sh).F90
exe=\$(basename \$0 .sh).exe
nml=\$(basename \$0 .sh).nml

f90=ifort
DOPT=" -fpp -CB -traceback -fpe0 -check all"
OPT=" -fpp -convert big_endian -assume byterecl"

#f90=gfortran
#DOPT=" -ffpe-trap=invalid,zero,overflow,underflow -fcheck=array-temps,bounds,do,mem,pointer,recursion"
#OPT=" -L. -lncio_test -O2 "

# OpenMP
#OPT2=" -fopenmp "

cat<<EOF>\$nml
&para
&end
EOF

echo
echo Created \${nml}.
echo
ls -lh --time-style=long-iso \${nml}
echo


echo
echo \${src}.
echo
ls -lh --time-style=long-iso \${src}
echo

echo Compiling \${src} ...
echo
echo \${f90} \${DOPT} \${OPT} \${src} -o \${exe}
echo
\${f90} \${DOPT} \${OPT} \${OPT2} \${src} -o \${exe}
if [ \$? -ne 0 ]; then

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
ls -lh \${exe}
echo

echo
echo \${exe} is running ...
echo
D1=\$(date -R)
\${exe}
# \${exe} < \${nml}
if [ \$? -ne 0 ]; then
echo
echo "=============================================="
echo
echo "   ERROR in \$exe: RUNTIME ERROR!!!"
echo
echo "=============================================="
echo
echo TERMINATED.
echo
D2=\$(date -R)
echo "START: \$D1"
echo "END:   \$D2"
exit 1
fi
echo
echo "Done \${exe}"
echo
D2=\$(date -R)
echo "START: \$D1"
echo "END:   \$D2"
END

chmod u+x $1


SRC=$(basename $1 .sh).F90
echo $SRC
cat <<EOF >$SRC
PROGRAM 
! $(date -R)
! $(hostname)
! $(pwd)

!IMPLICIT NONE
!
!CHARACTER INDIR*500,INFLE*500,IN*1000,ODIR*500,OFLE*500,&
!OUT*1000

!INTEGER
!INTEGER,ALLOCATABLE,DIMENSION(:)::
!REAL
!REAL,ALLOCATABLE,DIMENSION(:)::

!REAL,PARAMETER::UNDEF=

!NAMELIST /PARA/
!READ(*,NML=PARA)

!ALLOCATE()

PRINT *

IN=TRIM(INDIR)//'/'//TRIM(INFLE)

!OPEN(11,FILE=IN,ACTION='READ')
!CLOSE(11)

!OPEN(21,FILE=OUT)
!OPEN(21,FILE=OUT,FORM='UNFORMATTED',ACCESS='DIRECT',&
!RECL=4*NX*NY)

!CLOSE(21)

END PROGRAM
EOF



echo
ls -lh --time-style=long-iso $1
ls -lh --time-style=long-iso $SRC
echo
