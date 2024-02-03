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


f90=ifort
DOPT=" -fpp -CB -traceback -fpe0 "
 OPT=" -fpp -convert big_endian -assume byterecl"



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

f90=${f90}
OPT="${OPT}"
DOPT="${DOPT}"

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
\${f90} \${DOPT} \${OPT} \${src} -o \${exe}
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
exit 1
fi
echo
echo "Done \${exe}"
echo

END

chmod u+x $1


SRC=$(basename $1 .sh).F90
echo $SRC
cat <<EOF >$SRC
PROGRAM 
! $(date -R)
! $(hostname)
! $(pwd)
!

END PROGRAM
EOF



echo
ls -lh --time-style=long-iso $1
ls -lh --time-style=long-iso $SRC
echo
