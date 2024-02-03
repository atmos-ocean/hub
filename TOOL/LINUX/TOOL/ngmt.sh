#!/bin/sh

#
# Create a new bash script file for Generic Mapping Tools
#


templete()
{
  now=$(date -R)
  host=$(hostname)
  cwd=$(pwd)
  user=$(whoami)
  thisfile=$0
  cat << END > ./$namae
#!/bin/bash
# Description:
#
# Author: ${user}
#
# Host: ${host}
# Directory: ${cwd}
#
# Revision history:
#  This file is created by ${thisfile} at ${now}.

. ./gmtpar.sh
echo "Bash script \$0 starts."

range=
size=
xanot=
yanot=
anot=\${xanot}/\${yanot}WSne

in=""
if [ ! -f \$in ]; then
  echo Error in \$0 : No such file, \$in
  exit 1
fi
figdir="FIG_\$(basename \$0 .sh)"
if [ ! -d \${figdir} ];then
  mkdir -p \$figdir
fi
out=\${figdir}\$(basename \$in .txt).ps




xoffset=
yoffset=

export LANG=C

curdir1=\$(pwd); now=\$(date -R); host=\$(hostname)

time=\$(ls -l  --time-style=long-iso  \${in} | awk '{print \$6, \$7}')
timeo=\$(ls -l  --time-style=long-iso \${out} | awk '{print \$6, \$7}')

pstext <<EOF -JX6/1.5 -R0/1/0/1.5 -N -X\${xoffset:-0} -Y\${yoffset:-0} -O >> \$out
0 1.50  9 0 1 LM \$0 \$@
0 1.35  9 0 1 LM \${now}
0 1.20  9 0 1 LM \${curdir1}
0 1.05  9 0 1 LM INPUT: \${in} (\${time})
0 0.90  9 0 1 LM OUTPUT: \${out} (\${timeo})
EOF

echo
echo "INPUT : "
ls -lh --time-style=long-iso \$in
echo "OUTPUT : "
ls -lh --time-style=long-iso \$out
echo

echo "Done $(basename \$0)"
END

#if [ $? -ne 0 ];then
  echo Create a new file, ${namae}.
#fi
}


#
# Main routine
#
dir="/work09/am/mybin/"
CMDNAME=$(basename $0)

if [ $# != 1 ]; then
 echo "Usage $CMDNAME [file name]" 1>&2
 exit 1
fi
namae=$1

if [ -f "./${namae}.sh" ]; then
  echo "Error in $0 : ${namae}.sh already exists. Nothing has done."
  exit 1
fi

templete

filename="gmtpar.sh"
if [ -f ${filename} ]; then
     echo ${filename} already exists. Nothing has done.
else
  echo Copy ${filename}
  if [ -f ${dir}${filename} ]; then
    cp ${dir}${filename} .
    ls -lh --time-style=long-iso ./${filename}
  else
    echo Error in $0 : ${dir}${filename} does not exists.
    exit 1
  fi
fi

echo
echo Done shell script $(basename $0).
exit 0
