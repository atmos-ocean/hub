#!/bin/sh

#
# Create a new bash script file
#


templete()
{
  now=$(date -R)
  host=$(hostname)
  cwd=$(pwd)
  user=$(whoami)

cat << END > ./${SCRIPT}
#!/bin/bash
#
# ${now}
# ${host}
# ${cwd}
# ${user}
#

LOG=\$(basename \$0 .sh).LOG
# LOG=\$(basename \$0 .sh)_\$(date +"%y%m%d_%H%M").LOG

date -R  |tee    \$LOG
hostname |tee -a \$LOG
pwd      |tee -a \$LOG
ls -lh --time-style=long-iso \$(basename \$0) |tee -a \$LOG
echo     |tee -a \$LOG

# exe 2>&1 |tee -a \$LOG


echo |tee -a \$LOG
echo "Done \$(basename \$0)" |tee -a \$LOG
echo |tee -a \$LOG
echo "LOG: \$LOG" 
echo
END
}

#
# Main routine
#
dir="/usr/local/mybin/"

CMDNAME=$(basename $0)

if [ $# != 1 ]; then
 echo "Usage $CMDNAME [file name]" 1>&2
 exit 1
fi
SCRIPT=$1

if [ -f "./${SCRIPT}" ]; then
  echo "Error in $0 : ${SCRIPT} already exists. Nothing has done."
  exit 1
fi

templete

if [ -f "./${SCRIPT}" ]; then
  echo
  echo CREATED ${SCRIPT}.
  chmod u+x ${SCRIPT}
  ls -lh --time-style=long-iso ${SCRIPT}
  echo
else
  echo ERROR in $(basename $0) : CANNOT CREATE ${SCRIPT}.
  echo
  exit
fi

echo
echo Done shell script $(basename $0).
exit 0
