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
; 
; ${SCRIPT}
; 
; ${now}
; ${host}
; ${cwd}
; ${user}
;
script_name  = get_script_name()
print("script="+script_name)
script=systemfunc("basename "+script_name+ " .ncl")

;LOG=script+".LOG"

;NOW=systemfunc("date '+%y%m%d_%H%M' ")
;print("NOW="+NOW)
; LOG=script+NOW+".LOG"
HOST=systemfunc("hostname")
CWD=systemfunc("pwd")

arg    = getenv("NCL_ARG_2")




;FINFO=systemfunc("ls -lh --time-style=long-iso "+script_name)
;LOG_HEADER = (/"# "+ NOW, "# "+ HOST, "# "+ CWD, "# "+ FINFO /)
;hlist=[/LOG_HEADER/]
;write_table(LOG, "w", hlist, "%s")



print("")
print("Done " + script_name)
print("")
;print("LOG: "+LOG)
;print("")
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
