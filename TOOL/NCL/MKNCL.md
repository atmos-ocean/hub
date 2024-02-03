
List of the following files:
----------------------------
mkncl.sh

Machine info
----------------------------
calypso.bosai.go.jp
/work05/manda/mybin
Sun, 05 Jan 2020 09:42:19 +0900

## mkncl.sh
```bash
#!/bin/sh

#
# Create a new NCL script file
#


templete()
{
  now=$(date -R)
  host=$(hostname)
  cwd=$(pwd)
  user=$(whoami)

cat << END > ./${SCRIPT}


; 
script_name  = get_script_name()
;ARG    = getenv("NCL_ARG_2")

INDIR=""
INFLE=""
IN=INDIR+"/"+INFLE
;ODIR=""
;OFLE=""
;OUT=ODIR+"/"+OFLE
;FIG=systemfunc("basename "+INFLE+" .nc")
;TYP="pdf"

a=addfile(IN,"r")


;printVarSummary()


s
NOW=systemfunc("date -R")
HOST=systemfunc("hostname")
CWD=systemfunc("pwd")

print("")
;print("ODIR: "+ODIR)
;print("OFLE: "+OFLE)
;print("FIG : "+FIG+"."+TYP)
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
```

