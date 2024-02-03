/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL
CAPE3D_NCL
==============================
  
Fri, 06 Mar 2020 14:46:13 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- CS_CAPE.sh
  
## CS_CAPE.sh
```
#!/bin/bash
#
# Thu, 05 Mar 2020 15:35:53 +0900
# calypso.bosai.go.jp
# /work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL
# manda
#

LOG=$(basename $0 .sh).LOG
# LOG=$(basename $0 .sh)_$(date +"%y%m%d_%H%M").LOG

date -R  |tee    $LOG
hostname |tee -a $LOG
pwd      |tee -a $LOG
ls -lh --time-style=long-iso $(basename $0) |tee -a $LOG
echo     |tee -a $LOG

NCL=$(basename $0 .sh).ncl

runncl.sh $NCL 2>&1 |tee -a $LOG



echo |tee -a $LOG
echo "Done $(basename $0)" |tee -a $LOG
echo |tee -a $LOG
echo "LOG: $LOG" 
echo
```

  
CAPE3D_NCL
==============================
  
Fri, 06 Mar 2020 14:46:13 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- CS_CIN.sh
  
## CS_CIN.sh
```
#!/bin/bash
#
# Thu, 05 Mar 2020 15:35:53 +0900
# calypso.bosai.go.jp
# /work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL
# manda
#

LOG=$(basename $0 .sh).LOG
# LOG=$(basename $0 .sh)_$(date +"%y%m%d_%H%M").LOG

date -R  |tee    $LOG
hostname |tee -a $LOG
pwd      |tee -a $LOG
ls -lh --time-style=long-iso $(basename $0) |tee -a $LOG
echo     |tee -a $LOG

NCL=$(basename $0 .sh).ncl

runncl.sh $NCL 2>&1 |tee -a $LOG



echo |tee -a $LOG
echo "Done $(basename $0)" |tee -a $LOG
echo |tee -a $LOG
echo "LOG: $LOG" 
echo
```

  
CAPE3D_NCL
==============================
  
Fri, 06 Mar 2020 14:46:13 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- MAP_CAPE.sh
  
## MAP_CAPE.sh
```
#!/bin/bash
#
# Thu, 05 Mar 2020 15:35:53 +0900
# calypso.bosai.go.jp
# /work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL
# manda
#

LOG=$(basename $0 .sh).LOG
# LOG=$(basename $0 .sh)_$(date +"%y%m%d_%H%M").LOG

date -R  |tee    $LOG
hostname |tee -a $LOG
pwd      |tee -a $LOG
ls -lh --time-style=long-iso $(basename $0) |tee -a $LOG
echo     |tee -a $LOG

NCL=$(basename $0 .sh).ncl

runncl.sh $NCL 2>&1 |tee -a $LOG



echo |tee -a $LOG
echo "Done $(basename $0)" |tee -a $LOG
echo |tee -a $LOG
echo "LOG: $LOG" 
echo
```

  
CAPE3D_NCL
==============================
  
Fri, 06 Mar 2020 14:46:13 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- SRCDUMP.sh
  
## SRCDUMP.sh
```
#!/bin/bash

srclist=$(ls -1 *.sh)
SRCDUMP=NCL_$(basename $(pwd)).md

pwd > $SRCDUMP

for src in $srclist; do

shc=$(basename $src .sh)
ncl=$(basename $src .ncl)

srcdump.sh $shc $ncl >>$SRCDUMP

echo "  ">>$SRCDUMP

done
```

  
CAPE3D_NCL
==============================
  
Fri, 06 Mar 2020 14:46:13 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- runncl.sh
  
## runncl.sh
```
#!/bin/bash
#
# Universal wrapper script for ncl. 
# Pass arguments from the command line to environment variables
#
# version 0.1, Thierry Corti, C2SM ETH Zurich
# 

E_BADARGS=65

if [ ! -n "$1" ]
then
  echo "Usage: `basename $0` script.ncl argument1 argument2 etc."
  exit $E_BADARGS
fi  

# save number of arguments to environment variable NCL_N_ARG
export NCL_N_ARGS=$#

# save command line arguments to environment variable NCL_ARG_#
for ((index=1; index<=$#; index++))
do
  eval export NCL_ARG_$index=\$$index
done   

# run ncl
ncl -Q -n $1

```

  
