/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL/CHECK_FNL
CHECK_FNL
==============================
  
Fri, 06 Mar 2020 14:45:40 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL/CHECK_FNL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- CHECK_CAPE.sh
  
## CHECK_CAPE.sh
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

  
CHECK_FNL
==============================
  
Fri, 06 Mar 2020 14:45:40 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL/CHECK_FNL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- CHECK_CIN.sh
  
## CHECK_CIN.sh
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

  
CHECK_FNL
==============================
  
Fri, 06 Mar 2020 14:45:40 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL/CHECK_FNL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- CHECK_PSFC.sh
  
## CHECK_PSFC.sh
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

  
CHECK_FNL
==============================
  
Fri, 06 Mar 2020 14:45:40 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL/CHECK_FNL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- CHECK_Q.sh
  
## CHECK_Q.sh
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

  
CHECK_FNL
==============================
  
Fri, 06 Mar 2020 14:45:40 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL/CHECK_FNL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- CHECK_RH.sh
  
## CHECK_RH.sh
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

  
CHECK_FNL
==============================
  
Fri, 06 Mar 2020 14:45:40 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL/CHECK_FNL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- CHECK_T.sh
  
## CHECK_T.sh
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

  
CHECK_FNL
==============================
  
Fri, 06 Mar 2020 14:45:40 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL/CHECK_FNL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- CHECK_Z.sh
  
## CHECK_Z.sh
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

  
CHECK_FNL
==============================
  
Fri, 06 Mar 2020 14:45:40 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL/CHECK_FNL
  
**Machine info**
processor	: 15
model name	: Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB
  
**List of the following files:**
- CHECK_ZSFC.sh
  
## CHECK_ZSFC.sh
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

  
CHECK_FNL
==============================
  
Fri, 06 Mar 2020 14:45:40 +0900
calypso.bosai.go.jp
/work05/manda/K17_GOUU/OVERVIEW/FNL/CAPE3D_NCL/CHECK_FNL
  
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

  
