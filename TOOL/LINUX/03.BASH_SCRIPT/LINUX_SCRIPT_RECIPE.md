# LINUX_SCRIPT_RECIPE

[[_TOC_]]

## ひな型作成

### new.sh

```bash
#!/bin/sh
#
# Create a new bash script file
#

templete()
{
NOW=$(date -R); HOST=$(hostname); CWD=$(pwd); USER=$(whoami)
THISFILE=$0

cat << END > ./$NEW_SCRIPT
#!/bin/bash
#
# ${NOW}
# ${CWD}
# ${NEW_SCRIPT}

#EXE=
#\${EXE}

echo "DONE \$(basename \$0)."
echo
#echo "INPUT : \$IN"
#echo "OUTPUT: \$OUT"
echo
exit 0



# THE FOLLOWING LINES ARE SAMPLES:
<<COMMENT


# INPUT FILE
IN=""
if [ ! -f \$in ];then echo NO SUCH FILE, \$IN; exit 1
fi

# WHILE LOOP
IS=1; IE=3
I=\$IS
while [ \$I -le \$IE ]; do

  I=\$(expr \$I + 1)
done

# FOR LOOP 
ALIST="a b c"
for A in \$ALIST; do

done

# IF BLOCK
i=3
if [ \$i -le 5 ]; then

else if  [ \$i -le 2 ]; then
COMMENT
END

if [ $? -ne 0 ];then
  echo Create a new file, ${NEW_SCRIPT}.
fi
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
NEW_SCRIPT=$1

if [ -f "./${NEW_SCRIPT}" ]; then
  echo "Error in $0 : ${NEW_SCRIPT} already exists. Nothing has done."
  exit 1
fi

templete

echo
echo DONE $(basename $0).
exit 0
```

#### 使用例

```bash
$ new.sh A.sh

DONE new.sh.
```

#### 出来たファイルの内容確認

```bash
$ head A.sh
#!/bin/bash
#
# Wed, 10 Jan 2024 12:13:45 +0900
# /work09/am/00.WORK/2023.HEAT_FLUX_TREND/32.JOFURO3_DECOMP_FLUX/22.14.DECOMP_TAV_MON
# A.sh

#EXE=
#${EXE}

echo "DONE $(basename $0)."


$ tail A.sh
if [ $value_t = "foo" ]; then
  type="foo"
elif [ $value_t = "boo" ]; then
  type="boo"
else
  type=""
fi
shift $(expr $OPTIND - 1)

COMMENT
```



## ファイル内容の書き出し

### myls

lsの最初の5行と最後の5行を書き出す

```bash
#/bin/bash
ls | head -5; echo; echo ...; echo; ls| tail -5
```



### mycat

開始行と終了行を指定してファイルの内容を書き出す

```bash
#/bin/bash

if [ $# -ne 3 ];then
echo ERROR: WRONG ARGUMENTS.
echo "USAGE: $(basename $0) INPUT LINE1 LINE2"
echo "   LINE1: NUMBER OF THE FIRST LINE."
echo "   LINE2: NUMBER OF THE LAST LINE."
echo; exit 1
fi

IN=$1  # Input file
S=$2   # The first line
E=$3   # The last line

head -n $E $IN | tail -n $(($E - $S + 1))
```

