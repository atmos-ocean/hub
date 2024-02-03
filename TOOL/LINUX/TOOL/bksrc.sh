#!/bin/sh

export LANG=C

#
# bksrc
# making backups of source files
#
# Copyright 2005- Atsuyoshi Manda
#

# version number
ver=1.2

function PrintHelp {
    # Print help #
    echo
    echo "$0 version $ver"
    echo
    echo "This command creates backup of the source file(s) in the current "
    echo direcoty. The current version regards the file whose extention
    echo is f or f90 or c or h as source files.
    echo
    echo "Usage: $0 [OPTION] EXT"
    echo "  EXT: Name of the file extensions"
    echo "    If not specified, default values will be used."
    echo "    Default values: $ext"
    echo "  OPTION: command options."
    echo "    -h, --help        Print this help message."
    echo
    exit 0
}
function PrintExample {
    echo
    echo "$0 version $ver"
    echo
    echo "Example"
    echo
    echo "   [aym@oceani17 temp]$ ls"
    echo "   test01.f  test02.c  test03.pl  test04.h  test05.for  test06.txt"
    echo "   [aym@oceani17 temp]$"
    echo "   [aym@oceani17 temp]$ ls"
    echo "   test01.f  test02.c  test03.pl  test04.h  test05.for  test06.txt"
    echo "   [aym@oceani17 temp]$"
    echo "   [aym@oceani17 temp]$ bksrc.sh"
    echo "   "
    echo "   ./srcbackup is creasted."
    echo "   "
    echo "   Create ./srcbackup/20071201-160429 directory."
    echo "   "
    echo "   Name of the file extensions to be copied to ./srcbackup/20071201-160429:"
    echo "      f f90 for c cpp h"
    echo "   "
    echo "   /work1/aym/MYBIN/MYSCRIPT/bksrc.sh terminated nomally."
    echo "   "
    echo "   [aym@oceani17 temp]$ ls"
    echo "   srcbackup  test01.f  test02.c  test03.pl  test04.h  test05.for  test06.txt"
    echo "   [aym@oceani17 temp]$ ls srcbackup"
    echo "   20071201-160429"
    echo "   [aym@oceani17 temp]$ ls srcbackup/20071201-160429"
    echo "   0.log20071201-160429.txt  test01.f  test02.c  test04.h  test05.for"
    echo "   [aym@oceani17 temp]$"
    echo "   [aym@oceani17 temp]$ bksrc.sh txt"
    echo "   Create ./srcbackup/20071201-160528 directory."
    echo "   "
    echo "   Name of the file extensions to be copied to ./srcbackup/20071201-160528:"
    echo "      f f90 for c cpp h txt"
    echo "   "
    echo "   /work1/aym/MYBIN/MYSCRIPT/bksrc.sh terminated nomally."
    echo "   "
    echo "   [aym@oceani17 temp]$ ls srcbackup/20071201-160528"
    echo "   0.log20071201-160528.txt  test01.f  test02.c  test04.h  test05.for  test06.txt"
    echo
    echo
    exit 0
}

# Default file extensions
ext="f f90 F F90 FOR for c cpp h F pl sh csh ncl NCL gs GS "

# Makefiles
mkfile="[Mm]ake*"

#
# Check arguments and options
#
num=0
while [ "$1" != "" ] ; do
  num=`expr $num + 1`
##  echo "Argument $num is " $1
  if [ $1 = '-h' -o $1 = '--help' ] ; then
    PrintHelp
  elif [ $1 = '--example' ]; then
    PrintExample
  elif [ ${1:1:1} != '-' ];then
    ext="$ext $1"
  fi
  shift 1
done

# directory : directory name where backup files to be copied
timestamp=`date '+%Y%m%d-%H%M'`
timestamp2=`date '+%Y%m%d-%H%M%S'`

directory="SRCBAK_${timestamp}"
directory_suppl="SRCBAK_${timestamp2}"


# src: list of source file names
src=""
for var in $ext
do
  src="$src `ls *.$var 2> /dev/null`"
done

# prefix: prefix for backup file
prefix='bak'


# Create a backup directory, the name of which is specified by `directory.'
#
if [ -f ${directory} ] ; then
  if [ ! -f ${directory_suppl} ] ; then
    echo
    echo "${directory} already exists."
    echo "This will cause the conflict of file and directory names."
    echo "${directory_suppl} has been created instead of ${directory}."
        directory=$directory_suppl
  fi
# Error handling
  echo
  echo "ERROR: "
  echo "The files whose names are ${directory} and ${directory_suppl} both exist."
  echo "$0 terminated. No action was taken."
  echo
  exit 1
fi

if [ ! -d ${directory} ] ; then
        mkdir ${directory}
        echo
        echo ${directory} is creasted.
        echo
fi

if [ -d ${directory} -a -d ${directory2} ]; then
  echo
  echo Error: ${directory} & ${directory2} already exisit.
  echo Run $0 again later.
  echo
  exit 1
fi

echo
echo "Name of the file extensions to be copied to ${dir2}:"
echo "   $ext"
echo

echo
echo "Please input a one-line note on this backup."
echo "This is IMPORTANT since you might forget WHY you make this backup."
echo
read note

LOG="${directory}/0.${timestamp}".LOG
echo > $LOG
date -R >> $LOG
hostname >> $LOG
pwd >> $LOG
whoami >> $LOG
echo >> $LOG
echo "Note:" >> $LOG
echo $note >> $LOG
echo >> $LOG
echo
#
# Checking if there is a file whose file name is the same as the backup file to be created.
#

for var in $src
do
  newfile=${dir2}/${var}
  if [ -f ${newfile} ] ; then
    echo
    echo "ERROR: "
    echo "${newfile} already exists."
    echo "$0 terminated."
    echo
    exit 1
  fi
done


#
# making backup files, then copying
#
maxsize=1000 #file size in megabytes

for var in $src
do
  size=`du -m $var  | awk '{print $1}'`
  if [ $size -lt $maxsize ]; then

    cp -av $var ${directory}'/'${var} 2>&1|tee -a $LOG
  else
    errmsg="$var not copied. Size of $var exceeds $maxsize MB."
    echo $errmsg >> $LOG
    echo $errmsg
  fi
done

# Copy makefile(s)
if [ -f ${mkfile} ];then
  cp -av ${mkfile} ${directory} 2>&1 $LOG
fi

echo
du -sch ${directory}
echo
ls -lh --time-style=long-iso ${directory}
echo
echo $0 terminated nomally.
echo
#exit with a status code (0:normal, 1:error)
exit $?


# References
# http://www.ybi.co.jp/koike/src/BSH.htm#B06
# http://www.atmarkit.co.jp/flinux/rensai/shell04/shellbasic.html
# http://www.rhythm-cafe.com/shell/Sample03.aspx
# http://www.tsden.org/takamiti/shText/
# http://www.netfort.gr.jp/~tomokuni/lms/shell/text/shell.txt

2019-11-27_14-53 
/lustre1/home1/zamanda/mybin
zamanda@hfront01
$ cat bksrc.sh
#!/bin/sh
#
# bksrc
# making backups of source files
#
# Copyright 2005- Atsuyoshi Manda
#

# version number
ver=1.2

function PrintHelp {
    # Print help #
    echo
    echo "$0 version $ver"
    echo
    echo "This command creates backup of the source file(s) in the current "
    echo direcoty. The current version regards the file whose extention
    echo is f or f90 or c or h as source files.
    echo
    echo "Usage: $0 [OPTION] EXT"
    echo "  EXT: Name of the file extensions"
    echo "    If not specified, default values will be used."
    echo "    Default values: $ext"
    echo "  OPTION: command options."
    echo "    -h, --help        Print this help message."
    echo
    exit 0
}
function PrintExample {
    echo
    echo "$0 version $ver"
    echo
    echo "Example"
    echo
    echo "   [aym@oceani17 temp]$ ls"
    echo "   test01.f  test02.c  test03.pl  test04.h  test05.for  test06.txt"
    echo "   [aym@oceani17 temp]$"
    echo "   [aym@oceani17 temp]$ ls"
    echo "   test01.f  test02.c  test03.pl  test04.h  test05.for  test06.txt"
    echo "   [aym@oceani17 temp]$"
    echo "   [aym@oceani17 temp]$ bksrc.sh"
    echo "   "
    echo "   ./srcbackup is creasted."
    echo "   "
    echo "   Create ./srcbackup/20071201-160429 directory."
    echo "   "
    echo "   Name of the file extensions to be copied to ./srcbackup/20071201-160429:"
    echo "      f f90 for c cpp h"
    echo "   "
    echo "   /work1/aym/MYBIN/MYSCRIPT/bksrc.sh terminated nomally."
    echo "   "
    echo "   [aym@oceani17 temp]$ ls"
    echo "   srcbackup  test01.f  test02.c  test03.pl  test04.h  test05.for  test06.txt"
    echo "   [aym@oceani17 temp]$ ls srcbackup"
    echo "   20071201-160429"
    echo "   [aym@oceani17 temp]$ ls srcbackup/20071201-160429"
    echo "   0.log20071201-160429.txt  test01.f  test02.c  test04.h  test05.for"
    echo "   [aym@oceani17 temp]$"
    echo "   [aym@oceani17 temp]$ bksrc.sh txt"
    echo "   Create ./srcbackup/20071201-160528 directory."
    echo "   "
    echo "   Name of the file extensions to be copied to ./srcbackup/20071201-160528:"
    echo "      f f90 for c cpp h txt"
    echo "   "
    echo "   /work1/aym/MYBIN/MYSCRIPT/bksrc.sh terminated nomally."
    echo "   "
    echo "   [aym@oceani17 temp]$ ls srcbackup/20071201-160528"
    echo "   0.log20071201-160528.txt  test01.f  test02.c  test04.h  test05.for  test06.txt"
    echo
    echo
    exit 0
}

# Default file extensions
ext="f f90 for c cpp h F pl sh ncl"

# Makefiles
mkfile="[Mm]ake*"

#
# Check arguments and options
#
num=0
while [ "$1" != "" ] ; do
  num=`expr $num + 1`
##  echo "Argument $num is " $1
  if [ $1 = '-h' -o $1 = '--help' ] ; then
    PrintHelp
  elif [ $1 = '--example' ]; then
    PrintExample
  elif [ ${1:1:1} != '-' ];then
    ext="$ext $1"
  fi
  shift 1
done

# directory : directory name where backup files to be copied
directory='./srcbackup'
directory_suppl='./srcbackup_dir'


# src: list of source file names
src=""
for var in $ext
do
  src="$src `ls *.$var 2> /dev/null`"
done

# prefix: prefix for backup file
prefix='bak'


timestamp=`date '+%Y%m%d-%H%M%S'`
#echo $timestamp

#
# Create a backup directory, the name of which is specified by `directory.'
#
if [ -f ${directory} ] ; then
  if [ ! -f ${directory_suppl} ] ; then
    echo
    echo "${directory} already exists."
    echo "This will cause the conflict of file and directory names."
    echo "${directory_suppl} has been created instead of ${directory}."
        directory=$directory_suppl
  fi
# Error handling
  echo
  echo "ERROR: "
  echo "The files whose names are ${directory} and ${directory_suppl} both exist."
  echo "$0 terminated. No action was taken."
  echo
  exit 1
fi

if [ ! -d ${directory} ] ; then
        mkdir ${directory}
        echo
        echo ${directory} is creasted.
        echo
fi

dir2=${directory}/${timestamp}
if [ -d ${dir2} ]; then
  echo
  echo Error: ${dir2} already exisits.
  echo Run $0 again later.
  echo
  exit 1
fi

echo "Create ${dir2} directory."
mkdir -p ${dir2}

echo
echo "Name of the file extensions to be copied to ${dir2}:"
echo "   $ext"
echo

echo
echo "Please input a one-line note on this backup."
echo "This is IMPORTANT since you might forget WHY you make this backup."
echo
read note

LOG="${dir2}/0.log_back${timestamp}".txt
echo > $LOG
date >> $LOG
hostname >> $LOG
pwd >> $LOG
whoami >> $LOG
echo >> $LOG
echo "Note:" >> $LOG
echo $note >> $LOG
echo >> $LOG
echo
#
# Checking if there is a file whose file name is the same as the backup file to be created.
#

for var in $src
do
  newfile=${dir2}/${var}
  if [ -f ${newfile} ] ; then
    echo
    echo "ERROR: "
    echo "${newfile} already exists."
    echo "$0 terminated."
    echo
    exit 1
  fi
done


#
# making backup files, then copying
#
maxsize=10 #file size in megabytes

for var in $src
do
  size=`du -m $var  | awk '{print $1}'`
  if [ $size -lt $maxsize ]; then
    cp -v $var ${dir2}'/'${var}  >> $LOG
  else
    errmsg="$var not copied. Size of $var exceeds $maxsize MB."
    echo $errmsg >> $LOG
    echo $errmsg
  fi
done

# Copy makefile(s)
if [ -f ${mkfile} ];then
  cp -v ${mkfile} ${dir2} >> $LOG
fi

echo $0 terminated nomally.
echo
#exit with a status code (0:normal, 1:error)
exit $?


# References
# http://www.ybi.co.jp/koike/src/BSH.htm#B06
# http://www.atmarkit.co.jp/flinux/rensai/shell04/shellbasic.html
# http://www.rhythm-cafe.com/shell/Sample03.aspx
# http://www.tsden.org/takamiti/shText/
# http://www.netfort.gr.jp/~tomokuni/lms/shell/text/shell.txt
