#!/bin/sh

#
# Create a new bash script file
#


templete()
{
  now=$(date +"%H:%M on %m-%d-%Y")
  host=$(hostname)
  cwd=$(pwd)
  user=$(whoami)
  thisfile=$0
  cat << END > ./$namae.sh
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


echo "Shell script, \$(basename \$0) starts."
echo

# exe=
# \${exe}

echo "Done \$0"
echo





#
# The following lines are samples:
#

# Sample for handling options

# CMDNAME=$(basename \$0)
# Ref. https://sites.google.com/site/infoaofd/Home/computer/unix-linux/script/tips-on-bash-script/hikisuuwoshorisuru
#while getopts t: OPT; do
#  case $OPT in
#    "t" ) flagt="true" ; value_t="$OPTARG" ;;
#     * ) echo "Usage $CMDNAME [-t VALUE] [file name]" 1>&2
#  esac
#done
#
#value_t=${value_t:-"NOT_AVAILABLE"}
#
#if [ $value_t = "foo" ]; then
#  type="foo"
#elif [ $value_t = "boo" ]; then
#  type="boo"
#else
#  type=""
#fi
#shift $(expr $OPTIND - 1)




# Sample for checking arguments

#if [ \$# -lt 1 ]; then
#  echo Error in \$0 : No arugment
#  echo Usage: \$0 arg
#  exit 1
#fi

#if [ \$# -lt 2 ]; then
#  echo Error in \$0 : Wrong number of arugments
#  echo Usage: \$0 arg1 arg2
#  exit 1
#fi



# Sample for checking input file

#in=""
#if [ ! -f \$in ]; then
#  echo Error in \$0 : No such file, \$in
#  exit 1
#fi



# Sample for creating directory

#dir=""
#if [ ! -d \$dir ]; then
#  mkdir -p \${dir}
#  echo Directory, \${dir} created.
#fi

#out=\$(basename \$in .asc).ps

#echo Input : \$in
#echo Output : \$out



# Sample of do-while loop

#i=1
#n=3
#while [ \$i -le \$n ]; do
#  i=\$(expr \$i + 1)
#done



# Sample of for loop

# list_item="a b c"
#for item in list_item
#do
#  echo \$item
#done



# Sample of if block

#i=3
#if [ \$i -le 5 ]; then
#
#else if  [ \$i -le 2 ]; then
#
#else
#
#fi
END
#if [ $? -ne 0 ];then
  echo Create a new file, ${namae}.sh.
#fi
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
namae=$1

if [ -f "./${namae}.sh" ]; then
  echo "Error in $0 : ${namae}.sh already exists. Nothing has done."
  exit 1
fi

templete

echo
echo Done shell script $(basename $0).
exit 0
