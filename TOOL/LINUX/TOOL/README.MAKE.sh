#!/bin/bash

function recursive {
# ls ?ccB?a?at@C?c $1 ?w??ds?B
# fBNg?c recursive d?AI??ÑëoÅEB
# http://www.shido.info/misc/misc.php?id=29
    for FILE in `ls ${2}`
        do
           local FULL=${2}/${FILE}
           if [ -f $FULL ]; then
             $1 $FULL
           elif [ -d $FULL ]; then
             rec $1 $FULL
           fi
        done
}



function usage {
cat <<EOF
Usage: $(basename $0) [-t][-l][-s][-h]

 -t : testing (just printing what will be done)
 -l : print directory listings
 -s : print source files
 -h : print help
EOF
}



readme=0.README.txt
bakdir=BAK_README
testing=

encoding=ja_JP.SJIS #"ja_JP.eucJP" #ja_JP.UTF-8
export LANG=$encoding

FLG_T="FALSE"
FLG_L="FALSE"
FLG_S="FALSE"
FLG_H="FALSE"

while getopts tlsh OPT
do
  case $OPT in
    "t" ) FLG_T="TRUE" ;;
    "l" ) FLG_L="TRUE" ;;
    "s" ) FLG_S="TRUE" ;;
    "h" ) FLG_H="TRUE" ;;
  esac
done
shift $(expr $OPTIND - 1)

if [ $FLG_T = "TRUE" ]; then
  testing=echo
fi

if [ $FLG_H = "TRUE" ]; then
  usage
fi

if [ -f ./$readme ]; then
  if [ ! -d $bakdir ]; then
    echo
    echo Creating backup directory, ${bakdir}.
    mkdir -p $bakdir
    echo
  fi
  echo
  echo Create backup of ${readme}.
  bak=${bakdir}/$readme.bak.$(date "+%Y-%m-%d_%H%M")
  cp -vf $readme $bak
  echo
fi

echo Creating $readme file.

now=$(date -R)
machine=$(hostname)
cwd=$(pwd)
me=$(whoami)

cat << EOF > $readme
============================================

   $readme file

============================================
EOF
echo >> $readme
echo >> $readme
echo >> $readme

cat << EOF >> $readme
Directory: $cwd
Hostname: $machine
Date and Time: $now
User Name: $me

Character encoding: $encoding



EOF

cat << EOF >> $readme
*******************************************

   Amount of Data

*******************************************
EOF
du -sch $(pwd) >>$readme
echo >>$readme
du -sch $(pwd)/* >>$readme
echo >>$readme
echo >>$readme
echo >>$readme

cat << EOF >> $readme
*******************************************

   Directory Tree

*******************************************
EOF
${testing} tree -d $(pwd) -N >> $readme
echo >>$readme
echo >>$readme
echo >>$readme

cat << EOF >> $readme
*******************************************

  Usage

*******************************************

DESCRIBE HOW TO USE YOUR PROGRAMS (SCRIPTS).


Program (script) Name:

Purpose:

How to compile (if needed):

How to  Run:


Figures (if available):


EOF


cat << EOF >> $readme
*******************************************

   Notes

*******************************************

EOF

if [ $FLG_L = "TRUE" ]; then
${testing} ls -R -B -g -F $(pwd) --time-style=long-iso >>$readme
#${testing} ls -R -B -g -1 $(pwd) > tmp
#if [ $(wc -l tmp|awk '{print $1}') -gt 50 ]; then
#  head -25 tmp >>$readme
#  echo ..... >> $readme
#  tail -25 tmp >> readme
#else
#  cat tmp >>$readme
#fi
echo >>$readme
echo >>$readme
echo >>$readme
fi


if [ $FLG_S = "TRUE" ]; then
  echo
  echo Print program lists.
  echo

cat << EOF >> $readme
*******************************************

  Program Lists

*******************************************
EOF
${testing} recursive print_source.sh $(pwd) >> $readme
fi # FLG_S


cat << EOF >> $readme
============================================

   END of $readme file

============================================
EOF
