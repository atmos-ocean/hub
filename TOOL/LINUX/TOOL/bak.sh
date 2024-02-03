#!/bin/bash

if [ $# -lt 1 ];then
echo
echo ERROR in $0 : Wrong argument.
echo
echo Usage : $0 ORG_FILE1  ORG_FILE2 ...
echo
fi

export LANG=C

arg_list=$*

for i in $*
do
  src=$1

  if [ -e $1 ]; then

    FILE_NAME=${1}

    FILE=${FILE_NAME%.*}

    EXTENSION=${FILE_NAME##*.}

    DATE=$(date +"%y%m%d-%H%M")

    if [ "${FILE}" == "${EXTENSION}" ];then
      NEW_FILE_NAME=${FILE}_${DATE}
      EXTENSION=""
    else
      NEW_FILE_NAME=BAK_${FILE}_${DATE}.${EXTENSION}
    fi

    cp -arv ${FILE_NAME} ${NEW_FILE_NAME}

    echo "oooooooooooooooooooo ORG oooooooooooooooooooooo" 
    ls -lh ${FILE_NAME}
    echo "ooooooooooooooooooooooooooooooooooooooooooooooo"
    echo "bbbbbbbbbbbbbbbbbbbb BAK bbbbbbbbbbbbbbbbbbbbbb" 
    ls -lh ${NEW_FILE_NAME}
    echo "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    echo
  fi

  shift
done

# https://cloudpack.media/19739
