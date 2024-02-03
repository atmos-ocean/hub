#!/bin/bash

while getopts h: OPT
do
  case $OPT in
    "h" ) FLG_H="TRUE" ; line="$OPTARG" ;;
  esac
done
shift $(expr $OPTIND - 1)



WRFOUT=$1
if [ ! -f $WRFOUT ]; then
echo
echo ERROR in $0 : NO SUCH FILE, $WRFOUT
echo
exit 1
fi

ncdump -h $WRFOUT |head -${line}

exit 0
