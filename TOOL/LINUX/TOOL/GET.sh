#!/bin/bash

if [ $# -lt 1 ]; then
echo
echo "ERROR in $(basename $0): NO ARGUMENT"
echo "USAGE : $(basename $0) DIRNAME"
echo
exit 1
fi

org=zamanda@hfront01.bosai.go.jp:$1

echo
echo "GETTING $org ..."
echo

expect -c "
set timeout 10
spawn scp -r ${org} .
        expect \"zamanda@hfront01.bosai.go.jp's password:\"
        send \"Qms+7Ffcu4\n\"
interact
"

echo
echo "DONE."
echo 
echo "ORG  : ${org}"
echo "COPY :"
echo "$(ls -lh $(basename $org))"
echo
exit 0
