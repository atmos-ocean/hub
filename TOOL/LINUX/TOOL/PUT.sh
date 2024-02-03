#!/bin/bash

if [ $# -lt 1 ]; then
echo
echo ERROR in $0 : NO ARGUMENT
echo USAGE : $0 ORG DIR 
echo "DIR : DESTINATION DIR WITHOUT /data07/thotspot/zamanda" 
echo
exit 1
fi

dest="."

org=$1
dir=$2

if [ ! -f $org -a ! -d $org ]; then
echo
echo ERROR in $0 : NO SUCH FILE, $org
echo
exit 1
fi

root="zamanda@hfront01.bosai.go.jp:/data07/thotspot/zamanda"
dest=${root}/${dir}

echo
echo "SENDING $org ..."
echo

expect -c "
set timeout 10
spawn scp -r ${org} ${dest} 
        expect \"zamanda@hfront01.bosai.go.jp's password:\"
        send \"Qms+7Ffcu4\n\"
interact
"

#expect -c "
#set timeout 10
#spawn scp -r ${org} ${dest} 
#expect \"Are you sure you want to continue connecting (yes/no)?\" {
#        send \"yes\n\"
#        expect \"zamanda@hfront01.bosai.go.jp's password:\"
#        send \"Qms+7Ffcu4\n\"
#}
#interact
#"

echo
echo "DONE."
echo 
echo "ORG  : ${org}"
echo "DEST : ${dest}"
echo
exit 0
