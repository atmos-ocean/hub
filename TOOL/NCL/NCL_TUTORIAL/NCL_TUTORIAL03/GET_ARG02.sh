#!/bin/bash

# 引数を変更しながら繰り返しNCLスクリプトを実行する

NCL=GET_ARG.ncl #NCLスクリプト名

if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCLスクリプトが存在しない場合，エラーメッセージを表示して終了

which runncl.sh &> /dev/null
if [ $? -ne 0 ]; then echo NO SUCH FILE,runncl.sh;exit 1;fi
# もしrunncl.shというファイルが見つからなかったらエラー

DSET=ERA5
VARLIST="DLR DSR NLR"

for VAR in $VARLIST; do
runncl.sh $NCL $DSET $VAR
done


