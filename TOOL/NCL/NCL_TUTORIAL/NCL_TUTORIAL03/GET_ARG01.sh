#!/bin/bash

NCL=GET_ARG.ncl #使用するNCLスクリプト名

if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCLスクリプトが存在しない場合，エラーメッセージを表示して終了

which runncl.sh &> /dev/null
if [ $? -ne 0 ]; then echo NO SUCH FILE,runncl.sh;exit 1;fi
# もしrunncl.shというファイルが見つからなかったらエラー

DSET=ERA5; VAR=DLR

runncl.sh $NCL $DSET $VAR
# 引数DSETとVARを渡してNCLスクリプトを実行
