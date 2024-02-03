#!/bin/bash

# namelistファイルから情報を読むこむ

NCL=READ_NML.ncl #NCLスクリプト名

if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCLスクリプトが存在しない場合，エラーメッセージを表示して終了

which runncl.sh &> /dev/null
if [ $? -ne 0 ]; then echo NO SUCH FILE,runncl.sh;exit 1;fi
# もしrunncl.shというファイルが見つからなかったらエラー

NML=NAMELIST.TXT
if [ ! -f $NML ];then echo NO SUCH FILE,$NML;exit 1;fi
# namelist fileが存在しない場合，エラーメッセージを表示して終了

runncl.sh $NCL $NML
