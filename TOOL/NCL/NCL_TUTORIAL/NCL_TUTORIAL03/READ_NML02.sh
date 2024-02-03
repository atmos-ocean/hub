#!/bin/bash

# namelistファイルから情報を読むこむ

NCL=READ_NML.ncl #NCLスクリプト名

if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCLスクリプトが存在しない場合，エラーメッセージを表示して終了

which runncl.sh &> /dev/null
if [ $? -ne 0 ]; then echo NO SUCH FILE,runncl.sh;exit 1;fi
# もしrunncl.shというファイルが見つからなかったらエラー

NML=NAMELIST.TXT
# catコマンドを使って, EOF (End of file)までの内容を,
# $NML (=NAMELIST.TXT)に書き出す
cat <<EOF>$NML
INFLE=INPUT.TXT
OFLE=OUTPUT.TXT
A=1.0
B=2.0
EOF

runncl.sh $NCL $NML

