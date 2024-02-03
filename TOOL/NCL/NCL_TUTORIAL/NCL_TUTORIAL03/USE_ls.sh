#!/bin/bash

# 共通の文字列を名前にもつ複数のファイルを処理する

INDIR=INPUT_DIR
if [ ! -d $INDIR ];then echo NO SUCH DIR,$INDIR;exit 1;fi
# ディレクトリINDIRが存在しない場合，エラーメッセージを表示して終了

INLIST=$(ls $INDIR/IN*.TXT)
# lsコマンドの結果をINLISTという変数に代入

NCL=USE_ls.ncl
if [ ! -f $NCL ];then echo NO SUCH FILE,$NCL;exit 1;fi
# NCLスクリプトが存在しない場合，エラーメッセージを表示して終了

which runncl.sh &> /dev/null
if [ $? -ne 0 ]; then echo NO SUCH FILE,runncl.sh;exit 1;fi
# もしrunncl.shというファイルが見つからなかったらエラー

# 変数INLISTに含まれる要素一つ一つを順番に変数INFLEに代入した後,
# runncl.shを実行する
for INFLE in $INLIST; do
runncl.sh $NCL $INFLE
done

