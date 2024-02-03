#!/bin/bash

# 共通の文字列を名前にもつ複数のファイルを処理する

INDIR=INPUT_DIR
if [ ! -d $INDIR ];then echo NO SUCH DIR,$INDIR;exit 1;fi
# ディレクトリINDIRが存在しない場合，エラーメッセージを表示して終了

INLIST=$(ls $INDIR/IN*.TXT)
# lsコマンドの結果をINLISTという変数に代入

# 変数INLISTに含まれる要素一つ一つを順番に変数INFLEに代入した後,
# echo INFLEを実行する
for INFLE in $INLIST; do
echo INFLE: $INFLE
done

