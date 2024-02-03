# TEST.READ.TXT.FILE

/work03/am/2022.BGC_ARGO_SO/24.00.TIME-DEPTH_BD5904767_2016/12.12.MAP_ARGO_POSITION

## スクリプト

TEST.READ.TXT.FILE.py

```python
# coding: utf-8
# 日本語を使いたい時はcoding: utf-8をファイルの先頭に入れる

# /work03/am/2022.BGC_ARGO_SO/24.00.TIME-DEPTH_BD5904767_2016/12.12.MAP_ARGO_POSITION

# テキストファイルの読み込み

#入力ファイル名
INFLE='TEST.READ.TXT.FILE.TXT'

# 読み込み用にファイルを開く
f = open(INFLE, 'r')

#入力ファイルを読み込んで行単位で分割してリストとして取得する
#入力ファイルの1行目は読み飛ばす
datalist = f.readlines()[1:]

f.close() #ファイルを閉じる

#datalistの内容を書き出す。rstripで改行文字を取り除く
for data in datalist:
  print (data.rstrip('\n'))
```

## 実行例

```bash
$ python3 TEST.READ.TXT.FILE.py 
L1
L2
L3
```

## 入力ファイル

TEST.READ.TXT.FILE.TXT

```bash
THIS LINE IS A HEADER LINE
L1
L2
L3
```

