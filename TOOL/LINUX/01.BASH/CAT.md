# CATでテキストファイルを連結

## テストデータの作成

```
$ cat > 20200701.TXT
12.00 20200701.TXT
```

```
$ cat > 20200702.TXT
13.00 20200702.TXT
```

```
$ cat > 20200703.TXT
14.00 20200703.TXT
```

```
$ ls
20200701.TXT  20200702.TXT  20200703.TXT
```



## 全部のファイルの内容をつなげて，ALL.TXTに出力

```
$ LIST=$(ls *.TXT)
```

```
$ echo $LIST
20200701.TXT 20200702.TXT 20200703.TXT
```

```
$ cat $LIST > ALL.TXT
```



## 結果の確認

```
$ cat ALL.TXT
12.00 20200701.TXT
13.00 20200702.TXT
14.00 20200703.TXT
```



## シェルスクリプト

```bash
#!/bin/bash

OFLE=CAT.ALL.TXT
# 出力ファイル名

LIST=$(ls 20*.TXT)
# 入力ファイルの一覧を取得

if [ -f $OFLE ];then rm -v $OFLE;fi
touch $OFLE
# 出力ファイルを用意する（空のファイルを作る）

for INFLE in $LIST; do
cat $INFLE >> $OFLE
# 入力ファイルの内容をを一つづつOFLEに書き加えていく

done

if [ -f $OFLE ];then echo OUTPUT: $OFLE;fi
```

```bash
$ CAT.ALL.sh 
`CAT.ALL.TXT' を削除しました
OUTPUT: CAT.ALL.TXT
```

```bash
$ cat CAT.ALL.TXT
12.00 20200701.TXT
13.00 20200702.TXT
14.00 20200703.TXT
```

