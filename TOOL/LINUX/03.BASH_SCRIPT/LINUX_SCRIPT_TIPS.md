# BASHスクリプトのTips

[TOC]

## バックアップ

### データの出力先に使用したスクリプトをコピーする

保存しているデータを作成するために使用したスクリプトをコピーしておく。スクリプトには，「いつ，どのディレクトリで作業を行ったか」を追記しておく。

下記の例では，`ODIR`という変数でデータの出力先のディレクトリ名を指定する。また，本体のスクリプト (`$0`で指定している)以外に，サブのスクリプトを使用している場合，その名称を`$SUB`で指定する。

```bash
echo MMMMM BACK-UP SCRIPTS
ODIR= ; mkdir -vp $ODIR #出力先ディレクトリの指定
SUB= #サブのスクリプト名の指定
TMP=TMP_$(basename $0) #スクリプトの先頭に書き込む情報
echo "# #!/bin/bash" >   $TMP; echo "# BACK UP of $0" >>$TMP
echo "# $(date -R)" >>   $TMP; echo "# $(pwd)"        >>$TMP
echo "# $(basename $0)">>$TMP; echo "# "              >>$TMP
BAK=$ODIR/$0;   cat $TMP $0   > $BAK; ls $BAK # $ODIRにバックアップを取る
BAK=$ODIR/$SUB; cat $TMP $SUB > $BAK; ls $SUB
rm -f $TMP #一時ファイルの削除
echo MMMMM
```



## 計算

### 異なる2つのファイルに含まれるデータを使って計算を行う

注意：２つのファイルの**行数がそろっていないといけない**

実行例
$ TEST.DIFF.sh 
removed `TEST.DIFF_OUT.txt'


removed `TEST.DIFF_IN1.txt'
removed `TEST.DIFF_IN2.txt'
OUTPUT:
-rw-rw-r-- 1 manda manda 37 2019-05-23 12:38 TEST.DIFF_OUT.txt

1 4 -3 
2 3 -1 
3 2 1 
4 1 3 
5 5 0 

スクリプト： TEST.DIFF.sh

```bash
#!/bin/bash
function caldiff () {
in1=$1
in2=$2
col=$(expr $3 - 1 )
out=$4
rm -vf $out
touch $out
i=0
while read BUF ; do
 ary=(`echo $BUF`)  # 配列に格納
 if [ ${ary[0]} != "#" ]; then
  x1[$i]=${ary[$col]}
  i=$(expr $i + 1)
 fi
done < $in1
echo
echo
i=0
while read BUF ; do
 ary=(`echo $BUF`)  # 配列に格納
 if [ ${ary[0]} != "#" ]; then
  x2[$i]=${ary[$col]}
  i=$(expr $i + 1)
 fi
done < $in2
n=$i
i=0
nm1=$(expr $n - 1)
while [ $i -le $nm1 ]; do
 dx[$i]=$(echo "scale=2; ${x1[$i]} - ${x2[$i]}" |bc)
 echo "${x1[$i]} ${x2[$i]} ${dx[$i]} " >> $out
 i=$(expr $i + 1)
done
} #END OF function calcdiff

in1=$(basename $0 .sh)_IN1.txt
cat <<EOF>$in1
# $in1
1
2
3
4
5
EOF

in2=$(basename $0 .sh)_IN2.txt
cat <<EOF>$in2
# $in2
4
3
2
1
5
EOF
col=1
out=$(basename $0 .sh)_OUT.txt
caldiff $in1 $in2 $col $out
rm -vf $in1 $in2
echo OUTPUT:
ls -lh --time-style=long-iso $out
echo
cat $out
```

