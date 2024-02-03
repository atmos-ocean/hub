Linux シェルスクリプトの基礎
=========================
[[_TOC_]]

シェルスクリプトとは？
------------------------------------
スクリプト＝ 台本  
シェルスクリプト ≒ 簡易プログラミング言語  
テキストファイルにコマンドを並べて書き、作業の手順を指示する。  
用途  
煩雑な作業の自動化  
長時間の作業の自動化  
定型処理の自動化  
準備  
```
$ dirname="~/$(date +%y%m%d)"

$ mkdir -pv $dirname

$ cd $dirname

$ pwd
```



viによるシェルスクリプト編集と実行
------------------------------------

viの基本操作に関しては下記サイトを参照のこと  
https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/02.VIM/01.VIM_TUTORIAL.md

https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/02.VIM/02.VIM_TIPS.md


以下の例では、編集・実行するシェルスクリプトの名前をfoo.shとする。  

注意  
ESCはエスケープキーを押すことを意味する。CTLはコントロールキーを押すことを意味する。
C-aは、CTLとaを同時に押すことを意味する  

下記でESCはCTL+[でも代用可能（こちらの方が押しやすい場合がある）  
[1] viの起動  

```
$ vi foo.sh
```
[2] vi上でファイル編集  

[3] viをサスペンドして，シェルに戻る

vi 上でCTL+Z

```
[1]+  停止                  vim foo.sh
```

[4] 編集中のファイル(foo.sh)に実行許可を与える  
```
$ chmod u+x foo.sh
```
(注：この作業は最初の一回だけでよい)  

[5] 実行  

```
$ foo.sh
```

[7] viに戻る   

```
$ fg
```



シェルスクリプトの簡単な例  
------------------------------------
下記の例では4つのコマンド（date, pwd, whoami, hostname）を順番に実行する  

```
$ cat > script01.sh
```
```
#!/bin/sh

date      # Print data and time
pwd       # Print current directory
whoami    # Print user name
hostname  # Print domain name
C-d
```

注意：  
C-dはコントロールキー（Ctrl)とDを同時に押すことを意味する。  
\# から右側に書いてある内容はコメントと見なして無視する  
```
$ sh script01.sh
```
/work03/gu1/LAB/2021-05-20_LAB  
gu1  
p5820.bio.mie-u.ac.jp  
```
$ chmod u+x script01.sh
```
```
$ ./script01.sh
```
/work03/gu1/LAB/2021-05-20_LAB  
gu1  
p5820.bio.mie-u.ac.jp  

シェルスクリプトの構成要素  
------------------------------------
- シェル変数  
　- データ（ファイル名、数値、文字列など）を保存``

コマンド  
- データに対してなんらかの処理を行う  
  
- 分岐  
　- 条件によって処理を分ける  

- 繰り返し  
　- 同じ処理を繰り返す  

- 関数
　- 複雑な作業を分業化する  

- シェル変数  

シェルスクリプトは一種のプログラミング言語であり、変数を使用することができる。  
シェルスクリプトにおける変数はシェル変数と呼ばれ、型は自動判別される。  

```
$ cat > script_var.sh  
#!/bin/sh

var1=1
echo "var1= ${var1}"

var2="1.5"
echo "var2= ${var2}"

var3="Hello!"
echo "var3= ${var3}"

exit 0
C-d
```
```
$ sh script_var.sh
```
var1= 1  
var2= 1.5  
var3= Hello!  


引用符の使用法  
------------------------------------
シングルクォーテーション「'」の中はすべて文字列であると解釈される。  

ダブルクォーテーション「"」の引用符内に変数があるとその内容が展開される。  

```
$ cat > quote.sh
#!/bin/sh

var1=Hello!
echo "var1= $var1"
echo 'var1= $var1'

echo
var2=Hi!
echo "var2= ${var2}"
echo 'var2= "${var2}'
echo


echo
var3='Hey, yo! what'\'s' up?'
\# 解説
\# シングルクォーテーション自身を内部でエスケープする場合、一度閉じて「\'」とエスケープ表現し連結する。
echo 'var3=' $var3

exit 0
C-d
```
```
$ sh quote.sh
```
var1= Hello!  
var1= $var1  

var2= Hi!  
var2= "${var2}  


var3= Hey, yo! what's up?  

  


シェル変数の連結  
------------------------------------
```
$ cat > shellvar.sh
#!/bin/sh

var1="I care, "
var2="because you do."

echo var1= ${var1}
echo var2= ${var2}
echo var1var2= ${var1}${var2}

exit 0
```
```
$ sh shellvar.sh
```
var1= I care,
var2= because you do.
var1var2= I care, because you do.




部分文字列   
------------------------------------
$ cat > string.sh
```
#!/bin/sh

var=abcdefg

echo 'var= '$var
echo
echo '${var:0:2}='${var:0:2}
echo '${var:0:5}='${var:0:5}
echo '${var:2:2}='${var:2:2}

exit 0
```
C-d

$ sh string.sh
```
var= abcdefg

${var:0:2}=ab
${var:0:5}=abcde
${var:2:2}=cd
```



### デフォルト値の設定    

```
TIME=${TIME:=00Z23JAN2011}
```

TIMEに何も値が入力されていない場合，TIMEの値が自動的に 00Z23JAN2011となる。何か値が入力されていれば，その値が上書きされる  

   

### 文字列置換・削除        

後方からの検索の一致で一番初めにマッチした部分を削除     

```
${変数%マッチパターン}
```


後方からの検索の一致で一番後ろまでマッチした部分までを削除     

```
${変数%%マッチパターン}
```


前方からの検索の一致で一番初めにマッチした部分を削除  

```
${変数#マッチパターン}
```


前方からの検索の一致で一番後ろまでマッチした部分までを削除   

```
${変数##マッチパターン}
```


最初にマッチしたもののみ文字列を置換     

```
${変数/文字列/処理後文字列}
```


全ての文字列を置換     

```
${変数//文字列/処理後文字列}
```


ファイル名の取得      
```
${変数名##*/}
```


拡張子の削除      

```
${変数名%.*}
```


拡張子の取得      

```
${変数名##*.}
```


### コマンドの出力をシェル変数に代入する  

$ cat > script_var2.sh  

```
#!/bin/sh

date +%y%m%d

var1=$(date +%y%m%d)
echo
echo "var1= ${var1}"
echo

exit 0
```
$ sh script_var2.sh  
130119  

var1= 130119  



引数の指定
------------------------------------
コマンドと同様にシェルスクリプトでも引数を受け取ることができる。  

$ cat > script02.sh  
```
#!/bin/sh

echo Script name: $0

echo Arguments: $1 $2 $3 $4 $5 $6 $7 $8 $9

echo All arguments: $@

exit 0
```
C-d  

$ sh script02.sh arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9  
Script name: script02.sh  
Arguments: arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9  
All arguments: arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9    

注：引数の数は最大9個    




条件分岐
------------------------------------
if文を使って条件分岐ができる。    

### if文の構文    

```
if [ 条件式 ]; then  
  処理  
fi    
```

条件式が真(true)であれば、処理が実行される。    


条件式の書き方については、付録1参照のこと    

### if文のその他の構文と使用例    

複数の条件式を指定したい場合は、elifを使います。  

```
if [条件式1]; then  
  処理1  
elif [条件式2]; then  
  処理2  
fi  
```

​    
この場合、条件式1が真であれば処理1が実行される。 条件式1が偽で、条件式2が真であれば処理2が実行される。    

条件式にマッチしなかった場合の処理を指定したい場合は、elseを使う。  

```
if [条件式1]; then  
  処理1  
elif [条件式2]; then  
  処理2  
else  
  処理3  
fi  
```


この場合、条件式1と2が偽の場合、処理3が実行される。 



### 数値比較演算子

| 演算子          |                       | 意味                           |
| :-------------- | --------------------- | :----------------------------- |
| 数値1 -eq 数値2 | equal                 | 数値1と数値2が等しければ真     |
| 数値1 -ne 数値2 | not equal             | 数値1と数値2が等しくなければ真 |
| 数値1 -gt 数値2 | greater than          | 数値1が数値2より大きければ真   |
| 数値1 -lt 数値2 | less than             | 数値1が数値2より小さければ真   |
| 数値1 -ge 数値2 | greater than or equal | 数値1が数値2以上であれば真     |
| 数値1 -le 数値2 | less than or equal    | 数値1が数値2以下であれば真     |

   

使用例  
$ cat > if.sh   

```
#!/bin/sh  

a=4
echo "a= $a"
if [ $a -lt 5 ]; then
  echo "a < 5"
elif [ $a -ge 5 -a $a -lt 15 ]; then
  echo "5 <= a < 15"
else
  echo "a >= 15"
fi
echo

a=10
echo "a= $a"
if [ $a -lt 5 ]; then
  echo "a < 5"
elif [ $a -ge 5 -a $a -lt 15 ]; then
  echo "5 <= a < 15"
else
  echo "a >= 15"
fi
echo

a=20
echo "a= $a"
if [ $a -lt 5 ]; then
  echo a < 5
elif [ $a -ge 5 -a $a -lt 15 ]; then
  echo "5 <= a < 15"
else
  echo "a >= 15"
fi
echo

exit 0
```
C-d    

$ sh if.sh  
a= 4  
a < 5  

a= 10  
5 <= a < 15  

a= 20  
a >= 15  



引数の数のチェック
------------------------------------

$ cat > script03.sh    
```
#!/bin/sh

if [ $# -ne 2 ]; then
  echo Error in $0 : Wrong number of arugments
  echo Usage: $0 arg1 arg2
  exit 1
fi

echo Script name: $0

echo Arguments: $1 $2

exit 0
```
C-d    

$ sh script03.sh arg1 arg3  
Script name: script03.sh  
Arguments: arg1 arg3  

$ sh script03.sh arg1  
Error in script03.sh : Wrong number of arugments  
Usage: script03.sh arg1 arg2  

$ sh script03.sh arg1 arg2 arg3  
Error in script03.sh : Wrong number of arugments  
Usage: script03.sh arg1 arg2  




入力ファイルが存在するかチェック
------------------------------------
$ cat > script04.sh    
```
#!/bin/sh

input_file=$1

if [ ! -f $input_file ]; then
  echo No such file, $input_file
  exit 1
fi

ls -l $input_file

exit 0
```
C-d    

```
$ touch file1$ sh script04.sh file1  
```

-rw-rw-r-- 1 tempuser tempuser 0  1月 19 09:11 2013 file1    

```
$ sh script04.sh file2  
```

No such file, file2  



計算
------------------------------------
整数の計算  

$ cat > calc01.sh  
```
#!/bin/sh

a=1
b=2

c1=$(expr $a + $b)
c2=$(expr $a - $b)
c3=$(expr $a \* $b)
c4=$(expr $a / $b)

echo "c1= $c1"
echo "c2= $c2"
echo "c3= $c3"
echo "c4= $c4"

exit 0
```
$ sh calc01.sh  
c1= 3  
c2= -1  
c3= 2  
c4= 0  

注意：exprコマンドは整数の計算のみを行う。実数の計算には下記のbcコマンドかawkを使う。    




実数の計算
------------------------------------
$ cat > calc02.sh  
```
#!/bin/sh
a=1.5
b=0.8
echo "a= $a"
echo "a= $b"

c1=$(expr $a \* $b)
c2=$(echo "scale=3; $a * $b" | bc)

echo "c1= $c1 (using expr)"
echo "c2= $c2 (using bc)"

exit 0
```
C-d    

$ sh calc02.sh  
a= 1.5  
a= 0.8  
expr: non-numeric argument  
c1=  (using expr)  
c2= 1.20 (using bc)  

if文で実数を比較する  
bashのif文では整数の比較しか行えないので，bcコマンドを併用する．    

例  
varが0.5に等しければ、iflag=1となり、そうでなければ0となる  
```
iflag=$(echo "scale=3;if( $var == 0.5 ) 1 else 0" | bc)
```
max1がsmaxより大きければ，xに1が入り，そうでなければ0が入る．    
```
iflag=$(echo "scale=3;if( $max1 > $smax ) 1 else 0" | bc)
if [ $iflag = 1 ]; then
  smax=$max1
fi
```



繰り返し処理（ループ）
------------------------------------

while文を使ったループ
------------------------------------
$ cat > script05.sh  
```
#!/bin/sh

i=1

while [ $i -le 10 ]; do
  echo "i=$i"
  i=$(expr $i + 1)
done

exit 0
```
C-d    

$ sh script05.sh  
i=1  
i=2  
i=3  
i=4  
i=5  
i=6  
i=7  
i=8  
i=9  
i=10  



for文を使ったループ
------------------------------------
$ cat > for.sh    
```
#!/bin/sh   

wbc="sugiuchi utsumi yamaguchi sawamura yamai asao maeda imamura noumi"

for player in $wbc; do
  echo $player
done

exit 0
```
$ sh for.sh  
sugiuchi  
utsumi  
yamaguchi  
sawamura  
yamai  
asao  
maeda  
imamura  
noumi  

forの構文  
for 変数 in 変数配列; do  
 文1  
 文2  
 ...  
done  

参考  
変数配列が長くなったときは、\をつかって行を継続させる    

$ cat > for2.sh  
```
#!/bin/sh

wbc="sugiuchi utsumi yamaguchi sawamura yamai asao maeda imamura noumi \
makita wakui settsu oodonari morifuku abe aikawa sumiya murata sakamoto"

for player in $wbc; do
  echo $player
done

exit 0
```

$ sh for2.sh  
sugiuchi  
utsumi  
yamaguchi  
sawamura  
yamai  
asao  
maeda  
imamura  
noumi  
makita  
wakui  
settsu  
oodonari  
morifuku  
abe  
aikawa  
sumiya  
murata  
sakamoto  




配列
------------------------------------
空の配列を生成する    
```
array=()
```
配列の要素数    
```
echo ${#array[@]}
```

$ cat > array01.sh    
```
#!/bin/sh

xtc[0]="White Music"
xtc[1]="Go 2"
xtc[2]="Drums and Wires"
xtc[3]="Black Sea"
xtc[4]="English Settlement"
xtc[5]="Mummer"

i=0
while [ $i -le 5 ]; do
  echo ${xtc[$i]}
  i=$(expr $i + 1)
done

exit 0
```

$ sh array01.sh  
White Music  
Go 2  
Drums and Wires  
Black Sea  
English Settlement  
Mummer    




関数
------------------------------------
$ cat > func.sh    
```
#!/bin/sh


usage(){
  echo Usage : $0 input_file
  echo input_file : input file name
}


if [ $# -ne 1 ]; then
  echo Error in $0 : Wrong number of argument.
  usage
  exit 1
fi

input_file=$1
if [ ! -f $input_file ]; then
  echo Error in $ : No such file, $input_file
  usage
  exit 1
fi

exit 0
```
C-d  

$ sh func.sh  
Error in func.sh : Wrong number of argument.  
Usage : func.sh input_file  
input_file : input file name  

$ ls  
130113  array01.sh  file3   for2.sh    result_tree.txt  
130119  calc02.sh   for.sh  func.sh  shuffle.sh  

```
$ sh func.sh TMP_FILE
```


Error in $ : No such file, TMP_FILE  
Usage : func.sh input_file  
input_file : input file name  




外部ファイルのインクルード
------------------------------------
外部ファイルをとりこむことをインクルードという。外部ファイルの内容がそのまま指定された位置に書き加えられる。  

$ cat > func01.sh  
```
usage(){
  echo Usage : $0 input_file
  echo input_file : input file name
}
```
C-d

$ cat > func02.sh  
```
#!/bin/sh

. ./func01.sh

if [ $# -ne 1 ]; then
  echo Error in $0 : Wrong number of argument.
  usage
  exit 1
fi

input_file=$1
if [ ! -f $input_file ]; then
  echo Error in $ : No such file, $input_file
  usage
  exit 1
fi

exit 0
```
C-d    




オプションの処理
------------------------------------
オプションが引数を取らない場合
------------------------------------
$ cat > mydate.sh  
```
#!/bin/sh

usage(){
  echo "Usage $0 [-d]"
  echo  -d : print date in digit 
}

export LANG=C

flag="false"
while getopts d OPT; do
  case $OPT in
    "d" ) flag="true" ;  ;;
     * ) usage
  esac
done

echo "flag = $flag"

shift $(expr $OPTIND - 1)

if [ $flag = "true" ]; then
  date +%y%m%d 
else
  date
fi

exit 0
```
C-d    

$ sh mydate.sh -d  
flag = true  
130120  

$ sh mydate.sh  
flag = false  
Sun Jan 20 19:26:50 JST 2013  




オプションが引数を取る場合
------------------------------------
$ cat > opt2.sh    
```
#!/bin/sh

usage(){
  echo "Usage $0 [-p] arg "
}

flagp="false"

while getopts p: OPT; do
  case $OPT in
    "p" ) flagp="true" ; valuep="$OPTARG" ;;
     *  ) usage
  esac
done

value_p=NOT_AVAILABLE

echo "flagp = $flagp"
echo "valuep= $valuep"

shift $(expr $OPTIND - 1)

arg1=$1
echo "arg1= $arg1"

exit 0
```
C-d    

オプションを指定する場合
------------------------------------
```
$ sh opt2.sh -p aaa input_file  
```

flagp = true  
valuep= aaa  
arg1= input_file  

```
$ sh opt2.sh -p   
```

flagp = true  
valuep= aaa  
arg1=  

オプションを指定しない場合  

```
$ sh opt2.sh input_file  
```

flagp = false  
valuep=  
arg1= input_file  

```
$ sh opt2.sh  
```

flagp = false  
valuep=  
arg1=  



引数を取るオプションと取らないオプションの両方がある場合
------------------------------------

$ cat > opt3.sh  
```
#!/bin/sh

usage(){
  echo "Usage $0 [-p] [-d]  arg "
}

flagp="false"
flagd="false"

while getopts p:d OPT; do
  case $OPT in
    "p" ) flagp="true" ; valuep="$OPTARG" ;;
    "d" ) flagd="true" ;                  ;;
     *  ) usage
  esac
done

value_p=NOT_AVAILABLE

echo "flagp = $flagp"
echo "valuep= $valuep"
echo "flagd = $flagd"

shift $(expr $OPTIND - 1)

arg1=$1
echo "arg1= $arg1"

exit 0
```
```
$ sh opt3.sh -p aaa -d input_file 
```


flagp = true  
valuep= aaa  
flagd = true  
arg1= input_file  

```
$ sh opt3.sh  -d input_file  
```

flagp = false  
valuep=  
flagd = true  
arg1= input_file  

```
$ sh opt3.sh  -d  
```

flagp = false  
valuep=  
flagd = true  
arg1=  




シェルスクリプト内で良く使うコマンド
------------------------------------

awk - 文字処理と簡単な計算を行う
------------------------------------
### 特定の列を抜き出す  

```
$ cat > file10  
```

```
-999.   1    2
0      10    0
0       0   10
```
C-d  

```
$ awk '{print $2, $3}' file10   
```

1 2  
10 0  
0 10  

### 条件を満たす行だけ処理を行う  

#### 欠損値を飛ばす例  

```
$ awk '{if ($1 > -999. ) print $2, $3}' file10  
```

10 0  
0 10    

#### コメント行を読み飛ばす例  

```
$ cat > file11
# Comment line
-999.   1    2
0      10    0
0       0   10
```
C-d  

```
$ awk '{if ($1 != "#" ) print $1, $3}' file11
```
-999. 2  
0 0  
0 10  


列の指定にシェル変数を使う
------------------------------------
```
$ col=2  
$  awk '{ print$'"$col"'}' file10  
```

1  
10  
0  

書式を指定して出力する
------------------------------------
### 整数型  

```
$ awk '{printf "%03d %03d \n", $2, $3}' file10  
```

001 002  
010 000  
000 010  

実数型
------------------------------------
```
$ awk '{printf "%5.2f %6.3f\n", $2, $3}' file10  
```

1.00  2.000  
10.00  0.000  
0.00 10.000  

四則演算を行う
------------------------------------
```
$ awk '{print $2 + $3}' file10  
```

3  
10  
10  

```
$ awk '{print $2 - $3}' file10  
```

-1  
10  
-10  

```
$ awk '{print $2 * $3}' file10  
```

2  
0  
0  

```
$ awk '{if ($3 != 0) print $2 / $3}' file10  
```

0.5  
0  


関数の使用例
------------------------------------
```
$ cat > file13  
```

10 30  
5  45  
10 60  
10 0  
10 120  
C-d  

```
$ awk -v pi=3.1415926536 '{print $1*cos($2*pi/180.), $1*sin($2*pi/180.)}' file13> file14  
```
$ cat file14  
8.66025 5  
3.53553 3.53553  
5 8.66025  
10 0  
-5 8.66025  



定数を与える
------------------------------------
```
$ awk -v pi=3.1415926536 '{print $1*cos($2*pi/180.), $1*sin($2*pi/180.)}' file13> file14
```
のように、-vオプションを使うと定数の値を与えることができる。2個以上定数の値を与えたい場合は    

```
awk -v c1=2.5 -v c2=3.1 '{ ...    
```

のように1個の定数につき、一個づつ-vオプションを使う 。  

また、下記の例のようにシェル変数も使用できる。  

```
const1=2.5  
const2=3.1  
awk -v c1=$const1 -v c2=const2 '{ ...  
```



ベクトルの成分を、極座標表示に変える
------------------------------------
GMTのpsxyでベクトルを書く場合に使う。  
```
$ awk -v pi=3.1415926536 '{ print atan2($2, $1)*180.0/pi, sqrt($1*$1+$2*$2)}' file 
```
GMTのpsxyで使う場合には、    

経度(x座標)、緯度(y座標)、角度、ベクトルの大きさ    

の順にデータを並べ、結果をパイプしてpsxyに渡す。    


例  
1列目がx座標, 2列目がy座標, 3列目がベクトルのx成分, 4列目がベクトルのy成分の場合  
```
$ awk -v pi=3.1415926536 '{ print $1, $2, atan2($4, $3)*180.0/pi, sqrt($3*$3+$4*$4)}' input_file
```



注意：よくある誤り
------------------------------------

入力ファイルを指定しないと、入力待ちの状態でいつまでたっても止まったままになる。意外とこの誤りは気がつきにくい。  

例  

```
awk '{print $1}' 
```

→　止まったまま  

in=input.txt  

```
awk '{print $1}' $in
```

→　処理が進む。    




sed (Stream Editor)
------------------------------------
多様な機能があるが、下記の文字列の置換機能は頻繁に使われる。  

sed 's/(置換前の文字)/(置換後の文字)/g' ファイル名  

例１  
```
$ sed 's/,/:/g' input_file  
```
とすると、input_fileの中の「,」が「:」に置換される。    


例２  
「/」自体を置換したい場合は、直前に\(バックスラッシュ)をつける。 これはエスケープ文字である。   

  ```
$ sed 's/\//:/g' input_file
  ```
とすると、input_fileの中の「/」が「:」に置換される。  


例3：数字の置き換え  

var1という変数に, k000～k999という文字列が含まれていたら、それをokikaeという文字列に置き換えて、var2という変数に代入  
```
$ var2=$(echo ${var2} | sed -e "s/k[0-9]\{3\}/okikae/")
```



printf
------------------------------------
書式を指定して出力する  


$ cat > print.sh  
```
#!/bin/sh

i=5
f=5.0
g=0.000001
s="string"

printf "%2d\n"  $i
printf "%03d\n" $i
echo
printf "%5.1f\n"  $f
printf "%7.2f\n"  $f
echo
printf "%11.4e\n"  $g
echo
printf "%s\n" $s

# \nは改行記号

exit 0
```
C-d  

```
$ sh print.sh
```

``` 
5
005

  5.0
   5.00

1.0000e-06

string
```


演習 
------------------------------------
2.1 数表の作成  
南緯90度から北緯90度まで15度毎に有効数字5桁でコリオリ係数を計算し、表を作成する。  

(1) 準備  

```
$ dirname=~/$(date +%y%m%d)/coriolis 
```

```
$ mkdir -p $dirname  
```

```
$ cd $dirname  
```

```
$ pwd  
```

/home/tempuser/130121/coriolis  


(2) 地球の自転角速度の計算  

地球の自転角速度を計算して、その値をomegaというシェル変数に保存する。  

ヒント: awkを使う  

計算式については、下記参照  
http://cachu.xrea.jp/blog/archives/2006/09/151750.html  

参考  
$ cat > cor1.sh  
```
#!/bin/sh

pi=3.14159265

echo "86164.09" |awk -v pi=3.14159265 '{print 2.0*pi/$1}'

exit 0
```
C-d  

(3) コリオリ係数のテスト計算  

北緯30度のコリオリ係数を計算し、その値をfというシェル変数に保存する。  

角度はラジアンで与える  

printfコマンドを使って、fの値を10桁まで出力する  

チェック用の値  
pi=    3.14159  
lat=   30.00000  
f=         0.0000729212  


(4) コリオリ係数の表を作成  

南緯90度から北緯90度まで15度毎にコリオリ係数を計算し、表を作成する。  

ヒント  
whileでループを作成する。コリオリ係数の計算にはawkを使う。  

結果の例  

```
Latitude [deg.]     f [s-1]  
-90.0               -0.0001458420  
-75.0               -0.0001408730  
-60.0               -0.0001263030  
-45.0               -0.0001031260  
-30.0               -0.0000729212  
-15.0               -0.0000377468  
  0.0                0.0000000000  
 15.0                0.0000377468  
 30.0                0.0000729212  
 45.0                0.0001031260  
 60.0                0.0001263030  
 75.0                0.0001408730  
 90.0                0.0001458420  
```



2.2 日付処理  
2000年01月01日から2012年12月31日までの日付を    

20000101  
20000102  
...  
20121230  
20121231  

のような書式で表示する。  

日付を名前とするディレクトリやファイルを作成することが良くある。そのための練習。  


準備  

```
$ dirname=~/$(date +%y%m%d)/dir_date  
```

```
$ mkdir -p $dirname  
```

```
$ cd $dirname  
```

(1) 年  

例  
$ cat > dir1.sh  
```
#!/bin/sh

iy=2000

while [ $iy -le 2012 ]; do

  echo $iy

  iy=$(expr $iy + 1)

done
```
C-d  

(2) 月  

例  
$ cat > dir2.sh  
```
#!/bin/sh

iy=2000

while [ $iy -le 2012 ]; do

  im=1
  while [ $im -le 12 ]; do
    month=$(printf %02d $im)
    echo ${iy}${month}
    im=$(expr $im + 1)
  done
    
  iy=$(expr $iy + 1)

done
```
C-d  

(3) 日  

ヒント
各月の日数はif文を使って判断する
```bash
#
# https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/03.BASH_SCRIPT/LINUX_SCRIPT_TUTORIAL.md
# https://kappuccino-2.hatenadiary.org/entry/20081025/1224906495
# https://gist.github.com/iidaatcnt/cdbf0e7b1dab9e06d0cf

ys=1988; ye=2022
year=$ys

while [ $year -le $ye ];do

a=$(expr $year % 400); b=$(expr $year % 100); c=$(expr $year % 4)

if [ $a -eq 0 -o \( $c -eq 0 -a $b -ne 0 \) ]; then
  nd=29
else
  nd=28
fi

if [ $nd -eq 28 ];then
echo $year $nd
else
echo $year $nd "*"
fi

year=$(expr $year + 1)

done
```
参考  

下記のプログラムを使うと、うるう年を考慮した2月の日数を計算することができる。  
```bash
a=$(expr $year % 400); b=$(expr $year % 100); c=$(expr $year % 4)

if [ $a -eq 0 -o \( $c -eq 0 -a $b -ne 0 \) ]; then
  nd=29
else
  nd=28
fi
```



2.3 特定の文字列を含む名前のファイルのみコピーする  

下記の仕様をもつシェルスクリプトを作成せよ  
コマンドラインから引数を二つ受け取る  
最初の引数の名前をもつディレクトリを作成する  
2番目の引数で指定した文字列を含むファイルのみ、作成したディレクトリにコピーする  
指定した文字列を含むファイルが存在しないときは、エラーメッセージを表示して実行を停止する  
準備  

```
$ dirname=~/$(date +%y%m%d)/copy_string  

$ mkdir -p $dirname   

$ cd $dirname  

```

```
$ pwd  

```

/home/tempuser/130121/copy_string  

```
$ touch aaa_tmp.txt bbb_tmp.txt ccc_tmp.txt  

$ touch aaa_exe.txt bbb_exe.txt ccc_exe.txt  

$ touch aaa_tmp.asc bbb_tmp.asc ccc_tmp.asc  
```

```
$ ls  
```

aaa_exe.txt  aaa_tmp.txt  bbb_tmp.asc  ccc_exe.txt  ccc_tmp.txt  
aaa_tmp.asc  bbb_exe.txt  bbb_tmp.txt  ccc_tmp.asc



```
$ sh cp_str.sh AAA aaa  
```

```
aaa_exe.txt aaa_tmp.asc aaa_tmp.txt  
\`aaa_exe.txt\' -> \`AAA/aaa_exe.txt\'  
\`aaa_tmp.asc\' ->\`AAA/aaa_tmp.asc\'  
\`aaa_tmp.txt\' -> \`AAA/aaa_tmp.txt\'  
```

```
$ sh cp_str.sh AAA zzz
```


Error in $ : Cannot find the file that containts zzz.     

ヒント  
指定した文字列を含むファイルのリストを作成するためには、lsコマンドの結果をパイプ(|)で渡して、grepコマンドを適用する。  

指定した文字列を含むファイルが存在しない場合の判定には、  
```
${#file_list}
```
を使う。これで、$file_listというシェル変数にいくつの文字が含まれているかチェックできる。0なら指定した文字列を含むファイルは一つも見つからなかったことを意味する。  

参考 http://itpro.nikkeibp.co.jp/article/COLUMN/20060228/231152/




2.4 指定した数値以上の容量をもつファイルをコピーする
ディスクの残り容量が少なくなってきたときに、大きなファイルだけとりえあずバックアップ用のハードディスクに保存しておくような場合に使う

(1) 準備
```
$ dirname=~/$(date +%y%m%d)/copy_large_file

$ mkdir -p $dirname 

$ cd $dirname

$ pwd

$ mkdir -p ${dirname}/src

$ cd ${dirname}/src
```
コピーに使う練習用ファイルの作成
```
$ cat > exerc.files.sh
#!/bin/sh
rm -rf a b c d f1 f2 g1 g2
touch a;  i=1; while [ $i -le 1 ]; do    echo Line $i >> a;  i=$(expr $i + 1) ;done
touch b;  i=1; while [ $i -le 10 ]; do   echo Line $i >> b;  i=$(expr $i + 1) ;done
touch c;  i=1; while [ $i -le 100 ]; do  echo Line $i >> c;  i=$(expr $i + 1) ;done
touch d;  i=1; while [ $i -le 2000 ]; do echo Line $i >> d;  i=$(expr $i + 1) ;done
touch f1; i=1; while [ $i -le 1 ]; do    echo Line $i >> f1; i=$(expr $i + 1) ;done
touch f2; i=1; while [ $i -le 10 ]; do   echo Line $i >> f2; i=$(expr $i + 1) ;done
touch g1; i=1; while [ $i -le 100 ]; do  echo Line $i >> g1; i=$(expr $i + 1) ;done
touch g2; i=1; while [ $i -le 2000 ]; do echo Line $i >> g2; i=$(expr $i + 1) ;done
```
C-d
```
$ sh exerc.files.sh
```
コピー先のディレクトリを作成する。
```
$ destination=../dest

$ mkdir -p ${destination}
```
```
$ ls ..
dest  src

$  ls -lh .
合計 168K
-rw-rw-r-- 1 tempuser tempuser   78  1月 21 16:40 2013 a
-rw-rw-r-- 1 tempuser tempuser  863  1月 21 16:40 2013 b
-rw-rw-r-- 1 tempuser tempuser 9.5K  1月 21 16:40 2013 c
-rw-rw-r-- 1 tempuser tempuser 106K  1月 21 16:40 2013 d
-rw-rw-r-- 1 tempuser tempuser  682  1月 21 16:40 2013 exerc.files.sh
-rw-rw-r-- 1 tempuser tempuser   78  1月 21 16:40 2013 f1
-rw-rw-r-- 1 tempuser tempuser  863  1月 21 16:40 2013 f2
-rw-rw-r-- 1 tempuser tempuser 9.5K  1月 21 16:40 2013 g1
-rw-rw-r-- 1 tempuser tempuser  16K  1月 21 16:40 2013 g2
```


(2) 手順

この練習では、10KB以上のファイルを${destination}にコピーする。  

シェルスクリプト作成手順の概要を以下に示す  

lsの結果をfile_listという変数に代入する  

```
file_list=$(ls src)  
```

forループを使い、file_listに保存されているファイル一つ一つの容量を
```
ls -l --block-size=1k
```
でチェックしていく。--block-size=1kは、ファイル容量表示の単位を1KBとするという意味。  


awkで、ファイル容量を示す列を抜き出す。5列目がファイル容量を示す数字。  

ファイル容量が10KBよりも大きい場合は、そのファイルをcpコマンドで${destination}へコピーする。   




付録1 if文で使われる比較演算子
------------------------------------

ファイル比較演算子
-f filename
意味: filename が普通のファイルである場合に真 
使用例: if [ -f /usr/bin/grep ]; then

-e filename
意味: filenameが存在する(exist)場合に真
使用例: if [ -e /var/log/syslog ]; then

-d filename
意味: filename がディレクトリである場合に真 
使用例: if [ -d /tmp/mydir ]; then

-L filename 
意味: filename がシンボリック・リンクである場合に真
使用例: if  [ -L /usr/bin/grep ]; then

-r filename 
意味: filename が読み取り可能(readable)である場合に真 
使用例: if [ -r /var/log/syslog ]; then

-w filename  
意味: filename が書き込み可能である場合に真
使用例: if [ -w /var/mytmp.txt ]; then

-x filename  
意味: filename が実行可能(executable)である場合に真
使用例: if [ -L /usr/bin/grep ]; then

filename1 -nt filename2
意味: filename1 がfilename2 よりも新しい(newer than)場合に真
使用例:  if [ /tmp/install/etc/services -nt /etc/services ]; then

filename1 -ot filename2 
意味:filename1 がfilename2 よりも古い場合(older than)に真
使用例: if [ /boot/bzImage -ot arch/i386/boot/bzImage ]; then


数字の比較（算術比較演算子）

num1 -eq num2
意味: 等しい (equal)
使用例: if [ 3 -eq $mynum ]; then

num1 -ne num2
意味: 等しくない (not equal)
使用例: if [ 3 -ne $mynum ]; then

num1 -lt num2
意味: より小 (lesser than)
使用例: if [ 3 -lt $mynum ]; then

num1 -le num2
意味: 以下 (lesser than or equal)
使用例: if [ 3 -le $mynum ]; then

num1 -gt num2
意味: より大 (greater than)
使用例: if [ 3 -gt $mynum ]; then

num1 -ge num2 (greater than or equal)
意味: 以上
使用例: if [ 3 -ge $mynum ]; then


文字列比較演算子
string1 = string2
意味: string1 とstring2 が等しい場合に真
使用例: if [ "$myvar" = "one two three" ]; then

string1 != string2
意味: string1 とstring2 が等しくない場合に真
使用例: if [ "$myvar" != "one two three" ]; then

-z string
意味: string の長さがゼロの場合に真 
使用例: if [ -z "$myvar" ]; then

-n string
意味: string の長さがゼロ以外の場合に真 
使用例: if [ -n "$myvar" ]; then


論理演算子
-a
意味: かつ(and)
使用例: if [ $n -gt 10 -a $n -le 20 ]; then

-o
意味: かつ(or)
使用例: if [ $n -gt 10 -o $n -lt -10 ]; then



付録2 シェルスクリプトのデバッグ法
------------------------------------

変数の内容を書き出して実行を止める
echoコマンドで変数の内容を表示させ、その直後でexitコマンドで強制終了させる。

これをシェルスクリプトの上から順番に行う。もしくは、怪しいところ周辺で行う。

実行結果を逐次表示する
foo.shというシェルスクリプトをデバッグしたいとする。

コマンドラインで、

```
$ /bin/bash -x foo.sh
```

のように実行する。

-xオプションにより、コマンドの実行状況が標準エラー出力に出力される。

例として、以下のシェルスクリプト（backup.sh）のデバッグについて考える。
```
$ cat > backup.sh
#!/bin/sh

TIME=`date +%y%m%d`

while [ $# -gt 0 ]
do
  cp -r $1 $1.$TIME
  shift
done
```
C-d


-xオプションを付けて実行すると、以下のように変数の内容や実行過程が表示される。
```
$ /bin/sh -x backup.sh gterm-error01.png sample.pdf
++ date +%y%m%d
+ TIME=050824
+ '[' 2 -gt 0 ']'
+ cp -r gterm-error01.png gterm-error01.png.050824
+ shift
+ '[' 1 -gt 0 ']'
+ cp -r sample.pdf sample.pdf.050824
+ shift
+ '[' 0 -gt 0 ']'
```


付録3 環境変数の設定

環境変数の参照
```
$ echo $PATH
/opt/intel/composer_xe_2011_sp1.9.293/bin/intel64:/usr/local/bin/:/usr/lib64/qt-3.3/bin:/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/grads-2.0.1/bin:/opt/intel/composer_xe_2011_sp1.9.293/mpirt/bin/intel64:/usr/local/mybin:/home/tempuser/bin
```

環境変数の内容の更新
```
$ export PATH=$PATH:.
$ echo $PATH
/opt/intel/composer_xe_2011_sp1.9.293/bin/intel64:/usr/local/bin/:/usr/lib64/qt-3.3/bin:/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/local/grads-2.0.1/bin:/opt/intel/composer_xe_2011_sp1.9.293/mpirt/bin/intel64:/usr/local/mybin:/home/tempuser/bin:.
```

設定ファイルの参照
各ユーザーの設定ファイル
```
$ cat ~/.bashrc
```

全ユーザー共通の設定ファイル
```
$ cat /etc/bashrc
```


付録4　便利なコマンド
特定の名称のファイル以外を削除する
```
$ ls |grep -v -E 'GRADS' | xargs rm -rv
```
今自分がいるディレクトリにあるファイルとディレクトリのうち、名前がGRADS以外のものをすべて削除する。確認せずに削除してしまうので要注意。
```
$ ls |grep -v -E 'GRADS'
```
まで実行してどのファイルが削除されるか事前に確認すること。

```
$ ls | grep -v -E 'tar$' | xargs rm -r
```
名前が tar で終わるもの以外のファイル・ディレクトリを削除する

ファイル名の置換（rename）
```
$ rename FROM TO FILE
```
FROM: 置き換え前の文字列
TO:　　置き換え後の文字列
FILE:　変換対象のファイル

例：
置き換え前
```
$ ls
K120701_R02J_S_k50_wrfout_d03_height.ctl
K120701_R02J_S_k50_wrfout_d03_height_2012-07-01_00:00.dat
```

置換
```
$ rename K120701 KE120701 ./K120701_R02J*
```
K120701_R02Jで始まるファイルのK120701をKE120701に置き換える  


置き換え後  
```
$ ls
KE120701_R02J_S_k50_wrfout_d03_height.ctl
KE120701_R02J_S_k50_wrfout_d03_height_2012-07-01_00:00.dat
```

付録5 参加者名簿をシャッフルするためのシェルスクリプト
```
shuffle.sh
#!/bin/sh
allusers=$( cut -d: -f1 /etc/passwd )

participants=""

exception_list="am prog share kawamoto tempuser"

for username in ${allusers} ; do
  uid=$(id -u ${username})
  if [ $uid -ge 500 -a $uid -le 600 ]; then

    iflag=0
    for exception in ${exception_list}; do
      if [ $exception = $username ]; then
        iflag=1
      fi
    done
    if [ $iflag -eq 0 ]; then
      participants="${participants} ${username}"
    fi
  fi
done

n=0
for p in $participants; do
  user[${n}]="$p"
  n=$(expr $n + 1)
done

echo "Number of participants= $n"
echo

echo Participants:
i=0
for p in $participants; do
  echo $i ${user[${i}]}
  i=$(expr $i + 1)
done
echo

echo Shuffling...
echo

i=0
random_list="0"
while : ; do

  random=$(( (RANDOM % $n) + 1 )) 
  iflag=0
  for temp_random in ${random_list}; do
    if [ $temp_random -eq $random ]; then
      iflag=1
    fi
  done
  if [ $iflag -eq 0 -a $random -le $n ]; then
    no=$(expr $random - 1)
    user1=${user[$no]}
    random_list="${random_list} $random"
    i=$(expr $i + 1)
    echo $no $user1
  fi

  if [ $i -eq $n ]; then
    break
  fi
done

exit 0
```

```
$ sh shuffle.sh
Number of participants= 14

Participants:
0 satsuki
1 taku
2 aykatk23
3 yuk178bz
4 akira
5 makitoshi
6 yuka1007
7 takuro
8 iwashita
9 maika
10 watanabe
11 masahiro
12 haruhiko
13 ning

Shuffling...

6 yuka1007
9 maika
5 makitoshi
0 satsuki
1 taku
13 ning
12 haruhiko
8 iwashita
2 aykatk23
3 yuk178bz
10 watanabe
11 masahiro
4 akira
7 takuro
```



## 上達のためのポイント

**エラーが出た時の対応の仕方でプログラミングの上達の速度が大幅に変わる**。

上記のような検証法を常に試みると上達が早い。ポイントは次の3つである

1. エラーメッセージをよく読む
2. エラーメッセージを検索し，ヒットしたサイトをよく読む
3. 変数に関する情報を書き出して確認する

エラーメッセージは，プログラムが不正終了した直接の原因とその考えられる理由が書いてあるので，よく読むことが必要不可欠である。

記述が簡潔なため，内容が十分に理解できないことも多いが，その場合**エラーメッセージをブラウザで検索**してヒットした記事をいくつか読んでみる。

エラーの原因だけでなく，**考えうる解決策**が記載されていることも良くある。

エラーを引き起こしていると思われる箇所の**変数の情報**や**変数の値そのものを書き出して**，**期待した通りにプログラムが動作しているか確認する**ことも重要である。

エラーの場所が特定できれば，エラーの修正の大部分は完了したと考えてもよいほどである。

エラーメッセージや検索してヒットするウェブサイトは英語で記載されていることも多いが，**重要な情報は英語で記載されていることが多い**ので，よく読むようにする。

重要そうに思われるが，一回で理解できないものは，PDFなどに書き出して後で繰り返し読んでみる。どうしても**内容が頭に入らないものは印刷してから読む**。



## 参考になるWebsite

bash入門  
http://www.hpc.cs.ehime-u.ac.jp/~aman/linux/bash/shell_prog1.html  
http://www.hpc.cs.ehime-u.ac.jp/~aman/linux/bash/shell_prog2.html  

awk の使い方  
http://www.not-enough.org/abe/manual/unix2-au99/awk1.html  

第６回ａｗｋにおける算術演算  
http://homepage2.nifty.com/mozu/koza/awk_koza/awk_koza_06.html  

シェルスクリプトとawkによるデータ解析  
http://ryuiki.agbi.tsukuba.ac.jp/~nishida/MEMO/awk/index.html  

参考文献  
林晴比古 (2004): 改訂 新Linux/UNIX入門, ソフトバンククリエイティブ, 528 pp  