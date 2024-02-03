日付処理
==================
[[_TOC_]]

日付によるループ
-------------------------------

### 例１ yyyy年mm1月dd1日からyyyy2年mm2月dd2日までループさせる
```bash
#!/bin/bash
# C:\Users\atmos\Dropbox\MARKDOWN\LINUX
# LINUX_DATE.md

if [ $# -ne 2 ]; then
  echo
  echo Error in $0 : Wrong arguments.
  echo $0 yyyymmdd1 yyyymmdd2
  echo
  exit 1
fi

yyyymmdd1=$1; yyyymmdd2=$2

yyyy1=${yyyymmdd1:0:4}; mm1=${yyyymmdd1:4:2}; dd1=${yyyymmdd1:6:2}
yyyy2=${yyyymmdd2:0:4}; mm2=${yyyymmdd2:4:2}; dd2=${yyyymmdd2:6:2}

start=${yyyy1}/${mm1}/${dd1}; end=${yyyy2}/${mm2}/${dd2}

jsstart=$(date -d${start} +%s);   jsend=$(date -d${end} +%s)
jdstart=$(expr $jsstart / 86400); jdend=$(expr   $jsend / 86400)

nday=$( expr $jdend - $jdstart)

i=0
while [ $i -le $nday ]; do
  date_out=$(date -d"${yyyy1}/${mm1}/${dd1} ${i}day" +%Y%m%d)
  yyyy=${date_out:0:4}; mm=${date_out:4:2}; dd=${date_out:6:2}

  echo $yyyy $mm $dd

  i=$(expr $i + 1)
done

exit 0
```
実行例
```
$ ./date.out.sh 20030101 20130101 | head
2003 01 01
2003 01 02
2003 01 03
2003 01 04
2003 01 05
2003 01 06
2003 01 07
2003 01 08
2003 01 09
2003 01 10
```
[Sat Apr 11 13:29:54 JST 2015]
[~/NetCDF2txt_MSM]
[elham@aofd31]

```
$ ./date.out.sh 20030101 20130101 | tail
2012 12 23
2012 12 24
2012 12 25
2012 12 26
2012 12 27
2012 12 28
2012 12 29
2012 12 30
2012 12 31
2013 01 01
```

### 1時間間隔

```bash
#!/bin/bash

# 日付の処理
# https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/03.BASH_SCRIPT/LINUX_DATE.md

EXE= #.sh

# if [ ! -f $EXE ];then echo ERROR: NO SUCH FILE, $EXE; exit 1; fi

yyyymmdd1=$1; yyyymmdd2=$2

yyyymmdd1=${yyyymmdd1:-20220618}; yyyymmdd2=${yyyymmdd2:-20220619}

yyyy1=${yyyymmdd1:0:4}; mm1=${yyyymmdd1:4:2}; dd1=${yyyymmdd1:6:2}

yyyy2=${yyyymmdd2:0:4}; mm2=${yyyymmdd2:4:2}; dd2=${yyyymmdd2:6:2}

start=${yyyy1}/${mm1}/${dd1}; end=${yyyy2}/${mm2}/${dd2}

jsstart=$(date -d${start} +%s);   jsend=$(date -d${end} +%s)
jdstart=$(expr $jsstart / 86400); jdend=$(expr   $jsend / 86400)

nday=$( expr $jdend - $jdstart)

i=0
while [ $i -le $nday ]; do

  date_out=$(date -d"${yyyy1}/${mm1}/${dd1} ${i}day" +%Y%m%d)
  yyyy=${date_out:0:4}; mm=${date_out:4:2}; dd=${date_out:6:2}

  h=0;he=23;dh=1

  while [ $h -le $he ]; do
    hh=$(printf %02d $h)
    echo $EXE ${yyyy} ${mm} ${dd} ${hh}
    h=$(expr $h + $dh)
  done #h
  
  i=$(expr $i + 1)
done #i

exit 0
```



### 例２ ある日から～日間ループさせる

```
#!/bin/sh

if [ $# -ne 2 ]; then
  echo Error in $0 : Wrong number of arguments
  echo Usage: $0 start tday
  exit 1
fi
start=$1
tday=$2

d=0
while [ $d -lt $tday ]; do
  year=$(date -d "$start $d day" "+%Y")
  month=$(date -d "$start $d day" "+%m")
  day=$(date -d "$start $d day" "+%d")

  echo $year $month $day
  # YOUR COMMAND GOES HERE.

  d=$(expr $d + 1 ) 
done
```

### 例３ yyyy1年01月01日からyyyy2年12月31日までループさせる
```
exe=run.sh

ty=2010
tm=13
td=31

i=2000
while [ $i -lt $ty ] ; do
  j=0
  while [ $j -lt $tm ] ; do
  k=0
    while [ $k -lt $td ] ; do
    k=$(expr ${k} + 1) # カウンタの更新
      today="$(printf %4.4d $i)$(printf %2.2d $j)$(printf %2.2d $k)"
#      echo $today
      date -d "$today" > /dev/null 2>&1
      if [ $? -eq 0 ];then
         ./run.sh $today
#        echo $today
      fi
    done
    j=$(expr ${j} + 1) # カウンタの更新
  done
  i=$(expr ${i} + 1) # カウンタの更新
done
```



## 日付表示のフォーマット例

```
$ T1=$(date +"%Y/%m/%d %H:%M:%S")
$ echo $T1
2020/09/01 16:01:51

$ date  -d"$T1" +"%Y/%m/%d %H:%M:%S"
2020/09/01 16:01:51

$ date  -d"$T1 5 second" +"%Y/%m/%d %H:%M:%S"
2020/09/01 16:01:56

$ date  -d"$T1 1 minute" +"%Y/%m/%d %H:%M:%S"
2020/09/01 16:02:51

$ date  -d"$T1 3 hour" +"%Y/%m/%d %H:%M:%S"
2020/09/01 19:01:51

$  date  -d"$T1 1hour 30 minute" +"%Y/%m/%d %H:%M:%S"
2020/09/01 17:31:51

$ date -d"$T1" +"%Y%m%d%H%M%S"
20200901160151
```



### 日付を使ったIF文

$ cat TEST_IF_DATE.sh

```
T0=20200901160151
T1=20200901160152
if [ $T0 -le $T1 ]; then
echo "$T0 <= $T1"
else
echo "$T0 >  $T1"
fi
```
```
$ TEST_IF_DATE.sh 
20200901160151 <= 20200901160152
```



日数を計算する
------------------------------

nod.sh
```
#!/bin/sh

if [ $# -ne 2 ]; then
  echo Error in $0 : Wrong number of arguments
  echo Usage: $0 start tday
  exit 1
fi

#dataset=$1
start=$1
end=$2

jsstart=$(date -d${start} +%s)
jsend=$(date -d${end} +%s)
jdstart=$(expr $jsstart / 86400)
jdend=$(expr   $jsend / 86400)

tday=$( expr $jdend - $jdstart)

echo $tday
```

使用例
```
$ sh nod.sh 1985/01/01 2014/07/10
10782
```

書式指定出力
```
$ date +"%Y%m%d %H %M"
20120731 07 37
```
```
$ set temp = `date +"%Y%m%d-%H%M"` ; echo $temp
```
20120803-1158

前日の日付を取得する
-------------------------------------------

"--date" オプションを使って 1 日前なら次のように指定します。
```
$ date +"%Y/%m/%d" --date "1 day ago"
```
3 日後なら次のように指定します。

```
$ date +"%Y/%m/%d" --date "3 days"
```
"day" でも "days" でも取得できます。

日付指定のオプション
Redhat や Debian は "--date"
BSD 系の UNIX の date コマンド:  "-v"


ユリウス日（その年の1月1日からの日数）を表示
---------------------------------------------------
```
$ date -d2012/06/01 +%j
153
```
```
$ date -d2012/01/01 +%j
001
```
スラッシュ無くても可
```
$ date -d20120101 +%j
001
```
```
$ date -d20121020 +%j
294
```


6月1日～3月13日って、何日あるの？
-----------------------------------------------
起算日に注意
```
$ echo "$(date -d2012/06/01 +%j) - $(date -d2012/03/13 +%j) + 1" |bc
81
```
好きな日付から、～日後の日付を書き出す
-------------------------------------------
2011年8月11日から2日後の日付は？
```
$ date -d"2011/08/11 2 day"
```
2011年  8月 13日 土曜日 00:00:00 JST

2010年1月5日から5日後の日付は？
------------------------------------
```
$ date -d"2010/01/05 5 day" '+%Y%m%d'
```
20100110

ファイル名に日付をつかう
----------------------------------------------

バッチ処理で、前日の日付が必要なときなど に役に立つ。

[Wed Jul  7 09:02:42 JST 2010]
```
$ today=$(date '+%Y%m%d')
$ echo $today
20100707
```
```
$ yesterday=$(date '+%Y%m%d' --date "1 day ago")
$ echo $yesterday
20100706
```



日付・時刻をGrADS用に変換
-------------------------------------
```bash
YYYYMMDDHH=${YYYYMMDD:=2011012100}

YYYY=${YYYYMMDDHH:0:4}; MM=${YYYYMMDDHH:4:2}; DD=${YYYYMMDDHH:6:2}
HH=${YYYYMMDDHH:8:2}

if [ $MM = "01" ]; then MMM="JAN"; fi; if [ $MM = "02" ]; then MMM="FEB"; fi
if [ $MM = "03" ]; then MMM="MAR"; fi; if [ $MM = "04" ]; then MMM="APR"; fi
if [ $MM = "05" ]; then MMM="MAY"; fi; if [ $MM = "06" ]; then MMM="JUN"; fi
if [ $MM = "07" ]; then MMM="JUL"; fi; if [ $MM = "08" ]; then MMM="AUG"; fi
if [ $MM = "09" ]; then MMM="SEP"; fi; if [ $MM = "10" ]; then MMM="OCT"; fi
if [ $MM = "11" ]; then MMM="NOV"; fi; if [ $MM = "12" ]; then MMM="DEC"; fi

TIME=${HH}Z${DD}${MMM}${YYYY}
```



数字で表された月を月の名前に変換
-------------------------------------

```bash
if [ $M -eq 1 ];  then MMM="JAN"; fi; if [ $M -eq 2 ];  then MMM="FEB"; fi
if [ $M -eq 3 ];  then MMM="MAR"; fi; if [ $M -eq 4 ];  then MMM="APR"; fi
if [ $M -eq 5 ];  then MMM="MAY"; fi; if [ $M -eq 6 ];  then MMM="JUN"; fi
if [ $M -eq 7 ];  then MMM="JUL"; fi; if [ $M -eq 8 ];  then MMM="AUG"; fi
if [ $M -eq 9 ];  then MMM="SEP"; fi; if [ $M -eq 10 ]; then MMM="OCT"; fi
if [ $M -eq 11 ]; then MMM="NOV"; fi; if [ $M -eq 12 ]; then MMM="DEC"; fi
```
月を表す数字が文字型の場合
```bash
if [ $MM = "01" ]; then MMM="JAN"; fi; if [ $MM = "02" ]; then MMM="FEB"; fi
if [ $MM = "03" ]; then MMM="MAR"; fi; if [ $MM = "04" ]; then MMM="APR"; fi
if [ $MM = "05" ]; then MMM="MAY"; fi; if [ $MM = "06" ]; then MMM="JUN"; fi
if [ $MM = "07" ]; then MMM="JUL"; fi; if [ $MM = "08" ]; then MMM="AUG"; fi
if [ $MM = "09" ]; then MMM="SEP"; fi; if [ $MM = "10" ]; then MMM="OCT"; fi
if [ $MM = "11" ]; then MMM="NOV"; fi; if [ $MM = "12" ]; then MMM="DEC"; fi
```


英語標記の月を数字に変換

```bash
if [ $MMM = "JAN"  ]; then MM=01 ; fi; if [ $MMM = "FEB"  ]; then MM=02 ; fi
if [ $MMM = "MAR"  ]; then MM=03 ; fi; if [ $MMM = "APR"  ]; then MM=04 ; fi
if [ $MMM = "MAY"  ]; then MM=05 ; fi; if [ $MMM = "JUN"  ]; then MM=06 ; fi
if [ $MMM = "JUL"  ]; then MM=07 ; fi; if [ $MMM = "AUG"  ]; then MM=08 ; fi
if [ $MMM = "SEP"  ]; then MM=09 ; fi; if [ $MMM = "OCT"  ]; then MM=10 ; fi
if [ $MMM = "NOV"  ]; then MM=11 ; fi; if [ $MMM = "DEC"  ]; then MM=12 ; fi
```



関数として使う

```bash
function MM2MMM(){
if [ $mm = "01" ]; then mmm="Jan"; fi; if [ $mm = "02" ]; then mmm="Feb"; fi
if [ $mm = "03" ]; then mmm="Mar"; fi; if [ $mm = "04" ]; then mmm="Apr"; fi
if [ $mm = "05" ]; then mmm="May"; fi; if [ $mm = "06" ]; then mmm="Jun"; fi
if [ $mm = "07" ]; then mmm="Jul"; fi; if [ $mm = "08" ]; then mmm="Aug"; fi
if [ $mm = "09" ]; then mmm="Sep"; fi; if [ $mm = "10" ]; then mmm="Oct"; fi
if [ $mm = "11" ]; then mmm="Nov"; fi; if [ $mm = "12" ]; then mmm="Dec"; fi
}
```
```bash
mm=$mm1
MM2MMM
mmm1=$mmm
```



ラグをとる
---------------------------------

2018-12-09_14-16
/work3/am/2018.KUROSHIO.CONVECTION/SNAPSHOT/SI_MSM_STABILITY
am@localhost
$ LAG_DATE_TEST.sh

```
======================================================
ORG: 00Z09Dec2018
LAG = 3 HR
LAG: 21Z08Dec2018
======================================================
```
```
#!/bin/bash

mm_to_mmm(){
      if [ $1 = "01" ]; then mmm="Jan"; fi; if [ $1 = "02" ]; then mmm="Feb"; fi
      if [ $1 = "03" ]; then mmm="Mar"; fi; if [ $1 = "04" ]; then mmm="Apr"; fi
      if [ $1 = "05" ]; then mmm="May"; fi; if [ $1 = "06" ]; then mmm="Jun"; fi
      if [ $1 = "07" ]; then mmm="Jul"; fi; if [ $1 = "08" ]; then mmm="Aug"; fi
      if [ $1 = "09" ]; then mmm="Sep"; fi; if [ $1 = "10" ]; then mmm="Oct"; fi
      if [ $1 = "11" ]; then mmm="Nov"; fi; if [ $1 = "12" ]; then mmm="Dec"; fi
}

yyyy=2018; mm=12; dd=09; hh=00
LAG=3


mm_to_mmm $mm

date="${dd}${mmm}${yyyy}"

if [ $hh -eq 0 ]; then
  lagdate=$(date -d"${yyyy}/${mm}/${dd} 1 day ago"  '+%Y%m%d')
  lagyyyy=${lagdate:0:4}
  lagmm=${lagdate:4:2}
  lagdd=${lagdate:6:2}
  laghh=21
else
  lagyyyy=${yyyy}
  lagmm=${mm}
  lagdd=${dd}
  tmp=$(expr $hh - $LAG)
  laghh=$(printf %02d $tmp)
fi

mm_to_mmm $lagmm

lagmmm=$mmm

lagdtime="${laghh}Z${lagdd}${lagmmm}${lagyyyy}"

echo "======================================================"
echo "ORG: ${hh}Z$date"
echo "LAG = $LAG HR"
echo "LAG: $lagdtime"
echo "======================================================"
echo
exit 0
```