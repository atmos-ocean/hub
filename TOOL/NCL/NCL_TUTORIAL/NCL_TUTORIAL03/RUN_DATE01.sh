# 日付の処理
# https://gitlab.com/infoaofd/lab/-/blob/master/LINUX/03.BASH_SCRIPT/LINUX_DATE.md

if [ $# -ne 2 ]; then
  echo ERROR IN $0 : wRONG ARGUMENTS.; echo $0 yyyymmdd1 yyyymmdd2
  # 引数が2つなかったらエラーを表示して終了
  exit 1
fi

yyyymmdd1=$1 #1番目の引数は開始日
yyyymmdd2=$2 #2番目の引数は終了日

# 年,月,日に分離
yyyy1=${yyyymmdd1:0:4}; mm1=${yyyymmdd1:4:2}; dd1=${yyyymmdd1:6:2}
yyyy2=${yyyymmdd2:0:4}; mm2=${yyyymmdd2:4:2}; dd2=${yyyymmdd2:6:2}

#dateコマンドの書式に合わせる
start=${yyyy1}/${mm1}/${dd1}; end=${yyyy2}/${mm2}/${dd2}

#日付をユリウス日で表す
jsstart=$(date -d${start} +%s); jsend=$(date -d${end} +%s)
#秒単位
jdstart=$(expr $jsstart / 86400); jdend=$(expr   $jsend / 86400)
#日単位

nday=$( expr $jdend - $jdstart)
# 総日数

i=0
while [ $i -le $nday ]; do
  date_out=$(date -d"${yyyy1}/${mm1}/${dd1} ${i}day" +%Y%m%d)
  # dateコマンドで日付を書き出す

  yyyy=${date_out:0:4}; mm=${date_out:4:2}; dd=${date_out:6:2}
  # yyyy,mm,ddにそれぞれ年,月,日を代入する

  echo ${yyyy} ${mm} ${dd}

  i=$(expr $i + 1)
done
