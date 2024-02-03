# nohup rsync



実行前のテスト
----------------------------------

`--dry-run`オプションをつけると，実際にコピーを行うことはせず，コマンドを実行した際にどのような動作をするかのみが画面に表示される

```bash
DRY_RUN="--dry_run"
ORG=ORGDIR # ORIGINAL
DST=DSTDIR # DESTINATION
```

```
rsync -arq ${DRY_RUN} ${ORG}/ ${DST}
```

その他のオプションの説明

a: ファイルの属性をできる限り保持する
r: 下層のディレクトリ・ファイルもコピーする
q: メッセージ表示を少なくする



## 特定のファイルをのみをコピー

下記の例では拡張子が`.sh`と`.csh`のファイルのみがコピーされる

```bash
ORG=ORGDIR # ORIGINAL
DST=DSTDIR # DESTINATION
INC="--include \*.sh --include \*csh"
```

```bash
rsync -arq $INC --exclude="*" ${ORG}/ ${DST}
```

**注**: オプションは並んだ順に先頭から解釈される。上記の例では，

1. `.sh` をコピーする (`--include \*.sh`)

2. `.csh`をコピーする (`--include \*.csh`)

2. その他はコピーしない(`--exclude="*"`)

となる。先に指定されたルールが優先される。



ディレクトリをコピー
----------------------------------
コピー元のディレクトリ名を「dir1」と指定するか、「dir1/」とスラッシュ（/）を追加するかで挙動が変わる。

例えば、dir1のように指定すると、前者はdir1ディレクトリの中身と、そのディレクトリ自体がコピーされますが、スラッシュを付けて**dir1/**とするとdir1ディレクトリの中身はコピーされるが、**ディレクトリはコピーされない**。

$ rsync -av dir1 /media/disk/backup/

$ rsync -av dir1/ /media/disk/backup/

　まとめると、

```bash
そのディレクトリも含めてコピーしたい場合：「/」なし
そのディレクトリ以下のツリーをコピーしたい場合：「/」あり
```

ということになる。なお、こうした末尾のスラッシュが与える影響を考えなければならないのは、コピー元の指定のみで、コピー先の指定では考慮する必要はない。

### 例

この例では、ORGDIRにある、DIRで始まるディレクトリとその中身だけ, DESTDIRにコピーする方法について説明する。

#### コピー元の確認

$ ls -R ORGDIR
ORGDIR:
DIR0/  DIR1/  DIR2/  DIR3/  TMP1/  TMP2/  TMP3/  TMP4/

ORGDIR/DIR0:
200202-1220.txt

ORGDIR/DIR1:
200202-1220.txt

ORGDIR/DIR2:
200202-1220.txt

ORGDIR/DIR3:
200202-1220.txt

ORGDIR/TMP1:
200202-1220.txt

ORGDIR/TMP2:
200202-1220.txt

ORGDIR/TMP3:
200202-1220.txt

ORGDIR/TMP4:
200202-1220.txt

#### コピー先の確認

/work05/manda/TEST
$ ls -R DSTDIR
DSTDIR:

#### スクリプト
```bash
#!/bin/bash

TIME=$(date -R);HOST=$(hostname);CWD=$(pwd)

ORG=ORGDIR
DST=DSTDIR

DRY_RUN="--dry-run"

EXC1=\*.o; EXC2="\*.exe"

EXCL="--exclude ${EXC1} --exclude ${EXC2}"

rsync -arv ${DRY_RUN} ${EXCL} ${ORGDIR}/ ${DSTDIR}
```




特定のファイルを除いてコピー
----------------------------------
### CP.WRF.sh 
/work05/manda/WRF.K17.2020-04-17

wrfinputで始まるファイルと, rslで始まるファイルの除いて、ディレクトリINDIRをカレントディレクトリのOUTDIRという名前でコピーする。

```bash
#!/bin/bash

if [ $# -ne 2 ]; then
COMMAND=$(basename $0)
echo
echo ERROR in $COMMAND : WRONG ARGUMENTS
echo
echo USAGE : $COMMAND INDIR OUTDIR
echo
exit 1
fi

INDIR=$1
OUTDIR=$2
rsync -av ${INDIR}/* ./${OUTDIR} --exclude "wrfinput*" --exclude \ "rsl.*.*"
```



ログアウトしてもコピーを続行
----------------------------------

## WRF.RSYNC.ALL.sh
```bash
#!/bin/bash

CASE=H30
RUN=R24
RUNNAME=${CASE}.${RUN}

export LANG=C

#LOGDIR=LOG_$(basename $0 .sh)
mkdir -vp $LOGDIR

LOGDIR=$(pwd)
LOG=$LOGDIR/$(basename $0 .sh).LOG
#LOG=$LOGDIR/$(basename $0 .sh)_$(date +"%Y%m%d").LOG
#LOG=$LOGDIR/$(basename $0 .sh)_$(date +"%Y%m%d_%H%M%S").LOG

#====================================
HOST=zamanda@hfront01.bosai.go.jp
ORGDIR_ROOT=${HOST}:/work07/thotspot/zamanda/WRF3.7.1/WRFV3.H30.TREND/test

DESTDIR_ROOT=/work06/manda/WRF.H30.TREND

#====================================

date -R       |tee -a $LOG
pwd           |tee -a $LOG
echo $0 $@    |tee -a $LOG
echo $RUNNAME |tee -a $LOG
echo $HOST    |tee -a $LOG
echo $ORGDIR_ROOT  |tee -a $LOG
echo $DESTDIR_ROOT |tee -a $LOG

echo
echo "GETTING WRFOUT FILES ..." |tee -a $LOG
echo

echo 
echo YOUR PASSWORD
echo
echo CTL+z
echo jobs -l
echo bg 1; disown -h %1
echo

ORGDIR=${ORGDIR_ROOT}/${RUNNAME}*

rsync -aqr ${ORGDIR} ${DESTDIR_ROOT} \
--exclude "wrfb_*" --exclude "wrfi*"  --exclude "wrfl*" \
--exclude "*tar"   --exclude "*CAM*"  --exclude "wrff*" \
--exclude "wrfrst*" --exclude "*exe"   --exclude "*DATA*" \
 |tee -a $LOG

echo |tee -a $LOG
ls -lh --time-style=long-iso ${RUNNAME}*/*_d??_* |tee -a $LOG
echo |tee -a $LOG

echo "DONE." |tee -a $LOG

```





```bash
$ WRF.RSYNC.ALL.sh
```
nohup: ignoring input and appending output to `nohup.out'
Enter passphrase for key '/home/manda/.ssh/id_rsa':
nb01607@ht210f.bosai.go.jp's password:
パスワード入力
```
CTL+Z
```
[1]+  停止                  WRF.RSYNC.ALL.sh

```
$ jobs -l
```
[1]+ 15841 停止しました      WRF.RSYNC.ALL.sh

```bash
$ bg 1; disown -h %1
```
bg 1で, ジョブ番号[1]をバックグランドジョブにして、disown -hでログアウトしても実行するように設定する。**-h**が重要





コピーする際に、特定の名前のディレクトリを除く
----------------------------------------------------------------------
例：dirAにある outputとpostというディレクトリを除いて，dirBにコピーする。
```
cd $dirA; tar czf - . --exclude output --exclude post | (mkdir -p $dirB; tar xzf - -C $dirB)
```
実行例
./03.Test_Interpolation/の下にあるファイルとディレクトリを../05.Momentum/03.Test1Dにコピーする
```
$ dirA="./03.Test_Interpolation/"
$ dirB="../05.Momentum/03.Test1D"
$ cd $dirA; tar czf - . --exclude output --exclude post | (mkdir -p $dirB; tar xzf - -C $dirB); cd ..
```
結果
元
```
$ ls 03.Test_Interpolation/
0.Readme.txt               input/       post/        tmp_runlog_2534.txt
0.a_03.Test_Interpolation  log/         runcold.sh*  tmp_stdout_2534.txt
Bak100508/                 nohup.out    runhot.sh*   ts_loc.txt
BathyFig/                  output/      src/
fort.41                    oyasumi.sh*  srcbackup/
```
コピー
```
$ ls 05.Momentum/03.Test1D
0.Readme.txt               fort.41    oyasumi.sh*  srcbackup/
0.a_03.Test_Interpolation  input/     runcold.sh*  tmp_runlog_2534.txt
Bak100508/                 log/       runhot.sh*   tmp_stdout_2534.txt
BathyFig/                  nohup.out  src/         ts_loc.txt
```
参考
http://okwave.jp/qa/q1982179.html



ログをとる
----------------------------------------------------------------------

$ cat BACKUP.sh 
```bash
#!/bin/bash

DRY= #"--dry-run"
OPT="$DRY -avz --delete --log-file=/var/log/rsync-$(date +"%Y%m%d-%H%M").log"

ORG=/work01; BAK=/work02
rsync $OPT $ORG $BAK

ORG=/work03; BAK=/work04
rsync $OPT $ORG $BAK 

#
# rsyncでファイル転送を行った後にどのファイルが転送されたのか確認したい場合があります。
# rsyncには--log-fileがありこれを指定すればログを書き出してくれます。
# https://www.rootlinks.net/2016/02/02/rsync%E3%81%AE%E3%83%AD%E3%82%B0%E3%82%AA%E3%83%97%E3%82%B7%E3%83%A7%E3%83%B3-log-file/

```

