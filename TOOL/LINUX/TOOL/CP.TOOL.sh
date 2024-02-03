#!/bin/bash

usage(){
  echo USAGE $(basename $0) [-h][-d] ARG ; exit 0
}

flagh="false"; flagd="false"; flagb="false"
while getopts hdl OPT; do
  case $OPT in
    "h" ) usage ;;
    "d" ) flagd="true";;
    "b" ) flagg="true";;
     *  ) usage
  esac
done
shift $(expr $OPTIND - 1)

if [ $# -lt 1 ]; then
echo; echo "ERROR in $(basename $0): NO ARGUMENT"
usage; exit 1
fi

DRY_RUN=
if [ $flagd = "ture" ]; then DRY_RUN="--dry_run"; fi

INC1=\*sh;   INC2=\*csh;  INC3=\*bash; INC4=\*py;  INC5=\*PY; 
INC6=\*gs;   INC7=\*GS;   INC8=\*f;    INC9=\*F;   INC10=\*f90;
INC11=\*F90; INC12=\*pl;  INC13=\*PL;  INC14=\*ctl;INC15=\*CTL;
INC16=\*c;   INC17=\*cpp; INC18=\*h;   INC19=\*README\*
INC90=Make\*;INC91=make\*
INC="--include ${INC1}  --include ${INC2}  --include ${INC3}  \
     --include ${INC4}  --include ${INC5}  --include ${INC6}  \
     --include ${INC7}  --include ${INC8}  --include ${INC9}  \
     --include ${INC10} --include ${INC11} --include ${INC12} \
     --include ${INC13} --include ${INC14} --include ${INC15} \
     --include ${INC16} --include ${INC17} --include ${INC18} \
     --include ${INC19} \
     --include ${INC90} --include ${INC91} \
     --include "*/" \
     "

ORG=$1
if [ ! -f $ORG -a ! -d $ORG ]; then
echo ERROR in $0 : NO SUCH FILE/DIR, $ORG; exit 1
fi

INFLE=$(basename $1)

echo $INFLE
#INPFX=${INFLE%.*}

TMP=$(date "+%Y-%m-%d_%H")_$INFLE #$INPFX

DST=/work04/manda/00.TOOL/$TMP

mkdir -vp $DST 

CWD=$(pwd); TIME=$(date -R)

OFLE=$DST/000.README_$(basename $0 .sh).TXT

touch $OFLE
echo  $0    > $OFLE; echo  >>$OFLE
echo  $TIME>> $OFLE; echo  >>$OFLE
echo  $ORG >> $OFLE; echo  >>$OFLE

echo "mmm GETTING $ORG ..."

OPT="-ar"
if [ $flagd = "true" ];then OPT="-avn" ; fi

rsync ${OPT} $INC --exclude * ${ORG}/ ${DST} 

# a: ファイルの属性をできる限り保持する
# r: 下層のディレクトリ・ファイルもコピーする
# q: メッセージ表示を少なくする

#expect -c "
#set timeout 10
#spawn rsync -arq ${DRY_RUN} $INC --exclude="*" ${ORG}/ ${DST} 
#        expect \"zamanda@hfront01.bosai.go.jp's password:\"
#        send \"Qms+7Ffcu4\n\"
#interact
#"

echo; echo "mmm DONE $(basename $0)."; echo 
echo "mmm ORG : $ORG"; echo; 
echo "mmm DST : $DST"; echo

ls -lh $DST

exit 0
