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

