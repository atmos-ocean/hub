echo MMMMM Ex 1
cat <<EOF
date
EOF
echo
echo MMMMM RESULT 1
date
echo;echo


echo MMMMM Ex 2
cat <<EOF
date -R
EOF
echo
echo MMMMM RESULT 2
date -R
echo;echo


echo MMMMM Ex 3
cat <<"EOF"
date +"%Y-%m-%d_%H:%M:%S"
EOF
date +"%Y-%m-%d_%H:%M:%S"

echo
echo MMMMM Ex 3
cat <<"EOF"
date +"%Y-%m-%d_%H:%M:%S"
echo $T1
EOF
echo MMMMM RESULT 3
date +"%Y-%m-%d_%H:%M:%S"
echo $T1
echo;echo


echo MMMMM Ex 4
cat <<"EOF"
yyyy1=2021; mm1=08; dd1=21
start=${yyyy1}/${mm1}/${dd1}
jsstart=$(date -d${start} +%s)
echo $jsstart SECONDS
jhstart=$(expr $jsstart / 3600)
echo $jhstart HOURS
EOF
echo
echo MMMMM RESULT 4
yyyy1=2021; mm1=08; dd1=21
start=${yyyy1}/${mm1}/${dd1}
jsstart=$(date -d${start} +%s)
echo $jsstart SECONDS
jhstart=$(expr $jsstart / 3600)
echo $jhstart HOURS
echo; echo


echo MMMMM Ex 5
cat <<"EOF"
yyyy1=2021; mm1=08; dd1=21; hh1=0
start="${yyyy1}/${mm1}/${dd1} ${hh1}:00:00"
jsstart=$(date -d"${start}" +%s)
echo $jsstart SECONDS
jhstart=$(expr $jsstart / 3600)
echo $jhstart HOURS
EOF
echo
echo MMMMM RESULT 5
yyyy1=2021; mm1=08; dd1=21; hh1=0
start="${yyyy1}/${mm1}/${dd1} ${hh1}:00:00"
jsstart=$(date -d"${start}" +%s)
echo $jsstart SECONDS
jhstart=$(expr $jsstart / 3600)
echo $jhstart HOURS
echo; echo


echo MMMMM Ex 6
cat <<"EOF"
yyyy2=2021; mm2=08; dd2=21; hh2=1
end="${yyyy2}/${mm2}/${dd2} ${hh2}:00:00"
jsend=$(date -d"${end}" +%s)
jhend=$(expr $jsend / 3600)
echo $jhend HOURS
EOF
echo
echo MMMMM RESULT 6
yyyy2=2021; mm2=08; dd2=21; hh2=1
end="${yyyy2}/${mm2}/${dd2} ${hh2}:00:00"
jsend=$(date -d"${end}" +%s)
jhend=$(expr $jsend / 3600)
echo $jhend HOURS
echo
