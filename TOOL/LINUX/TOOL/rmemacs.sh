#!/bin/bash

date +"%Y%m%d-%H%M"


#2019-11-27_14-57 
#/lustre1/home1/zamanda/mybin
#zamanda@hfront01
#$ cat rmemacs.sh 
#!/bin/bash

# Delete back-ups of Emacs file

usage(){
cat <<EOF
EOF

Usage : $(basename $0) [-h]

-h : Print this help

EOF
}


flag="false"
while getopts t OPT; do
  case $OPT in
    "t" ) flag="true" ;  ;;
     * ) usage
  esac
done

if [ $flag = "true" ]; then
  usage
  exit 0
fi

shift $(expr $OPTIND - 1)


find . -name '*~' -print | xargs rm -v
