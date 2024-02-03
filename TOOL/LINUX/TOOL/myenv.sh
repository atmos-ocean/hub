cat /proc/cpuinfo|grep processor | tail -1; cat /proc/cpuinfo|grep "model name" |tail -1; cat /proc/meminfo|head -1; cat /etc/`ls /etc -F | grep "release$\|version$"`
