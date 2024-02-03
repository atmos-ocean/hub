https://webkaru.net/linux/groupadd-command/
https://eng-entrance.com/linux-command-usermod

2019-08-22_15-20 
/work03/am
am@Precision-5820-Tower
$ sudo groupadd lab

2019-08-22_15-20 
/work03/am
am@Precision-5820-Tower
$ sudo cat /etc/group | grep lab
lab:x:1003:

2019-08-22_15-21 
/work03/am
am@Precision-5820-Tower
$ sudo usermod -G lab am

2019-08-22_15-21 
/work03/am
am@Precision-5820-Tower
$ sudo usermod -G lab takehata

2019-08-22_15-21 
/work03/am
am@Precision-5820-Tower
$ sudo usermod -G lab sano

2019-08-22_15-21 
/work03/am
am@Precision-5820-Tower
$ grep sano /etc/passwd
sano:x:1002:1002::/work03/sano:/bin/bash

2019-08-22_15-21 
/work03/am
am@Precision-5820-Tower
$ grep am /etc/passwd
games:x:5:60:games:/usr/games:/usr/sbin/nologin
am:x:1000:1000:am,,,:/home/am:/bin/bash

2019-08-22_15-22 
/work03/am
am@Precision-5820-Tower
$ grep takehata /etc/passwd
takehata:x:1001:1001::/work03/takehata:/bin/bash

2019-08-22_15-22 
/work03/am
am@Precision-5820-Tower
$ sudo id am
uid=1000(am) gid=1000(am) groups=1000(am),1003(lab)

2019-08-22_15-22 
/work03/am
am@Precision-5820-Tower
$ sudo id takehata
uid=1001(takehata) gid=1001(takehata) groups=1001(takehata),1003(lab)

2019-08-22_15-23 
/work03/am
am@Precision-5820-Tower
$ sudo id sano
uid=1002(sano) gid=1002(sano) groups=1002(sano),1003(lab)