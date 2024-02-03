# bash変数の一部を取り出す

CUT.STRING.sh

```bash
#!/bin/bash

echo
HOGE=A_BB_CCC_DDDD_EEEEE
echo "HOGE = $HOGE"

echo
MOGE=${HOGE%_*} 
echo '${HOGE%_*} = '$MOGE

echo
MOGE=${HOGE%%_*} 
echo '${HOGE%%_*} = '$MOGE

echo
MOGE=${HOGE#*_}
echo '${HOGE#*_} = '$MOGE

echo
MOGE=${HOGE##*_}
echo '${HOGE##*_} = '$MOGE

echo
echo
echo

HOGE=A.BB.CCC.DDDD.EEEEE
echo "HOGE = $HOGE"

echo
MOGE=${HOGE%.*} 
echo '${HOGE%.*} = '$MOGE

echo
MOGE=${HOGE%%.*} 
echo '${HOGE%%.*} = '$MOGE

echo
MOGE=${HOGE#*.}
echo '${HOGE#*.} = '$MOGE

echo
MOGE=${HOGE##*.}
echo '${HOGE##*.} = '$MOGE
```



```
$ CUT.STRING.sh

HOGE = A_BB_CCC_DDDD_EEEEE

${HOGE%_*} = A_BB_CCC_DDDD

${HOGE%%_*} = A

${HOGE#*_} = BB_CCC_DDDD_EEEEE

${HOGE##*_} = EEEEE



HOGE = A.BB.CCC.DDDD.EEEEE

${HOGE%.*} = A.BB.CCC.DDDD

${HOGE%%.*} = A

${HOGE#*.} = BB.CCC.DDDD.EEEEE

${HOGE##*.} = EEEEE
```

