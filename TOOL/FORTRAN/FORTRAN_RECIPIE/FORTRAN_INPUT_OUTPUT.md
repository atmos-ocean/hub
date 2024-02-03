FORTRAN INPUT/OUTPUT
================================
バイナリ出力 (直接アクセス)
------------------------------------------------------
```Fortran
open(IU,file=trim(OFLE),form='unformatted',&
access='direct',recl=IM*JM*4)
irec=0

! 2D DATA
irec=irec+1
write(IU,rec=irec)((XLAT(I,J),I=1,IM),J=1,JM)
irec=irec+1
write(IU,rec=irec)((XLONG(I,J),I=1,IM),J=1,JM)

! 3D DATA
DO K=1,KM
irec=irec+1
write(IU,rec=irec)((LWDN(I,J,K),I=1,IM),J=1,JM)
END DO !K

close(IU)
```

データ数が未知のファイルを読み込む
------------------------------------------------------
test_read.f90

```fortran
program test_read

character(len=500)::infle
real,allocatable::x(:) 
character(len=2000)::strm

namelist /para/infle

read(*,nml=para)

open(11,file=infle,action="read")

n=0
count_vaild_data: do
  read(11,'(A)',iostat=ios)strm
  if(ios<0)exit
  if(strm(1:1) == "#")then
    cycle
  else
    n=n+1
  endif
enddo count_vaild_data

nt=n

rewind(11)
allocate(x(nt))

n=0
skip_comment: do
  read(11,'(A)',iostat=ios)strm
  if(ios<0)exit
  if(strm(1:1) == "#")then
    cycle
  else
    n=n+1
    read(strm,*)x(n)
  endif
enddo skip_comment

print *
do n=1,nt
  print *,'x(', n ,')=',x(n)
end do

print *
end program test_read
```

count_vaild_dataのラベルのついたdoループ

#で始まる行はコメント行とみなしスキップし、コメント行以外の行数をカウントします。

skip_commentのラベルのついたdoループ

#で始まる行はコメント行とみなしスキップし、コメント行以外の行のデータを読み込みます。

infle: 入力ファイル名

strm: 1行のデータを一時的に読み込む変数

nt: 総データ数

x(:): 入力データを収納する配列



実行用シェルスクリプト

test_read.run.sh

```
#/bin/bash

exe=test_read

infle="test_data.txt"

cat << EOF > $infle

# Comment 1
# Comennt 2
1
2
3
# Comment 4
4
5
EOF

echo "${infle}: "
cat $infle
echo
namelist=${exe}.namelist.txt

cat <<EOF > $namelist
&para
infle=$infle
&end
EOF

$exe < $namelist

exit 0
```

実行例
```
$ chmod u+x *.sh
$ test_read.run.sh
test_data.txt:
# Comment 1
# Comennt 2
1
2
3
# Comment 4
4
5
```
```
 x(           1 )=   1.000000
 x(           2 )=   2.000000
 x(           3 )=   3.000000
 x(           4 )=   4.000000
 x(           5 )=   5.000000
```