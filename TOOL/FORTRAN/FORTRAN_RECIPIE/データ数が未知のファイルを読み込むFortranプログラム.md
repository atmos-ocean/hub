# データ数が未知のファイルを読み込むFortranプログラム



```
count_vaild_dataのラベルのついたdoループ
```

```
#で始まる行はコメント行とみなしスキップし、コメント行以外の行数をカウントします。
```

```
skip_commentのラベルのついたdoループ
```

```
#で始まる行はコメント行とみなしスキップし、コメント行以外の行のデータを読み込みます。
```

```
infle: 入力ファイル名
```

```
strm: 1行のデータを一時的に読み込む変数
```

```
NM: 総データ数
```

```
x(:): 入力データを収納する配列
```



```Fortran
program test_read
character(len=500)::infle
character(len=2000)::strm

real,allocatable::x(:) 

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
NM=n
rewind(11)
allocate(x(NM))

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
do n=1,NM
  print *,'x(', n ,')=',x(n)
end do
print *
```

