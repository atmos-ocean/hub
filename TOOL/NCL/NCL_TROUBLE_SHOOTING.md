# NCL Trouble Shooting

## warning:VarVarWrite

warning:VarVarWrite: rhs has no dimension name or coordinate variable, deleting name of lhs dimension number (1) and destroying coordinate var,  use "(/../)" if this is not desired outcome  

例：  

```
mlt!0="time"  
mlt!1="lat"  
mlt!2="lon"  
mlt(n,:,:) = mlt_tmp(:,:)  
```

対応  
mlt_tmpの各次元にも名前をつける  

```
mlt_tmp!0="lat"  
mlt_tmp!1="lon"  
```



## check_for_y_lat_coord

### Warning: Data either does not contain a valid latitude coordinate array or doesn't contain one at all.

A valid latitude coordinate array should have a '**units**'
attribute equal to one of the following values: 
    'degrees_north' 'degrees-north' 'degree_north' 'degrees north' 'degrees_N' 'Degrees_north' 'degree_N' 'degreeN' 'degreesN' 'deg north'



## segmentation fault

使用する変数の数が多くなると  
segmentation fault  

というエラーが表示されることがある。  
そのときは、  
delete  
を使って、不要な変数を削除する。  

```
delete([/a,b,c/]) 
```


とすると、a,b,cという変数をまとめて削除できる。  

他にユーザーがいない場合、他にメモリを食っているプロセスが無い場合には、  

```
$ ulimit -s unlimited 
```


を使って、スタックサイズを大きくする。  

スタックとは一時的な記憶領域のことである。詳しくは、下記サイトに大変分かりやすい説明がある。  
https://www.uquest.co.jp/embedded/learning/lecture07-1.html  



# addfiles

addfilesではなくaddfileを使う  
ホームページのサンプルで、  
addfilesでファイルを開くと、    

fatal:(a) not reference to a valid file  

というエラーが出て、データを読めないことがる。  

単一の入力ファイルを使うときは、addfileを使うこと。  

http://mailman.ucar.edu/pipermail/ncl-talk/2015-July/003345.html  
wrf_user_intrp3dのマニュアルの誤植  



## write_table  

write_table実行時にSegmentation fault  で実行停止してしまう  

出力先のディレクトリが存在していないと、Segmentation fault となる。  

対策  

```
system("mkdir -vp "+outdir)  
```

のようにして、ディレクトリを前もって作成しておく  



