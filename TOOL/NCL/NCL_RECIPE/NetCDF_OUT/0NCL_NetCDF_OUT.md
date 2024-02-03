# NCLでのNetCDFの出力

[[_TOC_]]

## 出力ファイルを開く

```fortran
diro="/work01/DATA/J-OFURO3/V1.2_PRE/DAILY_CLIM/"
system("mkdir -vp "+diro)
filo="DAILY_CLIM_SMO_"+ys+"-"+ye+"_"+VAR+".nc"
print("MMMMM filo="+filo)
out=diro+filo
; 同名のファイルが存在していたらまず消去
system("/bin/rm -f "+out)

; 出力ファイルを開く
out    = addfile (out, "c")
```

## データ出力

### 略式

**xClmDay_sm**が**スクリプトの中**で使用している変数の名称，<u>VAR_NC</u>が<u>書き込み先のファイルの中</u>での変数の名称とすると，下記のようになる。

```
out->VAR_NC = xClmDay_sm
```



### 正式

```fortran
print("MMMMM CREATE NETCDF")
; 出力データの次元の設定
dimx   = dimsizes(xClmDay_sm)
ntim   = dimx(0)
nlat   = dimx(1)
mlon   = dimx(2)
year_day=xClmDay_sm&year_day

; ファイル属性(ファイルに関する情報)の設定
filAtt = 0
filAtt@title = vName+": Daily Climatology: "+yrStrt+"-"+yrLast  
filAtt@input_directory_ = INDIR
filAtt@creation_date = systemfunc("date -R")
fileattdef( out, filAtt )           ; copy file attributes  

setfileoption(out,"DefineMode",True)
; 変数名の設定
varNC_sm   = "VAR_NC"

;変数の設定
; 次元
dimNames = (/"year_day", "latitude", "longitude"/)  
dimSizes = (/ ntim , nlat,  mlon/) 
dimUnlim = (/ True , False, False/)   
filedimdef(out,dimNames,dimSizes,dimUnlim)

filevardef(out, "year_day"  ,typeof(year_day),  getvardims(year_day)) 
filevardef(out, "latitude"  ,typeof(latitude),  getvardims(latitude)) 
filevardef(out, "longitude" ,typeof(longitude), getvardims(longitude))
filevardef(out, varNC_sm,typeof(xClmDay_sm),getvardims(xClmDay_sm))    

; 属性
filevarattdef(out,"year_day"  ,year_day)   ; copy time attributes
filevarattdef(out,"latitude"  ,latitude)   ; copy lat attributes
filevarattdef(out,"longitude" ,longitude)  ; copy lon attributes
filevarattdef(out,varNC_sm, xClmDay_sm)                

; 変数の値の書き出し
out->year_day   = (/year_day/)     
out->latitude   = (/latitude/)
out->longitude  = (/longitude/)
out->$varNC_sm$ = (/xClmDay_sm/)
; 書き込み先のファイルの中での変数の名称をvarNC_smの変数の中身(今の場合VAR_NC)とする。
; 書き出す変数の値はxClmDay_smの値
print("MMMMM OUTPUT:")
print(out)
```

