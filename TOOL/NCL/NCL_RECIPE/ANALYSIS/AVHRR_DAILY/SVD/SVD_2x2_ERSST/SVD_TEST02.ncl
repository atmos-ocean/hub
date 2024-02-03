; 
; SVD_TEST02.ncl
; 
; Sun, 05 Jan 2020 14:59:59 +0900
; calypso.bosai.go.jp
; /work05/manda/NCL/SVD
; manda
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

script_name  = get_script_name()
script=systemfunc("basename "+script_name+ " .ncl")
LOG=script+".LOG"

NOW=systemfunc("date '+%y%m%d_%H%M' ")
;print("NOW="+NOW)
; LOG=script+NOW+".LOG"

NOW=systemfunc("date -R")
HOST=systemfunc("hostname")
CWD=systemfunc("pwd")

print("script="+script_name)

FINFO=systemfunc("ls -lh --time-style=long-iso "+script_name)

LOG_HEADER = (/"# "+ NOW, "# "+ HOST, "# "+ CWD, "# "+ FINFO /)
hlist=[/LOG_HEADER/]

write_table(LOG, "w", hlist, "%s")



  y1      = 1958                    ; 最初の年
  y2      = 2015                    ; 最後の年
  nsvd    =    3                    ; 求めるモード数

  indir1="/work05/manda/DATA_PROC/JRA55/Monthly/OUT_NC_CAT/"
  indir2="/work05/manda/DATA/SST/ERSST/V4/200104/"

  infile1 = indir1+"hgt.1958-2015.nc"    ; 変数1(Z500)のデータ
  infile2 = indir2+"sst.mnmean.v4.nc"    ; 変数2(SST)のデータ
  out     = "eps"                   ; 出力形式

;print("")
;system("ncdump -h "+infile1)
;print("")
;system("ncdump -h "+infile2)
;print("")

  ;;; 変数1(Z500)のデータを読む
  in1     = addfile(infile1,"r")             ; 読み込むファイルを取得   
  time    = in1->time                        ; 時間の配列を取得
  YYYYMM  = cd_calendar(time,-1)             ; timeからYYYYMMの配列を作成
  t1      = ind(YYYYMM.eq.y1*100+1)          ; 1958年1月に対応するインデックスを取得
  t2      = ind(YYYYMM.eq.y2*100+12)         ; 2015年12月に対応するインデックスを取得
  hgt     = in1->hgt(t1:t2:12,{500},:,:)     ; Z500の値を取得。{500}で500hPa面を指定(高さ次元の中身をあらかじめ知っている)。
                                             ; 時間次元はt1からt2まで12おき
                                             ; これは1958年から2015年の1月の値のみ切り取ることに対応
  lat1    = in1->lat                         ; Z500に対応する緯度の配列を取得
  lon1    = in1->lon                         ; Z500に対応する経度の配列を取得
lat1=tofloat(lat1)
lon1=tofloat(lon1)
  hgt!0   = "time"                           ; hgtに座標をつけておく
  hgt!1   = "lat"
  hgt!2   = "lon"
  delete([/time,YYYYMM/])                    ; timeとYYYYMMの配列を消しておく
                                             ; (SSTデータの読み込みでも同じ変数名を使うから)
printVarSummary(hgt)



  ;;; 変数2(SST)のデータを上と同様にして読む
  in2     = addfile(infile2,"r")
  time    = in2->time
  YYYYMM  = cd_calendar(time,-1)
  t1      = ind(YYYYMM.eq.y1*100+1)
  t2      = ind(YYYYMM.eq.y2*100+12)
  sst     = in2->sst(t1:t2:12,:,:)
  sst     = dtrend_msg_n(ispan(y1,y2,1),sst,True,False,0)
  lat2    = in2->lat
  lon2    = in2->lon
  sst!0   = "time"
  sst!1   = "lat"
  sst!2   = "lon"
  delete([/time,YYYYMM/])
printVarSummary(sst)




  ;;; SVD解析の下ごしらえをしましょう
  ;;; まずは変数1(Z500)に対して
  LAT1  = ind(lat1.ge.20.and.lat1.lt.89)      ; 変数1のSVDの領域(緯度範囲)を指定
  LON1  = ind(lon1.ge.105.and.lon1.le.255)    ;                 (経度範囲)
  var1  = hgt(lat|LAT1,lon|LON1,time|:)       ; Z500の必要な領域を切り出し
                                              ; ただし，時間と空間の次元の順番を入れ替える
  dim1  = dimsizes(var1)                      ; 各次元の大きさを左からNY1,NX1,NT1とする
  NY1   = dim1(0)
  NX1   = dim1(1)
  NT1   = dim1(2)
 ;;; 各グリッドの面積の重みをかける
 ;;; SVD解析では共分散行列の各成分に対して面積の重みをかけるべきなので，
 ;;; 偏差に対しては面積の平方根の重みをかけることに注意
;  wgt1  = sqrt(cos(lat1(LAT1)*atan(1.)/45.))  ; √cos(緯度)の重み(緯度幅は一様)
  wgt1  = tofloat(sqrt(cos(lat1(LAT1)*atan(1.)/45.)) )
  NW1   = dimsizes(wgt1)

;  printVarSummary(var1)
;  printVarSummary(wgt1)


  do j = 0, NW1-1
    var1(j,:,:) = var1(j,:,:) * wgt1(j)
  end do
  ;;; var1の形を変える。緯度経度の次元をまとめて，空間×時間の2次元配列とする。
  var1 := reshape(var1,(/NY1*NX1,NT1/))
  ;;; SVDの関数は未定義値が含まれているとうまく動かない。そこで，未定義値を取り除く必要がある。
  ;;; いま，(SSTデータの陸地のように)時間によらず同じ空間的位置に未定義値が含まれているとする
  NMS1  = ind(.not.ismissing(var1(:,0)))      ; 空間次元のうち未定義でない場所を取得
  var1 := var1(NMS1,:)                        ; 未定義でない値だけ切り出して配列を小さくする
                                              ; ここで定義したNMS1はあとでもとの大きさに戻すときに必要

  ;;; 同様に変数2(SST)に対しても下ごしらえをしましょう 
  LAT2  = ind(lat2.ge.-20.and.lat2.le.20)
  LON2  = ind(lon2.ge.30.and.lon2.le.290)
  var2  = sst(lat|LAT2,lon|LON2,time|:)
  dim2  = dimsizes(var2)
  NY2   = dim2(0)
  NX2   = dim2(1)
  NT2   = dim2(2)                             ; NT1と同じでなくてはならない 
  wgt2  = sqrt(cos(lat2(LAT2)*atan(1.)/45.))
  NW2   = dimsizes(wgt2)
  do j = 0, NW2-1
    var2(j,:,:) = var2(j,:,:) * wgt2(j)
  end do
  var2 := reshape(var2,(/NY2*NX2,NT2/))
  NMS2  = ind(.not.ismissing(var2(:,0)))
  var2 := var2(NMS2,:)



print("")
print("Done " + script_name)
print("")
print("LOG: "+LOG)
print("")

end
