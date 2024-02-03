; 
; SVD_TEST.ncl
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
;system("ncdump -c "+infile1)
;print("")
;system("ncdump -h "+infile2)
;print("")

print_clock("PREPROCESS Z500")
  ;;; 変数1(Z500)のデータを読む
  in1     = addfile(infile1,"r")             ; 読み込むファイルを取得


  time    = in1->time                        ; 時間の配列を取得
  YYYYMM  = cd_calendar(time,-1)             ; timeからYYYYMMの配列を作成
  t1      = ind(YYYYMM.eq.y1*100+1)          ; 1958年1月に対応するインデックスを取得
  t2      = ind(YYYYMM.eq.y2*100+12)         ; 2015年12月に対応するインデックスを取得

;;;;;
print("t1="+t1+" YYYYMM="+YYYYMM(t1))
utc_date = ut_calendar(time(t1), 0)
year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
date_str = sprinti("%0.4i", year)+" "+ sprinti("%0.2i", month)
print("date_str="+date_str)

print("t2="+t2+" YYYYMM="+YYYYMM(t2))
utc_date = ut_calendar(time(t2), 0)
year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
date_str = sprinti("%0.4i", year)+" "+ sprinti("%0.2i", month)
print("date_str="+date_str)
;;;;;


  hgt     = in1->hgt(t1:t2:12,{50000},:,:)     ; Z500の値を取得。{500}で500hPa面を指定(高さ次元の中身をあらかじめ知っている)。
                                             ; 時間次元はt1からt2まで12おき
                                             ; これは1958年から2015年の1月の値のみ切り取ることに対応
;;;
print_clock("CHECK Z500 RAW")
OUTDIR_TMP="OUT_SVD_TEST04/"
system("mkdir -vp "+OUTDIR_TMP)
dim=dimsizes(hgt)
nt=dim(0)
print("nt="+nt)
;print("hgt "+hgt(nt-1,:,:))

time_tmp=in1->time(t1:t2:12)

do n=0,nt-1
;print("time_tmp="+time_tmp(n))
;print("n="+n+" YYYYMM="+YYYYMM(n*12))
;TEMP_OUT=OUTDIR_TMP+"hgt"+YYYYMM(n*12)
;print("OUTPUT: "+TEMP_OUT)

  ;;; ここからお絵描きの設定
  res1 = True                                  ; Z500の描画に用いるresource
  res1@gsnDraw          = False                ; gsn_csm_contour_map_ce実行時にDrawしない
  res1@gsnFrame         = False                ; gsn_csm_contour_map_ce実行時にFrame更新しない
  res1@cnFillOn         = True                 ; シェードを使う
res1@cnLineLabelsOn             = True
res1@cnLevelSelectionMode = "ManualLevels"	; manually set the contour levels with the following 3 resources
res1@cnMinLevelValF  = 5000.			; set the minimum contour level
res1@cnMaxLevelValF  = 5900.			; set the maximum contour level
res1@cnLevelSpacingF = 50.			; set the interval between contours
res1@mpCenterLonF           = 180
;  wks   = gsn_open_wks("eps",TEMP_OUT)
;  plot1 = gsn_csm_contour_map_ce(wks,hgt(n,:,:),res1)
;  draw(plot1)
;  frame(wks)
;  delete([/plot1,res1/])
delete(res1)
end do
;;;



  hgt     = dtrend_msg_n(ispan(y1,y2,1),hgt,True,False,0)
                                             ; 平均とトレンドの除去

;;;
print_clock("CHECK Z500 DETREND")
printVarSummary(hgt)
OUTDIR_TMP="OUT_SVD_TEST04/"
system("mkdir -vp "+OUTDIR_TMP)
dim=dimsizes(hgt)
nt=dim(0)
print("nt="+nt)
;print("hgt "+hgt(nt-1,:,:))

time_tmp=in1->time(t1:t2:12)

do n=0,nt-1
;print("time_tmp="+time_tmp(n))
;print("n="+n+" YYYYMM="+YYYYMM(n*12))
;TEMP_OUT=OUTDIR_TMP+"hgt_dtrnd"+YYYYMM(n*12)
;print("OUTPUT: "+TEMP_OUT)

  ;;; ここからお絵描きの設定
  res1 = True                                  ; Z500の描画に用いるresource
  res1@gsnDraw          = False                ; gsn_csm_contour_map_ce実行時にDrawしない
  res1@gsnFrame         = False                ; gsn_csm_contour_map_ce実行時にFrame更新しない
  res1@cnFillOn         = True                 ; シェードを使う
res1@cnLineLabelsOn             = True
res1@cnLevelSelectionMode = "ManualLevels"	; manually set the contour levels with the following 3 resources
res1@cnMinLevelValF  = -100.			; set the minimum contour level
res1@cnMaxLevelValF  =  100.			; set the maximum contour level
res1@cnLevelSpacingF = 10.			; set the interval between contours
res1@mpCenterLonF           = 180
;  wks   = gsn_open_wks("eps",TEMP_OUT)
;  plot1 = gsn_csm_contour_map_ce(wks,hgt(n,:,:),res1)
;  draw(plot1)
;  frame(wks)
;  delete([/plot1,res1/])
delete(res1)
end do
;;;


  lat1    = in1->lat                         ; Z500に対応する緯度の配列を取得
  lon1    = in1->lon                         ; Z500に対応する経度の配列を取得
  hgt!0   = "time"                           ; hgtに座標をつけておく
  hgt!1   = "lat"
  hgt!2   = "lon"
  delete([/time,YYYYMM/])                    ; timeとYYYYMMの配列を消しておく
                                             ; (SSTデータの読み込みでも同じ変数名を使うから)
printVarSummary(hgt)



print_clock("READ SST")
  ;;; 変数2(SST)のデータを上と同様にして読む
  in2     = addfile(infile2,"r")
  time    = in2->time
  YYYYMM  = cd_calendar(time,-1)
  t1      = ind(YYYYMM.eq.y1*100+1)
  t2      = ind(YYYYMM.eq.y2*100+12)

;;;;;
print("t1="+t1+" YYYYMM="+YYYYMM(t1))
utc_date = ut_calendar(time(t1), 0)
year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
date_str = sprinti("%0.4i", year)+" "+ sprinti("%0.2i", month)
print("date_str="+date_str)

print("t2="+t2+" YYYYMM="+YYYYMM(t2))
utc_date = ut_calendar(time(t2), 0)
year   = tointeger(utc_date(:,0))    ; Convert to integer for
month  = tointeger(utc_date(:,1))    ; use sprinti 
date_str = sprinti("%0.4i", year)+" "+ sprinti("%0.2i", month)
print("date_str="+date_str)
;;;;;


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
print_clock("PREPROCESS Z500")

  ;;; まずは変数1(Z500)に対して
  LAT1  = ind(lat1.ge.20.and.lat1.lt.89)      ; 変数1のSVDの領域(緯度範囲)を指定
  LON1  = ind(lon1.ge.105.and.lon1.le.255)    ;                 (経度範囲)

;print("LAT1 "+LAT1)

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
print_clock("PREPROCESS SST")
  ;;; 同様に変数2(SST)に対しても下ごしらえをしましょう 
  LAT2  = ind(lat2.ge.-20.and.lat2.le.20)
  LON2  = ind(lon2.ge.30.and.lon2.le.290)

;print("LAT2 "+LAT2)

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

  printVarSummary(NMS1)
  printVarSummary(NMS2)



print_clock("SVDCOV_SV ")
  ;;; SVDを実行。今回は特異ベクトルも同質・異質相関マップも両方求める
  ;;; まずはsvdcov_svを用いて特異ベクトルを計算
  var1_svc = new((/nsvd,dimsizes(NMS1)/),float)             ; 変数1(Z500)の特異ベクトル
  var2_svc = new((/nsvd,dimsizes(NMS2)/),float)             ; 変数2(SST)の特異ベクトル
  pcvar_a  = svdcov_sv(var1,var2,nsvd,var1_svc,var2_svc)    ; SVD!

print_clock("SVDCOV")
  var1_hom = new((/nsvd,dimsizes(NMS1)/),float)             ; 変数1(Z500)の同質相関マップ
  var1_het = new((/nsvd,dimsizes(NMS1)/),float)             ; 変数2(SST)の同質相関マップ
  var2_hom = new((/nsvd,dimsizes(NMS2)/),float)             ; 変数1(Z500)の異質相関マップ
  var2_het = new((/nsvd,dimsizes(NMS2)/),float)             ; 変数2(SST)の異質相関マップ
  pcvar_b  = svdcov(var1,var2,nsvd,var1_hom,var1_het,var2_hom,var2_het)  ; SVD!

  ;;; 同じSVDを行っているのでpcvar_aとpcvar_bは同一の値が入るはずである(attributeは異なる)
  ;;; pcvar_bのほうには，@akとして変数1の時間関数が，@bkとして変数2の時間関数が付随している。
  ;;; ただし，時間関数はnsvd×時間の長さの1次元配列であることに注意
  hgt_pc   = reshape(pcvar_b@ak,(/nsvd,NT1/))               ; 変数1(Z500)の時間関数
  sst_pc   = reshape(pcvar_b@bk,(/nsvd,NT2/))               ; 変数2(SST)の時間関数

  ;;; ちなみに，時間関数は元のデータを特異ベクトルへ射影したものであるから，下のように計算することができる。
  ;;;    hgt_pc   = var1_svc#var1
  ;;;    sst_pc   = var2_svc#var2
  ;;; ただし，SVDでは異なるモードの時間関数は直交しないので，
  ;;; 元のデータを時間関数へ射影することで特異ベクトルを求めることはできないので注意。

print_clock("CONTRIBUTION")
  hgt_pc@pcvar = pcvar_b                          ; 共分散寄与率
  sst_pc@pcvar = pcvar_b

print_clock("HGT_SVC")
  hgt_svc  = new((/nsvd,NY1*NX1/),float)          ; Z500の特異ベクトル(空間次元は未定義値も含む大きさで定義)
                                                  ; デフォルトでは全要素に未定義値が入る
  hgt_svc(:,NMS1) = var1_svc                      ; もとも未定義値でなかった場所へSVDで得られた特異ベクトルを代入
  hgt_svc := reshape(hgt_svc,(/nsvd,NY1,NX1/))    ; 空間次元を緯度×経度の2次元に戻す

print_clock("SST_SVC")
  sst_svc  = new((/nsvd,NY2*NX2/),float)          ; SSTの特異ベクトル
  sst_svc(:,NMS2) = var2_svc
  sst_svc := reshape(sst_svc,(/nsvd,NY2,NX2/))

print_clock("Z500_HOMOG_MAP")
  hgt_hom  = new((/nsvd,NY1*NX1/),float)          ; Z500の同質相関マップ
  hgt_hom(:,NMS1) = var1_hom
  hgt_hom := reshape(hgt_hom,(/nsvd,NY1,NX1/))

print_clock("SST_HOMOG_MAP")
  sst_hom  = new((/nsvd,NY2*NX2/),float)          ; SSTの同質相関マップ
  sst_hom(:,NMS2) = var2_hom
  sst_hom := reshape(sst_hom,(/nsvd,NY2,NX2/))

print_clock("Z500_HETERO_MAP")
  hgt_het  = new((/nsvd,NY1*NX1/),float)          ; Z500の異質相関マップ
  hgt_het(:,NMS1) = var1_het
  hgt_het := reshape(hgt_het,(/nsvd,NY1,NX1/))

print_clock("SST_HOMOG_MAP")
  sst_het  = new((/nsvd,NY2*NX2/),float)          ; SSTの異質相関マップ
  sst_het(:,NMS2) = var2_het
  sst_het := reshape(sst_het,(/nsvd,NY2,NX2/))
  delete([/var1,var2,var1_svc,var2_svc,var1_hom,var1_het,var2_hom,var2_het/])

  ;;; 重みの分だけ戻してあげる
  do j = 0, NW1-1
    hgt_svc(:,j,:) = hgt_svc(:,j,:) / wgt1(j)
  end do
  do j = 0, NW2-1
    sst_svc(:,j,:) = sst_svc(:,j,:) / wgt2(j)
  end do



  ;;; 描画に備えて各変数に座標を設定
print_clock("PLOTTING")
print_clock("SET COORDINATE")
  hgt_svc!0    = "svd"
  hgt_svc!1    = "lat_hgt"
  hgt_svc!2    = "lon_hgt"
  hgt_svc&svd  = ispan(1,nsvd,1)
  hgt_svc&lat_hgt = lat1(LAT1)
  hgt_svc&lon_hgt = lon1(LON1)
  copy_VarCoords(hgt_svc,hgt_hom)
  copy_VarCoords(hgt_svc,hgt_het)
  hgt_pc!0     = "svd"
  hgt_pc!1     = "year"
  hgt_pc&svd   = ispan(1,nsvd,1)
  hgt_pc&year  = ispan(y1,y2,1)
  sst_svc!0    = "svd"
  sst_svc!1    = "lat_sst"
  sst_svc!2    = "lon_sst"
  sst_svc&svd  = ispan(1,nsvd,1)
  sst_svc&lat_sst = lat2(LAT2)
  sst_svc&lon_sst = lon2(LON2)
  copy_VarCoords(sst_svc,sst_hom)
  copy_VarCoords(sst_svc,sst_het)
  sst_pc!0    = "svd"
  sst_pc!1    = "year"
  sst_pc&svd  = ispan(1,nsvd,1)
  sst_pc&year = ispan(y1,y2,1)

  ;;; 必ずしも必要ではないが，時間関数を規格化し，特異ベクトルを次元を持つ量に
print_clock("NORMALIZE TIME COEF")  
  do i = 0, nsvd-1
    hgt_svc(i,:,:) = hgt_svc(i,:,:)*stddev(hgt_pc(i,:))
    sst_svc(i,:,:) = sst_svc(i,:,:)*stddev(sst_pc(i,:))
    hgt_pc(i,:)    = hgt_pc(i,:)/stddev(hgt_pc(i,:))
    sst_pc(i,:)    = sst_pc(i,:)/stddev(sst_pc(i,:))
  end do

print_clock("CONFIGURE PLOT RES")
  ;;; ここからお絵描きの設定
  res1 = True                                  ; Z500の描画に用いるresource
  res1@gsnDraw          = False                ; gsn_csm_contour_map_ce実行時にDrawしない
  res1@gsnFrame         = False                ; gsn_csm_contour_map_ce実行時にFrame更新しない
  res1@cnFillOn         = True                 ; シェードを使う
  res1@cnLinesOn        = False                ; コンターは使わない
  res1@cnLineLabelsOn   = False                ; コンターラベルもない
  res1@cnInfoLabelOn    = False                ; コンター情報もない
  res1@lbLabelBarOn     = False                ; 各パネルごとにはカラーバーを描かない(今のところ)
                                               ; あとで一時的にTrueとなる
  res1@lbTitleOn        = True                 ; カラーバーにタイトルを付ける
  res1@lbTitlePosition  = "Right"              ; カラーバーのタイトルの位置
  res1@lbTitleDirection = "Across"             ; カラーバーのタイトルの向き
  res1@pmLabelBarOrthogonalPosF    = 0.3       ; カラーバーの位置を少し下にずらす
  res1@mpGeophysicalLineThicknessF = 2         ; 海岸線の太さ
  res1@gsnAddCyclic     = False                ; 経度方向に周期的ではない
  res1@tmXTOn           = False                ; 上側のx軸を用いない(目盛を消す)
  res1@cnLevelSelectionMode = "ManualLevels"   ; コンターレベルの設定を"ManualLevels"モードに
 
  ;;; 実はここまではSSTの描画に用いるresourceと共通。
  ;;; なのでSSTの描画に用いるresourceをres3としてコピー
  res3 = res1                         

  ;;; これ以降，個別の設定を行っていく
  ;;; Z500に対して
  res1@mpMinLatF                = 25.          ; 地図の南端
  res1@mpMaxLatF                = 90.          ;       北端
  res1@mpMinLonF                = 105.         ;       東端
  res1@mpMaxLonF                = 255.         ;       西端
  res1@mpCenterLonF             = 180.         ; 0°～360°の経度座標を持つ
  res1@cnMinLevelValF           = -75.         ; コンター最小値
  res1@cnMaxLevelValF           =  75.         ; コンター最大値
  res1@cnLevelSpacingF          =  15.         ; コンター間隔
  res1@lbTitleString            = "m"          ; カラーバーのタイトル
  res1@lbTitleFontHeightF       = 0.021        ; カラーバーのタイトルの文字の大きさ
  res1@lbLabelFontHeightF       = 0.021        ; カラーバーのラベルの文字の大きさ
  res1@gsnLeftString            = "Z500"       ; LeftStringの文字
  res1@gsnLeftStringFontColor   = "red"        ; LeftStringの文字の色
  res1@gsnLeftStringFontHeightF = 0.038        ; LeftStringの文字の大きさ
  res1@tmYLOn                   = True         ; 左側のy軸を使う
  res1@tmYLLabelsOn             = True         ; タテ軸のラベルは左側のy軸を使う
  res1@tmYROn                   = False        ; 右側のy軸を使わない
  res1@tmYRLabelsOn             = False        ; タテ軸のラベルは右側のy軸を使わない
  res1@tmXBLabelFontHeightF     = 0.025        ; ヨコ軸ラベルの文字の大きさ
  res1@tmYLLabelFontHeightF     = 0.025        ; タテ軸ラベルの文字の大きさ

  ;;; SSTに対して(基本的には同様で，異なる値を入れていく)
  res3@mpMinLatF                = -20.
  res3@mpMaxLatF                = 20.
  res3@mpMinLonF                = 30.
  res3@mpMaxLonF                = 290.
  res3@mpCenterLonF             = 180.
  res3@cnMinLevelValF           = -0.9
  res3@cnMaxLevelValF           =  0.9
  res3@cnLevelSpacingF          =  0.1
  res3@lbTitleString            = "~S~o~N~C"
  res3@lbTitleFontHeightF       = 0.008
  res3@lbLabelFontHeightF       = 0.008
  res3@gsnLeftString            = "SST"
  res3@gsnLeftStringFontColor   = "blue"
  res3@gsnLeftStringFontHeightF = 0.014
  res3@tmYLOn                   = False        ; 左側のy軸を使わない
  res3@tmYLLabelsOn             = False
  res3@tmYROn                   = True         ; 右側のy軸を使う
  res3@tmYRLabelsOn             = True
  res3@tmXBTickSpacingF         = 45
  res3@tmXBLabelFontHeightF     = 0.009
  res3@tmYLLabelFontHeightF     = 0.009
  res3@mpShapeMode              = "FreeAspect" ; 地図プロットの縦横比を可変に
  res3@vpWidthF                 = 0.3          ; 図の幅(attachしたとき横の長さが変わる)
                                               ; これに合わせて地図は縦長になる
  res3@tmXBTickSpacingF         = 45           ; 横軸の目盛間隔を45°に

  res2                          = True         ; 時間関数の描画に用いるresource
  res2@gsnDraw                  = False        ; gsn_csm_xy実行時にDrawしない
  res2@gsnFrame                 = False        ; gsn_csm_xy実行時にFrame更新しない
  res2@trXMinF                  = y1           ; ヨコ軸の範囲
  res2@trXMaxF                  = y2           ;
  res2@trYMinF                  = -3           ; タテ軸の範囲
  res2@trYMaxF                  =  3           ;
  res2@tmXTOn                   = False        ; 上側のx軸を用いない(目盛を消す)
  res2@vpHeightF                = 0.3
  res2@tmYRLabelsOn             = False        ; 右側のy軸にラベルを描かない
  res2@xyLineColors   = (/"red","blue"/)       ; 折れ線の色
  res2@xyDashPattern            = 0            ; 折れ線の線種(実線)
  res2@gsnYRefLine              = 0            ; y=0の線を引く
  res2@tmXBLabelFontHeightF     = 0.045        ; ヨコ軸ラベルの文字の大きさ
  res2@gsnLeftStringFontHeightF = 0.045        ; LeftStringの文字の大きさ
 
  pres                            = True       ; パネルプロットのresource
  pres@gsnPanelTop                = 0.9        ; パネルプロットの上10%に余白を作る
  pres@gsnPanelBottom             = 0.1        ; パネルプロットの下10%に余白を作る
  pres@gsnPanelYWhiteSpacePercent = 10         ; 各パネル間に10%の余白を挟む
  pres@gsnPanelLabelBar           = False      ; カラーバーを描かない(今のところ)
                                               ; あとでTrueになる
  pres@pmLabelBarWidthF           = 0.6        ; カラーバーの幅
  pres@pmLabelBarHeightF          = 0.05       ; カラーバーの高さ
  pres@lbLabelFontHeightF         = 0.012      ; カラーバーのラベルの文字の大きさ

  ;;; attachのresources
  attach_res1 = True
  attach_res2 = True
  attach_res3 = True

print_clock("PLOT 1 : TIME COEF & SINGULAR VECTOR")
  ;;; 1枚目：時間関数と特異ベクトル
  wks   = gsn_open_wks(out,"svd_sv")
  plot1 = new(nsvd,graphic)
  plot2 = new(nsvd,graphic)
  plot3 = new(nsvd,graphic)
  dum   = new(nsvd,graphic)
  ;;; モードごとに描画
  do i = 0, nsvd-1
    ;; 最後のモードの時だけ，Z500とSSTそれぞれのカラーバーを描く
    if (i.eq.nsvd-1) then
      res1@lbLabelBarOn  = True
      res3@lbLabelBarOn  = True
    end if
    ;; 各パネルのタイトル，res2につけることで中央付近に位置するようになる
    ;; " ~C~ "は改行
    res2@gsnLeftString = "SVD "+sprinti("%1.1i",hgt_svc&svd(i)) + \
                         " : "+sprintf("%3.1f",hgt_pc@pcvar(i)) + "% ~C~ "
    ;; Z500の特異ベクトルを描く
    plot1(i) = gsn_csm_contour_map_ce(wks,hgt_svc(i,:,:),res1)
    ;; 時間関数を描く
    plot2(i) = gsn_csm_xy(wks,hgt_pc&year,(/hgt_pc(i,:),sst_pc(i,:)/),res2)
    ;; SSTの特異ベクトルを描く
    plot3(i) = gsn_csm_contour_map_ce(wks,sst_svc(i,:,:),res3)
    ;; SSTの特異ベクトルのグラフを時間関数のグラフにattach
    dum(i)   = gsn_attach_plots(plot2(i),plot3(i),attach_res2,attach_res3)
    ;; さらにそれをZ500の特異ベクトルのグラフにattach
    dum(i)   = gsn_attach_plots(plot1(i),plot2(i),attach_res1,attach_res2)
  end do
  gsn_panel(wks,plot1,(/nsvd,1/),pres)      ; パネルプロット
  delete([/wks,plot1,plot2,plot3,dum/])     ; 次の描画のために削除

  ;;; 同質・異質相関マップの描画に備えて一部変更
  res1@lbLabelBarOn     = False             ; 個々のパネルに対してはカラーバーを描かない
  res3@lbLabelBarOn     = False             ;
  pres@gsnPanelLabelBar = True              ; その代わり全体にカラーバーをつける
 
  res1@cnMinLevelValF   = -0.9              ; コンターの最大・最小・間隔も変更
  res1@cnMaxLevelValF   =  0.9              ; 相関係数なので－1から＋1までです
  res1@cnLevelSpacingF  =  0.1
  res3@cnMinLevelValF   = -0.9
  res3@cnMaxLevelValF   =  0.9
  res3@cnLevelSpacingF  =  0.1

  ;;; 2枚目：時間関数と同質相関マップ
print_clock("PLOT 2: TIME COEF. & HOMOG MAP")
  wks   = gsn_open_wks(out,"svd_hom")
  plot1 = new(nsvd,graphic)
  plot2 = new(nsvd,graphic)
  plot3 = new(nsvd,graphic)
  dum   = new(nsvd,graphic)
  do i = 0, nsvd-1
    res2@gsnLeftString = "SVD "+sprinti("%1.1i",hgt_hom&svd(i)) + \
                         " : "+sprintf("%3.1f",hgt_pc@pcvar(i)) + "% ~C~ "
    plot1(i) = gsn_csm_contour_map_ce(wks,hgt_hom(i,:,:),res1)
    plot2(i) = gsn_csm_xy(wks,hgt_pc&year,(/hgt_pc(i,:),sst_pc(i,:)/),res2)
    plot3(i) = gsn_csm_contour_map_ce(wks,sst_hom(i,:,:),res3)
    dum(i)   = gsn_attach_plots(plot2(i),plot3(i),attach_res2,attach_res3)
    dum(i)   = gsn_attach_plots(plot1(i),plot2(i),attach_res1,attach_res2)
  end do
  gsn_panel(wks,plot1,(/nsvd,1/),pres)
  delete([/wks,plot1,plot2,plot3,dum/])

  ;;; 3枚目：時間関数と異質相関マップ
print_clock("PLOT 3: HETERO MAP")
  wks   = gsn_open_wks(out,"svd_het")
  plot1 = new(nsvd,graphic)
  plot2 = new(nsvd,graphic)
  plot3 = new(nsvd,graphic)
  dum   = new(nsvd,graphic)
  do i = 0, nsvd-1
    res2@gsnLeftString = "SVD "+sprinti("%1.1i",hgt_het&svd(i)) + \
                         " : "+sprintf("%3.1f",hgt_pc@pcvar(i)) + "% ~C~ "
    plot1(i) = gsn_csm_contour_map_ce(wks,hgt_het(i,:,:),res1)
    plot2(i) = gsn_csm_xy(wks,hgt_pc&year,(/hgt_pc(i,:),sst_pc(i,:)/),res2)
    plot3(i) = gsn_csm_contour_map_ce(wks,sst_het(i,:,:),res3)
    dum(i)   = gsn_attach_plots(plot2(i),plot3(i),attach_res2,attach_res3)
    dum(i)   = gsn_attach_plots(plot1(i),plot2(i),attach_res1,attach_res2)
  end do
  gsn_panel(wks,plot1,(/nsvd,1/),pres)
  delete([/wks,plot1,plot2,plot3,dum/])


print("")
print("Done " + script_name)
print("")
print("LOG: "+LOG)
print("")
system("ls -lh --time-style=long-iso *."+out)
print("")

end
