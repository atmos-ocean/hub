2020-01-04_08-52 
/work05/manda/NCL/EOF
manda@calypso
$ ncl -nQ EOF_ERSSTv4.ncl 
warning:dim_rmvmean: 2699 rightmost sections of the input array contained all missing values



$ ll eofs.eps 
-rw-rw-r-- 1 manda manda 1.4M 2020-01-04 08:52 eofs.eps



List of the following files:
----------------------------
EOF_ERSSTv4.ncl

Machine info
----------------------------
calypso.bosai.go.jp
/work05/manda/NCL/EOF
Sat, 04 Jan 2020 09:28:36 +0900

## EOF_ERSSTv4.ncl
```ncl
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

  y1      = 1979        ; 最初の年
  y2      = 2015        ; 最後の年
  mon     =    1        ; 解析に使う月
  lat1    =  -60        ; EOF解析に使う領域(南端)
  lat2    =   60        ;                  (北端)
  neof    =    3        ; 求めるEOFの数

  indir   = "/work05/manda/DATA/SST/ERSST/V4/200104/"                    ; 読むデータのディレクトリ
  infile  = "sst.mnmean.v4.nc"           ; 読むデータのファイル名
  out     = "eps"                        ; 出力形式
;  outdir  = "~/plot/"                    ; 出力ファイルのディレクトリ
  outdir = "./"
  outfile = "eofs"                       ; 出力ファイル名
 
  in      = addfile(indir+infile,"r")    ; 読み込むファイルを取得
  time    = in->time                     ; 時間の配列を取得
  YYYYMM  = cd_calendar(time,-1)         ; timeからYYYYMMの配列を作成
  t1      = ind(YYYYMM.eq.y1*100+mon)    ; 1979年1月に対応するインデックスを取得
  t2      = ind(YYYYMM.eq.y2*100+12)     ; 2015年12月に対応するインデックスを取得
  sst     = in->sst(t1:t2:12,:,:)        ; sstの値を取得
                                         ; ただし，時間次元はt1からt2まで12おき
                                         ; これは1979年から2015年の1月の値のみ切り取ることに対応
  lat     = in->lat                      ; 緯度の配列を取得
  lon     = in->lon                      ; 経度の配列を取得

  LAT     = ind(lat.ge.lat1.and.lat.le.lat2)  ; EOFを計算する緯度範囲を取得
  var     = sst(lat|LAT,lon|:,time|:)         ; sstから計算領域を切りとる
                                              ; あとでeofuncを使うので時間次元を右側に
                                              ; v6.4.0以降はeofunc_nが使えるので次元の入れ替え不要
  var   = dim_rmvmean(var)                    ; 時間平均を除去して偏差に
  var   = dtrend(var,False)                   ; トレンドを除く

  ;;; 各グリッドの面積の重みをかける
  ;;; EOF解析では共分散行列の各成分に対して面積の重みをかけるべきなので，
  ;;; 偏差に対しては面積の平方根の重みをかけることに注意
  wgt     = sqrt(cos(lat(LAT)*atan(1.)/45.))  ; √cos(緯度)の重み
                                              ; cosの中身をラジアンにするのを忘れずに
  NW      = dimsizes(wgt)
  do j = 0, NW-1
    var(j,:,:) = var(j,:,:) * wgt(j)
  end do

  opt   = True
  opt@jopt = 0                                ; 共分散行列を使用(1なら相関行列)
  eof   = eofunc(var,neof,opt)                ; EOFを計算
  pc    = eofunc_ts(var,eof,False)            ; varをeofに射影し時間関数を計算
  eval  = eof@eval                            ; 固有値を取得
  pcvar = eof@pcvar                           ; 分散寄与率を取得
  delete(var)

  ;;; 現時点では固有ベクトル規格化され，時間関数が次元を持つ量になっている
  ;;; 慣例に従い，固有ベクトルを次元を持つ量とし，時間関数を規格化する
  do i = 0, neof-1
    eof(i,:,:) = eof(i,:,:)*sqrt(eval(i))
    pc(i,:)  = pc(i,:)/sqrt(eval(i))
  end do

  ;;; 重みの分だけ戻してあげる
  do j = 0, NW-1
    eof(:,j,:) = eof(:,j,:) / wgt(j)
  end do

  ;;; 固有ベクトルと時間関数に座標をつける
  eof!0   = "eof"
  eof!1   = "lat"
  eof!2   = "lon"
  eof&eof = ispan(1,neof,1)
  eof&lat = lat(LAT)
  eof&lon = lon

  pc!0    = "eof"
  pc!1    = "year"
  pc&eof  = ispan(1,neof,1)
  pc&year = ispan(y1,y2,1)


  ;;; ここからお絵描きの設定
  wks   = gsn_open_wks(out,outdir+outfile)
  plot1 = new(neof,graphic)
  plot2 = new(neof,graphic)
  dum   = new(neof,graphic) 

  res1 = True                                 ; 固有ベクトルの描画に用いるresource
  res1@gsnDraw          = False               ; gsn_csm_contour_map_ce実行時にDrawしない
  res1@gsnFrame         = False               ; gsn_csm_contour_map_ce実行時にFrame更新しない
  res1@cnFillOn         = True                ; シェードを使う
  res1@cnLinesOn        = False               ; コンターは使わない
  res1@cnLineLabelsOn   = False               ; コンターラベルもない
  res1@cnInfoLabelOn    = False               ; コンター情報もない
  res1@lbLabelBarOn     = False               ; 各パネルごとにはカラーバーを描かない
  res1@mpMinLatF        = lat1                ; 地図の南端
  res1@mpMaxLatF        = lat2                ;       北端
  res1@mpMaxLonF        = 359                 ; 地図の東端
         ; mpMaxLonFは，地図の0°のラベルが右側の時間関数の1980年のラベルと
         ; 被らないようにその場しのぎで設定しただけ
  res1@mpCenterLonF     = 180                 ; 日付変更線中心の地図
  res1@mpGeophysicalLineColor = "darkgreen"   ; 海岸線の色
  res1@mpLandFillColor  = "darkgreen"         ; 陸地の塗りつぶしの色
  res1@gsnLeftStringFontHeightF = 0.03        ; LeftStringの文字の大きさ
  res1@pmTickMarkDisplayMode    = "Always"    ; 軸ラベルいい感じに
  res1@tmXBLabelFontHeightF     = 0.02        ; ヨコ軸ラベルの文字の大きさ
  res1@tmYLLabelFontHeightF     = 0.02        ; タテ軸ラベルの文字の大きさ
  res1@cnLevelSelectionMode  = "ManualLevels" ; コンターレベルの設定を"ManualLevels"モードに
  res1@cnMinLevelValF   = -1.                 ; コンター最小値
  res1@cnMaxLevelValF   =  1.                 ; コンター最大値
  res1@cnLevelSpacingF  = 0.1                 ; コンター間隔
  res1@cnFillPalette    = "NCV_jaisnd"        ; カラーバーの種類

  res2           = True                       ; 時間関数の描画に用いるresource
  res2@gsnDraw   = False                      ; gsn_csm_xy実行時にDrawしない
  res2@gsnFrame  = False                      ; gsn_csm_xy実行時にFrame更新しない
  res2@vpHeightF = 0.3                        ; 図の高さ(あとでplot1にattachするので，横の長さが変わる)
  res2@xyLineColor  = -1                      ; 折れ線の色(-1なので透明)
  res2@trXMinF      = y1                      ; ヨコ軸の範囲
  res2@trXMaxF      = y2                      ;
  res2@trYMinF      = -3                      ; タテ軸の範囲
  res2@trYMaxF      =  3                      ;
  res2@tmYRLabelsOn = True                    ; タテ軸のラベルは右側のy軸を使う
  res2@tmYLMinorOn  = False                   ; 小目盛を利用しない
  res2@gsnYRefLine  = 0                       ; y=0の線を引く
  res2@gsnBelowYRefLineColor = "deepskyblue"  ; y=0より下の部分を塗りつぶす
  res2@gsnAboveYRefLineColor = "tomato"       ; y=0より上の部分を塗りつぶす
  res2@tmXBLabelFontHeightF = 0.02            ; ヨコ軸ラベルの文字の大きさ
  res2@tmYLLabelFontHeightF = 0.02            ; タテ軸ラベルの文字の大きさ
 
  pres = True                                 ; パネルプロットのresource
  pres@gsnPanelTop      = 0.9                 ; パネルプロットの上10%に余白を作る
  pres@gsnPanelBottom   = 0.1                 ; パネルプロットの下10%に余白を作る
  pres@gsnPanelYWhiteSpacePercent = 8         ; 各パネル間に8%の余白を挟む
  pres@gsnPanelLabelBar = True                ; カラーバーを描く
  pres@pmLabelBarParallelPosF  =  -0.1        ; カラーバーの位置を少し左にずらす
  pres@pmLabelBarWidthF  = 0.6                ; カラーバーの幅
  pres@pmLabelBarHeightF = 0.05               ; カラーバーの高さ
  pres@lbTitleOn        = True                ; カラーバーにタイトルを付ける
  pres@lbTitleFontHeightF = 0.012             ; カラーバーのタイトルの文字の大きさ
  pres@lbLabelFontHeightF = 0.012             ; カラーバーのラベルの文字の大きさ
  pres@lbTitleString    = "~S~o~N~C"          ; カラーバーのタイトル
  pres@lbTitlePosition  = "Right"             ; カラーバーのタイトルの位置
  pres@lbTitleDirection = "Across"            ; カラーバーのタイトルの向き

  ;;; attachのresource
  attach_res1 = True
  attach_res2 = True

  ;;; モードごとに描画
  do i = 0, neof-1
    ;; 各パネルのタイトル
    res1@gsnLeftString = "EOF"+sprinti("%1.1i",pc&eof(i)) + " : " + \
                         sprintf("%3.1f",pcvar(i)) + " %"
    ;; 固有ベクトルを描く
    plot1(i) = gsn_csm_contour_map_ce(wks,eof(i,:,:),res1)
    ;; 時間関数を描く
    plot2(i) = gsn_csm_xy(wks,pc&year,pc(i,:),res2)
    ;; 時間関数のグラフを固有ベクトルのグラフにattach
    dum(i)   = gsn_attach_plots(plot1(i),plot2(i),attach_res1,attach_res2)
  end do
  gsn_panel(wks,plot1,(/neof,1/),pres)      ; パネルプロット
 
end
```

