;平年値の作図

INDIR="/work01/DATA/JRA55C/MON/SFC/FCT/" 
;入力ファイルの存在するディレクトリ

PREFIX="fcst_phy2m125.C.121_lhtfl."
; ファイル名の一部

filenames = systemfunc("ls "+INDIR+PREFIX+"*")
; ファイル名一覧を作ってfilenamesという変数に代入する

f = addfiles(filenames, "r")
; ファイルを開く

LHF=f[:]->LHTFL_GDS0_SFC_S130
; データの読み込み

LHF30=LHF(481-360+1:481, :, :)
;30年分のデータを抜き出す

LHFC = dim_avg_n_Wrap(LHF30, 0)
; 30年分の平均を計算する

g0_lon_2=f[0]->g0_lon_2
; 入力ファイルから経度を読み込む
g0_lat_1=f[0]->g0_lat_1
; 入力ファイルから緯度を読み込む

print("MMMMM PLOTTING")
; 作図開始

FIG="JRA55C_LHF_CLIM_EX04-06"
TYP="pdf"
; 図のファイル名とファイルの種類(pdf)を指定

wks = gsn_open_wks(TYP, FIG)
;作図するファイルを開く

resmp=True
; 地図の描画の設定に関する情報を記載する変数(resmp)を用意する

res=resmp
; resmpの情報をresに引き継ぐ
res@cnFillOn = True
; Trueの場合, 色で塗分けする
res@cnLinesOn = False
; Falseの場合, 等値線を書かない

res@gsnAddCyclic = True
;地球一周分のデータでない場合Falseにする

res@mpCenterLonF           = 180
;図の中心となる経度を180度にする

LHFC!0 = "lat" ;SSTCの0番目の配列要素の名称をlatにする
LHFC!1 = "lon" ;SSTCの0番目の配列要素の名称をlonにする
LHFC&lat= g0_lat_1 
; LHFCのlatという座標の値として変数g0_lat_1に収納されているものを用いる
LHFC&lon= g0_lon_2 
; LHFCのlonという座標の値として変数g0_lon_2に収納されているものを用いる

plot1=gsn_csm_contour_map(wks,LHFC,res)
; カラーシェード図と地図の作図

print("MMMMM FIG: "+FIG+"."+TYP)

