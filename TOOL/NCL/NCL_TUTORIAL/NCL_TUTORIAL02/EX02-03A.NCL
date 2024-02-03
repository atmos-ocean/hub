print("MMMMM READ INPUT DATA.")
INDIR="/work01/DATA/ERA5/EASIA/6HR/SFC/SST/07BP02/"
;入力ファイルの存在するディレクトリ名

INFLE="ERA5_EASIA_SST_6HR_202107_02.grib"
;入力ファイル名

IN=INDIR+INFLE
;INDIRとINFLEをつないで一つの変数INを作る

print("IN="+IN)
; 変数INの内容を画面に表示する

a=addfile(IN,"r")
; 読み込み(r)モードで変数INに記憶されたファイルを開く
; aはファイルを指すポインター

SST=a->SSTK_GDS0_SFC
; ポインターaで指定された入力ファイルに含まれるSSTK_GDS0_SFCという
; 名称の変数に記憶されたデータを読み込む。
; 読み込み先の変数の名称はSSTとする


print("MMMMM PLOTTING")
; 作図開始

FIG="ERA5_SST_EX02-03A"
TYP="pdf"
; 図のファイル名とファイルの種類(pdf)を指定

wks = gsn_open_wks(TYP, FIG)
;作図するファイルを開く

resmp=True
; 地図の描画の設定に関する情報を記載する変数(resmp)を用意する

resmp@mpMinLatF = 0.
; 地図の南の端
resmp@mpMaxLatF = 50.
; 地図の北の端
resmp@mpMinLonF = 80.
; 地図の西の端
resmp@mpMaxLonF = 150.
; 地図の東の端

res=resmp
; resmpの情報をresに引き継ぐ
res@cnFillOn = True
; Trueの場合, 色で塗分けする
res@cnLinesOn = False
; Falseの場合, 等値線を書かない

res@gsnAddCyclic = False
;地球一周分のデータでない場合Falseにする

SSTC=SST(0,:,:)-273.15
; 絶対温度から摂氏への換算

plot1=gsn_csm_contour_map(wks,SSTC,res)
; カラーシェード図と地図の作図

print("MMMMM IN : "+IN)
print("MMMMM FIG: "+FIG+"."+TYP)
