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

printVarSummary(SST)
; 読み込んだSSTという変数の概要を画面に表示させる
