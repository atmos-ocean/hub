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

printVarSummary(a)
;開いたファイルに関する概要を画面に表示させる


