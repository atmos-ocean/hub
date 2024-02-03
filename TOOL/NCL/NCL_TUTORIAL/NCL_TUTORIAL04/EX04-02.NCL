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

printVarSummary(LHF)
; 読み込み結果の確認

