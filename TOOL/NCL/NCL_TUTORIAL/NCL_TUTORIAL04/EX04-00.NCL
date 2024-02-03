;入力変数名の確認

INDIR="/work01/DATA/JRA55C/MON/SFC/FCT/" 
;入力ファイルの存在するディレクトリ

INFLE="fcst_phy2m125.C.121_lhtfl.201201_201212"
; ファイル名

IN=INDIR+INFLE

f = addfile(IN, "r")
; ファイルを開く

print (f)
; 変数名の確認

