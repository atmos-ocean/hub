script_name  = get_script_name() ;スクリプトの名前を得る

DSET  = getenv("NCL_ARG_2") ;2番目の引数を変数DSETに代入
VAR   = getenv("NCL_ARG_3") ;3番目の引数を変数VARに代入

print("SCRIPT="+script_name)  ;スクリプト名を画面表示
print("DSET="+DSET) ;変数DSETの内容を画面表示
print("VAR="+VAR)   ;変数VARの内容を画面表示
print("DONE.")
print("") ;1行改行
