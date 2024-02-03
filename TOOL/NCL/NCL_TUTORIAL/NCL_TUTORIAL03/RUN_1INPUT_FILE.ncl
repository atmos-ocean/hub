script_name  = get_script_name() ;スクリプトの名前を得る

INFLE  = getenv("NCL_ARG_2") ;2番目の引数を変数INFLEに代入

print("SCRIPT="+script_name)  ;スクリプト名を画面表示
print("INFLE="+INFLE) ;変数INFLEの内容を画面表示
print("DONE.")
print("") ;1行改行
