script_name  = get_script_name() ;スクリプトの名前を得る

NML  = getenv("NCL_ARG_2") ;2番目の引数を変数NMLに代入

; namelistファイルの読み込み
INFLE = systemfunc("grep INFLE " +NML+ "|cut -f2 -d'='|cut -f1 -d','")
; namelistファイルの中で,キーワードINFLEで始まる行を探して, 
; 等号の右の値を変数INFLEに読み込む

OFLE  = systemfunc("grep OFLE  " +NML+ "|cut -f2 -d'='|cut -f1 -d','")

A = tofloat(systemfunc("grep A  " +NML+ "|cut -f2 -d'='|cut -f1 -d','"))
; キーワードAで始まる行を探して, 等号の右の値をtofloatで
; 実数型に変換して変数Aに代入する

B = tofloat(systemfunc("grep B  " +NML+ "|cut -f2 -d'='|cut -f1 -d','"))

print("SCRIPT="+script_name)  ;スクリプト名を画面表示
print("INFLE: "+INFLE)
print(" OFLE: "+ OFLE)
print("A = " + A)
print("B = " + B)

print("DONE.")
print("") ;1行改行
