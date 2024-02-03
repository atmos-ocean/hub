; 
; SVD_TEST01.ncl
; 
; Sun, 05 Jan 2020 14:59:59 +0900
; calypso.bosai.go.jp
; /work05/manda/NCL/SVD
; manda
;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin

script_name  = get_script_name()
script=systemfunc("basename "+script_name+ " .ncl")
LOG=script+".LOG"

NOW=systemfunc("date '+%y%m%d_%H%M' ")
;print("NOW="+NOW)
; LOG=script+NOW+".LOG"

NOW=systemfunc("date -R")
HOST=systemfunc("hostname")
CWD=systemfunc("pwd")

print("script="+script_name)

FINFO=systemfunc("ls -lh --time-style=long-iso "+script_name)

LOG_HEADER = (/"# "+ NOW, "# "+ HOST, "# "+ CWD, "# "+ FINFO /)
hlist=[/LOG_HEADER/]

write_table(LOG, "w", hlist, "%s")



  y1      = 1958                    ; 最初の年
  y2      = 2015                    ; 最後の年
  nsvd    =    3                    ; 求めるモード数

  indir1="/work05/manda/DATA_PROC/JRA55/Monthly/OUT_NC_CAT/"
  indir2="/work05/manda/DATA/SST/ERSST/V4/200104/"

  infile1 = indir1+"hgt.1958-2015.nc"    ; 変数1(Z500)のデータ
  infile2 = indir2+"sst.mnmean.v4.nc"    ; 変数2(SST)のデータ
  out     = "eps"                   ; 出力形式

  ;;; 変数1(Z500)のデータを読む
  in1     = addfile(infile1,"r")             ; 読み込むファイルを取得   
  time    = in1->time                        ; 時間の配列を取得
  YYYYMM  = cd_calendar(time,-1)             ; timeからYYYYMMの配列を作成
  t1      = ind(YYYYMM.eq.y1*100+1)          ; 1958年1月に対応するインデックスを取得
  t2      = ind(YYYYMM.eq.y2*100+12)         ; 2015年12月に対応するインデックスを取得
  hgt     = in1->hgt(t1:t2:12,{500},:,:)     ; Z500の値を取得。{500}で500hPa面を指定(高さ次元の中身をあらかじめ知っている)。
                                             ; 時間次元はt1からt2まで12おき
                                             ; これは1958年から2015年の1月の値のみ切り取ることに対応


printVarSummary(hgt)


print("")
print("Done " + script_name)
print("")
print("LOG: "+LOG)
print("")

end
