GRIBファイルをバイナリファイルに変換

wgrib バイナリ形式に変換
ダイレクトアクセスバイナリ形式(direct-access binary format)への変換．

```bash
IN=ERA5P.2017-01-05_10.grib

VARLIST="V Z T"
#VARLIST="U V Z T"

for VAR in $VARLIST; do

echo mmm $IN TO $BIN
BIN=ERA5P.2017-01-05_10_${VAR}.BIN
wgrib -s $IN|egrep "(:${VAR}:)" |wgrib -i $IN -o $BIN 
echo mmm $IN TO $GRB
GRB=ERA5P.2017-01-05_10_${VAR}.grib
wgrib -s $IN|egrep "(:${VAR}:)" |wgrib -i -grib $IN -o $GRB

echo "mmm INPUT"
ls -lh $IN; echo
echo "mmm OUTPUT"
ls -lh $BIN; ls -lh $GRB; echo
g1print.exe $GRB|head -3; echo

done
```




バイナリにおとすには？

    wgrib grib_file | wgrib -i grib_file -o binary_file


風(u,v)・気温(T)を取り出すには？(GRIB format で出力)

    wgrib -s grib_file | egrep "(:UGRD:|:VGRD:|:TMP:)" | wgrib -i -grib \
          grib_file -o new_grib_file









```
2022-08-08_16-32
$ g1print.exe ERA5P.2017-01-05_10.grib |head
----------------------------------------------------
 rec GRIB GRIB  Lvl  Lvl  Lvl         Time      Fcst
 Num Code name  Code one  two                   hour
----------------------------------------------------
   1 155 D      100  100    0  2017-01-05_00:00 + 00
   2 129 Z      100  100    0  2017-01-05_00:00 + 00
   3  60 PV     100  100    0  2017-01-05_00:00 + 00
   4 157 R      100  100    0  2017-01-05_00:00 + 00
   5 133 Q      100  100    0  2017-01-05_00:00 + 00
   6 130 T      100  100    0  2017-01-05_00:00 + 00
   7 131 U      100  100    0  2017-01-05_00:00 + 00
   8 132 V      100  100    0  2017-01-05_00:00 + 00
   9 135 W      100  100    0  2017-01-05_00:00 + 00
  10 138 VO     100  100    0  2017-01-05_00:00 + 00
```



## ncl setfileoption

バイナリを書く際のエンディアンの指定，

setfileoption("bin","WriteByteOrder","BigEndian")



## ncl fbindirwrite

The following will write `ntimes` records, each of length 64x128 float words.

```ncl
ntimes = 100
z    = new ( (/ntimes,64,128/), float)
path = "/dummy/file"
do nt=0,ntimes-1
  fbindirwrite(path,z(nt,:,:) )
end do
```

The binary file will be the same as one opened via Fortran's open statement:

```fortran
open(...,form="unformatted",access="direct",recl= )   ; recl is system dependent
```

 https://www.ncl.ucar.edu/Applications/Scripts/write_bin_1.ncl

```
   file_out = "example.bin"            ; the "bin" extension is arbitrary
   system ("rm -f " + file_out)  ; remove any previously exist file

 ;************************************************************
 ; note the -1 indicates to just add on to the end of the file
 ; the (/.../) syntax means output the values only with no meta
 ; data
 ;************************************************************
   fbinrecwrite (file_out,-1, (/ fi->lat /))     
   fbinrecwrite (file_out,-1, (/ fi->lon /))
   fbinrecwrite (file_out,-1, (/ t(0,:,1,:) /))  ; output subsection 
```

