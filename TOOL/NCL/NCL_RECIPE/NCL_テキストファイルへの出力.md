# NCL テキストファイルへの出力

```bash
; Check data
do n=0,nt-1
  print(stimes(n)+" "+hfx_i(n)+" "+lh_i(n)+" "+sst_i(n)+" "+t2_i(n)\
        +" "+q2_i(n)+" "+Ws10_i(n)+" "+psfc_i(n))
end do

; Output file
ofle="table.out.txt"

;Header line
header = (/\
"# Input: "+ infile1, \
"# Input: "+ infle2 , \
"# datetime            hfx       lh        sst         T2         Q2     Ws10        PSFC"/)
hlist=[/header/]

; Create rows in table
alist= [/stimes, hfx_i, lh_i, sst_i, t2_i, q2_i, Ws10_i, psfc_i/]

; Output the table
write_table(ofle, "w", hlist, "%s")
write_table(ofle, "a", alist, "%s %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f")
```

