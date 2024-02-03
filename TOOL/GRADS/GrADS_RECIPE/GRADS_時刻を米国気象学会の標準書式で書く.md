GrADSで時刻を米国気象学会の標準書式で書く（例：00:00UTC19JUN2022）

```bash
datetime1='${hh}Z'${dd}''mmm''yyyy
```

```bash
'set time 'datetime1; 'q dims'; line=sublin(result,5)

dt1=subwrd(line,6); t1=subwrd(line,9)

hh1=substr(dtime1,1,2); dd1=substr(dtime1,4,2); mmm1=substr(dtime1,6,3)
yyyy1=substr(dtime1,9,4)
utc1=hh1':00UTC'dd1''mmm1''yyyy1
```

