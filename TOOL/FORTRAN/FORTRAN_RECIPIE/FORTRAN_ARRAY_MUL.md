TEST_MUL_ARRAY
============================-
[TOC]

Thu, 11 Jun 2020 15:43:56 +0900
calypso.bosai.go.jp
/work05/manda/WRF.POST/K17/CNV.DECOMP/TEST_MUL_ARRAY

```
srcdump.sh TEST_MUL_ARRAY.F90
```

### HOW TO RUN
```
2020-06-11_15-42 
manda@calypso
/work05/manda/WRF.POST/K17/CNV.DECOMP/TEST_MUL_ARRAY
$ ifort TEST_MUL_ARRAY.F90 
(base) 
2020-06-11_15-42 
manda@calypso
/work05/manda/WRF.POST/K17/CNV.DECOMP/TEST_MUL_ARRAY
$ a.out
 a    b    c
   1.000000       3.000000       3.000000    
   2.000000       4.000000       8.000000  
```

### INFO
**Machine info**
processor       : 15
model name      : Intel(R) Xeon(R) CPU E5-2690 0 @ 2.90GHz
MemTotal:       65988728 kB

### SOURCE FILES
- TEST_MUL_ARRAY.F90
  
#### TEST_MUL_ARRAY.F90
```fortran
real,dimension(1,2)::a,b,c

a(1,1)=1
a(1,2)=2

b(1,1)=3
b(1,2)=4

c=a*b

print *,'a    b    c'
do j=1,2
print *,a(1,j),b(1,j),c(1,j)
end do

end
```