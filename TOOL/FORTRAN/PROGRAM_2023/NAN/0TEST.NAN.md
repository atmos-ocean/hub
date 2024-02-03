# Not a number (NaN)の判定

変数の値が異常値となったか判定する

- 0で割り算した場合

- -1の平方根となる実数を求めようとした場合 (下記の例)

## gfortran

```fortran
! https://gcc.gnu.org/onlinedocs/gcc-4.7.4/gfortran/ISNAN.html
program test_nan
  implicit none
  real :: x
  x = -1.0
  x = sqrt(x)
  if (isnan(x)) stop '"x" is a NaN'
end program test_nan
```

isnan関数の戻り値は論理型 (値は.true.か.false.のどちらか)

```bash
$ gfortran 99TEST.NAN.F90 -o 99TEST.NAN.EXE 

$ 99TEST.NAN.EXE 
STOP "x" is a NaN
```

#### `ISNAN` — Test for a NaN

- *Description*:

  `ISNAN` tests whether a floating-point value is an IEEE Not-a-Number (NaN). 

- *Standard*:

  GNU extension 

- *Class*:

  Elemental function 

- *Syntax*:

  `ISNAN(X)` 

- *Arguments*:

  XVariable of the type `REAL`.  

- *Return value*:

  Returns a default-kind `LOGICAL`. The returned value is `TRUE` if X is a NaN and `FALSE` otherwise.
