# WRAPIT

NCLから他の言語のサブルーチンや関数を呼び出すためのツール

https://www.ncl.ucar.edu/Document/Tools/WRAPIT.shtml

## Fortran 90

NCLで呼び出したいFortranサブルーチン

**cquad.f90**

```fortran
subroutine cquad(a,b,c,nq,x,quad)
  implicit none
  integer, intent(in)  ::nq
  real,    intent(in)  ::a,b,c,x(nq)
  real,    intent(out) ::quad(nq)
  integer              ::i

  quad = a*x**2+b*x+c
  return
end subroutine cquad
```



サブルーチンの引数に関する情報を書いたファイルを別途作成する

<font color="red">**FORTRAN77の書式で書く**</font>

**cquad.stub**

```fortran
C NCLFORTSTART
      subroutine cquad(a,b,c,nq,x,quad)
      real a,b,c
      integer nq
      dimension x(nq),quad(nq)
C NCLEND
```



WRAPITでライブラリ作成

```bash
$ WRAPIT cquad90.stub cquad.f90
```



NCLのスクリプト

**MAIN.ncl**

```
external EX01 "./ex01.so"
; WRAPITで作成したライブラリのリンク 
begin
   nump = 3
   x    = (/ -1., 0.0, 1.0 /)
   qval = new(nump,float)              
   EX01::cquad(-1., 2., 3., nump, x, qval)
; Fortranサブルーチンの呼び出し
   print("Polynomial value = " + qval)
end
```

実行

```
$ ncl MAIN.ncl
```

