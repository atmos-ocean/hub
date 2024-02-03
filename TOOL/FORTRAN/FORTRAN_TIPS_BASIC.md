FORTRAN TIPS (BASIC)
=====================================
[TOC]
SYSTEM
------------------------------------
### カレントディレクトリの情報を得る
https://gcc.gnu.org/onlinedocs/gfortran/GETCWD.html
```fortran
PROGRAM TEST_GETCWD
  CHARACTER(LEN=255) :: CWE
  CALL GETCWD(CWD)
  WRITE(*,*) TRIM(CWD)
END PROGRAM
```



### 現在時刻を得る
https://docs.oracle.com/cd/E19957-01/805-4942/6j4m3r8t2/index.html
```fortran
INTEGER NOW(8)
CHARACTER*10 NOWC(3)

CALL DATE_AND_TIME(NOWC(1), NOWC(2), NOWC(3), NOW)

WRITE(21,'(a,i4.4,1x,i2.2,1x,i2.2,1x,i2.2,1x,i2.2)')&
'# ',NOW(1),NOW(2),NOW(3),NOW(5),NOW(6)
```



INPUT
------------------------------------



OUTPUT
------------------------------------



ALTHEMATIC
------------------------------------



MISC
------------------------------------