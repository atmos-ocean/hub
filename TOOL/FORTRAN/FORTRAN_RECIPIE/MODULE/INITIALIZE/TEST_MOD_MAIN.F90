PROGRAM MOD_MAIN
USE MOD_VAR

N=2
CALL MOD_SUB(N)

DO I=1,N
print *,X(I)
END DO !I

END PROGRAM MOD_MAIN
