SUBROUTINE MOD_SUB(N)
USE MOD_VAR
INTEGER,INTENT(IN)::N

ALLOCATE(X(N))

DO I=1,N
X(I)=I
END DO !I

END SUBROUTINE MOD_SUB
