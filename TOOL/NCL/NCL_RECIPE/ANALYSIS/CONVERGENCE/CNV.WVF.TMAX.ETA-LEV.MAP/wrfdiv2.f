C NCLFORTSTART
      SUBROUTINE DIVD2(DIV,U,V,MSFT,DX,DY,NX,NY,FVAL)
      IMPLICIT NONE
      INTEGER NX,NY
      DOUBLE PRECISION U(NX,NY),V(NX,NY)
      DOUBLE PRECISION DIV(NX,NY),MSFT(NX,NY)
      DOUBLE PRECISION DX,DY
      DOUBLE PRECISION FVAL
C NCLEND

      INTEGER JP1,JM1,IP1,IM1,I,J
      DOUBLE PRECISION DSY,DSX,DUDX,DVDY
      DOUBLE PRECISION MM

C Note all data must be on T-pts
      DO J = 1,NY
      JP1 = MIN(J+1,NY)
      JM1 = MAX(J-1,1)

      DO I = 1,NX
      IP1 = MIN(I+1,NX)
      IM1 = MAX(I-1,1)

      DSX = (IP1-IM1)*DX
      DSY = (JP1-JM1)*DY

C Careful with map factors...
      MM = MSFT(I,J)*MSFT(I,J)

      DUDX = ( U(IP1,J)/MSFT(IP1,J) -
     & U(IM1,J)/MSFT(IM1,J) )/DSX*MM

      DVDY = ( V(I,JP1)/MSFT(I,JP1) -
     & V(I,JM1)/MSFT(I,JM1) )/DSY*MM

      DIV(I,J) = DUDX + DVDY

      IF(U(I,J).GE.FVAL   .OR. V(I,J).GE.FVAL .OR. 
     &   U(IP1,J).GE.FVAL .OR. U(IM1,J).GE.FVAL .OR. 
     &   V(I,JP1).GE.FVAL .OR. V(I,JM1).GE.FVAL)THEN
        DIV(I,J)=FVAL
      END IF

      END DO
      END DO



      RETURN
      END

C
C http://forum.wrfforum.com/viewtopic.php?f=32&t=2267
C Re: NCL: need help to alculate Relative Vorticity & Divergence
C 
C Postby tomk Fri Jun 17, 2011 10:48 am
C wrf.div.f

c--------------------------------------------------------
