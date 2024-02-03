C NCLFORTSTART
      SUBROUTINE F77SUB(TEXT,ARG)
      implicit none
      character*(*) TEXT
      real ARG
C NCLEND

      write(6,'(A)')TEXT
      write(6,*    )ARG
      RETURN
      end
