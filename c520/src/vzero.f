      SUBROUTINE VZERO (IA,N)
 
C-----------------------------------------------------------------------
C CERN PROGLIB# F121    VZERO           .VERSION KERNFOR  4.16  870601
C ORIG. 01/07/71, MODIF. 24/05/87 TO SET INTEGER ZERO
C-----------------------------------------------------------------------
 
      DIMENSION IA(*)
C-----------------------------------------------------------------------
 
      IF (N.LE.0)  RETURN
      DO 9 I= 1,N
    9 IA(I)= 0
 
      RETURN
      END
