      SUBROUTINE UCOPY (A,B,N)
 
C-----------------------------------------------------------------------
C CERN PROGLIB# V301    UCOPY           .VERSION KERNFOR  1.0   710701
C ORIG. 01/01/65 JZ
C-----------------------------------------------------------------------
 
      DIMENSION A(*),B(*)
C-----------------------------------------------------------------------
 
C                  NO OVERLAP  OR  BEGINNING OF A ON END OF B
      IF (N.EQ.0) RETURN
         DO 21 I=1,N
   21 B(I)=A(I)
      RETURN
      END
