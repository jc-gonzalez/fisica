      SUBROUTINE GRANOR(A,B)
 
C-----------------------------------------------------------------------
C  TWO GAUSSIAN DISTRIBUTED RANDOM NUMBERS (MEAN=0, SIGMA=1)
C
C  RANDOM NUMBER GENERATORS USED BY THE GHEISHA ROUTINES
C  ADAPTED FOR USE WITH THE CORSIKA RANDOM NUMBER GENERATORS
C
C  DESIGN  : J. KNAPP   IK1  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      REAL RD(2)
C-----------------------------------------------------------------------
 
      CALL RMMAR(RD,2,1)
      U1 = SQRT(-2.*LOG(RD(1)))
      U2 = RD(2) * 6.28318530718
      A = COS(U2) * U1
      B = SIN(U2) * U1
      RETURN
      END
