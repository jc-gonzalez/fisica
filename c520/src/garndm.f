      REAL FUNCTION GARNDM(DUMMY)
 
C-----------------------------------------------------------------------
C  1 EXPONENTIALLY DISTRIBUTED RANDOM NUMBER
C-----------------------------------------------------------------------
 
      REAL RD(1)
C-----------------------------------------------------------------------
 
      CALL RMMAR(RD,1,1)
      GARNDM = -LOG(RD(1))
      RETURN
      END
