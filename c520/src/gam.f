      DOUBLE PRECISION FUNCTION GAM( Z )
 
C-----------------------------------------------------------------------
C  GAM(MA FUNCTION)
C
C  EULER'S GAMMA FUNCTION
C  THE INTERNAL PRECISION OF THIS FUNCTION IS ONLY SINGLE PRECISION.
C  THIS FUNCION IS CALLED FROM NKG
C  ARGUMENT:
C   Z      = ARGUMENT OF GAMMA FUNCTION (0 < Z < 57)
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
 
      DOUBLE PRECISION U,Y,YY,Z
C-----------------------------------------------------------------------
 
C  CALCULATE CORRESPONDING FUNCTION VALUE IN INTERVAL 1 ... 2
      Y   = MOD(Z,1.D0)
      YY  = Y + 1.D0
 
C  PARAMETRIZATION FOR VALUES IN INTERVAL 1 ... 2
      GAM = 1.D0 + Y*(-0.5771017D0 + Y*(0.9858540D0+
     *             Y*(-0.8764218D0 + Y*(0.8328212D0+
     *             Y*(-0.5684729D0 + Y*(0.2548205D0+
     *             Y*(-0.0514993D0  )))))))
 
C  GET FUNCTION VALUE IN DESIRED INTERVAL BY ITERATION
      IF ( Z .LT. 1.D0 ) THEN
C  GAMMA(Z-1) IS  GAMMA(Z) / (Z-1)
        GAM = GAM / Z
      ELSE
C  GAMMA(Z+1) IS  GAMMA(Z) * Z
        DO  1  U = YY, Z-1.D0, 1.D0
          GAM = GAM * U
 1      CONTINUE
      ENDIF
 
      RETURN
      END
