      DOUBLE PRECISION FUNCTION DPFUNC( ENER )
 
C-----------------------------------------------------------------------
C  D(IFFRACTION) P(ORTION) FUNC(TION)
C
C  CALCULATES THE FRACTION OF DIFFRACTION
C  THIS FUNCTION IS CALLED FROM HDPM
C  ARGUMNENT:
C   ENER   = C.M. ENERGY
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
      DOUBLE PRECISION ENER
C-----------------------------------------------------------------------
 
C  FUNCTION DPFUNC IS DUMMY
      DPFUNC = 0.15D0
 
      RETURN
      END
