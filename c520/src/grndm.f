      SUBROUTINE GRNDM(RVEC,LENV)
 
C-----------------------------------------------------------------------
C  G(ENERATOR OF) R(A)ND(O)M (NUMBERS)
C
C  THIS ROUTINE IS IDENTICAL TO RMMAR
C  DESCRIPTION OF ALGORITHM SEE SUBROUTINE RMMAR
C  THIS SUBROUTINE IS CALLED FROM GHEISHA ROUTINES
C  ARGUMENTS:
C   RVEC   = VECTOR FIELD TO BE FILLED WITH RANDOM NUMBERS
C   LENV   = LENGTH OF VECTOR (# OF RANDNUMBERS TO BE GENERATED)
C
C  CERN PROGLIB# V113    RMMAR           .VERSION KERNFOR  1.0
C  ORIG. 01/03/89 FCA + FJ
C-----------------------------------------------------------------------
 
      REAL RVEC(*)
      COMMON /RANMA2/ IU(1030),JSEQ
      COMMON /RANMA3/ TWOM24,TWOM48,CD,CM,CINT,MODCNS
      INTEGER I97(0:1030),J97(0:1030),NTOT(0:1030),NTOT2(0:1030),
     *        IJKL(0:1030)
      REAL    U(1030),C(0:1030)
      EQUIVALENCE (IJKL(0),IU(1)),(NTOT(0),IU(2)),(NTOT2(0),IU(3))
      EQUIVALENCE (U(1),IU(4)),(C(0),IU(101)),(I97(0),IU(102))
      EQUIVALENCE (J97(0),IU(103))
C-----------------------------------------------------------------------
 
      ISEQ = 1
      IF ( ISEQ .GT. 0 ) JSEQ = ISEQ
      IBASE = (JSEQ-1)*103
 
      DO 100  IVEC = 1,LENV
        UNI = U(IBASE+I97(IBASE))-U(IBASE+J97(IBASE))
        IF ( UNI .LT. 0. ) UNI = UNI+1.
        U(IBASE+I97(IBASE)) = UNI
        I97(IBASE) = I97(IBASE)-1
        IF ( I97(IBASE) .EQ. 0 ) I97(IBASE) = 97
        J97(IBASE) = J97(IBASE)-1
        IF ( J97(IBASE) .EQ. 0 ) J97(IBASE) = 97
        C(IBASE)   = C(IBASE) - CD
        IF ( C(IBASE) .LT. 0. ) C(IBASE) = C(IBASE)+CM
        UNI = UNI-C(IBASE)
        IF ( UNI .LT. 0. ) UNI = UNI+1.
C  REPLACE EXACT ZEROES BY UNIFORM DISTR. *2**-24
        IF ( UNI .EQ. 0. ) THEN
          UNI = TWOM24*U(2)
C  AN EXACT ZERO HERE IS VERY UNLIKELY, BUT LET'S BE SAFE.
          IF ( UNI .EQ. 0. ) UNI = TWOM48
        ENDIF
        RVEC(IVEC) = UNI
  100 CONTINUE
 
      NTOT(IBASE) = NTOT(IBASE) + LENV
      IF ( NTOT(IBASE) .GE. MODCNS )  THEN
        NTOT2(IBASE) = NTOT2(IBASE) + 1
        NTOT(IBASE)  = NTOT(IBASE) - MODCNS
      ENDIF
 
      RETURN
      END
