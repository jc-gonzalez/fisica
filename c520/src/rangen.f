      FUNCTION RANGEN()
 
C-----------------------------------------------------------------------
C  RAN(DOM  NUMBER) GEN(ERATOR)
C
C  SEE SUBROUTINE RMMAR
C  THIS FUNCTION IS CALLED FROM MANY VENUS ROUTINES
C
C  CERN PROGLIB# V113    RMMAR           .VERSION KERNFOR  1.0
C  ORIG. 01/03/89 FCA + FJ
C
C  CHANGES  : D. HECK    IK3  FZK KARLSRUHE
C  DATE     : FEB  02, 1994
C-----------------------------------------------------------------------
 
C     REAL RVEC(1)
      COMMON /RANMA2/ IU(1030),JSEQ
      COMMON /RANMA3/ TWOM24,TWOM48,CD,CM,CINT,MODCNS
      INTEGER I97(0:1030),J97(0:1030),NTOT(0:1030)
      INTEGER NTOT2(0:1030),IJKL(0:1030)
      REAL    U(97),C(0:1030)
      EQUIVALENCE (IJKL(0),IU(1)),(NTOT(0),IU(2)),(NTOT2(0),IU(3))
      EQUIVALENCE (U(1),IU(4)),(C(0),IU(101)),(I97(0),IU(102))
      EQUIVALENCE (J97(0),IU(103))
C-----------------------------------------------------------------------
 
C     ISEQ = 1
      LENV = 1
C     IF ( ISEQ .GT. 0 ) JSEQ = ISEQ
C     IBASE = (JSEQ-1)*103
      IBASE = 0
 
      IVEC = 1
C     DO 100  IVEC = 1,LENV
        UNI = U(     +I97(IBASE))-U(     +J97(IBASE))
        IF ( UNI .LT. 0. ) UNI = UNI+1.
        U(     +I97(IBASE)) = UNI
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
        RANGEN = UNI
  100 CONTINUE
 
      NTOT(IBASE) = NTOT(IBASE) + LENV
      IF ( NTOT(IBASE) .GE. MODCNS )  THEN
        NTOT2(IBASE) = NTOT2(IBASE) + 1
        NTOT(IBASE)  = NTOT(IBASE) - MODCNS
      ENDIF
 
      RETURN
      END