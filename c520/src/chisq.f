      DOUBLE PRECISION FUNCTION CHISQ(F)
 
C-----------------------------------------------------------------------
C  CHI SQ(UARE)
C
C  THIS FUNCTION CALCULATES THE CHI**2 OBTAINED WITH THE FITFUNCTION
C  AMOEBA USING THE PARAMETER SET F
C    F(1) = HEIGHT AT MAXIMUM
C    F(2) = SHOWER STARTING POINT
C    F(3) = T AT MAXIMUM
C    F(4) = WIDTH PARAMETER 1
C    F(5) = WIDTH PARAMETER 2 T
C    F(6) = WIDTH PARAMETER 3 T**2
C  THIS FUNCTION IS CALLED FROM LONGFT AND FROM AMOEBA
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,CURVE.
      COMMON /CURVE/   CHAPAR,DEP,ERR,NSTP
      DOUBLE PRECISION CHAPAR(1100),DEP(1100),ERR(1100)
      INTEGER          NSTP
*KEEP,RUNPAR.
      COMMON /RUNPAR/  FIXHEI,THICK0,HILOECM,HILOELB,
     *                 STEPFC,NRRUN,NSHOW,PATAPE,MONIIN,
     *                 MONIOU,MDEBUG,NUCNUC,
     *                 CETAPE,
     *                 SHOWNO,ISHW,NOPART,NRECS,NBLKS,MAXPRT,NDEBDL,
     *                 N1STTR,MDBASE,
     *                 DEBDEL,DEBUG,FDECAY,FEGS,FIRSTI,FIXINC,FIXTAR,
     *                 FIX1I,FMUADD,FNKG,FPRINT,FDBASE
     *                ,GHEISH,GHESIG
      COMMON /RUNPAC/  DSN,HOST,USER
      DOUBLE PRECISION FIXHEI,THICK0,HILOECM,HILOELB
      REAL             STEPFC
      INTEGER          NRRUN,NSHOW,PATAPE,MONIIN,MONIOU,MDEBUG,NUCNUC,
     *                 SHOWNO,ISHW,NOPART,NRECS,NBLKS,MAXPRT,NDEBDL,
     *                 N1STTR,MDBASE
      INTEGER          CETAPE
      CHARACTER*79     DSN
      CHARACTER*20     HOST,USER
 
      LOGICAL          DEBDEL,DEBUG,FDECAY,FEGS,FIRSTI,FIXINC,FIXTAR,
     *                 FIX1I,FMUADD,FNKG,FPRINT,FDBASE
     *                ,GHEISH,GHESIG
*KEND.
 
      DOUBLE PRECISION AUXIL,BALL,BASE,EXPO,F(6),T
      INTEGER          I
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'CHISQ : PARAMETERS =',F
 
C  EXCLUDE PATHOLOGICAL PARAMETER SETTINGS
      IF ( F(1) .LE. 0.D0  .OR.  F(2) .GE. F(3)  .OR.
     *    (F(4).EQ.0.D0 .AND. F(5).EQ.0.D0 .AND. F(6).EQ.0.D0) ) THEN
        CHISQ = 1.D16
        RETURN
      ENDIF
 
      CHISQ = 0.D0
C  LOOP OVER THE LONGITUDINAL DISTRIBUTION
      DO 1 I=1,NSTP
        T = DEP(I)
        IF ( T .GT. F(2) ) THEN
          BASE  = (T-F(2)) / (F(3)-F(2))
          AUXIL = F(4) + T*F(5) + T**2*F(6)
          IF ( AUXIL .LT. 1.D-20 ) THEN
            CHISQ = CHISQ + 1.D16
            GOTO 1
          ENDIF
          EXPO  = (F(3)-T) / AUXIL
CC        IF(DEBUG)WRITE(MDEBUG,*)'CHISQ : I,BASE,EXPO=',I,
CC   *                            SNGL(BASE),SNGL(EXPO)
          BALL = F(1) * BASE ** EXPO
        ELSE
          BALL = 0.D0
        ENDIF
        CHISQ = CHISQ + ((BALL-CHAPAR(I))/ERR(I))**2
 1    CONTINUE
      CHISQ = CHISQ / (NSTP-6)
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'CHISQ : CHI**2 =',SNGL(CHISQ)
 
      RETURN
      END
