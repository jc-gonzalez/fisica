      SUBROUTINE RNEGBI( N,XN,ECM )
 
C-----------------------------------------------------------------------
C  R(ANDOM NUMBER WITH) NEG(ATIVE) BI(NOMIAL DISTRIBUTION)
C
C  RANDOM NUMBER GENERATOR FOR INTEGER NUMBERS DISTRIBUTED ACCORDING TO
C  A NEGATIVE BINOMIAL DISTRIBUTION WITH PARAMETERS <N> AND K
C  DELIVERS ONLY EVEN NUMBERS AS CHARGE MUST BE CONSERVED
C  THIS SUBROUTINE IS CALLED FROM HDPM
C  ARGUMENTS:
C   XN     = <N>  AVERAGE VALUE OF N
C   ECM    = CENTER OF MASS ENERGY
C   N      = RANDOM NUMBER DISTRIBUTED WITH NEG. BIN. DISTR.
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,RANDPA.
      COMMON /RANDPA/  FAC,U1,U2,RD,NSEQ,ISEED,KNOR
      DOUBLE PRECISION FAC,U1,U2
      REAL             RD(3000)
      INTEGER          ISEED(103,10),NSEQ
      LOGICAL          KNOR
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
 
      DOUBLE PRECISION ECM,P,PN,Q,R,SUM,XI,XK,XN
      INTEGER          N
C-----------------------------------------------------------------------
 
CC    IF ( DEBUG ) WRITE(MDEBUG,*) 'RNEGBI: XN,ECM=',SNGL(XN),SNGL(ECM)
 
C  PARAMETRIZATION OF PARAMETER K OF NEG.BIN. DISTRIBUTION ACCORDING
C  TO  UA5 COLLABORATION, PHYS. LETT. 167B (1986) 476
      XK  = 1.D0 / ( -0.104D0 + 0.058D0 * LOG(ECM) )
C  OTHER PARAMETERS
      R   = XN / XK
      Q   = 1.D0 / (1.D0 + R)
      P   = R * Q
 
C  VALUES FOR N EQUAL 0
  1   CONTINUE
      N   = 0
      PN  = Q**XK
      SUM = PN
C  GET UNIFORM RANDOM NUMBER
      CALL RMMAR( RD,1,1 )
      IF ( RD(1) .LE. SUM ) GOTO 100
C  COMPARE WITH SUM OVER P(N)
      DO  2  XI = 1.D0, 1350.D0
        PN  = PN * P * (XK - 1.D0 + XI) / XI
        SUM = SUM + PN
        IF ( RD(1) .LE. SUM ) THEN
          N = XI
          GOTO 100
        ENDIF
  2   CONTINUE
      N = 1350
 
 100  CONTINUE
      IF ( MOD(N,2) .NE. 0  .AND.  N .NE. 1 ) GOTO 1
CC    IF (DEBUG) WRITE(MDEBUG,*)'RNEGBI: RD(1),N,<N>=',RD(1),N,SNGL(XN)
 
      RETURN
      END
