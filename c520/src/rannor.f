      DOUBLE PRECISION FUNCTION RANNOR( A,B )
 
C-----------------------------------------------------------------------
C  RAN(DOM NUMBER) NOR(MALLY DISTRIBUTED)
C
C  GENERATES NORMAL DISTRIBUTED RANDOM NUMBER
C  DELIVERS 2 UNCORRELATED RANDOM NUMBERS,
C  THEREFORE RANDOM CALLS ARE ONLY NECESSARY EVERY SECOND TIME.
C  REFERENCE : NUMERICAL RECIPES, W.H. PRESS ET AL.,
C              CAMBRIDGE UNIVERSITY PRESS, 1992  ISBN 0 521 43064 X
C  THIS FUNCTION IS CALLED FROM HDPM, LEADDF, NIHILA, PARRAP, QGSTOR,
C  UPDATE, AND VAPOR
C  ARGUMENTS:
C   A      = MEAN VALUE
C   B      = STANDARD DEVIATION
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
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
 
      DOUBLE PRECISION A,B,RR
C-----------------------------------------------------------------------
 
CC    IF ( DEBUG ) WRITE(MDEBUG,100) SNGL(A),SNGL(B)
CC100 FORMAT(' RANNOR: A,B=',1P,2E10.3)
      IF ( KNOR ) THEN
  1     CONTINUE
        CALL RMMAR( RD,2,1 )
        U1 = 2.D0*RD(1) - 1.D0
        U2 = 2.D0*RD(2) - 1.D0
        RR = U1**2 + U2**2
        IF ( RR .GE. 1.D0 .OR. RR .EQ. 0.D0 ) GOTO 1
        FAC = SQRT( -2.D0 * LOG(RR) / RR )
 
        RANNOR = FAC * U1 * B + A
        KNOR   = .FALSE.
      ELSE
        RANNOR = FAC * U2 * B + A
        KNOR   = .TRUE.
      ENDIF
CC    IF ( DEBUG ) WRITE(MDEBUG,101) RANNOR
CC101 FORMAT('+',34X,' RANNOR =',1P,E12.5)
 
      RETURN
      END
