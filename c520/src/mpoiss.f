      SUBROUTINE MPOISS(AMEAN,NPRAN)
 
C-----------------------------------------------------------------------
C   M(UON COULOMB SCATTERING) POISS(ON DISTRIBUTION)
C
C  GENERATES A RANDOM NUMBER POISSON DISTRIBUTED WITH MEAN VALUE AMEAN.
C  THIS SUBROUTINE IS IN ANALOGY WITH SUBROUTINE GPOISS.
C  (AUTHOR: L. URBAN) OF GEANT321
C  SEE CERN PROGRAM LIBRARY LONG WRITEUP W5013.
C  THIS SUBROUTINE IS CALLED FROM MUCOUL
C  ARGUMENTS:
C   AMEAN =  MEAN VALUE OF RANDOM NUMBER
C   NPRAN =  RANDOM NUMBER POISSON DISTRIBUTED
C
C  REDESIGN: D. HECK    IK3  FZK KARLSRUHE
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
 
      DOUBLE PRECISION AMEAN,AN,HMXINT,P,PLIM,RR,S,X
      INTEGER          NPRAN
      DATA             PLIM/16.D0/,HMXINT/2.D9/
C-----------------------------------------------------------------------
 
C  PROTECTION AGAINST NEGATIVE MEAN VALUES
      AN = 0.D0
      IF ( AMEAN .GT. 0.D0 ) THEN
        IF ( AMEAN .LE. PLIM ) THEN
          CALL RMMAR(RD,1,1)
          P = EXP(-AMEAN)
          S = P
          IF ( RD(1) .LE. S ) GOTO 20
 10       AN = AN + 1.D0
          P = P * AMEAN / AN
          S = S + P
          IF ( S .LT. RD(1)  .AND.  P .GT. 1.D-30 ) GOTO 10
        ELSE
          CALL RMMAR(RD,2,1)
          RR  = SQRT( -2.D0*LOG(RD(1)) )
          X = RR * COS( PI2 * RD(2) )
          AN = MIN( MAX( AMEAN+X*SQRT(AMEAN), 0.D0 ), HMXINT )
        ENDIF
      ENDIF
 20   NPRAN = AN
 
      RETURN
      END
