      DOUBLE PRECISION FUNCTION PCL( A,B )
 
C-----------------------------------------------------------------------
C  P (MOMENTUM) C(ENTER OF MASS) L(ONGITUDINAL)
C
C  RANDOM SELECTION OF LONGITUDINAL MOMENTUM IN CENTER OF MASS
C  EXPONENTIAL DISTRIBUTION   PCL(X) = EXP(-X/A)
C  THIS FUNCTION IS CALLED FORM HMESON, ISOBAR, AND VHMESO
C  ARGUMENTS:
C   A      = MEAN VALUE OF DISTRIBUTION
C   B      = CUT FOR LIMITING MOMENTUM
C
C  CHANGES : J. KNAPP   IK1  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
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
 
C-----------------------------------------------------------------------
 
C     IF ( DEBUG ) WRITE(MDEBUG,*) 'PCL   : A,B=',SNGL(A),SNGL(B)
 
    1 CONTINUE
      CALL RMMAR( RD,1,1 )
      PCL = -A * LOG ( RD(1) )
      IF ( PCL .GT. B ) GOTO 1
      IF ( DEBUG ) WRITE(MDEBUG,*) 'PCL   : PCL =',SNGL(PCL)
 
      RETURN
      END
