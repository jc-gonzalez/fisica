      SUBROUTINE STANKG
 
C-----------------------------------------------------------------------
C  STA(RT) NKG
C
C  INITIALIZE ARRAYS FOR SINGLE SHOWERS NKG CALCULATED VARIABLES
C  THIS SUBROUTINE IS CALLED FROM MAIN
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,NKGS.
      COMMON /NKGS/    CZX,CZY,CZXY,CZYX,SAH,SL,ZNE
      DOUBLE PRECISION CZX(-10:10,2),CZY(-10:10,2),CZXY(-10:10,2),
     *                 CZYX(-10:10,2),SAH(10),SL(10),ZNE(10)
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
 
      INTEGER I,K
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'STANKG:'
 
C  CLEAR ARRAYS FOR AGE PARAMETER CALCULATION FOR EACH SHOWER
      DO 17  I = 1,10
        SAH(I) = 0.D0
        SL (I) = 0.D0
        ZNE(I) = 0.D0
  17  CONTINUE
C  CLEAR LATERAL ELECTRON DISTRIBUTION COUNTERS FOR EACH SHOWER
      DO 45  K = 1,2
      DO 45  I = -10,10
        CZX (I,K) = 0.D0
        CZY (I,K) = 0.D0
        CZXY(I,K) = 0.D0
        CZYX(I,K) = 0.D0
  45  CONTINUE
 
      RETURN
      END
