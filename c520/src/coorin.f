      SUBROUTINE COORIN( HEIGHT )
 
C-----------------------------------------------------------------------
C  COOR(DINATE) IN(ITIALIZATION)
C
C  INITIALIZES COORDINATE CORRECTION FOR EACH OBSERVATION LEVEL
C  ROUTINE SHOULD BE CALLED AFTER HEIGHT OF FIRST INTERACTION IS
C  DETERMINED. X,Y COORDINATES OF 1. INERACTION ARE ASSUMED TO BE 0,0.
C  THIS SUBROUTINE IS CALLED FROM MAIN, ELECTR, AND PHOTON
C  ARGUMENT:
C   HEIGHT = HEIGHT OF 1. INTERACTION
C
C  AUTHOR  : J. KNAPP   IK1  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,OBSPAR.
      COMMON /OBSPAR/  OBSLEV,THCKOB,XOFF,YOFF,THETAP,PHIP,
     *                 THETPR,PHIPR,NOBSLV
      DOUBLE PRECISION OBSLEV(10),THCKOB(10),XOFF(10),YOFF(10),
     *                 THETAP,THETPR(2),PHIP,PHIPR(2)
      INTEGER          NOBSLV
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
 
      DOUBLE PRECISION DXY,HEIGHT,TANTE
      INTEGER          I
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'COORIN: HEIGHT,THETA,PHI =',
     *                     SNGL(HEIGHT),SNGL(THETAP),SNGL(PHIP)
 
      TANTE = TAN( THETAP )
      DO  1  I = 1,NOBSLV
        DXY     = TANTE * ( HEIGHT - OBSLEV(I) )
        XOFF(I) = COS( PHIP ) * DXY
        YOFF(I) = SIN( PHIP ) * DXY
    1 CONTINUE
 
      IF ( DEBUG ) WRITE(MDEBUG,100)
     *                    (OBSLEV(I),XOFF(I),YOFF(I),I=1,NOBSLV)
  100 FORMAT(' COORIN: OBSLVL,XOFF,YOFF= ',1P,3E12.4)
 
      RETURN
      END
