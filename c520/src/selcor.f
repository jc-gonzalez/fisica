      SUBROUTINE SELCOR(X,Y)
 
C-----------------------------------------------------------------------
C  SEL(ECT) COR(E LOCATION)
C
C  SELECT A QUASI RANDOM CORE LOCATION
C  THIS SUBROUTINE IS CALLED FROM INPRM
C
C  DESIGN  : J. KNAPP   IK1  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
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
*KEEP,CEREN2.
      COMMON /CEREN2/  PHOTCM,XCER,YCER,UEMIS,VEMIS,CARTIM,ZEMIS,
     *                 DCERX,DCERY,ACERX,ACERY,
     *                 XCMAX,YCMAX,EPSX,EPSY,
     *                 DCERXI,DCERYI,FCERX,FCERY,
     *                 XSCATT,YSCATT,CERXOS,CERYOS,
     *                 NCERX,NCERY,ICERML
      REAL             PHOTCM,XCER,YCER,UEMIS,VEMIS,CARTIM,ZEMIS,
     *                 DCERX,DCERY,ACERX,ACERY,
     *                 XCMAX,YCMAX,EPSX,EPSY,
     *                 DCERXI,DCERYI,FCERX,FCERY,
     *                 XSCATT,YSCATT,CERXOS(20),CERYOS(20)
      INTEGER          NCERX,NCERY,ICERML
*KEND.
 
      REAL    RD(2),X,Y
      LOGICAL FIRST
      SAVE    FIRST
      DATA    FIRST /.TRUE./
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'SELCOR:'
 
C  INITIALIZE SOBOL NUMBER GENERATOR
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL SOBSEQ(-2,RD)
      ENDIF
C  TAKE A PAIR OF QUASI RANDOM NUMBERS
      CALL SOBSEQ(2,RD)
      X = XSCATT * (2.*RD(1)-1.)
      Y = YSCATT * (2.*RD(2)-1.)
 
      IF ( DEBUG ) WRITE(MDEBUG,*)'SELCOR: CORE LOCATION X=',X,' Y=',Y
      RETURN
      END
