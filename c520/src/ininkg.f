      SUBROUTINE ININKG
 
C-----------------------------------------------------------------------
C  INI(TIALIZE) NKG
C
C  INITIALIZES ARRAYS FOR NKG CALCULATING VARIABLES
C  THIS SUBROUTINE IS CALLED FROM MAIN
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,BUFFS.
      COMMON /BUFFS/   RUNH,RUNE,EVTH,EVTE,DATAB,LH
      INTEGER          MAXBUF,MAXLEN
      PARAMETER        (MAXBUF=39*7)
      PARAMETER        (MAXLEN=12)
      REAL             RUNH(MAXBUF),EVTH(MAXBUF),EVTE(MAXBUF),
     *                 RUNE(MAXBUF),DATAB(MAXBUF)
      INTEGER          LH
      CHARACTER*4      CRUNH,CRUNE,CEVTH,CEVTE
      EQUIVALENCE      (RUNH(1),CRUNH), (RUNE(1),CRUNE)
      EQUIVALENCE      (EVTH(1),CEVTH), (EVTE(1),CEVTE)
*KEEP,NKGI.
      COMMON /NKGI/    SEL,SELLG,STH,ZEL,ZELLG,ZSL,DIST,
     *                 DISX,DISY,DISXY,DISYX,DLAX,DLAY,DLAXY,DLAYX,
     *                 OBSATI,RADNKG,RMOL,TLEV,TLEVCM,IALT
      DOUBLE PRECISION SEL(10),SELLG(10),STH(10),ZEL(10),ZELLG(10),
     *                 ZSL(10),DIST(10),
     *                 DISX(-10:10),DISY(-10:10),
     *                 DISXY(-10:10,2),DISYX(-10:10,2),
     *                 DLAX (-10:10,2),DLAY (-10:10,2),
     *                 DLAXY(-10:10,2),DLAYX(-10:10,2),
     *                 OBSATI(2),RADNKG,RMOL(2),TLEV(10),TLEVCM(10)
      INTEGER          IALT(2)
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
 
      DOUBLE PRECISION DEPTH,HEIGH,RHOF,RMGCM,THICK
      INTEGER          I,IL,K,KL
      EXTERNAL         HEIGH,RHOF,THICK
      DATA             RMGCM / 9.6D0 /
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'ININKG:'
 
C  SET LATERAL DISTRIBUTION DISTANCES
      IF ( RADNKG .LE. 100.D0 ) THEN
        WRITE(MONIOU,*) 'ININKG: RADNKG=',RADNKG,' CM  TOO SMALL '
        RADNKG = 200.D2
        WRITE(MONIOU,*) '        RADNKG CORRECTED TO  ',RADNKG,' CM'
      ENDIF
      EVTH(148) = RADNKG
      DO I=1,10
        DIST(I)  = 100.D0 * 10.D0**(LOG10(RADNKG/100.D0)*0.1D0*I)
        DISX(I)  =  DIST(I)
        DISX(-I) = -DIST(I)
      ENDDO
      DISX(0) = 0.D0
 
C  MOLIERE RADIUS FOR COULOMB SCATTERING ; EQUIVALENT TO 9.6 G/CM**2
C  OBSERVATION LEVELS AND CORRESPONDING MOLIERE RADII (IN CM) FOR NKG
      OBSATI(1) = OBSLEV(NOBSLV)
      RMOL  (1) = RMGCM / RHOF(OBSATI(1))
      IF ( NOBSLV .GT. 1 ) THEN
        OBSATI(2) = OBSLEV(NOBSLV-1)
        RMOL  (2) = RMGCM / RHOF(OBSATI(2))
      ELSE
        OBSATI(2) = -1.D0
        RMOL  (2) =  0.D0
        IALT  (2) =  0
      ENDIF
 
C  CALCULATE COORDINATES OF POINTS ON THE X AND Y AXIS AND THE TWO
C  DIAGONAL LINES  Y IS X AND Y IS -X
      DO 3333  KL = -10,10
        DISY (KL)   =  DISX (KL)
        DISXY(KL,1) =  DISX (KL) / SQRT(2.D0)
        DISXY(KL,2) =  DISXY(KL,1)
        DISYX(KL,1) =  DISXY(KL,1)
        DISYX(KL,2) = -DISXY(KL,2)
 3333 CONTINUE
C  CLEAR ARRAY FOR LATERAL ELECTRON DISTR. (AVERAGE OVER ALL SHOWERS)
      DO 45  K = 1,2
      DO 45  I = -10,10
        DLAX (I,K) = 0.D0
        DLAY (I,K) = 0.D0
        DLAXY(I,K) = 0.D0
        DLAYX(I,K) = 0.D0
  45  CONTINUE
C  CLEAR ARRAY FOR AGE PARAMETER CALCULATION (AVERAGE OVER ALL SHOWERS)
      DO 17  I = 1,10
        SEL(I)   = 0.D0
        SELLG(I) = 0.D0
        STH(I)   = 0.D0
        ZELLG(I) = 0.D0
        ZEL(I)   = 0.D0
        ZSL(I)   = 0.D0
  17  CONTINUE
 
C  LAST OBSERVATION LEVEL DEPTH IS GIVEN IN G/CM**2
      DEPTH   = THICK(OBSATI(1))
      IALT(1) = MIN( 10, INT(DEPTH/102.D0)+1 )
C  CALCULATE 10 LEVELS AT EACH 100 G/CM**2
      DO 111  IL = 1,IALT(1)-1
        TLEV  (IL) = 100.D0 * IL
        TLEVCM(IL) = HEIGH(TLEV(IL))
 111  CONTINUE
C  FOR LAST LEVEL NOT IL*100 BUT OBSERVATION LEVEL
      TLEV  (IALT(1)) = DEPTH
      TLEVCM(IALT(1)) = OBSATI(1)
C  SECOND OBSERVATION LEVEL ?
      IF ( OBSATI(2) .GE. 0.D0 ) THEN
        DEPTH           = THICK(OBSATI(2))
        IALT(2)         = INT(DEPTH/102.D0) + 1
        IF ( IALT(2) .GE. IALT(1) ) IALT(2) = IALT(1) - 1
        TLEV  (IALT(2)) = DEPTH
        TLEVCM(IALT(2)) = OBSATI(2)
      ENDIF
 
      RETURN
      END
