      SUBROUTINE AVAGE
 
C-----------------------------------------------------------------------
C  AVE(ERAGE) AGE
C
C  CALCULATES AVERAGE AGE AS A FUNCTION OF RADIUS
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
*KEEP,ELABCT.
      COMMON /ELABCT/  ELCUT
      DOUBLE PRECISION ELCUT(4)
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
 
      DOUBLE PRECISION AJ,BJ,CJ,DF(10),SJ(10),SLLG,TH,ZF
      INTEGER          I,ID,IL,IOL,J,K,L
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'AVAGE :'
 
      IF ( FPRINT ) WRITE(MONIOU,1110) SHOWNO,ELCUT(3),ELCUT(4)
 1110 FORMAT (/' ---------- NKG - OUTPUT OF SHOWER NO ',I10,
     *         ' --------------------------------'/
     *       ' ELECTRON/PHOTON THRESHOLD AT ',F10.5,' /',F10.5,' GEV')
 
C  LOOP OVER ALL DISTANCES WHERE ELECTRON NUMBER IS CALCULATED
      DO 302  K = 1,2
        IF ( OBSATI(K) .GE. 0.D0 ) THEN
          DO 301  ID = -10,10
            DLAX (ID,K) = DLAX (ID,K) + CZX (ID,K)
            DLAY (ID,K) = DLAY (ID,K) + CZY (ID,K)
            DLAXY(ID,K) = DLAXY(ID,K) + CZXY(ID,K)
            DLAYX(ID,K) = DLAYX(ID,K) + CZYX(ID,K)
 301      CONTINUE
        ENDIF
 302  CONTINUE
 
C  CALCULATE LONGITUDINAL SHOWER DEVELOPMENT
      DO 311  IL = 1,IALT(1)
        IF ( SL(IL) .GT. 0.D0 ) THEN
          SEL(IL)   = SEL(IL) + SL(IL)
          SLLG      = LOG10(SL(IL))
          SELLG(IL) = SELLG(IL) + SLLG
          ZEL(IL)   = ZEL(IL) + SL(IL)**2
          ZELLG(IL) = ZELLG(IL) + SLLG**2
          ZF        = ZNE(IL) / SL(IL)
          CALL AGE( ZF,TH )
C  AGE PARAMETERS AVERAGED ON ALL SUBCASCADES AT THIS LEVEL
          SAH(IL) = TH
          STH(IL) = STH(IL) + TH
          ZSL(IL) = ZSL(IL) + TH**2
        ELSE
          SAH(IL) = 0.D0
        ENDIF
        EVTE(175+IL) = SL (IL)
        EVTE(185+IL) = SAH(IL)
        EVTE(215+IL) = TLEV(IL)
        EVTE(225+IL) = TLEVCM(IL)
 311  CONTINUE
 
C  PRINT LONGITUDINAL SHOWER DEVELOPMENT
      IF ( FPRINT ) WRITE(MONIOU,229)
     *  (I,TLEV(I),TLEVCM(I),SL(I),SAH(I),I=1,IALT(1))
  229 FORMAT(
     *  /' LEVEL',2X,'THICKNESS',8X,'HEIGHT',5X,'ELECT. NUMBER',7X,'AGE'
     *  /'  NO. ',2X,'  G/CM**2',8X,'    CM'/
     *        (' ',I4,F12.0,2X,F12.0,1X,F17.3,F10.3) )
 
      DO 312  IOL = 1,2
        IF ( OBSATI(IOL) .LT. 0.D0 ) GOTO 312
C  DETERMINE LOCAL AGE PARAMETER
        DO 50  J = 1,9
          IF ( CZX(J+1,IOL).GT.0.D0  .AND.  CZX(-J-1,IOL).GT.0.D0  .AND.
     *         CZXY(J+1,IOL).GT.0.D0 .AND.  CZXY(-J-1,IOL).GT.0.D0 .AND.
     *         CZYX(J+1,IOL).GT.0.D0 .AND.  CZYX(-J-1,IOL).GT.0.D0 .AND.
     *         CZY(J+1,IOL).GT.0.D0  .AND.  CZY(-J-1,IOL).GT.0.D0 ) THEN
            AJ = 0.125D0 * (
     *           CZX(J,IOL) /CZX(J+1,IOL) + CZX(-J,IOL) /CZX(-J-1,IOL)
     *         + CZXY(J,IOL)/CZXY(J+1,IOL)+ CZXY(-J,IOL)/CZXY(-J-1,IOL)
     *         + CZYX(J,IOL)/CZYX(J+1,IOL)+ CZYX(-J,IOL)/CZYX(-J-1,IOL)
     *         + CZY(J,IOL) /CZY(J+1,IOL) + CZY(-J,IOL) /CZY(-J-1,IOL) )
          ELSE
            AJ = 0.D0
          ENDIF
          IF ( AJ .GT. 0.D0 ) THEN
            BJ    = DIST(J) / DIST(J+1)
            CJ    = (DIST(J)+RMOL(IOL)) / (DIST(J+1)+RMOL(IOL))
            SJ(J) = LOG(AJ * BJ**2 * CJ**4.5D0) / LOG(BJ * CJ)
            DF(J) = 0.5D0 * (DIST(J) + DIST(J+1))
          ELSE
            SJ(J) = 0.D0
            DF(J) = 0.D0
          ENDIF
  50    CONTINUE
 
        DO L = 1,10
          EVTE(165+IOL*40+L) = SJ(L)
        ENDDO
 
        IF ( FPRINT ) THEN
C  WRITE LOCAL AGE PARAMETER
          WRITE(MONIOU,60) IOL,OBSATI(IOL), (I,DF(I),SJ(I),I=1,9)
  60      FORMAT(/' RADIAL BIN  DISTANCE(CM)  LOCAL AGE  AT LEVEL NO.',
     *          I4,' AT HEIGHT:',F10.0,' CM'/
     *         (' ',I10,'   ',F10.0,'  ',F10.3 ) )
 
C  PRINT LATERAL ELECTRON DISTRIBUTION
          WRITE(MONIOU,507) IOL,OBSATI(IOL)
 507      FORMAT(/' LATERAL ELECTRON DENSITY (/CM**2)    AT LEVEL NO.',
     *           I4,' AT HEIGHT:',F10.0,' CM'/
     *           ' --------------------------------------------------',
     *           '---------------------------'/
     *           '  DIST (CM)        CZX            CZY      ',
     *           '      CZXY           CZYX     ')
          WRITE(MONIOU,508) (DISX(I),CZX(I,IOL),CZY(I,IOL),
     *                      CZXY(I,IOL),CZYX(I,IOL),I=-10,10)
 508      FORMAT(' ',0P,F10.0,1P,4E15.5)
        ENDIF
 
 312  CONTINUE
 
      DO  L = 1,10
        EVTE(195+L) = DIST(L)
        EVTE(235+L) = DF(L)
      ENDDO
 
C  WRITE NKG - SHOWER INFORMATION TO EVENT END BLOCK
      DO 353  L = 1,21
        EVTE(  7+L) = CZX (-11+L,1)
        EVTE( 28+L) = CZY (-11+L,1)
        EVTE( 49+L) = CZXY(-11+L,1)
        EVTE( 70+L) = CZYX(-11+L,1)
        EVTE( 91+L) = CZX (-11+L,2)
        EVTE(112+L) = CZY (-11+L,2)
        EVTE(133+L) = CZXY(-11+L,2)
        EVTE(154+L) = CZYX(-11+L,2)
 353  CONTINUE
 
      RETURN
      END
