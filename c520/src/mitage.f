      SUBROUTINE MITAGE
 
C-----------------------------------------------------------------------
C  MIT(TELWERT) AGE   (AVERAGE AGE)
C
C  CALCULATES AVERAGE DISTRIBUTION FOR NKG FUNCTION OVER ALL SHOWERS
C  THIS SUBROUTINE IS CALLED FROM MAIN
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
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
 
      DOUBLE PRECISION AJ,ATH,BJ,CJ,DF(10),RISH,SELEC,SELCLG,SJ(10),
     *                 ZEC,ZECLG,ZSE
      INTEGER          I,ID,J,K,LI
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'MITAGE:'
 
      WRITE(MONIOU,349) ELCUT(3),ELCUT(4)
  349 FORMAT(/' ========== NKG - AVERAGE VALUES OF ALL SHOWERS ',
     *        '==============================='/
     *   ' ELECTRON/PHOTON THRESHOLDS AT ',F9.4,' /',F9.4,' GEV'//
     *   ' LEVEL  THICKNESS   HEIGHT      <EL NR>      DEV<EL>',
     *   '   <LOG(EL NR)>  DEV<LOG(E)>      <AGE>  DEV<AGE>'/
     *   '  NO.   (G/CM**2)     (M) ' )
 
C  NORMALIZE AVERAGE ELECTRON DENSITIES
      RISH = 1.D0 / ISHW
      DO 161  K = 1,2
        IF ( OBSATI(K) .GE. 0.D0 ) THEN
          DO 162  ID = -10,10
            DLAX (ID,K) = DLAX (ID,K) * RISH
            DLAY (ID,K) = DLAY (ID,K) * RISH
            DLAXY(ID,K) = DLAXY(ID,K) * RISH
            DLAYX(ID,K) = DLAYX(ID,K) * RISH
  162     CONTINUE
        ENDIF
  161 CONTINUE
 
      DO 16  LI = 1,IALT(1)
C  ELECTRON NUMBER <N_E>
        SELEC   = SEL(LI) * RISH
C  LOG10 ELECTRON NUMBER <N_E>
        SELCLG  = SELLG(LI) * RISH
C  <S_....> AVERAGE LONGITUDINAL AGE
        ATH     = STH(LI) * RISH
        IF ( ISHW .GT. 1 ) THEN
C  ELECTRON NUMBER <N_E>
          ZEC   = SQRT( abs(ZEL(LI) - SEL(LI)**2*RISH)/(ISHW-1.D0) )
C  LOG10 ELECTRON NUMBER <N_E>
          ZECLG = SQRT( abs(ZELLG(LI) - SELLG(LI)**2*RISH)/(ISHW-1.D0) )
C  <S_....> AVERAGE LONGITUDINAL AGE
          ZSE   = SQRT( abs(ZSL(LI)-STH(LI)**2*RISH)/(ISHW-1.D0) )
        ELSE
          ZEC   = 0.D0
          ZECLG = 0.D0
          ZSE   = 0.D0
        ENDIF
C  WRITE ELECTRON INFORMATION FOR ALL NKG LEVELS (LONG. DEVELOPMENT)
        WRITE(MONIOU,219) LI,TLEV(LI),TLEVCM(LI)*0.01,
     *                       SELEC,ZEC,SELCLG,ZECLG,ATH,ZSE
  219   FORMAT (' ',I4,F10.0,F11.2,1X,2F12.0,3X,2F12.5,F13.3,F9.3)
  16  CONTINUE
 
      DO 520  K = 1,2
        IF ( OBSATI(K) .LT. 0.D0 ) GOTO 520
C  DETERMINE LOCAL AGE PARAMETER
        DO 50  J = 1,9
          IF ( DLAX(J+1,K).GT.0.D0  .AND.  DLAX(-J-1,K).GT.0.D0  .AND.
     *         DLAXY(J+1,K).GT.0.D0  .AND.  DLAXY(-J-1,K).GT.0.D0  .AND.
     *         DLAYX(J+1,K).GT.0.D0  .AND.  DLAYX(-J-1,K).GT.0.D0  .AND.
     *         DLAY(J+1,K).GT.0.D0  .AND.  DLAY(-J-1,K).GT.0.D0 ) THEN
            AJ = 0.125D0 * (
     *           DLAX(J,K) /DLAX(J+1,K)  + DLAX(-J,K) /DLAX(-J-1,K)
     *         + DLAXY(J,K)/DLAXY(J+1,K) + DLAXY(-J,K)/DLAXY(-J-1,K)
     *         + DLAYX(J,K)/DLAYX(J+1,K) + DLAYX(-J,K)/DLAYX(-J-1,K)
     *         + DLAY(J,K) /DLAY(J+1,K)  + DLAY(-J,K) /DLAY(-J-1,K))
          ELSE
            AJ = 0.D0
          ENDIF
          IF ( AJ .GT. 0.D0 ) THEN
            BJ    = DIST(J) / DIST(J+1)
            CJ    = (DIST(J)+RMOL(K)) / (DIST(J+1)+RMOL(K))
            SJ(J) = LOG(AJ * BJ**2 * CJ**4.5D0) / LOG(BJ * CJ)
            DF(J) = 0.5D0* (DIST(J) + DIST(J+1))
          ELSE
            SJ(J) = 0.D0
            DF(J) = 0.D0
          ENDIF
  50    CONTINUE
 
C  WRITE LOCAL AGE PARAMETER
        WRITE(MONIOU,60) K,OBSATI(K), (I,DF(I),SJ(I),I=1,9)
  60    FORMAT(/' RADIAL BIN  DISTANCE(CM)  LOCAL AGE AT LEVEL NO.',
     *          I4,' AT HEIGHT:',F10.0,' CM'/
     *         (' ',I9,'   ',F10.0,'  ',F10.3 ) )
 
C  WRITE LATERAL ELECTRON DISTRIBUTION
        WRITE(MONIOU,507) K,OBSATI(K)
 507    FORMAT(/' AVERAGE ELECTRON DENSITY (/CM**2)  FOR LEVEL NO.',
     *         I4,' AT HEIGHT:',F10.0,' CM'/
     *         ' ==================================================',
     *         '=========================='/
     *         '  DIST (CM)       DLAX           DLAY    ',
     *         '       DLAXY          DLAYX')
        WRITE(MONIOU,508) (DISX(I),DLAX(I,K),DLAY(I,K),
     *                     DLAXY(I,K),DLAYX(I,K),I=-10,10)
  508   FORMAT(' ',0P,F10.0,1P,4E15.5)
 
 
  520 CONTINUE
 
 
      RETURN
      END
