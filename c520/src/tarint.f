      SUBROUTINE TARINT
 
C-----------------------------------------------------------------------
C  TAR(GET) INT(ERACTIONS)
C
C  ROUTINE DETERMINES HOW MANY INTERACTIONS OCCUR IN TARGET
C  THIS SUBROUTINE IS CALLED FROM HDPM
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*KEEP,DPMFLG.
      COMMON /DPMFLG/  NFLAIN,NFLDIF,NFLPI0,NFLCHE,NFLPIF,NFRAGM
      INTEGER          NFLAIN,NFLDIF,NFLPI0,NFLCHE,NFLPIF,NFRAGM
*KEEP,GNUPR.
      COMMON /GNUPR/   SE14,SE16,SE40
      DOUBLE PRECISION SE14(3,14),SE16(3,16),SE40(3,40)
*KEEP,INTER.
      COMMON /INTER/   AVCH,AVCH3,DC0,DLOG,DMLOG,ECMDIF,ECMDPM,ELAB,
     *                 FNEUT,FNEUT2,GNU,PLAB,POSC2,POSC3,POSN2,POSN3,
     *                 RC3TO2,S,SEUGF,SEUGP,SLOG,SLOGSQ,SMLOG,
     *                 WIDC2,WIDC3,WIDN2,WIDN3,YCM,YY0,ZN,
     *                 IDIF,ITAR
      DOUBLE PRECISION AVCH,AVCH3,DC0,DLOG,DMLOG,ECMDIF,ECMDPM,ELAB,
     *                 FNEUT,FNEUT2,GNU,PLAB,POSC2,POSC3,POSN2,POSN3,
     *                 RC3TO2,S,SEUGF,SEUGP,SLOG,SLOGSQ,SMLOG,
     *                 WIDC2,WIDC3,WIDN2,WIDN3,YCM,YY0,ZN
      INTEGER          IDIF,ITAR
*KEEP,PARPAR.
      COMMON /PARPAR/  CURPAR,SECPAR,PRMPAR,OUTPAR,C,
     *                 E00,E00PN,PTOT0,PTOT0N,THICKH,ITYPE,LEVL
      DOUBLE PRECISION CURPAR(14),SECPAR(14),PRMPAR(14),OUTPAR(14),
     *                 C(50),E00,E00PN,PTOT0,PTOT0N,THICKH
      INTEGER          ITYPE,LEVL
*KEEP,RANDPA.
      COMMON /RANDPA/  FAC,U1,U2,RD,NSEQ,ISEED,KNOR
      DOUBLE PRECISION FAC,U1,U2
      REAL             RD(3000)
      INTEGER          ISEED(103,10),NSEQ
      LOGICAL          KNOR
*KEEP,REST.
      COMMON /REST/    CONTNE,TAR,LT
      DOUBLE PRECISION CONTNE(3),TAR
      INTEGER          LT
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
*KEEP,SIGM.
      COMMON /SIGM/    SIGMA,SIGANN,SIGAIR,FRACTN,FRCTNO
      DOUBLE PRECISION SIGMA,SIGANN,SIGAIR,FRACTN,FRCTNO
*KEND.
 
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'TARINT: ITYPE,TAR,NFLAIN',
     *                                ITYPE,SNGL(TAR),NFLAIN
 
C  NFLAIN EQUAL 0 : NUMBER OF INTERACTIONS IN TARGET CHOSEN RANDOMLY
      IF ( NFLAIN .EQ. 0 ) THEN
C  SIGMA IS ALREADY CALCULATED IN BOX2
        DELSIG = SIGMA - 45.D0
        DSIGSQ = DELSIG**2
 
C  CHOOSE RANDOM NUMBER
        CALL RMMAR( RD,1,1 )
        IF ( DEBUG ) WRITE(MDEBUG,*)'TARINT: DELSIG,DSIGSQ,RD(1),TAR=',
     *                      SNGL(DELSIG),SNGL(DSIGSQ),RD(1),SNGL(TAR)
 
C  DO INTERACTION WITH CHOSEN TARGET ( N, O, AR )
        PROB  = 0.D0
C  TREAT INTERACTION WITH NITROGEN TARGET
        IF     ( TAR .EQ. 14.D0 ) THEN
C  SUM OF PROBABILITIES FOR COLLISION WITH NITROGEN TARGET
          DO 6151  JL = 1,14
            PROB = PROB +
     *             SE14(1,JL) + SE14(2,JL)*DELSIG + SE14(3,JL)*DSIGSQ
            IF ( RD(1) .LE. PROB ) GOTO 7332
 6151     CONTINUE
 
C  TREAT INTERACTION WITH OXYGEN TARGET
        ELSEIF ( TAR .EQ. 16.D0 ) THEN
C  SUM OF PROBABILITIES FOR COLLISION WITH OXYGEN TARGET
          DO 6152  JL = 1,16
            PROB = PROB +
     *             SE16(1,JL) + SE16(2,JL)*DELSIG + SE16(3,JL)*DSIGSQ
            IF ( RD(1) .LE. PROB ) GOTO 7332
 6152     CONTINUE
 
C  TREAT INTERACTION WITH ARGON TARGET
        ELSEIF ( TAR .EQ. 40.D0 ) THEN
C  SUM OF PROBABILITIES FOR COLLISION WITH ARGON TARGET
          DO 6153  JL = 1,40
            PROB = PROB +
     *             SE40(1,JL) + SE40(2,JL)*DELSIG + SE40(3,JL)*DSIGSQ
            IF ( RD(1) .LE. PROB ) GOTO 7332
 6153     CONTINUE
        ELSE
          WRITE(MONIOU,*) 'TARINT: UNKNOWN TARGET = ',SNGL(TAR)
        ENDIF
        JL  = 1
 
C  NUMBER OF COLLISIONS IN TARGET
 7332   CONTINUE
        GNU = DBLE(JL)
 
      ELSE
C  NFLAIN EQUAL 1 : AVERAGE NUMBER OF INTERACTIONS IN TARGET IS TAKEN
C  NEW PARAMETRIZATION OF J.N.CAPDEVIELLE (MARCH 93)
        GNU = (0.4826D0 + 3.522D-2 * SLOG) * TAR**0.31D0
      ENDIF
      IF ( DEBUG ) WRITE(MDEBUG,*)
     *          'TARINT: # COLLISIONS IN TARGET=',SNGL(GNU)
 
      RETURN
      END