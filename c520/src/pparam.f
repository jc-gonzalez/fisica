      SUBROUTINE PPARAM
 
C-----------------------------------------------------------------------
C  P(ARTICLE) PARAM(ETERS)
C
C  SETS PARAMETERS (PARTICLE TYP, TRANSVERSE MOMENTUM)
C  OF SECONDARY PARTICLES IN HDPM
C  THIS SUBROUTINE IS CALLED FROM HDPM
C
C  DESIGN  : D. HECK    IK3  FZK KARLSRUHE
C  CHANGES : J.N. CAPDEVIELLE CDF PARIS
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*KEEP,AVPT.
      COMMON /AVPT/    AVPT,AVPK,AVPN,AVPH,AVPE
      DOUBLE PRECISION AVPT,AVPK,AVPN,AVPH,AVPE
*KEEP,DPMFLG.
      COMMON /DPMFLG/  NFLAIN,NFLDIF,NFLPI0,NFLCHE,NFLPIF,NFRAGM
      INTEGER          NFLAIN,NFLDIF,NFLPI0,NFLCHE,NFLPIF,NFRAGM
*KEEP,INDICE.
      COMMON /INDICE/  NNUCN,NKA0,NHYPN,NETA,NETAS,NPIZER,
     *                 NNC,NKC,NHC,NPC,NCH,NNN,NKN,NHN,NET,NPN
      INTEGER          NNUCN(2:3),NKA0(2:3),NHYPN(2:3),NETA(2:3,1:4),
     *                 NETAS(2:3),NPIZER(2:3),
     *                 NNC,NKC,NHC,NPC,NCH,NNN,NKN,NHN,NET,NPN
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
*KEEP,LEPAR.
      COMMON /LEPAR/   LEPAR1,LEPAR2,LASTPI,NRESPC,NRESPN,NCPLUS
      INTEGER          LEPAR1,LEPAR2,LASTPI,NRESPC,NRESPN,NCPLUS
*KEEP,NEWPAR.
      COMMON /NEWPAR/  EA,PT2,PX,PY,TMAS,YR,ITYP,
     *                 IA1,IA2,IB1,IB2,IC1,IC2,ID1,ID2,IE1,IE2,IF1,IF2,
     *                 IG1,IG2,IH1,IH2,II1,II2,IJ1,NTOT
      DOUBLE PRECISION EA(3000),PT2(3000),PX(3000),PY(3000),TMAS(3000),
     *                 YR(3000)
      INTEGER          ITYP(3000),
     *                 IA1,IA2,IB1,IB2,IC1,IC2,ID1,ID2,IE1,IE2,IF1,IF2,
     *                 IG1,IG2,IH1,IH2,II1,II2,IJ1,NTOT
*KEEP,PAM.
      COMMON /PAM/     PAMA,SIGNUM
      DOUBLE PRECISION PAMA(6000),SIGNUM(6000)
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
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'PPARAM: NTOT,NPC,NCPLUS=',
     *                                      NTOT,NPC,NCPLUS
 
C  FILL PARTICLES INTO ARRAYS, CALCULATE PT AND SUM UP
      SPX   = 0.D0
      SPY   = 0.D0
      NPART = 3
C  PROTON ANTIPROTON PAIRS
      DO 1003  K = 1,NNC
        CALL RMMAR( RD,1,1 )
        IF ( RD(1) .LT. 0.5 ) THEN
          ITYP(NPART)   = 14
          ITYP(NPART+1) = 15
        ELSE
          ITYP(NPART)   = 15
          ITYP(NPART+1) = 14
        ENDIF
        CALL PTRAM( ZN,AVPN,PX(NPART),PY(NPART) )
        CALL PTRAM( ZN,AVPN,PX(NPART+1),PY(NPART+1) )
        SPX   = SPX + PX(NPART) + PX(NPART+1)
        SPY   = SPY + PY(NPART) + PY(NPART+1)
        NPART = NPART + 2
 1003 CONTINUE
C  K+ K- PAIRS
      DO 1004  K = 1,NKC
        CALL RMMAR( RD,1,1 )
        IF ( RD(1) .LT. 0.5 ) THEN
          ITYP(NPART)   = 11
          ITYP(NPART+1) = 12
        ELSE
          ITYP(NPART)   = 12
          ITYP(NPART+1) = 11
        ENDIF
        CALL PTRAM( ZN,AVPK,PX(NPART),PY(NPART) )
        CALL PTRAM( ZN,AVPK,PX(NPART+1),PY(NPART+1) )
        SPX   = SPX + PX(NPART) + PX(NPART+1)
        SPY   = SPY + PY(NPART) + PY(NPART+1)
        NPART = NPART + 2
 1004 CONTINUE
C  SIGMA PAIRS
      DO 1005  K = 1,NHC
        CALL RMMAR( RD,2,1 )
        IF ( RD(1) .LT. 0.5 ) THEN
          IF ( RD(2) .LT. 0.5 ) THEN
            ITYP(NPART)   = 19
            ITYP(NPART+1) = 27
          ELSE
            ITYP(NPART)   = 27
            ITYP(NPART+1) = 19
          ENDIF
        ELSE
          IF ( RD(2) .LT. 0.5 ) THEN
            ITYP(NPART)   = 21
            ITYP(NPART+1) = 29
          ELSE
            ITYP(NPART)   = 29
            ITYP(NPART+1) = 21
          ENDIF
        ENDIF
        CALL PTRAM( ZN,AVPH,PX(NPART),PY(NPART) )
        CALL PTRAM( ZN,AVPH,PX(NPART+1),PY(NPART+1) )
        SPX   = SPX + PX(NPART) + PX(NPART+1)
        SPY   = SPY + PY(NPART) + PY(NPART+1)
        NPART = NPART + 2
 1005 CONTINUE
 
C  DECIDE WITH WHICH CHARGED PION TO START WITH
C  NUMBER OF PIONS MAY BE ODD IN THE CASE IF ISEL IS 1
      CALL RMMAR( RD,1,1 )
      IF ( RD(1) .GT. 0.5 ) THEN
        NPIOCH = 0
      ELSE
        NPIOCH = 1
      ENDIF
      NPOS = NCPLUS
C  PI +-
      DO 1007  K = 1,NPC
        IF     ( NPC-K+1 .LE.  NPOS ) THEN
          NPIOCH = 1
          IF ( DEBUG ) WRITE(MDEBUG,*) '   NPC,K,NPOS,NPIOCH=',
     *                                     NPC,K,NPOS,NPIOCH
        ELSEIF ( NPC-K+1 .LE. -NPOS ) THEN
          NPIOCH = 0
          IF ( DEBUG ) WRITE(MDEBUG,*) '   NPC,K,-NPOS,NPIOCH=',
     *                                     NPC,K,-NPOS,NPIOCH
        ENDIF
        IF ( NPIOCH .EQ. 0 ) THEN
          ITYP(NPART) = 8
          NPIOCH = 1
          NPOS   = NPOS + 1
        ELSE
          ITYP(NPART) = 9
          NPIOCH = 0
          NPOS   = NPOS - 1
        ENDIF
        CALL PTRAM( ZN,AVPT,PX(NPART),PY(NPART) )
        SPX   = SPX + PX(NPART)
        SPY   = SPY + PY(NPART)
        NPART = NPART + 1
 1007 CONTINUE
C  NEUTRON ANTINEUTRON PAIRS
      DO 1008  K = 1,NNN
        CALL RMMAR( RD,1,1 )
        IF ( RD(1) .LT. 0.5 ) THEN
          ITYP(NPART)   = 13
          ITYP(NPART+1) = 25
        ELSE
          ITYP(NPART)   = 25
          ITYP(NPART+1) = 13
        ENDIF
        CALL PTRAM( ZN,AVPN,PX(NPART),PY(NPART) )
        CALL PTRAM( ZN,AVPN,PX(NPART+1),PY(NPART+1) )
        SPX   = SPX + PX(NPART) + PX(NPART+1)
        SPY   = SPY + PY(NPART) + PY(NPART+1)
        NPART = NPART + 2
 1008 CONTINUE
C  K0L K0S PAIRS
      DO 1009  K = 1,NKN
        CALL RMMAR( RD,1,1 )
        IF ( RD(1) .LT. 0.5 ) THEN
          ITYP(NPART)   = 10
          ITYP(NPART+1) = 16
        ELSE
          ITYP(NPART)   = 16
          ITYP(NPART+1) = 10
        ENDIF
        CALL PTRAM( ZN,AVPK,PX(NPART),PY(NPART) )
        CALL PTRAM( ZN,AVPK,PX(NPART+1),PY(NPART+1) )
        SPX   = SPX + PX(NPART) + PX(NPART+1)
        SPY   = SPY + PY(NPART) + PY(NPART+1)
        NPART = NPART + 2
 1009 CONTINUE
C  LAMDA/SIGMA0 PAIRS
      DO 1010  K = 1,NHN
        CALL RMMAR( RD,2,1 )
        IF ( RD(1) .LT. 0.5 ) THEN
          IF ( RD(2) .LT. 0.5 ) THEN
            ITYP(NPART)   = 18
            ITYP(NPART+1) = 28
          ELSE
            ITYP(NPART)   = 28
            ITYP(NPART+1) = 18
          ENDIF
        ELSE
          IF ( RD(2) .LT. 0.5 ) THEN
            ITYP(NPART)   = 26
            ITYP(NPART+1) = 20
          ELSE
            ITYP(NPART)   = 20
            ITYP(NPART+1) = 26
          ENDIF
        ENDIF
C      -----  CHANGE BY JNC DEC.96)
        IF ( ECMDPM .LE. 500.D0 ) THEN
          CALL PTRAN( ZN,AVPH,PX(NPART),PY(NPART) )
          CALL PTRAN( ZN,AVPH,PX(NPART+1),PY(NPART+1) )
        ELSE
          CALL PTRAM( ZN,AVPH,PX(NPART),PY(NPART) )
          CALL PTRAM( ZN,AVPH,PX(NPART+1),PY(NPART+1) )
        ENDIF
        SPX   = SPX + PX(NPART) + PX(NPART+1)
        SPY   = SPY + PY(NPART) + PY(NPART+1)
        NPART = NPART + 2
 1010 CONTINUE
C  ETA
      DO 1013  K = 1,NET
C  FIRST FOR ETAS FROM THIRD STRING
        IF     ( K .LE. NETA(3,1)                              ) THEN
          ITYP(NPART) = 71
        ELSEIF ( K .LE. NETA(3,1)+NETA(3,2)                    ) THEN
          ITYP(NPART) = 72
        ELSEIF ( K .LE. NETA(3,1)+NETA(3,2)+NETA(3,3)          ) THEN
          ITYP(NPART) = 73
        ELSEIF ( K .LE. NETA(3,1)+NETA(3,2)+NETA(3,3)+NETA(3,4)) THEN
          ITYP(NPART) = 74
C  NOW FOR ETAS FROM FIRST AND SECOND STRING
        ELSEIF ( K .LE. NETAS(3)+NETA(2,1)                     ) THEN
          ITYP(NPART) = 71
        ELSEIF ( K .LE. NETAS(3)+NETA(2,1)+NETA(2,2)           ) THEN
          ITYP(NPART) = 72
        ELSEIF ( K .LE. NETAS(3)+NETA(2,1)+NETA(2,2)+NETA(2,3) ) THEN
          ITYP(NPART) = 73
        ELSE
          ITYP(NPART) = 74
        ENDIF
C      -----  CHANGE BY JNC DEC.96)
        IF ( ECMDPM .LE. 500.D0 ) THEN
          CALL PTRAN( ZN,AVPE,PX(NPART),PY(NPART) )
        ELSE
          CALL PTRAM( ZN,AVPE,PX(NPART),PY(NPART) )
        ENDIF
        SPX   = SPX + PX(NPART)
        SPY   = SPY + PY(NPART)
        NPART = NPART + 1
 1013 CONTINUE
C  PI(0)
      DO 1014  K = 1,NPN
        ITYP(NPART) = 7
C      -----  CHANGE BY JNC DEC.96)
        IF ( ECMDPM .LE. 500.D0 ) THEN
          CALL PTRAN( ZN,AVPT,PX(NPART),PY(NPART) )
        ELSE
          CALL PTRAM( ZN,AVPT,PX(NPART),PY(NPART) )
        ENDIF
        SPX   = SPX + PX(NPART)
        SPY   = SPY + PY(NPART)
        NPART = NPART + 1
 1014 CONTINUE
 
C  ANTILEADER (FROM TARGET, THEREFORE ALWAYS NUCLEON OR DELTA RESONANCE)
      ITYP(2) = LEPAR2
C      -----  CHANGE BY JNC DEC.96)
      IF ( ECMDPM .LE. 500.D0 ) THEN
        CALL PTRAN( ZN,AVPN,PX(2),PY(2) )
      ELSE
        CALL PTRAM( ZN,AVPN,PX(2),PY(2) )
      ENDIF
 
C  FIRST PARTICLE IS LEADING PARTICLE
      ITYP(1) = LEPAR1
      IF     (  (LEPAR1 .GE.  7  .AND.  LEPAR1 .LE.  9)  .OR.
     *          (LEPAR1 .GE. 51  .AND.  LEPAR1 .LE. 53) ) THEN
C  LEADING PARTICLE IS PION OR RHO RESONANCE
        AVERPT = AVPT
C  LEADING PARTICLE IS KAON OR KAON RESONANCE
      ELSEIF ( LEPAR1 .EQ. 10  .OR.  LEPAR1 .EQ. 11  .OR.
     *         LEPAR1 .EQ. 12  .OR.  LEPAR1 .EQ. 16  .OR.
     *        (LEPAR1 .GE. 62  .AND. LEPAR1 .LE. 68) ) THEN
        AVERPT = AVPK
      ELSE
C  LEADING PARTICLE IS NUCLEON OR ANTINUCLEON OR DELTA RESONANCE
C  OR STRANGE BARYON
        AVERPT = AVPN
      ENDIF
C      -----  CHANGE BY JNC DEC.96)
      IF ( ECMDPM .LE. 500.D0 ) THEN
        CALL PTRAN( ZN,AVERPT,PX(1),PY(1) )
      ELSE
        CALL PTRAM( ZN,AVERPT,PX(1),PY(1) )
      ENDIF
      SPX = SPX + PX(1) + PX(2)
      SPY = SPY + PY(1) + PY(2)
 
C  AVERAGE EXCESS PT PER PARTICLE
      SPX = SPX / NTOT
      SPY = SPY / NTOT
 
C  RENORMALIZATION OF PT AND CALCULATION OF TRANSVERSE MASSES
      DO 130  I = 1,NTOT
        PX(I)   = PX(I) - SPX
        PY(I)   = PY(I) - SPY
        PT2(I)  = PX(I)**2 + PY(I)**2
        TMAS(I) = SQRT( PAMA(ITYP(I))**2 + PT2(I) )
 130  CONTINUE
 
      RETURN
      END
