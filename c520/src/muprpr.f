      SUBROUTINE MUPRPR
 
C-----------------------------------------------------------------------
C  MU(ON) P(AI)R PR(ODUCTION)
C
C  TREATES MUON PAIR PRODUCTION (WITHOUT POLARISATION EFFECTS)
C  IN ANALOGY WITH SUBROUTINE GPAIRM FROM GEANT WRITTEN BY L. URBAN
C  EXPLANATIONS SEE CERN PROGRM LIBRARY LONG WRITEUP W5013
C  THIS SUBROUTINE IS CALLED FROM MUTRAC
C
C  DESIGN  : D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,ELABCT.
      COMMON /ELABCT/  ELCUT
      DOUBLE PRECISION ELCUT(4)
*KEEP,GENER.
      COMMON /GENER/   GEN,ALEVEL
      DOUBLE PRECISION GEN,ALEVEL
*KEEP,MUPART.
      COMMON /MUPART/  AMUPAR,BCUT,CMUON,FMUBRM,FMUORG
      DOUBLE PRECISION AMUPAR(14),BCUT,CMUON(11)
      LOGICAL          FMUBRM,FMUORG
*KEEP,PAM.
      COMMON /PAM/     PAMA,SIGNUM
      DOUBLE PRECISION PAMA(6000),SIGNUM(6000)
*KEEP,PARPAR.
      COMMON /PARPAR/  CURPAR,SECPAR,PRMPAR,OUTPAR,C,
     *                 E00,E00PN,PTOT0,PTOT0N,THICKH,ITYPE,LEVL
      DOUBLE PRECISION CURPAR(14),SECPAR(14),PRMPAR(14),OUTPAR(14),
     *                 C(50),E00,E00PN,PTOT0,PTOT0N,THICKH
      INTEGER          ITYPE,LEVL
*KEEP,PARPAE.
      DOUBLE PRECISION GAMMA,COSTHE,PHI,H,T,X,Y,CHI,BETA,GCM,ECM
      EQUIVALENCE      (CURPAR(2),GAMMA),  (CURPAR(3),COSTHE),
     *                 (CURPAR(4), PHI ),  (CURPAR(5), H    ),
     *                 (CURPAR(6), T   ),  (CURPAR(7), X    ),
     *                 (CURPAR(8), Y   ),  (CURPAR(9), CHI  ),
     *                 (CURPAR(10),BETA),  (CURPAR(11),GCM  ),
     *                 (CURPAR(12),ECM )
*KEEP,POLAR.
      COMMON /POLAR/   POLART,POLARF
      DOUBLE PRECISION POLART,POLARF
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
*KEND.
 
      DOUBLE PRECISION AA,ALE,ALFA,AL10T,A1,A1R,B,BETA1,CC,C1,C2,
     *                 COSTH3,COSTH4,EKIN,EMUON,ENEG,EPOS,EPP,
     *                 PHI3,PPOS,R0,R0MAX,SCREJ,SINTH3,
     *                 TPOS,V,VC,VMAX,VMIN,V0,Z
      INTEGER          I
      DATA             AL10T/9.212D0/
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,9)
  444 FORMAT(' MUPRPR: CURPAR=',1P,9E10.3)
 
C  COPY COORDINATES TO SECPAR
      DO 11 I = 5,8
        SECPAR(I) = CURPAR(I)
 11   CONTINUE
      SECPAR( 9) = GEN
C  SET MATERIAL CONSTANTS CMUON(.) ACCORDING TO
C  TARGET INDEX LT (1=N, 2=O, 3=AR)  WHICH HAS BEEN SET IN BOX2
      IF     ( LT .EQ. 1 ) THEN
        Z = 7.D0
      ELSEIF ( LT .EQ. 2 ) THEN
        Z = 8.D0
      ELSE
        Z = 18.D0
      ENDIF
C  TOTAL AND KINETIC ENERGY OF MUON
      EMUON = PAMA(5) * GAMMA
      EKIN  = EMUON - PAMA(5)
      IF ( EKIN .LE. BCUT ) GOTO 900
C
      VMIN  = 4.D0 * PAMA(2) / EMUON
      VMAX  = 1.D0 - CMUON(10) * Z**OB3 / EMUON
      IF ( VMAX .LE. VMIN ) GOTO 900
      VC    = BCUT / EMUON
      ALE   = LOG(EMUON)
      ALFA  = 1.D0 + ALE/AL10T
      V0    = 0.18D0 * (4.D0 + ALE/AL10T) * ALFA * (ALFA*VMIN)**TB3
      BETA1 = 0.1D0 * (1.D0 + 3.D0 * ALE/AL10T)
      B     = 0.9D0 / (1.D0 + 0.4D0*ALE + 0.022D0*ALE**2)
      AA    = 1.D0 + 2.D0 * B * LOG(VC/V0)
      IF ( AA .LE. 1.D0 ) AA = 1.05D0
      A1    = 1.D0 - AA
      CC    = EXP(-0.25D0*A1*A1/B)
      A1R   = 1.D0 / A1
      C1    = VMAX**A1
      C2    = VC**A1
C  SAMPLE ENERGY FRACTION V AND RO
 50   CALL RMMAR(RD,2,1)
      V     = ( RD(1)*C1 + (1.-RD(1))*C2 )**A1R
      IF ( V .LE. VMIN ) GOTO 50
      IF ( V .LT. V0 ) THEN
        SCREJ = CC * ( (V-VMIN)/(V0-VMIN) )**BETA * (V0/V)**A1
      ELSE
        SCREJ = CC * (V0/V)**( A1 + B*LOG(V/V0) )
      ENDIF
      IF ( RD(2) .GT. SCREJ ) GOTO 50
      R0MAX = SCREJ * ( 1.D0 - 6.D0 *PAMA(5)/( EMUON**2 * (1.D0-V) ) )
      CALL RMMAR(RD,2,1)
      R0    = R0MAX * (2.*RD(1)-1.)
C  ENERGIES
      EPP  = V * EMUON
      EPOS = 0.5D0 * EPP * (1.D0 + R0)
      ENEG = EPP - EPOS
C  ANGLES
      COSTH3 = COS( PAMA(5)/EMUON )
      PHI3   = PI2 * RD(2)
C  POSITRON
      IF ( EPOS .GT. BCUT+PAMA(3) ) THEN
        CALL ADDANG( COSTHE,PHI, COSTH3,PHI3, SECPAR(3),SECPAR(4) )
        IF ( SECPAR(3) .GT. C(29) ) THEN
          SECPAR( 1) = 2.D0
          SECPAR( 2) = EPOS/PAMA(2)
          SECPAR(10) = H
          CALL TSTACK
        ENDIF
      ENDIF
C  ELECTRON
      IF ( ENEG .GT. BCUT+PAMA(3) ) THEN
        CALL ADDANG( COSTHE,PHI, COSTH3,-PHI3, SECPAR(3),SECPAR(4) )
        IF ( SECPAR(3) .GT. C(29) ) THEN
          SECPAR( 1) = 3.D0
          SECPAR( 2) = ENEG/PAMA(2)
          SECPAR(10) = H
          CALL TSTACK
        ENDIF
      ENDIF
C  REDUCE ENERGY OF MUON
 60   CONTINUE
      GAMMA = (EMUON - EPP)/ PAMA(5)
 
 900  CONTINUE
C  WRITE MUON TO STACK
      SECPAR( 1) = CURPAR(1)
      SECPAR( 2) = GAMMA
      SECPAR( 3) = CURPAR(3)
      SECPAR( 4) = CURPAR(4)
      SECPAR(10) = ALEVEL
      CALL TSTACK
 
      RETURN
      END
