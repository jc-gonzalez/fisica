      SUBROUTINE SDPM
 
C-----------------------------------------------------------------------
C  S(TARTING) D(UAL) P(ARTON) M(ODEL)
C
C  THIS ROUTINE DETERMINES THE TARGET NUCLEUS.
C  IT CALLS ALSO THE VARIOUS INTERACTION MODELS.
C  FOR HDPM, THIS ROUTINE LOOKS, HOW MANY NUCLEONS INTERACT AND WHICH
C  RESIDUAL FRAGMENT OF THE PROJECTILE NUCLEUS REMAINS.
C  THIS SUBROUTINE IS CALLED FROM NUCINT AND PIGEN
C
C  REDESIGN: D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,AIR.
      COMMON /AIR/     COMPOS,PROBTA,AVERAW,AVOGAD
      DOUBLE PRECISION COMPOS(3),PROBTA(3),AVERAW,AVOGAD
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,DPMFLG.
      COMMON /DPMFLG/  NFLAIN,NFLDIF,NFLPI0,NFLCHE,NFLPIF,NFRAGM
      INTEGER          NFLAIN,NFLDIF,NFLPI0,NFLCHE,NFLPIF,NFRAGM
*KEEP,GENER.
      COMMON /GENER/   GEN,ALEVEL
      DOUBLE PRECISION GEN,ALEVEL
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
*KEEP,ISTA.
      COMMON /ISTA/    IFINET,IFINNU,IFINKA,IFINPI,IFINHY
      INTEGER          IFINET,IFINNU,IFINKA,IFINPI,IFINHY
*KEEP,MULT.
      COMMON /MULT/    EKINL,MSMM,MULTMA,MULTOT
      DOUBLE PRECISION EKINL
      INTEGER          MSMM,MULTMA(37,13),MULTOT(37,13)
*KEEP,NCSNCS.
      COMMON /NCSNCS/  SIGN30,SIGN45,SIGN60,SIGO30,SIGO45,SIGO60,
     *                 SIGA30,SIGA45,SIGA60,PNOA30,PNOA45,PNOA60,
     *                 SIG30A,SIG45A,SIG60A
      DOUBLE PRECISION SIGN30(56),SIGN45(56),SIGN60(56),
     *                 SIGO30(56),SIGO45(56),SIGO60(56),
     *                 SIGA30(56),SIGA45(56),SIGA60(56),
     *                 PNOA30(1540,3),PNOA45(1540,3),PNOA60(1540,3),
     *                 SIG30A(56),SIG45A(56),SIG60A(56)
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
*KEEP,RANDPA.
      COMMON /RANDPA/  FAC,U1,U2,RD,NSEQ,ISEED,KNOR
      DOUBLE PRECISION FAC,U1,U2
      REAL             RD(3000)
      INTEGER          ISEED(103,10),NSEQ
      LOGICAL          KNOR
*KEEP,RANGE.
      COMMON /RANGE/   CC
      DOUBLE PRECISION CC(20)
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
*KEEP,VKIN.
      COMMON /VKIN/    BETACM
      DOUBLE PRECISION BETACM
*KEEP,VENUS.
      COMMON /VENUS/   ISH0,IVERVN,MTAR99,FVENUS,FVENSG
      INTEGER          ISH0,IVERVN,MTAR99
      LOGICAL          FVENUS,FVENSG
*KEND.
 
      DOUBLE PRECISION PFRX(60),PFRY(60)
      DOUBLE PRECISION COSTET,EA,P,PHIV,PTM,PT2,
     *                 SIGMAA,SIGMAN,SIGMAO,SIG45,S45SQ,S4530
      DOUBLE PRECISION CGHSIG,EKIN
      EXTERNAL         CGHSIG
      INTEGER          ITYP(60),I,IA,IANEW,INACTA,INACTZ,INDEX,INEUTR,
     *                 IZ,IZNEW,J,JFIN,KNEW,L,LL,NPRPRO,NNEPRO
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,9)
  444 FORMAT(' SDPM  : CURPAR=',1P,9E10.3)
 
C  IA IS MASS NUMBER OF PROJECTILE
      IA = ITYPE / 100
      IF ( IA .GT. 56 ) THEN
        WRITE(MONIOU,*) 'SDPM  : NOT FORESEEN PARTICLE TYPE=',ITYPE
        STOP
      ENDIF
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TREATMENT OF GAMMAS COMING FROM EGS4 (PIGEN)
      IF     ( ITYPE .EQ. 1 ) THEN
C  RATIOS OF CROSS SECTIONS GO LIKE A**0.91
        FRACTN = COMPOS(1) * 11.04019D0
        FRCTNO = FRACTN + COMPOS(2) * 12.46663D0
        SIGAIR = FRCTNO + COMPOS(3) * 28.69952D0
C  TARGET IS CHOSEN AT RANDOM
        CALL RMMAR( RD,1,1 )
        IF     ( RD(1)*SIGAIR .LE. FRACTN ) THEN
C  INTERACTION WITH NITROGEN
          LT  = 1
          TAR = 14.D0
        ELSEIF ( RD(1)*SIGAIR .LE. FRCTNO ) THEN
C  INTERACTION WITH OXYGEN
          LT  = 2
          TAR = 16.D0
        ELSE
C  INTERACTION WITH ARGON
          LT  = 3
          TAR = 40.D0
        ENDIF
 
C  GAMMAS ARE TREATED BY VENUS, IF SUFFICIENT ENERGY
        IF ( FVENUS  .AND.  CURPAR(2) .GT. HILOELB ) THEN
          CALL VENLNK
        ELSE
          CALL HDPM
        ENDIF
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  NORMAL HADRON PROJECTILE
      ELSEIF ( ITYPE .LT. 100 ) THEN
 
C  WITH WHAT KIND OF TARGET DOES PROJECTILE INTERACT?
        IF ( FIXTAR ) THEN
C  TARGET OF FIRST INTERACTION IS FIXED
          LT = N1STTR
          IF     ( N1STTR .EQ. 1 ) THEN
            TAR = 14.D0
          ELSEIF ( N1STTR .EQ. 2 ) THEN
            TAR = 16.D0
          ELSE
            TAR = 40.D0
          ENDIF
          FIXTAR = .FALSE.
        ELSE
C  TARGET IS CHOSEN AT RANDOM ACCORDING TO CROSS SECTION
C  SIGMA IS ENERGY DEPENDENT INELASTIC NUCLEON-NUCLEON CROSS SECTION
C  AND IS SET IN BOX2
C  AUXIL. QUANTITIES FOR INTERPOLATION
          SIG45  = SIGMA - 45.D0
          S45SQ  = SIG45**2 / 450.D0
          S4530  = SIG45 / 30.D0
C  INELASTIC CROSS SECTIONS FOR PROJECTICLE WITH MASS NUMBER 1
          SIGMAN = (1.D0 - 2.D0 * S45SQ) * SIGN45(1)
     *                  +(S45SQ - S4530) * SIGN30(1)
     *                  +(S45SQ + S4530) * SIGN60(1)
          FRACTN = COMPOS(1) * SIGMAN
          SIGMAO = (1.D0 - 2.D0 * S45SQ) * SIGO45(1)
     *                  +(S45SQ - S4530) * SIGO30(1)
     *                  +(S45SQ + S4530) * SIGO60(1)
          FRCTNO = FRACTN + COMPOS(2) * SIGMAO
          SIGMAA = (1.D0 - 2.D0 * S45SQ) * SIGA45(1)
     *                  +(S45SQ - S4530) * SIGA30(1)
     *                  +(S45SQ + S4530) * SIGA60(1)
C  INELASTIC CROSS SECTIONS OF AIR FOR PROJECTILE WITH MASS NUMBER 1
          SIGAIR = FRCTNO + COMPOS(3)*SIGMAA
 333      CONTINUE
          CALL RMMAR( RD,1,1 )
          IF(DEBUG)WRITE(MDEBUG,*)'SDPM  : FRACTN=',SNGL(FRACTN),
     *      'FRCTNO=',SNGL(FRCTNO),'RD=',RD(1)
          IF     ( RD(1)*SIGAIR .LE. FRACTN ) THEN
C  INTERACTION WITH NITROGEN
            LT  = 1
            TAR = 14.D0
          ELSEIF ( RD(1)*SIGAIR .LE. FRCTNO ) THEN
C  INTERACTION WITH OXYGEN
            LT  = 2
            TAR = 16.D0
          ELSE
C  INTERACTION WITH ARGON
            LT  = 3
            TAR = 40.D0
          ENDIF
        ENDIF
 
        IF ( FVENUS ) THEN
C  MESONS, NUCLEONS AND STRANGE BARYONS ARE TREATED BY VENUS (JAN 95)
          IF ( (ITYPE .GE.  7  .AND.  ITYPE .LE. 16)  .OR.
     *         (ITYPE .GE. 18  .AND.  ITYPE .LE. 32)  )THEN
            CALL VENLNK
          ELSE
            CALL HDPM
          ENDIF
        ELSE
          CALL HDPM
        ENDIF
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  HEAVY PRIMARY INCIDENT WITH IA NUCLEONS
      ELSEIF ( IA .LE. 56 ) THEN
 
        IZ = MOD(ITYPE,100)
C  WITH WHAT KIND OF TARGET DOES PROJECTILE INTERACT?
        IF ( FIXTAR ) THEN
C  TARGET OF FIRST INTERACTION IS FIXED
          LT = N1STTR
          IF     ( N1STTR .EQ. 1 ) THEN
            TAR = 14.D0
          ELSEIF ( N1STTR .EQ. 2 ) THEN
            TAR = 16.D0
          ELSE
            TAR = 40.D0
          ENDIF
          FIXTAR = .FALSE.
          CALL RMMAR( RD,2,1 )
        ELSE
C  ONLY INELASTIC INTERACTIONS WITH HEAVY PRIMARY/FRAGMENT
C  SIGMA IS ENERGY DEPENDENT INELASTIC NUCLEON-NUCLEON CROSS SECTION
C  AND IS SET IN BOX2
C  AUXIL. QUANTITIES FOR INTERPOLATION
          SIG45  = SIGMA - 45.D0
          S45SQ  = SIG45**2 / 450.D0
          S4530  = SIG45 / 30.D0
C  INELASTIC CROSS SECTIONS FOR PROJECTICLE WITH MASS NUMBER IA
          SIGMAN = (1.D0 - 2.D0 * S45SQ) * SIGN45(IA)
     *                  +(S45SQ - S4530) * SIGN30(IA)
     *                  +(S45SQ + S4530) * SIGN60(IA)
          FRACTN = COMPOS(1) * SIGMAN
          SIGMAO = (1.D0 - 2.D0 * S45SQ) * SIGO45(IA)
     *                  +(S45SQ - S4530) * SIGO30(IA)
     *                  +(S45SQ + S4530) * SIGO60(IA)
          FRCTNO = FRACTN + COMPOS(2) * SIGMAO
          SIGMAA = (1.D0 - 2.D0 * S45SQ) * SIGA45(IA)
     *                  +(S45SQ - S4530) * SIGA30(IA)
     *                  +(S45SQ + S4530) * SIGA60(IA)
C  INELASTIC CROSS SECTIONS OF AIR FOR PROJECTILE WITH MASS NUMBER IA
          SIGAIR = FRCTNO +COMPOS(3)*SIGMAA
C  TARGET IS CHOSEN AT RANDOM
          CALL RMMAR( RD,2,1 )
          IF(DEBUG)WRITE(MDEBUG,*)'SDPM  : FRACTN=',SNGL(FRACTN),
     *      'FRCTNO=',SNGL(FRCTNO),'RD=',RD(1)
          IF     ( RD(1)*SIGAIR .LE. FRACTN ) THEN
C  INTERACTION WITH NITROGEN
            LT  = 1
            TAR = 14.D0
          ELSEIF ( RD(1)*SIGAIR .LE. FRCTNO ) THEN
C  INTERACTION WITH OXYGEN
            LT  = 2
            TAR = 16.D0
          ELSE
C  INTERACTION WITH ARGON
            LT  = 3
            TAR = 40.D0
          ENDIF
        ENDIF
C  TREAT NUCLEUS BY VENUS, IF SELECTED AND ENERGY/NUCLEON HIGH ENOUGH
        IF ( FVENUS .AND. PAMA(ITYPE)*GAMMA .GT. HILOELB*IA ) THEN
          CALL VENLNK
          RETURN
        ENDIF
 
C  TREATMENT OF NUCLEUS-NUCLEUS INTERACTION IN HDPM BY SUPERPOSITION
C
C  INDEX CALCULATION 1<I=<56     NUCLEONS IN PROJECTILE
C                    1<J<I       INTERACTING NUCLEONS
C                    P(I,I)=1    CUMULATIVE PROBABILITIES
C                    P(I,J)  ---> P( I*(I-3)*0.5+J+1 )
C  IZ     IS NUMBER OF PROTONS IN PROJECTILE
C  LT     IS INDEX FOR TARGET 1 = N, 2 = O, 3 = AR
C  INACTA IS NUMBER OF INTERACTING NUCLEONS
C  INACTZ IS NUMBER OF INTERACTING PROTONS
 
C  LOOK, HOW MANY NUCLEONS INTERACT
        DO 100  J = 1,IA-1
          INACTA = J
          INDEX  = IA * (IA-3) * 0.5 + 1 + J
          P = ( 1.D0 - S45SQ *2.D0 ) * PNOA45(INDEX,LT)
     *            +( S45SQ - S4530 ) * PNOA30(INDEX,LT)
     *            +( S45SQ + S4530 ) * PNOA60(INDEX,LT)
          IF ( RD(2) .LT. P ) GO TO 110
  100   CONTINUE
C  ALL NUCLEONS INTERACT  (INACTA EQUAL IA)
        INACTA = INACTA + 1
 
  110   CONTINUE
        IANEW  = IA - INACTA
 
C  REMAINING PROJECTILE WITH IANEW NUCLEONS
        DO 120  L = 2,8
          SECPAR(L) = CURPAR(L)
  120   CONTINUE
 
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PROJECTILE NUCLEUS FRAGMENTS COMPLETELY, WRITE SPECTATOR NUCLEONS
C  ONTO STACK
        IF ( NFRAGM .EQ. 0 ) THEN
C  LOOK, HOW MANY PROTONS AND NEUTRONS ARE FORMED
          IZNEW  = IANEW / 2.15D0 + 0.7D0
          INEUTR = IANEW - IZNEW
          INACTZ = MAX( IZ-IZNEW, 0 )
 
          IF ( IZNEW .GT. 0 ) THEN
C  PROTONS
            SECPAR(1) = 14.D0
            DO 300  L = 1,IZNEW
              CALL TSTACK
  300       CONTINUE
          ENDIF
          IF ( INEUTR .GT. 0 ) THEN
C  NEUTRONS
            SECPAR(1) = 13.D0
            DO 310  L = 1,INEUTR
              CALL TSTACK
  310       CONTINUE
          ENDIF
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  NO FRAGMENTATION, BUT SUCCESSIVE ABRASION OF PROJECTILE NUCLEUS
        ELSE
          IF ( DEBUG ) WRITE( MDEBUG,111 ) TAR,INACTA,IANEW
  111     FORMAT(' SDPM  : TARGET=',F4.0,' INACTA=',I4,' IANEW=',I4)
 
C  ALL NUCLEONS INTERACT, NO RESIDUAL NUCLEUS
          IF     ( IANEW .EQ. 0 ) THEN
            INACTZ    = IZ
            IF ( DEBUG ) WRITE(MDEBUG,554) (CURPAR(I),I=1,9)
  554       FORMAT (' SDPM  : CURPAR=',1P,9E10.3)
            KNEW = 0
 
C  REMAINING NUCLEUS IS A NUCLEON
          ELSEIF ( IANEW .EQ. 1 ) THEN
            CALL RMMAR( RD,1,1 )
            IZNEW     = NINT(RD(1))
            INACTZ    = IZ - IZNEW
            KNEW      = 13 + IZNEW
 
C  REMAINING NUCLEUS GETS A CHARGE WHICH IS ABOUT HALF THE MASS NUMBER
          ELSEIF ( IANEW .GT. 1 ) THEN
            IZNEW = FLOAT(IANEW) / 2.15D0 + 0.7D0
            INACTZ = MAX( IZ - IZNEW, 0 )
            KNEW  = IANEW*100 + IZNEW
 
C  REMAINING NUCLEUS DEEXCITES BY EVAPORATION OF NUCLEONS/ALPHA PARTCLS.
            IF ( NFRAGM .GE. 2 ) THEN
              JFIN=0
              CALL VAPOR(IA,KNEW,JFIN,ITYP,PFRX,PFRY)
              IF ( JFIN .LE. 0 ) GOTO 190
              KNEW = 0
              DO  135 J=1,JFIN
                EA = GAMMA * PAMA(ITYP(J))
                IF (DEBUG) WRITE (MDEBUG,*)'SDPM  : J,ITYP,EA=',
     *                                              J,ITYP,SNGL(EA)
                PTM = EA**2 - PAMA(ITYP(J))**2
                PT2 = PFRX(J)**2 + PFRY(J)**2
                IF ( PT2 .GE. PTM ) THEN
                  IF (DEBUG) WRITE(MDEBUG,*)'SDPM  : PT REJECT ',J
                  GOTO 135
                ENDIF
                IF ( PTM .GT. 0.D0 ) THEN
                  COSTET = SQRT( 1.D0 - PT2/PTM )
                ELSE
                  COSTET = 1.D0
                ENDIF
                IF ( PFRX(J) .NE. 0.D0  .OR.  PFRY(J) .NE. 0.D0 ) THEN
                  PHIV = ATAN2( PFRY(J), PFRX(J) )
                ELSE
                  PHIV = 0.D0
                ENDIF
                CALL ADDANG( COSTHE,PHI, COSTET,PHIV,
     *                                           SECPAR(3),SECPAR(4) )
                IF ( SECPAR(3) .GE. C(29) ) THEN
                  IF ( J .LT. JFIN ) THEN
                    SECPAR(1) = ITYP(J)
                    CALL TSTACK
                  ELSE
                    KNEW  = ITYP(JFIN)
                    IANEW = KNEW/100
                  ENDIF
                ELSE
                  IF (DEBUG) WRITE(MDEBUG,*)'SDPM  : ANGLE REJECT ',J
                ENDIF
 135          CONTINUE
            ENDIF
          ENDIF
 
C  REMAINING NUCLEUS: MASS 5 CANNOT BE TREATED IN BOX2
          IF     ( KNEW/100 .EQ. 5 ) THEN
            IF ( MOD(KNEW,100) .GE. 3 ) THEN
C  MASS 5: SPLIT OFF ONE PROTON
              SECPAR(1) = 14.D0
              CALL TSTACK
              KNEW = KNEW - 101
            ELSE
C  MASS 5: SPLIT OFF ONE NEUTRON
              SECPAR(1) = 13.D0
              CALL TSTACK
              KNEW = KNEW - 100
            ENDIF
 
C  REMAINING NUCLEUS: MASS 8 CANNOT BE TREATED IN BOX2
          ELSEIF ( KNEW/100 .EQ. 8 ) THEN
            IF     ( MOD(KNEW,100) .GE. 5 ) THEN
C  MASS 8: SPLIT OFF ONE PROTON
              SECPAR(1) = 14.D0
              CALL TSTACK
              KNEW = KNEW - 101
            ELSEIF ( MOD(KNEW,100) .LE. 3 ) THEN
C  MASS 8: SPLIT OFF ONE NEUTRON
              SECPAR(1) = 13.D0
              CALL TSTACK
              KNEW = KNEW - 100
            ELSE
C  MASS 8: SPLIT OFF ONE ALPHA PARTICLE
              SECPAR(1) = 402.D0
              CALL TSTACK
              KNEW = KNEW - 402
            ENDIF
          ENDIF
 
          IF ( KNEW .GT. 0 ) THEN
            SECPAR(1) = KNEW
              CALL TSTACK
            IF ( DEBUG ) WRITE(MDEBUG,555) (SECPAR(I),I=1,9)
 555        FORMAT (' SDPM  : SECPAR=',1P,9E10.3)
          ENDIF
        ENDIF
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  HERE THE REACTING NUCLEONS ARE TREATED
 190    NPRPRO = INACTZ
        NNEPRO = INACTA - INACTZ
 
C  TREAT INTERACTING NEUTRONS FROM PROJECTILE
        IF ( NNEPRO .GE. 1 ) THEN
          CURPAR(1) = 13.D0
          ITYPE     = 13
C  CALCULATE GAMMA, BETA AND ENERGY IN CENTER OF MASS
          GCM       = SQRT( GAMMA * 0.5D0 + 0.5D0 )
          ECM       = PAMA(ITYPE) * GCM * 2.D0
          BETACM    = SQRT( 1.D0 - 1.D0 / GCM**2 )
          DO 200  LL = 1,NNEPRO
C  USE GHEISHA IF THE CROSS SECTION HAS BEEN CALCULATED FOR GHEISHA
            IF (  GHEISH  .AND.  ECM .LE. HILOECM ) THEN
              ELAB = PAMA(ITYPE) *  GAMMA
              PLAB = ELAB * BETA
              EKIN = ELAB - PAMA(ITYPE)
              SIGAIR = CGHSIG(SNGL(PLAB),SNGL(EKIN),ITYPE)
              CALL CGHEI
            ELSE
C  DETERMINE TYPE OF INTERACTION FOR NUCLEONS AND ANTINUCLEONS
              IF     ( ECM .GT. CC(4) ) THEN
C  DUAL PARTON MODEL
                CALL HDPM
              ELSEIF ( ECM .GT. CC(3) ) THEN
C  USE THE INTERACTION ROUTINES OF PKF GRIEDER
C  2 HEAVY ISOBARS AND ANNIHILATION
                CALL BOX63
              ELSEIF ( ECM .GT. CC(2) ) THEN
C  1 HEAVY ISOBAR + NUCLEON AND ANNIHILATION
                CALL BOX62
              ELSEIF ( ECM .GT. CC(1) ) THEN
C  1 LIGHT ISOBAR + NUCLEON AND ANNIHILATION
                CALL BOX61
              ELSE
C  ELASTIC SCATTERING AND ANNIHILATION
                CALL BOX60
              ENDIF
            ENDIF
 200      CONTINUE
        ENDIF
 
C  TREAT INTERACTING PROTONS FROM PROJECTILE IN ROUTINE HDPM
        IF ( NPRPRO .GE. 1 ) THEN
          CURPAR(1) = 14.D0
          ITYPE     = 14
C  CALCULATE GAMMA, BETA AND ENERGY IN CENTER OF MASS
          GCM       = SQRT( GAMMA * 0.5D0 + 0.5D0 )
          ECM       = PAMA(ITYPE) * GCM * 2.D0
          BETACM    = SQRT( 1.D0 - 1.D0 / GCM**2 )
          DO 210  LL = 1,NPRPRO
C  USE GHEISHA IF THE CROSS SECTION HAS BEEN CALCULATED FOR GHEISHA
            IF (  GHEISH  .AND.  ECM .LE. HILOECM ) THEN
              ELAB = PAMA(ITYPE) *  GAMMA
              PLAB = ELAB * BETA
              EKIN = ELAB - PAMA(ITYPE)
              SIGAIR = CGHSIG(SNGL(PLAB),SNGL(EKIN),ITYPE)
              CALL CGHEI
            ELSE
C  DETERMINE TYPE OF INTERACTION FOR NUCLEONS AND ANTINUCLEONS
              IF     ( ECM .GT. CC(4) ) THEN
C  DUAL PARTON MODEL
                CALL HDPM
              ELSEIF ( ECM .GT. CC(3) ) THEN
C  USE THE INTERACTION ROUTINES OF PKF GRIEDER
C  2 HEAVY ISOBARS AND ANNIHILATION
                CALL BOX63
              ELSEIF ( ECM .GT. CC(2) ) THEN
C  1 HEAVY ISOBAR + NUCLEON AND ANNIHILATION
                CALL BOX62
              ELSEIF ( ECM .GT. CC(1) ) THEN
C  1 LIGHT ISOBAR + NUCLEON AND ANNIHILATION
                CALL BOX61
              ELSE
C  ELASTIC SCATTERING AND ANNIHILATION
                CALL BOX60
              ENDIF
            ENDIF
 210      CONTINUE
        ENDIF
 
C  ALL PARTICLES, INCLUDING THE LEADING ONE, ARE NOW WRITTEN TO STACK
 
      ELSE
        WRITE(MONIOU,*) 'SDPM  : NOT FORESEEN PARTICLE TYPE=',ITYPE
        STOP
      ENDIF
 
      RETURN
      END
