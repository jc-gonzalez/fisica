      SUBROUTINE BOX2
 
C-----------------------------------------------------------------------
C
C  DETERMINES POINT OF INTERACTION OR DECAY FOR ANY PARTICLE
C  HEAVY PRIMARIES AND STRANGE BARYONS INCLUDED
C  ANNIHILATION CROSS SECTION INCLUDED
C  PRECISE MEAN FREE PATH FOR DECAYING PARTICLES
C  HAS INTERACTION LENGTH STATISTICS INCLUDED
C  THIS SUBROUTINE IS CALLED FROM MAIN
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,AIR.
      COMMON /AIR/     COMPOS,PROBTA,AVERAW,AVOGAD
      DOUBLE PRECISION COMPOS(3),PROBTA(3),AVERAW,AVOGAD
*KEEP,CHISTA.
      COMMON /CHISTA/  IHYCHI,IKACHI,IMUCHI,INNCHI,INUCHI,IPICHI
      INTEGER          IHYCHI(124),IKACHI(124),IMUCHI(124),
     *                 INNCHI(124),INUCHI(124),IPICHI(124)
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,KAONS.
      COMMON /KAONS/   CKA
      DOUBLE PRECISION CKA(80)
*KEEP,MUPART.
      COMMON /MUPART/  AMUPAR,BCUT,CMUON,FMUBRM,FMUORG
      DOUBLE PRECISION AMUPAR(14),BCUT,CMUON(11)
      LOGICAL          FMUBRM,FMUORG
*KEEP,NCSNCS.
      COMMON /NCSNCS/  SIGN30,SIGN45,SIGN60,SIGO30,SIGO45,SIGO60,
     *                 SIGA30,SIGA45,SIGA60,PNOA30,PNOA45,PNOA60,
     *                 SIG30A,SIG45A,SIG60A
      DOUBLE PRECISION SIGN30(56),SIGN45(56),SIGN60(56),
     *                 SIGO30(56),SIGO45(56),SIGO60(56),
     *                 SIGA30(56),SIGA45(56),SIGA60(56),
     *                 PNOA30(1540,3),PNOA45(1540,3),PNOA60(1540,3),
     *                 SIG30A(56),SIG45A(56),SIG60A(56)
*KEEP,OBSPAR.
      COMMON /OBSPAR/  OBSLEV,THCKOB,XOFF,YOFF,THETAP,PHIP,
     *                 THETPR,PHIPR,NOBSLV
      DOUBLE PRECISION OBSLEV(10),THCKOB(10),XOFF(10),YOFF(10),
     *                 THETAP,THETPR(2),PHIP,PHIPR(2)
      INTEGER          NOBSLV
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
*KEEP,STRBAR.
      COMMON /STRBAR/  CSTRBA
      DOUBLE PRECISION CSTRBA(11)
*KEEP,VENUS.
      COMMON /VENUS/   ISH0,IVERVN,MTAR99,FVENUS,FVENSG
      INTEGER          ISH0,IVERVN,MTAR99
      LOGICAL          FVENUS,FVENSG
*KEND.
 
      DOUBLE PRECISION CHIBRM,CHIPRM,CHIINT,CHI1,CHI2,CKA2,COR1,DH,
     *                 ELAB,ELABLG,ELABT,FRAPTN,FRPTNO,
     *                 HDEC,HEIGH,PLAB,PLABLG,SIGBRM,SIGPRM,
     *                 SIG45,S45SQ,S4530,THICK
      REAL             EKIN,GBRSGM,GPRSGM
      INTEGER          I,IA,IHY,IP,KA,MU,NI,NU
      EXTERNAL         HEIGH,THICK,GBRSGM,GPRSGM
      DOUBLE PRECISION SIGGHE,CGHSIG
      EXTERNAL         CGHSIG
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,8)
  444 FORMAT(' BOX2  : CURPAR=',1P,8E10.3)
 
      ITYPE = CURPAR(1)
 
      BETA = SQRT( GAMMA**2 - 1.D0 ) / GAMMA
 
C-----------------------------------------------------------------------
C  PHOTONS, ELECTRONS,PI(0), AND ETA ARE TREATED SEPARATELY (SEE BOX3)
      IF ( ITYPE .LE.  3  .OR.   ITYPE .EQ.  7  .OR.  ITYPE .EQ. 17 .OR.
     *    (ITYPE .GE. 71  .AND.  ITYPE .LE. 74)     ) THEN
        CHI = 0.D0
        RETURN
      ENDIF
 
C-----------------------------------------------------------------------
C  RESONANCES ARE TREATED SEPARATELY (SEE BOX3)
      IF ( ITYPE .GT. 50  .AND.  ITYPE .LE. 65 ) THEN
        CHI = 0.D0
        RETURN
      ENDIF
 
      THICKH = THICK(H)
      ELAB = PAMA(ITYPE) * GAMMA
 
C-----------------------------------------------------------------------
C  MU + , MU - DECAYS AFTER ITS LIFE TIME
C  MUON INTERACTS BY BREMSSTRAHLUNG OR PAIR PRODUCTION
      IF     ( ITYPE .EQ. 5  .OR.  ITYPE .EQ. 6 ) THEN
        CALL RMMAR( RD,3,1 )
        COR1 = -LOG(RD(1)) * C(25) * C(19)
        CALL PRANGE(COR1)
        DH   = H - HEIGH( THICKH + CHI*COSTHE )
        IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : ITYPE,RD(1),CHIDEC=',
     *                                 ITYPE,RD(1),SNGL(CHI)
        IF ( GAMMA .LE. 200.D0 ) THEN
          FDECAY = .TRUE.
 
        ELSE
C  AT HIGHER ENERGIES CHECK FOR MUON BREMSSTRAHLUNG AND PAIR PRODUCTION
          ELABLG = LOG(ELAB)
C  CALCULATE MUON BREMSSTRAHLUNG CROSS SECTION FOR AIR
          IF ( ELAB .LE. 1.D5 ) THEN
            FRACTN =          COMPOS(1)*GBRSGM( 7.,SNGL(ELAB))
            FRCTNO = FRACTN + COMPOS(2)*GBRSGM( 8.,SNGL(ELAB))
            SIGBRM = FRCTNO + COMPOS(3)*GBRSGM(18.,SNGL(ELAB))
          ELSE
C  PRELIMINARY PARAMETRIZED FOR ULTRAHIGH ENERGIES
            SIGBRM = EXP( ELABLG * 0.04437D0 - 1.4805D0 )
            FRACTN = SIGBRM * 0.78D0
            FRCTNO = SIGBRM * 0.99D0
          ENDIF
          IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : SIGBRM=',SNGL(SIGBRM)
C  CALCULATE MEAN FREE PATH FOR BREMSSTRAHLUNG
          CHIBRM = -LOG(RD(2)) * AVERAW / (AVOGAD * SIGBRM)
          IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : ITYPE,RD(2),CHIBRM=',
     *                                 ITYPE,RD(2),SNGL(CHIBRM)
          CHI1   = MIN( CHIBRM, CHI )
 
          IF ( ELAB .LE. 1.D6 ) THEN
C  CALCULATE  MUON PAIR PRODUCTION CROSS SECTION FOR AIR
            FRAPTN =          COMPOS(1)*GPRSGM( 7.,SNGL(ELAB))
            FRPTNO = FRAPTN + COMPOS(2)*GPRSGM( 8.,SNGL(ELAB))
            SIGPRM = FRPTNO + COMPOS(3)*GPRSGM(18.,SNGL(ELAB))
          ELSE
C  PRELIMINARY PARAMETRIZED FOR ULTRAHIGH ENERGIES
            SIGPRM = EXP( ELABLG * 0.2067D0 + 0.9169D0 )
            FRACTN = SIGPRM * 0.78D0
            FRCTNO = SIGPRM * 0.99D0
          ENDIF
          IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : SIGPRM=',SNGL(SIGPRM)
C  CALCULATE MEAN FREE PATH FOR PAIR PRODUCTION
          CHIPRM = -LOG(RD(3)) * AVERAW / (AVOGAD * SIGPRM)
          IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : ITYPE,RD(3),CHIPRM=',
     *                                 ITYPE,RD(3),SNGL(CHIPRM)
          CHI2   = MIN( CHIPRM, CHI1 )
          IF     ( CHI2 .EQ. CHI    ) THEN
            FDECAY = .TRUE.
          ELSEIF ( CHI2 .EQ. CHIBRM ) THEN
            FDECAY = .FALSE.
            FMUBRM = .TRUE.
C  TARGET IS CHOSEN AT RANDOM FOR MUON BREMSSTRAHLUNG
            CALL RMMAR( RD,1,1 )
            IF     ( RD(1)*SIGBRM .LE. FRACTN ) THEN
C  BREMSSTRAHLUNG WITH NITROGEN
              LT  = 1
              TAR = 14.D0
            ELSEIF ( RD(1)*SIGBRM .LE. FRCTNO ) THEN
C  BREMSSTRAHLUNG WITH OXYGEN
              LT  = 2
              TAR = 16.D0
            ELSE
C  BREMSSTRAHLUNG WITH ARGON
              LT  = 3
              TAR = 40.D0
            ENDIF
          ELSEIF ( CHI2 .EQ. CHIPRM ) THEN
            FDECAY = .FALSE.
            FMUBRM = .FALSE.
C  TARGET IS CHOSEN AT RANDOM FOR MUON PAIR PRODUCTION
            CALL RMMAR( RD,1,1 )
            IF     ( RD(1)*SIGPRM .LE. FRAPTN ) THEN
C  PAIR PRODUCTION WITH NITROGEN
              LT  = 1
              TAR = 14.D0
            ELSEIF ( RD(1)*SIGPRM .LE. FRPTNO ) THEN
C  PAIR PRODUCTION WITH OXYGEN
              LT  = 2
              TAR = 16.D0
            ELSE
C  PAIR PRODUCTION WITH ARGON
              LT  = 3
              TAR = 40.D0
            ENDIF
          ENDIF
          CHI = CHI2
        ENDIF
 
C  DECAY LENGTH STATISTICS
        MU = 1.D0 + DH * 1.D-5 / COSTHE
        MU = MIN( MU, 123 )
        IMUCHI( MU) = IMUCHI( MU) + 1
        IMUCHI(124) = IMUCHI(124) + 1
 
C-----------------------------------------------------------------------
C  CHARGED PIONS
      ELSEIF ( ITYPE .EQ. 8  .OR.  ITYPE .EQ. 9 ) THEN
        PLAB = ELAB * BETA
C  CALCULATION OF CROSS SECTION IN THE GHEISHA ROUTINES
        IF ( GHEISH  .AND.  (ELAB .LE. HILOELB) ) THEN
          EKIN   = ELAB - PAMA(ITYPE)
          SIGAIR = CGHSIG(SNGL(PLAB),EKIN,ITYPE)
          GHESIG = .TRUE.
        ELSE
          GHESIG = .FALSE.
C  SIGMA IS ENERGY DEPENDENT INELASTIC PION-NUCLEON CROSS SECTION
            IF     ( PLAB .LE. 5.D0 ) THEN
              SIGMA  = 20.64D0
            ELSEIF ( PLAB .LT. 1.D3 ) THEN
              PLABLG = LOG(PLAB)
C  INELASTIC CROSS SECTIONS FROM PARTICLE DATA GROUP
C  (A.BALDINI ET AL., LANDOLT-BOERNSTEIN NEW SERIES I/12A (1987) 193)
              SIGMA  = 24.3D0 - 12.3D0 * PLAB**(-1.91D0)
     *                + 0.324D0 * PLABLG**2 - 2.44D0 * PLABLG
            ELSE
C  FACTOR 0.6667 GIVES RATIO BETWEEN PION AND NUCLEON CROSS SECTION
              SIGMA  = 22.01D0 * ELAB**.0642D0 * 0.6667D0
            ENDIF
C  AUXIL. QUANTITIES FOR INTERPOLATION
            SIG45  = SIGMA - 45.D0
            S45SQ  = SIG45**2 / 450.D0
            S4530  = SIG45 / 30.D0
C  INELASTIC CROSS SECTIONS OF AIR FOR PROJECTILE WITH MASS NUMBER 1
            SIGAIR = (1.D0 - 2.D0 * S45SQ) * SIG45A(1)
     *                    +(S45SQ - S4530) * SIG30A(1)
     *                    +(S45SQ + S4530) * SIG60A(1)
        ENDIF
        IF ( DEBUG ) WRITE(MDEBUG,*)'BOX2  : SIGMA,SIGAIR,GHESIG=',
     *                           SNGL(SIGMA),SNGL(SIGAIR),GHESIG
 
        CALL RMMAR( RD,2,1 )
C  MEAN FREE PATH FOR INTERACTION (CHIINT)  OR DECAY (CHI)
        CHIINT = -LOG(RD(1)) * AVERAW / (AVOGAD * SIGAIR)
        IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : ITYPE,RD(1),CHIINT=',
     *                                 ITYPE,RD(1),SNGL(CHIINT)
        COR1   = -LOG(RD(2)) * C(25) * C(18)
        CALL PRANGE(COR1)
        IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : ITYPE,RD(2),CHIDEC=',
     *                                 ITYPE,RD(2),SNGL(CHI)
        CHI    = MIN( CHIINT, CHI )
        IF ( CHI .LT. CHIINT ) THEN
          FDECAY = .TRUE.
        ELSE
          FDECAY = .FALSE.
        ENDIF
 
C  INTERACTION LENGTH STATISTICS
        IP = 1.D0 + CHI * 0.1D0
        IP = MIN( IP, 123 )
        IPICHI( IP) = IPICHI( IP) + 1
        IPICHI(124) = IPICHI(124) + 1
 
C-----------------------------------------------------------------------
C  NUCLEONS AND ANTINUCLEONS
      ELSEIF ( ITYPE .EQ. 13  .OR.  ITYPE .EQ. 14  .OR.
     *         ITYPE .EQ. 15  .OR.  ITYPE .EQ. 25 ) THEN
        PLAB = ELAB * BETA
C  CALCULATION OF CROSS SECTION IN THE GHEISHA ROUTINES
        IF ( GHEISH  .AND.  (ELAB .LE. HILOELB) ) THEN
          EKIN   = ELAB - PAMA(ITYPE)
          SIGAIR = CGHSIG(SNGL(PLAB),EKIN,ITYPE)
          GHESIG = .TRUE.
        ELSE
          GHESIG = .FALSE.
C  SIGMA IS ENERGY DEPENDENT INELASTIC NUCLEON-NUCLEON CROSS SECTION
            IF     ( PLAB .LT. 1.D1 ) THEN
              SIGMA  = 29.9D0
            ELSEIF ( PLAB .LT. 1.D3 ) THEN
              PLABLG = LOG(PLAB)
C  INELASTIC CROSS SECTIONS FROM PARTICLE DATA GROUP
C  (A.BALDINI ET AL., LANDOLT-BOERNSTEIN NEW SERIES I/12B (1987) 150)
              SIGMA  = 30.9D0 - 28.9D0 * PLAB**(-2.46D0)
     *                + 0.192D0 * PLABLG**2 - 0.835D0 * PLABLG
            ELSE
              SIGMA  = 22.01D0 * ELAB**.0642D0
            ENDIF
 
C  ADD ANNIHILATION CROSS SECTION FOR ANTI-NUCLEONS
            IF ( ITYPE .EQ. 15  .OR.  ITYPE .EQ. 25 ) THEN
C  ANNIHILATION CROSS SECTIONS FROM PARTICLE DATA GROUP
C  (A.BALDINI ET AL., LANDOLT-BOERNSTEIN NEW SERIES I/12B (1987) 286)
              SIGANN = 0.532D0 + 0.634D2 * PLAB**(-0.71D0)
              SIGMA  = MIN( 120.D0, SIGMA + SIGANN )
            ENDIF
C  AUXIL. QUANTITIES FOR INTERPOLATION
            SIG45  = SIGMA - 45.D0
            S45SQ  = SIG45**2 / 450.D0
            S4530  = SIG45 / 30.D0
C  INELASTIC CROSS SECTIONS OF AIR FOR PROJECTILE WITH MASS NUMBER 1
            SIGAIR = (1.D0 - 2.D0 * S45SQ) * SIG45A(1)
     *                    +(S45SQ - S4530) * SIG30A(1)
     *                    +(S45SQ + S4530) * SIG60A(1)
        ENDIF
        IF ( DEBUG ) WRITE(MDEBUG,*)'BOX2  : SIGMA,SIGAIR,GHESIG=',
     *                           SNGL(SIGMA),SNGL(SIGAIR),GHESIG
 
C  MEAN FREE PATH FROM MOLECULAR WEIGHT, AVOGADRO'S CONSTANT AND SIGMA
        CALL RMMAR( RD,1,1 )
        CHI = -LOG(RD(1)) * AVERAW / (AVOGAD * SIGAIR)
        IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : ITYPE,RD(1),CHI=',
     *                                 ITYPE,RD(1),SNGL(CHI)
 
C  INTERACTION LENGTH STATISTICS
        NU = 1.D0 + CHI * 0.1D0
        NU = MIN( NU, 123 )
        INUCHI( NU) = INUCHI( NU) + 1
        INUCHI(124) = INUCHI(124) + 1
 
C-----------------------------------------------------------------------
C  KAONS (PARTICLE TYPES 10,11,12,16)
      ELSEIF ( ITYPE .EQ. 10  .OR.  ITYPE .EQ. 11  .OR.
     *         ITYPE .EQ. 12  .OR.  ITYPE .EQ. 16      ) THEN
        PLAB = ELAB * BETA
C  CALCULATION OF CROSS SECTION IN THE GHEISHA ROUTINES
        IF ( GHEISH  .AND.  (ELAB .LE. HILOELB) ) THEN
          EKIN   = ELAB - PAMA(ITYPE)
          SIGAIR = CGHSIG(SNGL(PLAB),EKIN,ITYPE)
          GHESIG = .TRUE.
        ELSE
          GHESIG = .FALSE.
C  SIGMA IS ENERGY DEPENDENT INELASTIC KAON-NUCLEON CROSS SECTION
            IF     ( PLAB .LE. 1.D1 ) THEN
              SIGMA  = 14.11D0
            ELSEIF ( PLAB .LT. 1.D3 ) THEN
              PLABLG = LOG(PLAB)
C  INELASTIC CROSS SECTIONS FROM PARTICLE DATA GROUP
C  (A.BALDINI ET AL., LANDOLT-BOERNSTEIN NEW SERIES I/12B (1987) 56)
              SIGMA  = 12.3D0 - 7.77D0 * PLAB**(-2.12D0)
     *              + 0.0326D0 * PLABLG**2 + 0.738D0 * PLABLG
            ELSE
C  FACTOR 0.5541 GIVES RATIO BETWEEN KAON AND NUCLEON CROSS SECTION
              SIGMA  = 22.01D0 * ELAB**.0642D0 * 0.5541D0
            ENDIF
C  AUXIL. QUANTITIES FOR INTERPOLATION
            SIG45  = SIGMA - 45.D0
            S45SQ  = SIG45**2 / 450.D0
            S4530  = SIG45 / 30.D0
C  INELASTIC CROSS SECTIONS OF AIR FOR PROJECTILE WITH MASS NUMBER 1
            SIGAIR = (1.D0 - 2.D0 * S45SQ) * SIG45A(1)
     *                    +(S45SQ - S4530) * SIG30A(1)
     *                    +(S45SQ + S4530) * SIG60A(1)
        ENDIF
        IF ( DEBUG ) WRITE(MDEBUG,*)'BOX2  : SIGMA,SIGAIR,GHESIG=',
     *                           SNGL(SIGMA),SNGL(SIGAIR),GHESIG
 
        CALL RMMAR( RD,2,1 )
C  MEAN FREE PATH FOR INTERACTION (CHIINT)  OR DECAY (CHI)
        CHIINT = -LOG(RD(1)) * AVERAW / (AVOGAD * SIGAIR)
        IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : ITYPE,RD(1),CHIINT=',
     *                                 ITYPE,RD(1),SNGL(CHIINT)
 
        IF     ( ITYPE .EQ. 16 ) THEN
          CKA2 = CKA(5)
        ELSEIF ( ITYPE .EQ. 10 ) THEN
          CKA2 = CKA(6)
        ELSE
          CKA2 = CKA(3)
        ENDIF
        COR1   = -LOG(RD(2)) * C(25) * CKA2
        IF ( SIGNUM(ITYPE) .EQ. 0.D0 ) THEN
C  NEUTRAL KAONS
          DH   = BETA * GAMMA * COSTHE * COR1
          HDEC = MAX( H - DH, -1.D5 )
          CHI  = ( THICK(HDEC) - THICKH ) / COSTHE
        ELSE
C  CHARGED KAONS
          CALL PRANGE(COR1)
        ENDIF
 
        IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : ITYPE,RD(2),CHIDEC=',
     *                                 ITYPE,RD(2),SNGL(CHI)
        CHI    = MIN( CHIINT, CHI )
        IF ( CHI .LT. CHIINT ) THEN
          FDECAY = .TRUE.
        ELSE
          FDECAY = .FALSE.
        ENDIF
 
C  INTERACTION LENGTH STATISTICS
        KA = 1.D0 + CHI * 0.1D0
        KA = MIN( KA, 123 )
        IKACHI( KA) = IKACHI( KA) + 1
        IKACHI(124) = IKACHI(124) + 1
 
C-----------------------------------------------------------------------
C  STRANGE BARYONS ( LAMBDA, SIGMA(+,0,-),XI(0,-), OMEGA- )
      ELSEIF ( (ITYPE .GE. 18  .AND.  ITYPE .LE. 24)  .OR.
     *         (ITYPE .GE. 26  .AND.  ITYPE .LE. 32)      ) THEN
        PLAB = ELAB * BETA
C  CALCULATION OF CROSS SECTION IN THE GHEISHA ROUTINES
        IF ( GHEISH  .AND.  (ELAB .LE. HILOELB) ) THEN
          EKIN   = ELAB - PAMA(ITYPE)
          SIGAIR = CGHSIG(SNGL(PLAB),EKIN,ITYPE)
C  SET CROSS SECTION VALUE TO A SMALL NUMBER FOR SIGMA0 AND ANTI SIGMA0
          IF ( ITYPE .EQ. 20 .OR. ITYPE .EQ. 28 ) THEN
            SIGAIR = 1.D-3
          ENDIF
          GHESIG = .TRUE.
        ELSE
          GHESIG = .FALSE.
C  CROSS SECTION FOR BARYONS IS ASSUMED TO BE THE SAME AS FOR NUCLEONS
C  SIGMA IS ENERGY DEPENDENT INELASTIC NUCLEON-NUCLEON CROSS SECTION
            IF     ( PLAB .LT. 1.D1 ) THEN
              SIGMA  = 29.9D0
            ELSEIF ( PLAB .LT. 1.D3 ) THEN
              PLABLG = LOG(PLAB)
C  INELASTIC CROSS SECTIONS FROM PARTICLE DATA GROUP
C  (A.BALDINI ET AL., LANDOLT-BOERNSTEIN NEW SERIES I/12B (1987) 150)
              SIGMA  = 30.9D0 - 28.9D0 * PLAB**(-2.46D0)
     *                + 0.192D0 * PLABLG**2 - 0.835D0 * PLABLG
            ELSE
              SIGMA  = 22.01D0 * ELAB**.0642D0
            ENDIF
C  AUXIL. QUANTITIES FOR INTERPOLATION
            SIG45  = SIGMA - 45.D0
            S45SQ  = SIG45**2 / 450.D0
            S4530  = SIG45 / 30.D0
C  INELASTIC CROSS SECTIONS OF AIR FOR PROJECTILE WITH MASS NUMBER 1
            SIGAIR = (1.D0 - 2.D0 * S45SQ) * SIG45A(1)
     *                  +(S45SQ - S4530) * SIG30A(1)
     *                  +(S45SQ + S4530) * SIG60A(1)
        ENDIF
        IF ( DEBUG ) WRITE(MDEBUG,*)'BOX2  : SIGMA,SIGAIR,GHESIG=',
     *                           SNGL(SIGMA),SNGL(SIGAIR),GHESIG
 
        CALL RMMAR( RD,2,1 )
C  MEAN FREE PATH FOR INTERACTION (CHIINT)  OR DECAY (CHI)
        IF     ( ITYPE .GE. 18  .AND.  ITYPE .LE. 21 ) THEN
          COR1 = -LOG(RD(2)) * C(25) * CSTRBA(ITYPE-17)
        ELSEIF ( ITYPE .GE. 22  .AND.  ITYPE .LE. 24 ) THEN
          COR1 = -LOG(RD(2)) * C(25) * CSTRBA(ITYPE-15)
        ELSEIF ( ITYPE .GE. 26  .AND.  ITYPE .LE. 29 ) THEN
          COR1 = -LOG(RD(2)) * C(25) * CSTRBA(ITYPE-25)
        ELSEIF ( ITYPE .GE. 30  .AND.  ITYPE .LE. 32 ) THEN
          COR1 = -LOG(RD(2)) * C(25) * CSTRBA(ITYPE-23)
        ENDIF
        IF ( SIGNUM(ITYPE) .EQ. 0.D0 ) THEN
C  NEUTRAL STRANGE BARYONS
          DH   = BETA * GAMMA * COSTHE * COR1
          HDEC = MAX( H - DH, -1.D5 )
          CHI  = ( THICK(HDEC) - THICKH ) / COSTHE
        ELSE
C  CHARGED STRANGE BARYONS
          CALL PRANGE(COR1)
        ENDIF
        IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : ITYPE,RD(2),CHIDEC=',
     *                                 ITYPE,RD(2),SNGL(CHI)
        CHIINT = -LOG(RD(1)) * AVERAW / (AVOGAD * SIGAIR)
        IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : ITYPE,RD(1),CHIINT=',
     *                                 ITYPE,RD(1),SNGL(CHIINT)
        CHI    = MIN( CHIINT, CHI )
          IF ( CHI .LT. CHIINT ) THEN
            FDECAY = .TRUE.
          ELSE
            FDECAY = .FALSE.
          ENDIF
C  GHEISHA CANNOT TREAT SIGMA0 AND ANTI-SIGMA0, LET THEM DECAY
        IF (GHESIG .AND. (ITYPE.EQ.20 .OR. ITYPE.EQ.28))FDECAY = .TRUE.
 
C  INTERACTION LENGTH STATISTICS
        IHY = 1.D0 + CHI * 0.1D0
        IHY = MIN( IHY, 123 )
        IHYCHI(IHY) = IHYCHI(IHY) + 1
        IHYCHI(124) = IHYCHI(124) + 1
 
C-----------------------------------------------------------------------
C  HEAVY PRIMARIES ( ITYPE = 100 * A + Z ,  FE -> ITYPE = 5626 )
C  ( APPEARING AT FIRST INTERACTION AND AS REMANENTS OF THE PRIMARY )
      ELSEIF ( ITYPE .GT. 100 ) THEN
        IA = ITYPE / 100
        IF ( IA .GT. 56 ) THEN
          WRITE(MONIOU,*) 'BOX2  : UNEXPECTED PARTICLE TYPE=',ITYPE
          STOP
        ENDIF
C  MEAN FREE PATH OF THE HEAVY PRIMARY IS DEDUCED FROM THAT OF A NUCLEON
C  ONLY INELASTIC SCATTERING AT INTERACTIONS WITH HEAVY PRIMARY/FRAGMENT
        ELAB = (PAMA(13) + PAMA(14)) * 0.5D0 * GAMMA
        PLAB = ELAB * BETA
C  CALCULATION OF CROSS SECTION IN THE GHEISHA ROUTINES
        ELABT = ELAB * IA
        
c> *** modified by fs (22/09/98) *******************************

c        IF ( GHEISH  .AND.  (ELAB .LE. HILOELB)  .AND.
c     *     (ITYPE.EQ.402 .OR. ITYPE.EQ.201 .OR. ITYPE.EQ.301) ) THEN
        IF ( GHEISH  .AND.  (ELAB .LE. HILOELB)  .AND.
     *       (ITYPE.LE.101) ) THEN

c> *** end of modification *************************************

          EKIN   = ELABT - PAMA(ITYPE)
          SIGGHE = CGHSIG(SNGL(PLAB),EKIN,ITYPE)
          IF ( SIGGHE .LE. 0. ) THEN
            GHESIG = .FALSE.
          ELSE
            GHESIG = .TRUE.
            SIGAIR = SIGGHE
          ENDIF
        ELSE
          GHESIG = .FALSE.
        ENDIF
        IF ( .NOT. GHESIG ) THEN
C  SIGMA IS ENERGY DEPENDENT INELASTIC NUCLEON-NUCLEON CROSS SECTION
            IF     ( PLAB .LT. 1.D1 ) THEN
              SIGMA  = 29.9D0
            ELSEIF ( PLAB .LT. 1.D3 ) THEN
              PLABLG = LOG(PLAB)
C  INELASTIC CROSS SECTIONS FROM PARTICLE DATA GROUP
C  (A.BALDINI ET AL., LANDOLT-BOERNSTEIN NEW SERIES I/12B (1987) 150)
              SIGMA  = 30.9D0 - 28.9D0 * PLAB**(-2.46D0)
     *              + 0.192D0 * PLABLG**2 - 0.835D0 * PLABLG
            ELSE
              SIGMA  = 22.01D0 * ELAB**.0642D0
            ENDIF
C  AUXIL. QUANTITIES FOR INTERPOLATION
          SIG45  = SIGMA - 45.D0
          S45SQ  = SIG45**2 / 450.D0
          S4530  = SIG45 / 30.D0
C  INELASTIC CROSS SECTIONS OF AIR FOR PROJECTILE WITH MASS NUMBER IA
          SIGAIR = (1.D0 - 2.D0 * S45SQ) * SIG45A(IA)
     *                  +(S45SQ - S4530) * SIG30A(IA)
     *                  +(S45SQ + S4530) * SIG60A(IA)
        ENDIF
        IF ( DEBUG ) WRITE(MDEBUG,*)'BOX2  : SIGMA,SIGAIR,GHESIG=',
     *                           SNGL(SIGMA),SNGL(SIGAIR),GHESIG
 
C  MEAN FREE PATH FROM MOLECULAR WEIGHT, AVOGADRO'S CONSTANT AND SIGMA
        IF ( SIGAIR .EQ. 0.D0 ) WRITE(MONIOU,*)
     *    'BOX2: SIGAIR=0.D0, PROGRAM STOPPED',
     *    'CHECK SELECTED CROSS SECTIONS AND PRIMARIES'
        CALL RMMAR( RD,1,1 )
        CHI = -LOG(RD(1)) * AVERAW / (AVOGAD * SIGAIR)
        IF(DEBUG)WRITE(MDEBUG,*)'BOX2  : ITYPE,RD(1),CHI=',
     *                                 ITYPE,RD(1),SNGL(CHI)
 
C  INTERACTION LENGTH STATISTICS
        NI = 1.D0 + CHI * 0.1D0
        NI = MIN( NI, 123 )
        INNCHI( NI) = INNCHI( NI) + 1
        INNCHI(124) = INNCHI(124) + 1
 
C-----------------------------------------------------------------------
C  ERROR IN PARTICLE CODE
      ELSE
        WRITE(MONIOU,*) 'BOX2  : UNEXPECTED PARTICLE TYPE=',ITYPE
        STOP
      ENDIF
 
      RETURN
      END
