      SUBROUTINE ISOBAR( E,KIND,AMASS,ASMASS,NOPI )
 
C-----------------------------------------------------------------------
C  ISOBAR
C
C  THREE AND FOUR PION DECAY OF HEAVY ISOBAR, DECAY IS PICKED AT
C  RANDOM FROM A UNIFORM DISTRIBUTION WITH EQUAL PROBABILITY
C  CHARGE IS DISTRIBUTED AT RANDOM WITH EQUAL PROBABILITY
C  DECAYS ARE COMPUTED VIA MOMENTA, HAVING UNIFORM DISTRIBUTION
C  UPPER LIMIT OF MOMENTUM DISTRIBUTIONS ARE SPECIFIED BY INPUT DATA
C  ENERGY IS STRICTLY CONSERVED, MOMENTA ONLY ON AVERAGE
C  THIS SUBROUTINE IS CALLED FROM MANY BOX ROUTINES
C  ARGUMENTS:
C   E      = AVAILABLE ENERGY IN CM
C   KIND   = 1  BACKWARD ISOBAR
C          = 0  FORWARD  ISOBAR
C   AMASS  = MASS OF HEAVY MESON
C   ASMASS = MASS TO BE LEFT OVER FOR OTHER PARTICLES
C   NOPI   = NUMBER OF PIONS TO BE GENERATED
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*KEEP,BAL.
      COMMON /BAL/     EBAL
      DOUBLE PRECISION EBAL(10)
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,ELASTY.
      COMMON /ELASTY/  ELAST,IELIS,IELHM,IELNU,IELPI
      DOUBLE PRECISION ELAST
      INTEGER          IELIS(20),IELHM(20),IELNU(20),IELPI(20)
*KEEP,MULT.
      COMMON /MULT/    EKINL,MSMM,MULTMA,MULTOT
      DOUBLE PRECISION EKINL
      INTEGER          MSMM,MULTMA(37,13),MULTOT(37,13)
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
*KEEP,VKIN.
      COMMON /VKIN/    BETACM
      DOUBLE PRECISION BETACM
*KEND.
 
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,201)E,KIND,AMASS,ASMASS,NOPI
 201  FORMAT(' ISOBAR: E,KIND,AMASS,ASMASS,NOPI=',1P,E10.4,I3,2E10.4,I3)
 
C  RETURN 1 KILLS PARTICLE
 
      ISET  = 1
      EDI   = 0.D0
      PACC  = 0.D0
      RESTE = AMASS
 
      IF ( KIND .NE. 0 ) GOTO 100
 
C-----------------------------------------------------------------------
C  FORWARD ISOBAR
C  PIONS FROM FORWARD ISOBAR
      IF ( NOPI .EQ. 1 ) THEN
        INOPI = NOPI
        A     = C(36)
        W     = 0.38D0
      ELSE
        W     = 1.5D0
        CALL RMMAR( RD,1,1 )
        IF ( RD(1) .LE. 0.5 ) THEN
          INOPI = 3
          A     = C(36)
        ELSE
          INOPI = 4
          A     = C(35)
        ENDIF
      ENDIF
 
      GIFCM  = (E**2+AMASS**2-ASMASS**2) * 0.5D0 /(E*AMASS)
      BEIFCM = SQRT(GIFCM**2 - 1.D0) / GIFCM
      GIFLAB = GCM * GIFCM * (1.D0+BETACM*BEIFCM)
      BEIFL  = SQRT(GIFLAB**2 - 1.D0) / GIFLAB
 
C  PION LOOP FOR FORWARD ISOBAR DECAY
 13   CONTINUE
      P      = PCL(A,W)
      PT     = PTRANS(DUMMY)
      GPIIF  = SQRT( P**2 / PAMA(8)**2 + 1.D0 )
      BEPIIF = SQRT(GPIIF**2 - 1.D0) / GPIIF
      EDI    = EDI + SQRT( PAMA(8)**2 + P**2 + PT**2 )
      RESTE  = RESTE - EDI
 
      CALL RMMAR( RD,3,1 )
      IF ( RD(1) .GT. 0.5 ) THEN
        GPILAB = GPIIF*GIFLAB*(1.D0-BEIFL*BEPIIF)
        PACC   = PACC - P
      ELSE
        GPILAB = GPIIF*GIFLAB*(1.D0+BEIFL*BEPIIF)
        PACC   = PACC + P
      ENDIF
 
C  CORRECTIVE ACTION IF PLLAB2 LE 1.E-6
      PLLAB2 = PAMA(8)**2 *(GPILAB**2 - 1.D0)
      PLLAB2 = MAX( 1.D-6, PLLAB2 )
      CTHETA = SQRT( PLLAB2 / (PT**2+PLLAB2) )
      IF ( CTHETA .GE. C(27) ) THEN
        CALL ADDANG( COSTHE,PHI, CTHETA,RD(2)*PI2, SECPAR(3),SECPAR(4) )
        IF ( SECPAR(3) .GE. C(29) ) THEN
          SECPAR(2) = GPILAB
C  CHARGE ASSIGNMENT
          IF     ( RD(3) .LE. OB3 ) THEN
            SECPAR(1) = 7.D0
          ELSEIF ( RD(3) .LE. TB3 ) THEN
            SECPAR(1) = 8.D0
          ELSE
            SECPAR(1) = 9.D0
          ENDIF
          DO  5  J = 5,8
            SECPAR(J) = CURPAR(J)
 5        CONTINUE
 
          CALL TSTACK
        ENDIF
      ENDIF
      IF ( RESTE .LE. PAMA(14)  .OR.  ISET .EQ. INOPI ) GOTO 14
      ISET = ISET + 1
      GOTO 13
 
C  NUCLEON FROM FORWARD ISOBAR
 14   CONTINUE
      PT    = PTRANS(DUMMY)
      MSMM  = MSMM + ISET
      EPT   = SQRT( PAMA(14)**2 + PT**2 )
      RESTE = RESTE - EPT
 
      IF ( RESTE .LE. 0.D0 ) THEN
        GNFLAB  = GIFLAB
        EBAL(1) = EBAL(1) + RESTE
      ELSE
        GNIF    = (RESTE+PAMA(14)) / PAMA(14)
        BENIF   = SQRT(GNIF**2 - 1.D0) / GNIF
        IF ( PACC .LE. 0.D0 ) THEN
          GNFLAB = GIFLAB * GNIF * (1.D0 + BENIF*BEIFL)
        ELSE
          GNFLAB = GIFLAB * GNIF * (1.D0 - BENIF*BEIFL)
        ENDIF
      ENDIF
 
      PLLAB2 = PAMA(14)**2 * (GNFLAB**2 - 1.D0)
      PLLAB2 = MAX( 1.D-6, PLLAB2 )
      CTHETA = SQRT( PLLAB2 / (PT**2+PLLAB2) )
      IF ( CTHETA .LT. C(27) ) RETURN
      CALL RMMAR( RD,2,1 )
      CALL ADDANG( COSTHE,PHI, CTHETA,RD(1)*PI2, SECPAR(3),SECPAR(4) )
      IF ( SECPAR(3) .LT. C(29) ) RETURN
      SECPAR(2) = GNFLAB
 
      IF ( RD(2) .LT. 0.5 ) THEN
        IADD = 1
      ELSE
        IADD = 0
      ENDIF
 
      IF ( ITYPE .EQ. 13  .OR.  ITYPE .EQ. 14 ) THEN
        SECPAR(1) = 14 - IADD
      ELSE
        SECPAR(1) = 15 + 10*IADD
      ENDIF
 
C  CHARGE ASSIGNMENT
      DO  9  J = 5,8
        SECPAR(J) = CURPAR(J)
    9 CONTINUE
      CALL TSTACK
 
C  FILL HISTOGRAM
      IN = 1.D0 + SECPAR(2) / GAMMA * 20.D0
      IN = MIN( IN, 20 )
      IELIS(IN) = IELIS(IN) + 1
 
      RETURN
 
C-----------------------------------------------------------------------
C  BACKWARD ISOBAR
C  PIONS FROM BACKWARD ISOBAR
  100 CONTINUE
 
      IF ( NOPI .EQ. 1 ) THEN
        INOPI = NOPI
      ELSE
        CALL RMMAR( RD,1,1 )
        IF ( RD(1) .LE. 0.5 ) THEN
          INOPI = 3
        ELSE
          INOPI = 4
        ENDIF
      ENDIF
      WORK = MIN( C(11), GAMMA*0.5D0 )
      MSMM = MSMM + INOPI
      DO 101  J = 1,INOPI
        CALL RMMAR( RD,3,1 )
        GPI    = RD(1)*(WORK-1.D0) + 1.D0
        PT     = PTRANS(DUMMY)
        EDI    = EDI+SQRT( PAMA(8)**2+PAMA(8)**2*(GPI**2-1.D0)+PT**2 )
        RESTE  = ASMASS - EDI
        PLLAB2 = PAMA(8)**2 * (GPI**2 - 1.D0)
        PLLAB2 = MAX( 1.D-6, PLLAB2 )
        CTHETA = SQRT( PLLAB2 / (PT**2+PLLAB2) )
        IF ( CTHETA .GE. C(27) ) THEN
          CALL ADDANG( COSTHE,PHI, CTHETA,RD(2)*PI2,
     *                                    SECPAR(3),SECPAR(4) )
          IF ( SECPAR(3) .GE. C(29) ) THEN
            SECPAR(2) = GPI
C  CHARGE ASSIGNMENT
            RR = RD(3)
            IF     ( RR .LE. OB3 ) THEN
              SECPAR(1) = 7.D0
            ELSEIF ( RR .LE. TB3 ) THEN
              SECPAR(1) = 8.D0
            ELSE
              SECPAR(1) = 9.D0
            ENDIF
            DO 104  I = 5,8
              SECPAR(I) = CURPAR(I)
  104       CONTINUE
 
            CALL TSTACK
          ENDIF
        ENDIF
        IF ( RESTE .LE. PAMA(14) ) GOTO 110
  101 CONTINUE
 
C  NUCLEON FROM BACKWARD ISOBAR
 
  110 CONTINUE
      WORK    = MIN( C(10), GAMMA*0.5D0 )
      CALL RMMAR( RD,3,1 )
      GNRLAB  = RD(1) * (WORK-1.D0) + 1.D0
      PT      = PTRANS(DUMMY)
      EDI = EDI+SQRT( PAMA(14)**2 + PAMA(14)**2*(GNRLAB**2-1.D0)+PT**2)
      RESTE   = ASMASS - EDI
      EBAL(2) = EBAL(2) + RESTE
      PLLAB2  = PAMA(14)**2 * (GNRLAB**2 - 1.D0)
      PLLAB2  = MAX( 1.D-6, PLLAB2 )
      CTHETA  = SQRT( PLLAB2 / (PT**2+PLLAB2) )
      IF ( CTHETA .LT. C(27) ) RETURN
      CALL ADDANG( COSTHE,PHI, CTHETA,RD(2)*PI2, SECPAR(3),SECPAR(4) )
      IF ( SECPAR(3) .LT. C(29) ) RETURN
      SECPAR(2) = GNRLAB
C  CHARGE ASSIGNMENT
      IF ( RD(3) .LT. 0.5 ) THEN
        SECPAR(1) = 13.D0
      ELSE
        SECPAR(1) = 14.D0
      ENDIF
      DO 113  J = 5,8
        SECPAR(J) = CURPAR(J)
  113 CONTINUE
      CALL TSTACK
 
      RETURN
      END
