      SUBROUTINE KDECAY( IGO )
 
C-----------------------------------------------------------------------
C  K(AON) DECAY
C
C  KAON DECAYS WITH FULL KINEMATIC, ENERGY AND MOMENTA CONSERVED
C  ALL SECONDARY PARTICLES ARE WRITTEN TO STACK
C  THIS SUBROUTINE IS CALLED FROM NUCINT
C  ARGUMENT:         (TO CHARACTERIZE THE DECAYING KAON)
C   IGO    = 1  K+
C          = 2  K-
C          = 3  K0S
C          = 4  K0L
C
C  DESIGN  : D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,DECAY.
      COMMON /DECAY/   GAM345,COS345,PHI345
      DOUBLE PRECISION GAM345(3),COS345(3),PHI345(3)
*KEEP,IRET.
      COMMON /IRET/    IRET1,IRET2
      INTEGER          IRET1,IRET2
*KEEP,KAONS.
      COMMON /KAONS/   CKA
      DOUBLE PRECISION CKA(80)
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
 
      DOUBLE PRECISION BETA3,COSTCM,COSTH3,GAMMA3,PHI3,RA,WORK1,WORK2
      INTEGER          I,ICHARG,IGO,IPI,J,M3
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,9)
  444 FORMAT(' KDECAY: CURPAR=',1P,9E10.3)
 
C  COPY COORDINATES INTO SECPAR
      DO 20  J = 5,8
        SECPAR(J) = CURPAR(J)
   20 CONTINUE
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DECAY OF K(+,-) (6 MODES)
 
      IF     ( IGO .LE. 2 ) THEN
   21   CALL RMMAR( RD,1,1 )
        RA = RD(1)
 
C  DECAY  K(+,-)  ---->  MU(+,-) + NEUTRINO
        IF     ( RA .LE. CKA(23) ) THEN
C  NEUTRINO IS DROPPED
          WORK1  = CKA(28) * GAMMA
          WORK2  = CKA(29) * BETA * WORK1
          CALL RMMAR( RD,2,1 )
          COSTCM = RD(1) * 2.D0 - 1.D0
C  MU(+,-)
          GAMMA3 = WORK1 + COSTCM * WORK2
          BETA3  = SQRT( 1.D0 - 1.D0 / GAMMA3**2 )
          COSTH3 = MIN( 1.D0, (GAMMA * GAMMA3 - CKA(28))
     *                   / (BETA * GAMMA * BETA3 * GAMMA3) )
          PHI3   = RD(2) * PI2
          CALL ADDANG( COSTHE,PHI, COSTH3,PHI3, SECPAR(3),SECPAR(4) )
          IF ( SECPAR(3) .GT. C(29) ) THEN
            SECPAR(1) = 4 + IGO
            SECPAR(2) = GAMMA3
C  DIRECTION OF PION IN THE MUON CM SYSTEM (= DIRECTION OF POLARIZATION)
C  SEE: G. BARR ET AL., PHYS. REV. D39 (1989) 3532, EQ. 5
C  POLART IS COS OF ANGLE BETWEEN KAON AND LABORATORY IN THE MU CM
C  POLARF IS ANGLE PHI AROUND THE LAB DIRECTION IN THE MU CM
C  POLART, POLARF WITH RESPECT TO THE MU DIRECTION IN THE LAB SYSTEM
            POLART = ( 2.D0*PAMA(11)*GAMMA*C(6) / (PAMA(5)*GAMMA3)
     *                 - C(6) - 1.D0 ) / ( BETA3 * (1.D0-C(6)) )
            POLARF = PHI3 - PI
C  PION DIRECTION IS DIRECTION OF POLARIZATION FOR K+, OPPOSITE FOR K-
            IF ( ITYPE .EQ. 12 ) THEN
              POLART = -POLART
              POLARF = POLARF + PI
            ENDIF
C  GET THE POLARIZATION DIRECTION IN THE MU CM RELATIVE TO THE CORSIKA
C  COORDINATE SYSTEM
            CALL ADDANG( SECPAR(3),SECPAR(4), POLART,POLARF,
     *                                             POLART,POLARF )
            SECPAR(11) = POLART
            SECPAR(12) = POLARF
            CALL TSTACK
            SECPAR(11) = 0.D0
            SECPAR(12) = 0.D0
          ENDIF
 
C  DECAY  K(+,-)  ---->  PI(+,-) + PI(0)
        ELSEIF ( RA .LE. CKA(47) ) THEN
          M3 = ITYPE - 3
          CALL DECAY1( ITYPE, M3, 7 )
 
C  DECAY   K(+,-)  ---->   PI(+,-) + PI(+,-) + PI(-,+)
        ELSEIF ( RA. LE. CKA(48) ) THEN
          CALL DECAY6( PAMA(11), PAMA(8),PAMA(8),PAMA(8),
     *                 CKA(51),CKA(52),CKA(53), CKA(54), 1 )
C  PI(+,-)  AND  PI(+,-) AND  THIRD (ODD) PI(-,+)
          DO 230  I = 1,3
            CALL ADDANG( COSTHE,PHI, COS345(I),PHI345(I),
     *                                 SECPAR(3),SECPAR(4) )
            IF ( SECPAR(3) .GT. C(29) ) THEN
              IF ( I .LE. 2 ) THEN
                SECPAR(1) =  7 + IGO
              ELSE
                SECPAR(1) = 10 - IGO
              ENDIF
              SECPAR(2) = GAM345(I)
              CALL TSTACK
            ENDIF
  230     CONTINUE
 
C  DECAY  K(+,-)  ---->  PI(0)  + E(+,-) + NEUTRINO
        ELSEIF ( RA. LE. CKA(49) ) THEN
          CALL DECAY6( PAMA(11), PAMA(7),PAMA(2),0.D0,
     *                 CKA(65),CKA(66),0.D0, CKA(67), 4 )
C  PI(0)  AND  E(+,-) / NEUTRINO IS DROPPED
          DO 250  I = 1,2
            CALL ADDANG( COSTHE,PHI, COS345(I),PHI345(I),
     *                                  SECPAR(3),SECPAR(4) )
            IF ( SECPAR(3) .GT. C(29) ) THEN
              IF ( I .EQ. 1 ) THEN
                SECPAR(1) = 7.D0
              ELSE
                SECPAR(1) = 1 + IGO
              ENDIF
              SECPAR(2) = GAM345(I)
              CALL TSTACK
            ENDIF
  250     CONTINUE
 
C  DECAY  K(+,-)  ---->  PI(0)  + MU(+,-) + NEUTRINO
        ELSEIF ( RA. LE. CKA(50) ) THEN
          CALL DECAY6( PAMA(11), PAMA(7),PAMA(5),0.D0,
     *                 CKA(68),CKA(69),0.D0, CKA(70), 3 )
C  PI(0)  AND  MU(+,-) / NEUTRINO IS DROPPED
          DO 260  I = 1,2
            CALL ADDANG( COSTHE,PHI, COS345(I),PHI345(I),
     *                                  SECPAR(3),SECPAR(4) )
            IF ( SECPAR(3) .GT. C(29) ) THEN
              SECPAR(2) = GAM345(I)
              IF ( I .EQ. 1 ) THEN
                SECPAR(1) = 7.D0
              ELSE
                SECPAR(1) = 4 + IGO
                IF ( SECPAR(1) .EQ. 6.D0 ) THEN
C  INVERT POLARIZATION DIRECTION FOR MU(-)
                  POLART  = -POLART
                  POLARF  =  POLARF + PI
                ENDIF
C  GET THE POLARIZATION DIRECTION IN THE MU CM RELATIVE TO THE CORSIKA
C  COORDINATE SYSTEM
                CALL ADDANG( SECPAR(3),SECPAR(4), POLART, POLARF,
     *                                             POLART,POLARF )
                SECPAR(11) = POLART
                SECPAR(12) = POLARF
              ENDIF
              CALL TSTACK
            ENDIF
            SECPAR(11) = 0.D0
            SECPAR(12) = 0.D0
  260     CONTINUE
 
C  DECAY  K(+,-)  ---->  PI(0) + PI(0) + PI(+,-)
        ELSE
          CALL DECAY6( PAMA(11), PAMA(7),PAMA(7),PAMA(8),
     *                 CKA(55),CKA(56),CKA(57), CKA(58), 1 )
C  PI(0)'S  AND  PI(+,-)
          DO 270  I = 1,3
            CALL ADDANG( COSTHE,PHI, COS345(I),PHI345(I),
     *                                  SECPAR(3),SECPAR(4) )
            IF ( SECPAR(3) .GT. C(29) ) THEN
              IF ( I .LE. 2 ) THEN
                SECPAR(1) = 7.D0
              ELSE
                SECPAR(1) = 7 + IGO
              ENDIF
              SECPAR(2) = GAM345(I)
              CALL TSTACK
            ENDIF
  270     CONTINUE
 
        ENDIF
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DECAY OF K0S  (2 MODES)
      ELSEIF ( IGO .EQ. 3 ) THEN
 
        CALL RMMAR( RD,1,1 )
C  DECAY  K0S  ---->  PI(+) + PI(-)
        IF ( RD(1) .LE. CKA(24) ) THEN
          CALL DECAY1( ITYPE, 8, 9 )
 
C  DECAY  K0S  ---->  PI(0) + PI(0)
        ELSE
          CALL DECAY1( ITYPE, 7, 7 )
 
        ENDIF
 
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DECAY OF K0L   (4 MODES)
      ELSEIF ( IGO .EQ. 4 ) THEN
        CALL RMMAR( RD,1,1 )
        RA = RD(1)
 
C  DECAY   K0L  ---->   PI(+,-)  + E(-,+) + NEUTRINO
        IF     ( RA .LE. CKA(27) ) THEN
          CALL DECAY6( PAMA(10), PAMA(8),PAMA(2),0.D0,
     *                 CKA(71),CKA(72),0.D0, CKA(73), 4 )
          CALL RMMAR( RD,1,1 )
C  CHARGE ASYMMETRY PREFERS FORMATION OF PI(-)
          ICHARG = INT(1.5016 + RD(1))
C  PI(+,-)  AND  E(-,+) / NEUTRINO IS DROPPED
          DO 420  I = 1,2
            CALL ADDANG( COSTHE,PHI, COS345(I),PHI345(I),
     *                                  SECPAR(3),SECPAR(4) )
            IF ( SECPAR(3) .GT. C(29) ) THEN
              SECPAR(1) = 10 - 3*I - (2*I-3)*ICHARG
              SECPAR(2) = GAM345(I)
              CALL TSTACK
            ENDIF
  420     CONTINUE
 
C  DECAY   K0L  ---->  PI(+,-)  + MU(-,+) + NEUTRINO
        ELSEIF ( RA .LE. CKA(26) ) THEN
          CALL DECAY6( PAMA(10), PAMA(8),PAMA(5),0.D0,
     *                 CKA(74),CKA(75),0.D0, CKA(76), 3 )
          CALL RMMAR( RD,1,1 )
C  CHARGE ASYMMETRY PREFERS FORMATION OF PI(-)
          ICHARG = INT(1.5016 + RD(1))
C  PI(+,-)  AND  MU(-,+) / NEUTRINO IS DROPPED
          DO 430  I = 1,2
            CALL ADDANG( COSTHE,PHI, COS345(I),PHI345(I),
     *                                  SECPAR(3),SECPAR(4) )
            IF ( I .EQ. 1 ) THEN
              SECPAR(1) = 7 + ICHARG
              IPI = SECPAR(1)
            ENDIF
            IF ( SECPAR(3) .GT. C(29) ) THEN
              SECPAR(2) = GAM345(I)
              IF     ( I .EQ. 2 ) THEN
                SECPAR(1) = 7 - ICHARG
                IF ( SECPAR(1) .EQ. 6.D0 ) THEN
C  INVERT POLARIZATION DIRECTION FOR MU(-)
                  POLART = -POLART
                  POLARF =  POLARF + PI
                ENDIF
C  GET THE POLARIZATION DIRECTION IN THE MU CM RELATIVE TO THE CORSIKA
C  COORDINATE SYSTEM
                CALL ADDANG( SECPAR(3),SECPAR(4), POLART,POLARF,
     *                                               POLART,POLARF )
                SECPAR(11) = POLART
                SECPAR(12) = POLARF
              ENDIF
              CALL TSTACK
            ENDIF
            SECPAR(11) = 0.D0
            SECPAR(12) = 0.D0
  430     CONTINUE
 
C  DECAY   K0L  ---->  PI(0) + PI(0) + PI(0)
        ELSEIF ( RA .LE. CKA(25) ) THEN
C  SEE: S.V. SOMALWAR ET AL., PHYS.REV.LET. 68(1992)2580
          CALL DECAY6( PAMA(10), PAMA(7),PAMA(7),PAMA(7),
     *                 CKA(59),-.00033D0,CKA(59), CKA(60), 1 )
C  PI(0)'S
          SECPAR(1) = 7.D0
          DO 440  I = 1,3
            CALL ADDANG( COSTHE,PHI, COS345(I),PHI345(I),
     *                                  SECPAR(3),SECPAR(4) )
            IF ( SECPAR(3) .GT. C(29) ) THEN
              SECPAR(2) = GAM345(I)
              CALL TSTACK
            ENDIF
  440     CONTINUE
 
C  DECAY   K0L  ---->   PI(+) + PI(-) + PI(0)
        ELSE
          CALL DECAY6( PAMA(10), PAMA(8),PAMA(8),PAMA(7),
     *                 CKA(61),CKA(62),CKA(63), CKA(64), 1 )
C  PI(+)  AND  PI(-)  AND  PI(0)
          DO 450  I = 1,3
            CALL ADDANG( COSTHE,PHI, COS345(I),PHI345(I),
     *                                  SECPAR(3),SECPAR(4) )
            IF ( SECPAR(3) .GT. C(29) ) THEN
              IF ( I .LE. 2 ) THEN
                SECPAR(1) = 7 + I
              ELSE
                SECPAR(1) = 7.D0
              ENDIF
              SECPAR(2) = GAM345(I)
              CALL TSTACK
            ENDIF
  450     CONTINUE
 
        ENDIF
      ENDIF
 
C  KILL CURRENT PARTICLE
      IRET1 = 1
 
      RETURN
      END
