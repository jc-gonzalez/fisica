      SUBROUTINE NUCINT
 
C-----------------------------------------------------------------------
C  NUC(LEAR) INT(ERACTION)
C
C  SELECTS TYPE OF INTERACTION PROCESS FOR ISOBAR MODEL, ACCORDING TO ECM
C  ISOBAR MASSES INDEPENDENT OF RESPECTIVE ENERGY RANGES
C  HEAVY PRIMARIES AND STRANGE BARYONS INCLUDED
C  THIS SUBROUTINE IS CALLED FROM MAIN
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,AIR.
      COMMON /AIR/     COMPOS,PROBTA,AVERAW,AVOGAD
      DOUBLE PRECISION COMPOS(3),PROBTA(3),AVERAW,AVOGAD
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,GENER.
      COMMON /GENER/   GEN,ALEVEL
      DOUBLE PRECISION GEN,ALEVEL
*KEEP,IRET.
      COMMON /IRET/    IRET1,IRET2
      INTEGER          IRET1,IRET2
*KEEP,KAONS.
      COMMON /KAONS/   CKA
      DOUBLE PRECISION CKA(80)
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
*KEEP,POLAR.
      COMMON /POLAR/   POLART,POLARF
      DOUBLE PRECISION POLART,POLARF
*KEEP,RANDPA.
      COMMON /RANDPA/  FAC,U1,U2,RD,NSEQ,ISEED,KNOR
      DOUBLE PRECISION FAC,U1,U2
      REAL             RD(3000)
      INTEGER          ISEED(103,10),NSEQ
      LOGICAL          KNOR
*KEEP,RANGE.
      COMMON /RANGE/   CC
      DOUBLE PRECISION CC(20)
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
*KEEP,STATI.
      COMMON /STATI/   SABIN,SBBIN,INBIN,IPBIN,IKBIN,IHBIN
      DOUBLE PRECISION SABIN(37),SBBIN(37)
      INTEGER          INBIN(37),IPBIN(37),IKBIN(37),IHBIN(37)
*KEEP,VKIN.
      COMMON /VKIN/    BETACM
      DOUBLE PRECISION BETACM
*KEND.
 
      DOUBLE PRECISION BETA3,COSMU,COSTCM,COSTH3,GAMMA3,
     *                 PHIMU,PHI3,WORK1,WORK2
      INTEGER          I,IGO,KJ
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,9)
  444 FORMAT(' NUCINT: CURPAR=',1P,9E10.3)
 
C  SET GENERATION AND LEVEL OF LAST INTERACTION
      SECPAR( 9) = GEN
      SECPAR(10) = ALEVEL
C  RESET POLARIZATION, NOT USED FOR PARTICLES OTHER THAN MUONS YET
      SECPAR(11) = 0.D0
      SECPAR(12) = 0.D0
 
C  CALCULATE KIN. ENERGY BIN
      EKINL = PAMA(ITYPE) * ( GAMMA - 1.D0 )
      IF ( EKINL .GE. .1D0 ) THEN
        KJ = INT( MIN( 37.D0, 4.D0 + 3.D0*LOG10(EKINL) ) )
      ELSE
        KJ = 1
      ENDIF
 
C-----------------------------------------------------------------------
C  PION INCIDENT
      IF     ( ITYPE .EQ.  8  .OR.  ITYPE .EQ.  9 ) THEN
        IF ( DEBUG ) WRITE(MDEBUG,*) 'NUCINT: PION EKINL=',SNGL(EKINL)
        IPBIN(KJ) = IPBIN(KJ) + 1
 
C  DECAY OR INTERACTION FOR PIONS ?
        IF ( FDECAY ) THEN
          DO 10  I = 5,8
            SECPAR(I) =  CURPAR(I)
 10       CONTINUE
C  DECAY  PI(+,-)  ---->  MU(+,-) + (ANTI)-NEUTRINO(MU)
          WORK1  = C(48) * GAMMA
          WORK2  = C(49) * BETA * WORK1
          CALL RMMAR( RD,2,1 )
          COSTCM = 2.D0 * RD(1) - 1.D0
          GAMMA3 = WORK1 + COSTCM * WORK2
          BETA3  = SQRT( 1.D0 - 1.D0 / GAMMA3**2 )
          COSTH3 = MIN( 1.D0, ( GAMMA * GAMMA3 - C(48) )
     *                      /( BETA * GAMMA * BETA3 * GAMMA3 ) )
          PHI3   = PI2 * RD(2)
C  MUON / NEUTRINO IS DROPPED
          CALL ADDANG( COSTHE,PHI, COSTH3,PHI3, COSMU,PHIMU )
          IF ( COSMU .GT. C(29) ) THEN
C  DIRECTION OF PION IN THE MUON CM SYSTEM (= DIRECTION OF POLARIZATION)
C  SEE: G. BARR ET AL., PHYS. REV. D39 (1989) 3532, EQ. 5
C  POLART IS COS OF ANGLE BETWEEN PION AND LABORATORY IN THE MU CM
C  POLARF IS ANGLE PHI AROUND THE LAB DIRECTION IN THE MU CM
C  POLART, POLARF ARE WITH RESPECT TO THE MU DIRECTION IN THE LAB SYSTEM
            POLART = ( 2.D0*PAMA(8)*GAMMA*C(7)/(PAMA(5)*GAMMA3)
     *                 - C(7) - 1.D0 ) / ( BETA3 * (1.D0 - C(7)) )
            POLARF = PHI3 - PI
C  PION DIRECTION IS DIRECTION OF POLARIZATION FOR PI+, OPPOSITE FOR PI-
            IF ( ITYPE .EQ. 9 ) THEN
              POLART = -POLART
              POLARF = POLARF + PI
            ENDIF
C  GET THE POLARIZATION DIRECTION IN THE MU CM RELATIVE TO THE CORSIKA
C  COORDINATE SYSTEM
            CALL ADDANG( COSMU,PHIMU, POLART,POLARF, POLART,POLARF )
C  MUON IS WRITTEN TO STACK
            SECPAR( 1) = CURPAR(1) - 3.D0
            SECPAR( 2) = GAMMA3
            SECPAR( 3) = COSMU
            SECPAR( 4) = PHIMU
            SECPAR(11) = POLART
            SECPAR(12) = POLARF
            CALL TSTACK
            SECPAR(11) = 0.D0
            SECPAR(12) = 0.D0
          ENDIF
          IRET1 = 1
          RETURN
        ENDIF
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PION INTERACTS
 
C  CALCULATE GAMMA, BETA AND ENERGY IN CENTER OF MASS
        ECM    = SQRT( C(45) * GAMMA + C(46) )
        GCM    = (PAMA(ITYPE) * GAMMA + PAMA(14)) / ECM
        BETACM = SQRT( 1.D0 - 1.D0 / GCM**2 )
 
C  LOW ENERGY HADRONIC INTERACTIONS
C  USE GHEISHA IF THE CROSS SECTION HAS BEEN CALCULATED FOR GHEISHA
        IF ( GHEISH ) THEN
          IF ( GHESIG ) THEN
            CALL CGHEI
          ELSE
            CALL SDPM
          ENDIF
        ELSE
C  DETERMINE TYPE OF INTERACTION FOR PIONS
          IF     ( ECM .GT. CC(8) ) THEN
C  DUAL PARTON MODEL
            CALL SDPM
          ELSEIF ( ECM .GT. CC(7) ) THEN
C  HEAVY ISOBAR + HEAVY MESON
            CALL BOX69
          ELSEIF ( ECM .GT. CC(6) ) THEN
            CALL RMMAR( RD,1,1 )
            IF ( RD(1) .LE. 0.5 ) THEN
C  HEAVY ISOBAR + PION
              CALL BOX68
            ELSE
C  HEAVY MESON + NUCLEON
              CALL BOX67
            ENDIF
          ELSEIF ( ECM .GT. CC(5) ) THEN
C  LIGHT ISOBAR + PION
            CALL BOX66
          ELSE
C  ELASTIC SCATTERING
            CALL BOX65
          ENDIF
        ENDIF
 
C-----------------------------------------------------------------------
C  NUCLEON OR ANTINUCLEON INCIDENT
      ELSEIF ( ITYPE .EQ. 13  .OR.  ITYPE .EQ. 14  .OR.
     *         ITYPE .EQ. 15  .OR.  ITYPE .EQ. 25 ) THEN
C  CALCULATE GAMMA, BETA AND ENERGY IN CENTER OF MASS
        GCM       = SQRT( GAMMA * 0.5D0 + 0.5D0 )
        ECM       = PAMA(ITYPE) * GCM * 2.D0
        BETACM    = SQRT( 1.D0 - 1.D0 / GCM**2 )
        IF ( DEBUG ) WRITE(MDEBUG,*) 'NUCINT: NUCL EKINL=',SNGL(EKINL)
        INBIN(KJ) = INBIN(KJ) + 1
 
C  LOW ENERGY HADRONIC INTERACTIONS
C  USE GHEISHA IF THE CROSS SECTION HAS BEEN CALCULATED FOR GHEISHA
        IF ( GHEISH ) THEN
          IF ( GHESIG ) THEN
            CALL CGHEI
          ELSE
            CALL SDPM
          ENDIF
        ELSE
C  DETERMINE TYPE OF INTERACTION FOR NUCLEONS AND ANTINUCLEONS
          IF     ( ECM .GT. CC(4) ) THEN
C  DUAL PARTON MODEL
            CALL SDPM
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
 
C-----------------------------------------------------------------------
C  KAON INCIDENT
      ELSEIF ( ITYPE .EQ. 11  .OR.  ITYPE .EQ. 12  .OR.
     *         ITYPE .EQ. 10  .OR.  ITYPE .EQ. 16 ) THEN
        IF ( DEBUG ) WRITE(MDEBUG,*) 'NUCINT: KAON EKINL=',SNGL(EKINL)
        IKBIN(KJ) = IKBIN(KJ) + 1
 
C  DECAY OR INTERACTION FOR KAONS ?
        IF ( FDECAY ) THEN
C  KAON DECAYS. DETERMINE DECAY MODE FOR KAONS AND SET LIFE TIME
          IF     ( ITYPE .EQ. 10 ) THEN
C  K(0,L)-MESON
            IGO = 4
          ELSEIF ( ITYPE .EQ. 11 ) THEN
C  K(+)-MESON
            IGO = 1
          ELSEIF ( ITYPE .EQ. 12 ) THEN
C  K(-)-MESON
            IGO = 2
          ELSE
C  K(0,S)-MESON
            IGO = 3
          ENDIF
          CALL KDECAY( IGO )
          RETURN
 
        ELSE
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  KAON INTERACTS
C  CALCULATE GAMMA, BETA AND ENERGY IN CENTER OF MASS
          ECM    = SQRT( CKA(13) * GAMMA + CKA(14) )
          GCM    = ( PAMA(ITYPE) * GAMMA + PAMA(14) ) / ECM
          BETACM = SQRT( 1.D0 - 1.D0 / GCM**2 )
C  LOW ENERGY HADRONIC INTERACTIONS
C  USE GHEISHA IF THE CROSS SECTION HAS BEEN CALCULATED FOR GHEISHA
          IF ( GHEISH ) THEN
            IF ( GHESIG ) THEN
              CALL CGHEI
            ELSE
              CALL SDPM
            ENDIF
          ELSE
C  KAON INTERACTS. DETERMINE TYPE OF INTERACTION FOR KAONS
            IF     ( ECM .GT. CC(12) ) THEN
C  DUAL PARTON MODEL
              CALL SDPM
C  USE THE INTERACTION ROUTINES OF PKF GRIEDER
            ELSEIF ( ECM .GT. CC(11) ) THEN
C  HEAVY ISOBAR + STRANGE MESON
              CALL BOX74
            ELSEIF ( ECM .GT. CC(10) ) THEN
              CALL RMMAR( RD,1,1 )
              IF ( RD(1) .GT. CKA(21) ) THEN
C  HEAVY ISOBAR + KAON
                CALL BOX73
              ELSE
C  STRANGE MESON + NUCLEON
                CALL BOX72
              ENDIF
            ELSEIF ( ECM .GT. CC(9) ) THEN
C  LIGHT ISOBAR + KAON
              CALL BOX71
            ELSE
C  ELASTIC SCATTERING
              CALL BOX70
            ENDIF
          ENDIF
        ENDIF
 
C-----------------------------------------------------------------------
C  STRANGE BARYON (LAMDA, SIGMA) INCIDENT
      ELSEIF ( (ITYPE .GE. 18  .AND.  ITYPE .LE. 24)  .OR.
     *         (ITYPE .GE. 26  .AND.  ITYPE .LE. 32) ) THEN
        IF ( DEBUG ) WRITE(MDEBUG,*) 'NUCINT: SBAR EKINL=',SNGL(EKINL)
        IHBIN(KJ) = IHBIN(KJ) + 1
C  DECAY OR INTERACTION FOR STRANGE BARYONS?
        IF ( FDECAY ) THEN
          CALL STRDEC
          RETURN
        ENDIF
C  CALCULATE GAMMA, BETA AND ENERGY IN CENTER OF MASS
        ECM    = SQRT( 2.D0 * PAMA(ITYPE) * PAMA(14) * GAMMA
     *              + PAMA(ITYPE)**2 + PAMA(14)**2 )
        GCM    = ( PAMA(ITYPE) * GAMMA + PAMA(14)) / ECM
        BETACM = SQRT( 1.D0 - 1.D0 / GCM**2 )
C  LOW ENERGY HADRONIC INTERACTIONS
C  USE GHEISHA IF THE CROSS SECTION HAS BEEN CALCULATED FOR GHEISHA
        IF ( GHEISH ) THEN
          IF ( GHESIG ) THEN
            CALL CGHEI
          ELSE
C  VENUS TREATS STRANGE BARYONS
            CALL SDPM
          ENDIF
        ELSE
          IF ( ECM .GT. CC(4) ) THEN
            CALL SDPM
          ELSE
C  USE THE INTERACTION ROUTINES OF PKF GRIEDER
C  ELASTIC SCATTERING
            CALL BOX60
          ENDIF
        ENDIF
 
C-----------------------------------------------------------------------
C  HEAVY PRIMARY INCIDENT
      ELSEIF ( ITYPE .GT. 100 ) THEN
        IF (DEBUG) WRITE(MDEBUG,*) 'NUCINT: HEAVY PRIMARY EKINL=',
     *                                               SNGL(EKINL)
C  USE GHEISHA IF THE CROSS SECTION HAS BEEN CALCULATED FOR GHEISHA
C   (THIS MIGHT BE THE CASE FOR DEUTERONS, TRITONS AND ALPHAS)
        IF ( GHEISH ) THEN
          IF ( GHESIG ) THEN
            CALL CGHEI
          ELSE
            CALL SDPM
          ENDIF
        ELSE
C  TREAT HEAVY PRIMARY IN SDPM
          CALL SDPM
        ENDIF
 
C-----------------------------------------------------------------------
C  ILLEGAL PARTICLE
      ELSE
        WRITE(MONIOU,*) 'NUCINT: ILLEGAL PARTICLE = ',ITYPE
        STOP
      ENDIF
 
C-----------------------------------------------------------------------
C  KILL PARTICLE
      IRET1 = 1
 
      RETURN
      END
