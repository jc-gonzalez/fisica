      SUBROUTINE VHMESO( E,AMASS,ASMASS )
 
C-----------------------------------------------------------------------
C  (STRANGE) H(EAVY) MESO(N)
C
C  HANDLES KAON INITIATED HEAVY MESON AND ITS DECAY INTO 1 KAON AND 2 PI
C  STRANGE HEAVY MESON EMITTED FORWARD
C  THIS SUBROUTINE IS CALLED FROM BOX72 AND BOX74
C  ARGUMENTS:
C   E      = AVAILABLE ENERGY IN CM
C   AMASS  = MASS OF STRANGE MESON
C   ASMASS = MASS TO BE LEFT OVER FOR OTHER PARTICLES
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*KEEP,BAL.
      COMMON /BAL/     EBAL
      DOUBLE PRECISION EBAL(10)
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
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
 
      IF ( DEBUG ) WRITE(MDEBUG,201) E,AMASS,ASMASS
 201  FORMAT(' VHMESO: E,AMASS,ASMASS=',1P,3E10.4)
 
      EDVH   = 0.D0
      W      = 0.6D0
 
C  GAMMA AND BETA OF HEAVY MESON IN CM AND LAB
C  E > AMASS + ASMASS  TO KEEP GHMCM > 1.
      GVHCM  = (E**2 + AMASS**2 - ASMASS**2) / (2.D0 * E * AMASS)
      BVHCM  = SQRT(GVHCM**2 - 1.D0) / GVHCM
      GVHLAB = GCM * GVHCM * (1.D0+BETACM*BVHCM)
      BVHLAB = SQRT(GVHLAB**2 - 1.D0) / GVHLAB
 
C  DECAY OF HEAVY MESON
 
C  KAON PART
C  CHOSE LONGITUDINAL MOMENTUM RANDOMLY FROM EXPONENTIAL DISTRIBUTION
      P      = PCL(CKA(2),W)
      PT     = PTRANS(DUMMY)
C  CALCULATE REST OF ENERGY FOR OTHER PARTICLES
      EDVH   = SQRT( PAMA(ITYPE)**2 + P**2 + PT**2 )
      RESTE  = AMASS - EDVH
C  GAMMA AND BETA OF KAON
      GKAVH  = SQRT( P**2 / PAMA(ITYPE)**2 + 1.D0 )
      BKAVH  = SQRT(GKAVH**2 - 1.D0) / GKAVH
C  KAON FORWARD OR BACKWARD ?
      CALL RMMAR( RD,1,1 )
      IF ( RD(1) .LT. 0.5 ) THEN
        GKALAB = GVHLAB * GKAVH * (1.D0+BKAVH*BVHLAB)
        PACC   = P
      ELSE
        GKALAB = GVHLAB * GKAVH * (1.D0-BKAVH*BVHLAB)
        PACC   = -P
      ENDIF
C  GET NEW DIRECTION
      PLLAB2 = PAMA(ITYPE)**2 * (GKALAB**2 - 1.D0)
      PLLAB2 = MAX( 1.D-6, PLLAB2 )
      CTHETA = SQRT( PLLAB2 / (PT**2+PLLAB2) )
      IF ( CTHETA .GE. C(27) ) THEN
        CALL RMMAR( RD,1,1 )
        CALL ADDANG( COSTHE,PHI, CTHETA,RD(1)*PI2, SECPAR(3),SECPAR(4) )
        IF ( SECPAR(3) .GE. C(29) ) THEN
          SECPAR(2) = GKALAB
C  CHARGE ASSIGNMENT
          SECPAR(1) = CURPAR(1)
          DO  8  J = 5,8
            SECPAR(J) = CURPAR(J)
    8     CONTINUE
          CALL TSTACK
        ENDIF
      ENDIF
 
C  PION PART
 
C  WHAT DECAY MODE ?
C    NDEC=1 : K,PI0,PI0
C    NDEC=2 : K,PI+,PI-
C    NDEC=3 : K,PI-,PI+
      CALL RMMAR( RD,2,1 )
      IF ( RD(1) .LT. 0.5 ) THEN
        NDEC = 1
      ELSE
        IF ( RD(2) .LT. 0.5 ) THEN
          NDEC = 2
        ELSE
          NDEC = 3
        ENDIF
      ENDIF
 
      IPI = 0
    2 CONTINUE
      IPI = IPI + 1
 
C  TRANSVERS MOMENTUM
      PT = PTRANS(DUMMY)
      IF ( IPI .EQ. 1 ) THEN
C  LONGITUDINAL MOMENTUM IS SELECTED FROM EXPONENTIAL DISTRIBUTION
        P = PCL(C(42),W)
C  CHARGE ASSIGNMENT
        IF     ( NDEC .EQ. 1 ) THEN
          SECPAR(1) = 7.D0
          PMA = PAMA(7)
        ELSEIF ( NDEC .EQ. 2 ) THEN
          PMA = PAMA(8)
          SECPAR(1) = 8.D0
        ELSE
          PMA = PAMA(9)
          SECPAR(1) = 9.D0
        ENDIF
      ELSE
C  LONGITUDINAL MOMENTUM AS ENERGY IS LEFT
        P2 = RESTE**2 - PMA**2 - PT**2
        P  = SQRT(MAX( P2, 0.D0 ))
        IF     ( NDEC .EQ. 1 ) THEN
          SECPAR(1) = 7.D0
        ELSEIF ( NDEC .EQ. 2 ) THEN
          SECPAR(1) = 9.D0
        ELSE
          SECPAR(1) = 8.D0
        ENDIF
      ENDIF
C  REST OF ENERGY FOR OTHER PARTICLES
      EDVH   = EDVH + SQRT( PMA**2 + P**2 + PT**2 )
      RESTE  = AMASS - EDVH
 
C  GAMMA AND BETA OF PION
      GPIVH  = SQRT(P**2/PMA**2 + 1.D0)
      BEPIVH = SQRT(GPIVH**2 - 1.D0) / GPIVH
C  FOR FIRST PION CHOSE RANDOMLY WHETHER FORWARD OR BACKWARD
C  FOR SECOND PION DECIDE ACCORDING TO ACCUMULATED P
      CALL RMMAR( RD,2,1 )
      IF ( IPI .EQ. 2 ) THEN
        IF ( PACC .LT. 0.D0 ) THEN
          RD(1) = 0.
        ELSE
          RD(1) = 1.
        ENDIF
      ENDIF
      IF ( RD(1) .LT. 0.5 ) THEN
C  BACKWARD PION
        GPILAB = GVHLAB * GPIVH * (1.D0-BEPIVH*BVHLAB)
        PACC   = PACC - P
      ELSE
C  FORWARD PION
        GPILAB = GVHLAB * GPIVH * (1.D0+BEPIVH*BVHLAB)
        PACC   = PACC + P
      ENDIF
 
      PLLAB2 = PMA**2 * (GPILAB**2 - 1.D0)
      PLLAB2 = MAX( 1.D-6, PLLAB2 )
      CTHETA = SQRT( PLLAB2 / (PT**2+PLLAB2) )
      IF ( CTHETA .GE. C(27) ) THEN
        CALL ADDANG( COSTHE,PHI, CTHETA,RD(2)*PI2, SECPAR(3),SECPAR(4) )
        IF ( SECPAR(3) .GE. C(29) ) THEN
          SECPAR(2) = GPILAB
          DO 11  J = 5,8
            SECPAR(J) = CURPAR(J)
   11     CONTINUE
          CALL TSTACK
        ENDIF
      ENDIF
 
      IF ( IPI .LT. 2  .AND.  RESTE .GT. PMA ) GOTO 2
 
      EBAL(5) = EBAL(5) + RESTE
      MSMM    = MSMM + IPI
 
      RETURN
      END
 
