      SUBROUTINE NIHILA
 
C-----------------------------------------------------------------------
C  (AN)NIHILA(TION)
C
C  TREATES ANNIHILATION OF ANTINUCLEONS WITH FREE NUCLEONS
C  MOMENTA CONSERVED IN ALL DIRECTIONS
C  ENERGY CONSERVED BY MULTPLICATION OF ALL MOMENTA WITH A CORRECTION
C  FACTOR, CONSERVING MOMENTUM BALANCE
C  THIS SUBROUTINE IS CALLED FROM BOX60, BOX61, BOX62, AND BOX63
C
C  DESIGN  : D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*KEEP,ANNI.
      COMMON /ANNI/    CAN,CANN
      DOUBLE PRECISION CAN(50),CANN(50)
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
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
 
      DOUBLE PRECISION E(10),PHIPAR(10),PL(10),PTR(10),PTSQ(10)
      DOUBLE PRECISION PX(10),PY(10)
      DIMENSION ISEQ(10),NTYP(10)
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,9)
  444 FORMAT(' NIHILA: CURPAR=',1P,9E10.3)
 
      IREP = 0
 
C  RANDOM DECISION FOR ANNIHILATION WITH PROTON OR NEUTRON
 
  20  CONTINUE
      CALL RMMAR( RD,2,1 )
      IREP = IREP + 1
C  AFTER THE 5TH TRY, QUIT THE ANNIHILATION WITHOUT ANY PION GENERATED
      IF ( IREP .GT. 5 ) GOTO 999
      IF ( RD(1) .LE. 0.5 ) THEN
 
C-----------------------------------------------------------------------
C  ANTIPROTON - PROTON AND ANTINEUTRON - NEUTRON ANNIHILATION
        IF     ( RD(2) .LE. CANN(1)  ) THEN
C  ANNIHILATION INTO PI+, PI-
          NPIPOS = 1
          NPINEG = 1
          NPIZ   = 0
        ELSEIF ( RD(2) .LE. CANN(2)  ) THEN
C  ANNIHILATION INTO PI+, PI-, PI0
          NPIPOS = 1
          NPINEG = 1
          NPIZ   = 1
        ELSEIF ( RD(2) .LE. CANN(3)  ) THEN
C  ANNIHILATION INTO PI+, PI-, 2PI0
          NPIPOS = 1
          NPINEG = 1
          NPIZ   = 2
        ELSEIF ( RD(2) .LE. CANN(4)  ) THEN
C  ANNIHILATION INTO PI+, PI-, 3PI0
          NPIPOS = 1
          NPINEG = 1
          NPIZ   = 3
        ELSEIF ( RD(2) .LE. CANN(5)  ) THEN
C  ANNIHILATION INTO PI+, PI-, 4PI0
          NPIPOS = 1
          NPINEG = 1
          NPIZ   = 4
        ELSEIF ( RD(2) .LE. CANN(6)  ) THEN
C  ANNIHILATION INTO 2PI+, 2PI-
          NPIPOS = 2
          NPINEG = 2
          NPIZ   = 0
        ELSEIF ( RD(2) .LE. CANN(7)  ) THEN
C  ANNIHILATION INTO 2PI+, 2PI-, PI0
          NPIPOS = 2
          NPINEG = 2
          NPIZ   = 1
        ELSEIF ( RD(2) .LE. CANN(8)  ) THEN
C  ANNIHILATION INTO 2PI+, 2PI-, 2PI0
          NPIPOS = 2
          NPINEG = 2
          NPIZ   = 2
        ELSEIF ( RD(2) .LE. CANN(9)  ) THEN
C  ANNIHILATION INTO 2PI+, 2PI-, 3PI0
          NPIPOS = 2
          NPINEG = 2
          NPIZ   = 3
        ELSEIF ( RD(2) .LE. CANN(10) ) THEN
C  ANNIHILATION INTO 3PI+, 3PI-
          NPIPOS = 3
          NPINEG = 3
          NPIZ   = 0
        ELSEIF ( RD(2) .LE. CANN(11) ) THEN
C  ANNIHILATION INTO 3PI+, 3PI-, PI0
          NPIPOS = 3
          NPINEG = 3
          NPIZ   = 1
        ELSEIF ( RD(2) .LE. CANN(12) ) THEN
C  ANNIHILATION INTO 3PI+, 3PI-, 2PI0
          NPIPOS = 3
          NPINEG = 3
          NPIZ   = 2
        ELSE
C  ANNIHILATION INTO 4PI0
          NPIPOS = 0
          NPINEG = 0
          NPIZ   = 4
        ENDIF
 
      ELSE
C-----------------------------------------------------------------------
C  ANTIPROTON - NEUTRON (OR ANTINEUTRON - PROTON) ANNIHILATION
        IF     ( RD(2) .LE. CANN(13) ) THEN
C  ANNIHILATION INTO PI-, PI0
          NPIPOS = 0
          NPINEG = 1
          NPIZ   = 1
        ELSEIF ( RD(2) .LE. CANN(14) ) THEN
C  ANNIHILATION INTO PI-, 2PI0
          NPIPOS = 0
          NPINEG = 1
          NPIZ   = 2
        ELSEIF ( RD(2) .LE. CANN(15) ) THEN
C  ANNIHILATION INTO PI-, 3PI0
          NPIPOS = 0
          NPINEG = 1
          NPIZ   = 3
        ELSEIF ( RD(2) .LE. CANN(16) ) THEN
C  ANNIHILATION INTO PI-, 4PI0
          NPIPOS = 0
          NPINEG = 1
          NPIZ   = 4
        ELSEIF ( RD(2) .LE. CANN(17) ) THEN
C  ANNIHILATION INTO PI-, 5PI0
          NPIPOS = 0
          NPINEG = 1
          NPIZ   = 5
        ELSEIF ( RD(2) .LE. CANN(18) ) THEN
C  ANNIHILATION INTO PI+, 2PI-
          NPIPOS = 1
          NPINEG = 2
          NPIZ   = 0
        ELSEIF ( RD(2) .LE. CANN(19) ) THEN
C  ANNIHILATION INTO PI+, 2PI-, PI0
          NPIPOS = 1
          NPINEG = 2
          NPIZ   = 1
        ELSEIF ( RD(2) .LE. CANN(20) ) THEN
C  ANNIHILATION INTO PI+, 2PI-, 2PI0
          NPIPOS = 1
          NPINEG = 2
          NPIZ   = 2
        ELSEIF ( RD(2) .LE. CANN(21) ) THEN
C  ANNIHILATION INTO PI+, 2PI-, 3PI0
          NPIPOS = 1
          NPINEG = 2
          NPIZ   = 3
        ELSEIF ( RD(2) .LE. CANN(22) ) THEN
C  ANNIHILATION INTO PI+, 2PI-, 4PI0
          NPIPOS = 1
          NPINEG = 2
          NPIZ   = 4
        ELSEIF ( RD(2) .LE. CANN(23) ) THEN
C  ANNIHILATION INTO 2PI+, 3PI-
          NPIPOS = 2
          NPINEG = 3
          NPIZ   = 0
        ELSEIF ( RD(2) .LE. CANN(24) ) THEN
C  ANNIHILATION INTO 2PI+, 3PI-, PI0
          NPIPOS = 2
          NPINEG = 3
          NPIZ   = 1
        ELSEIF ( RD(2) .LE. CANN(25) ) THEN
C  ANNIHILATION INTO 2PI+, 3PI-, 2PI0
          NPIPOS = 2
          NPINEG = 3
          NPIZ   = 2
        ELSEIF ( RD(2) .LE. CANN(26) ) THEN
C  ANNIHILATION INTO 2PI+, 3PI-, 3PI0
          NPIPOS = 2
          NPINEG = 3
          NPIZ   = 3
        ELSE
C  ANNIHILATION INTO 3PI+, 4PI-
          NPIPOS = 3
          NPINEG = 4
          NPIZ   = 0
        ENDIF
 
C  CHARGE INVERSION IF ANTINEUTRON ANNIHILATES WITH PROTON
        IF ( ITYPE .EQ. 25 ) THEN
          NPINEG = NPINEG - 1
          NPIPOS = NPIPOS + 1
        ENDIF
 
      ENDIF
 
      NPI  = NPIPOS + NPINEG + NPIZ
      FNPI = 1.D0 / NPI
      GCMI = 1.D0 / GCM
 
C-----------------------------------------------------------------------
C  CHARGE ASSIGNMENT
 
      DO 26  I = 1,NPI
        IF     ( I .LE. NPIZ          ) THEN
C  NEUTRAL PIONS
          NTYP(I) = 7
        ELSEIF ( I .LE. NPIZ+NPIPOS ) THEN
C  POSITIVE PIONS
          NTYP(I) = 8
        ELSE
C  NEGATIVE PIONS
          NTYP(I) = 9
        ENDIF
  26  CONTINUE
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  KINEMATIC CALCULATIONS
 
      ISCALE = 0
  27  CONTINUE
      ISCALE = ISCALE + 1
C  AFTER THE 5TH TRY, TAKE A NEW SET OF PIONS
      IF ( ISCALE .GT. 5 ) GOTO 20
 
C  DISTRIBUTUION OF TRANSVERSE MOMENTA PTR
 
      CORECT = 5.3333333333D0 * FNPI**1.5D0 * ECM
      DO 28  I = 1,NPI
        PTR(I) = PTRANS(DUMMY) * (1.33333333D0 + CORECT)
  28  CONTINUE
 
      SUMPX = 0.D0
      SUMPY = 0.D0
      CALL RMMAR( RD,NPI,1 )
      DO 29  I = 1,NPI
C  SELECT EMISSION ANGLE BY REDUCED RESIDUAL DIRECTION
        IF ( SUMPX .NE. 0.D0  .OR.  SUMPY .NE. 0.D0 ) THEN
          PHISUM  = ATAN2( SUMPY, SUMPX )
        ELSE
          PHISUM  = 0.D0
        ENDIF
        PHIPAR(I) = PHISUM + PI + PI * (NPI+1-I) * (2.*RD(I)-1.) * FNPI
        PX(I)     = COS( PHIPAR(I) ) * PTR(I)
        PY(I)     = SIN( PHIPAR(I) ) * PTR(I)
        SUMPX     = SUMPX + PX(I)
        SUMPY     = SUMPY + PY(I)
  29  CONTINUE
 
C  CORRECTION OF TRANSVERSE MOMENTA TO KEEP TRANSVERSE MOMENTUM BALANCE
      SUMPT2 = ECM
      DPX    = SUMPX * FNPI
      DPY    = SUMPY * FNPI
      DO 30  I = 1,NPI
        PX(I)   = PX(I) - DPX
        PY(I)   = PY(I) - DPY
        IF ( PX(I) .NE. 0.D0  .OR.  PY(I) .NE. 0.D0 ) THEN
          PHIPAR(I) = ATAN2( PY(I), PX(I) )
        ELSE
          PHIPAR(I) = 0.D0
        ENDIF
        PTSQ(I) = PX(I)**2 + PY(I)**2
        SUMPT2  = SUMPT2 - SQRT( PAMA(NTYP(I))**2 + PTSQ(I) )
  30  CONTINUE
 
C  CHECK, IF C.M. ENERGY IS EXHAUSTED BY TRANSVERSE MOMENTA
C  IF SO, TRY ANOTHER SET OF TRANSVERSE MOMENTA
      IF ( SUMPT2 .LE. 0.D0 ) GOTO 27
 
C  DISTRIBUTION OF LONGITUDINAL MOMENTA PL
 
C  SUM1PL IS SUM OF ABS. VALUES OF LONGITUDINAL MOMENTA
      F = SUMPT2 * FNPI
      SUM1PL = 0.D0
      DO 31  I = 1,NPI
        FWHM    = F + 0.5D0 * SQRT( PTSQ(I) )
        PL(I)   = ABS( RANNOR(0.D0,FWHM) )
        SUM1PL  = SUM1PL + PL(I)
C  SET SEQUENCE COUNTER
        ISEQ(I) = I
  31  CONTINUE
 
C  SORT ISEQ IN DECREASING SIZE OF THE LONGITUDINAL MOMENTUM
      DO 33  I = 1,NPI
        DO 32  K = I+1,NPI
          IF ( PL(ISEQ(I)) .LT. PL(ISEQ(K)) ) THEN
            IHELP   = ISEQ(I)
            ISEQ(I) = ISEQ(K)
            ISEQ(K) = IHELP
          ENDIF
  32    CONTINUE
  33  CONTINUE
 
C  TRY TO BALANCE LONGITUDINAL MOMENTA (TO MINIMIZE CORRECTIONS)
C  START WITH LONG. MOMENTA IN FORWARD/BACKWARD DIRECTION BY RANDOM
      CALL RMMAR( RD,1,1 )
      IF ( RD(1) .LE. 0.5 )  PL(ISEQ(1)) = - PL(ISEQ(1))
      SUMPL = PL(ISEQ(1))
      DO 34  I = 2,NPI
        SUM1PL = SUM1PL - PL(ISEQ(I))
C  IF THERE IS NOT ENOUGH MOMENTUM LEFT, SELECT FORWARD/BACKWARD TO
C  BALANCE MOMENTUM, ELSE CHOOSE DIRECTION BY RANDOM
        IF ( PL(ISEQ(I))+ABS(SUMPL) .GT. SUM1PL ) THEN
          IF ( PL(ISEQ(I))*SUMPL .GT. 0.D0 )  PL(ISEQ(I)) = -PL(ISEQ(I))
        ELSE
          CALL RMMAR( RD,1,1 )
          IF ( RD(1) .LE. 0.5 )  PL(ISEQ(I)) = - PL(ISEQ(I))
        ENDIF
        SUMPL = SUMPL + PL(ISEQ(I))
  34  CONTINUE
 
C  CORRECTION OF LONGITUDINAL MOMENTA TO KEEP MOMENTUM BALANCE
      DPL = SUMPL * FNPI
      DO 35  I = 1,NPI
        PL(I) = PL(I) - DPL
  35  CONTINUE
 
C  ITERATIVE CORRECTION OF ALL MOMENTA TO KEEP ENERGY BALANCE
 
      IREPET = 0
  36  CONTINUE
      IREPET = IREPET + 1
      IF ( IREPET .GT. 10 ) GOTO 27
      ETOT   = 0.D0
C  CHECK ENERGY CONSERVATION
      DO 37  I = 1,NPI
        PTSQ(I) = PX(I)**2 + PY(I)**2
        E(I)    = SQRT( PAMA(NTYP(I))**2 + PTSQ(I) + PL(I)**2 )
        ETOT    = ETOT + E(I)
  37  CONTINUE
 
      ECORR =  ECM / ETOT - 1.D0
 
C  LOOK WHETHER ENERGY IS CONSERVED WITHIN 1 %
      IF ( ABS(ECORR) .GT. .01D0 ) THEN
C  FACTOR IS MODIFIED WITH EMPIRICAL TERM 1/GCM FOR FASTER CONVERGENCE
        FACT = (0.5D0+GCMI) * ECORR * 0.02D0 * NPI
        DO 38  I = 1,NPI
          PX(I) = PX(I) * ( FACT         + 1.D0 )
          PY(I) = PY(I) * ( FACT         + 1.D0 )
          PL(I) = PL(I) * ( FACT * 20.D0 + 1.D0 )
  38    CONTINUE
        GOTO 36
      ENDIF
 
C  LORENTZ TRANSFORMATION FROM C.M. TO LAB. FRAME
 
      DO 40  K = 5,8
        SECPAR(K) = CURPAR(K)
  40  CONTINUE
      DO 41  I = 1,NPI
        PLLAB  = GCM * ( PL(I) + BETACM * E(I) )
        IF ( PLLAB .LE. 0.D0 ) GOTO 41
        CTHETA = PLLAB / SQRT( PTSQ(I) + PLLAB**2 )
        IF ( CTHETA .LT. C(27) ) GOTO 41
        CALL ADDANG( COSTHE,PHI, CTHETA,PHIPAR(I), SECPAR(3),SECPAR(4) )
        IF ( SECPAR(3) .LT. C(29) ) GOTO 41
        SECPAR(1) = NTYP(I)
        SECPAR(2) = GCM / PAMA(NTYP(I)) * ( PL(I) * BETACM + E(I) )
        CALL TSTACK
  41  CONTINUE
 
      MSMM  = MSMM + NPI
 999  CONTINUE
 
      RETURN
      END
