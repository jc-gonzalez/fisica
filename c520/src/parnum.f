      SUBROUTINE PARNUM( INUMFL )
 
C-----------------------------------------------------------------------
C  PART(ICLE TYPE) NUM(BERS)
C
C  DETERMINES THE NUMBERS OF SECONDARY PARTICLES FOR EACH TYPE
C  THIS SUBROUTINE IS CALLED FROM HDPM
C  ARGUMENT
C   INUMFL = 0  CORRECT DETERMINATION OF PARTICLE NUMBERS
C          = 1  SOMETHING WENT WRONG WITH NEUTRAL PARTICLE NUMBERS
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,EDECAY.
      COMMON /EDECAY/  CETA
      DOUBLE PRECISION CETA(5)
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
*KEEP,RANDPA.
      COMMON /RANDPA/  FAC,U1,U2,RD,NSEQ,ISEED,KNOR
      DOUBLE PRECISION FAC,U1,U2
      REAL             RD(3000)
      INTEGER          ISEED(103,10),NSEQ
      LOGICAL          KNOR
*KEEP,RATIOS.
      COMMON /RATIOS/  RPI0R,RPIER,RPEKR,RPEKNR,PPICH,PPINCH,PPNKCH,
     *                 ISEL,NEUTOT,NTOTEM
      DOUBLE PRECISION RPI0R,RPIER,RPEKR,RPEKNR,PPICH,PPINCH,PPNKCH
      INTEGER          ISEL,NEUTOT,NTOTEM
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
 
      REAL RDETA
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'PARNUM: NCH,NEUTOT,NTOTEM=',
     *                                      NCH,NEUTOT,NTOTEM
 
      INUMFL = 0
C  RESET PARTICLE NUMBERS
      NNC = 0
      NKC = 0
      NHC = 0
      NPC = 0
C  ISEL IS 1 MEANS VERY LOW MULTIPLICITY
C  CREATE ONLY PIONS (TO RISKY TO CREATE OTHER PARTICLES)
      IF ( ISEL .EQ. 1 ) THEN
        NNN = 0
        NKN = 0
        NET = 0
        NHN = 0
        NPN = 0
        NETAS(2) = 0
        NETAS(3) = 0
C  CREATE RANDOM NUMBERS
        CALL RMMAR( RD,NTOTEM,1 )
        DO 1000  I = 1,NTOTEM
          IF ( RD(I) .LE. TB3 ) THEN
            NPC = NPC + 1
          ELSE
            NPN = NPN + 1
          ENDIF
 1000   CONTINUE
C  NO NEUTRAL PARTICLES FOR THE 3RD STRING EXCEPT EVENTUALLY PI(0)
        NNUCN(3)  = 0
        NKA0(3)   = 0
        NHYPN(3)  = 0
        NETAS(3)  = 0
        NPIZER(3) = MAX( 0, NINT(RC3TO2/(1.D0+RC3TO2)*DBLE(NPN)) )
        IF ( DEBUG ) WRITE(MDEBUG,*) '   ISEL=1, NTOTEM=',NTOTEM
 
      ELSE
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  NOW THE CASE OF HAVING ENOUGH PARTICLES TO BE ABLE TO CREATE
C  KAONS, NUCLEONS, AND HYPERONS TOO.
 
C  ...FOR NEUTRALS
        NCOUNT = 0
C  BEGIN OF REJECT LOOP
 1002   K = 1
        CALL RMMAR( RD,NEUTOT+3,1 )
C  DETERMINE NUMBER OF PI(0), ETA, K0S/K0 PAIRS, NEUTRON/ANTINEUTRON
C  PAIRS, AND NEUTRAL HYPERON PAIRS  AND SUM UP THE GAMMAS
C  FOR 1ST + 2ND STRING: J IS 2;  FOR 3RD STRING: J IS 3
        SGAMMA = 0.D0
        DO 1010  J = 2,3
          NNUCN(J)  = 0
          NKA0(J)   = 0
          NHYPN(J)  = 0
          NETA(J,1) = 0
          NETA(J,2) = 0
          NETA(J,3) = 0
          NETA(J,4) = 0
          NPIZER(J) = 0
          IF ( J .EQ. 2 ) THEN
C  SET BOUNDARY FOR GAMMA SUM
            GABOU = SEUGF
            NNTOT = INT(FNEUT2)
C  CALCULATE BOUNDARY NNTOT OF PARTICLE LOOP RATHER AT RANDOM THAN BY
C  ROUNDING OF FNEUT2 TO AVOID DIGITIZING EFFECTS ON THE NEUTRAL
C  PARTICLE COMPOSITION AT COLLISIONS WITH LOW MULTIPLICITY
            IF ( NNTOT+RD(NEUTOT+2) .GE. FNEUT2 ) NNTOT = NNTOT+1
          ELSE
            IF ( RC3TO2 .LE. 0.D0 ) GOTO 1010
            GABOU = GABOU + SEUGF* RC3TO2
            NNTOT = INT(FNEUT)
            IF ( NNTOT+RD(NEUTOT+3) .GE. FNEUT ) NNTOT = NNTOT+1
          ENDIF
          IF ( DEBUG ) WRITE(MDEBUG,*) '   J,NNTOT=',J,NNTOT
C  START NEUTRAL PARTICLE PRODUCTION LOOP
 1003     CONTINUE
          IF     ( K .LT. NNTOT ) THEN
            RNDM = RD(K)
          ELSEIF ( K .EQ. NNTOT ) THEN
C  RENORMALIZE THE RANDOM NUMBER, THAT ONLY PI(0) OR ETA IS PRODUCED
C  BUT PAIR PRODUCTION BECOMES IMPOSSIBLE
            RNDM = RD(K) * RPIER
          ELSEIF ( K .GT. NNTOT ) THEN
            GOTO 1010
          ENDIF
          IF     ( RNDM .LE. RPI0R ) THEN
C  PI(0)
            SGAMMA    = SGAMMA + 2.D0
            NPIZER(J) = NPIZER(J) + 1
            K = K + 1
 
          ELSEIF ( RNDM .LE. RPIER ) THEN
C  ETA
            CALL RMMAR( RDETA,1,1 )
            IF     ( RDETA .LE. CETA(1) ) THEN
              SGAMMA    = SGAMMA + 2.D0
              NETA(J,1) = NETA(J,1) + 1
            ELSEIF ( RDETA .LE. CETA(2) ) THEN
              SGAMMA    = SGAMMA + 6.D0
              NETA(J,2) = NETA(J,2) + 1
            ELSEIF ( RDETA .LE. CETA(3) ) THEN
              SGAMMA    = SGAMMA + 2.D0
              NETA(J,3) = NETA(J,3) + 1
            ELSE
              SGAMMA    = SGAMMA + 1.D0
              NETA(J,4) = NETA(J,4) + 1
            ENDIF
            K = K + 1
 
          ELSEIF ( RNDM .LE. RPEKR ) THEN
C  K0S/K0L PAIR; RPEKR IS NORMALIZED FOR K0 PAIR FORMATION
C  THE UA5 GAMMA YIELD DOES NOT INCLUDE GAMMAS FROM K DECAY !!!
C  SEE: ANSORGE ET AL., Z. PHYS. C43 (1989) 75
            NKA0(J) = NKA0(J) + 1
            K = K + 2
          ELSEIF ( RNDM .LE. RPEKNR ) THEN
C  NEUTRON-ANTINEUTRON PAIR
            NNUCN(J) = NNUCN(J) + 1
            K = K + 2
          ELSE
C  HYPERON-ANTIHYPERON PAIR
C  AVERAGE NEUTRAL HYPERON PAIR L0 --> .357*2 GAMMAS = 0.714 GAMMAS
C                               S0 --> L0 + 1 GAMMA  = 1.714 GAMMAS
C  THEY ARE INCLUDED IN UA5 GAMMA MULTIPLICITIES, THEREFORE COUNT
            SGAMMA   = SGAMMA + 2.428D0
            NHYPN(J) = NHYPN(J) + 1
            K = K + 2
          ENDIF
          GOTO 1003
 1010   CONTINUE
        IF ( DEBUG ) WRITE(MDEBUG,1020) ( 2*NNUCN(J),2*NKA0(J),
     *            2*NHYPN(J),NETA(J,1),NETA(J,2),NETA(J,3),NETA(J,4),
     *            NPIZER(J),J=2,3 ), NNTOT,GABOU,SGAMMA,SGAMMA/GABOU
 1020   FORMAT(' PARNUM: NEUTRALS (1.,2.STRING)=',8I5,/
     *         '         NEUTRALS (3. STRING)  =',8I5,/
     *         '  NNTOT,SEUGF2+3,SGAMMA,RATIO=',I6,3(2X,F10.5))
C  REJECT ALL NEUTRALS, IF SUM OF GAMMAS DEVIATES BY MORE THAN SIGMA
        IF ( (SGAMMA - GABOU)**2 .GT. GABOU ) THEN
          NCOUNT = NCOUNT + 1
C  AFTER 20 TRIES SET FLAG INUMFL TO 1 AND RETURN
          IF ( NCOUNT .LE. 20 ) GOTO 1002
          INUMFL = 1
          RETURN
        ENDIF
C  ALL NEUTRALS
        NNN = NNUCN(2)  + NNUCN(3)
        NKN = NKA0(2)   + NKA0(3)
        NHN = NHYPN(2)  + NHYPN(3)
        NETAS(2) = NETA(2,1) + NETA(2,2) + NETA(2,3) + NETA(2,4)
        NETAS(3) = NETA(3,1) + NETA(3,2) + NETA(3,3) + NETA(3,4)
        NET = NETAS(2)  + NETAS(3)
        NPN = NPIZER(2) + NPIZER(3)
 
C  ...FOR CHARGED
        I = 1
        CALL RMMAR( RD,NCH-1,1 )
C  START CHARGED PARTICLE PRODUCTION LOOP
 1101   CONTINUE
        RNDM = RD(I)
        IF     ( RNDM .LT. PPICH  ) THEN
C  PI(+-)
          NPC = NPC + 1
          I   = I + 1
        ELSEIF ( RNDM .LT. PPINCH ) THEN
C  PROTON/ANTIPROTON PAIR
          NNC = NNC + 1
          I   = I + 2
        ELSEIF ( RNDM .LT. PPNKCH ) THEN
C  KAON(+,-) PAIR
          NKC = NKC + 1
          I   = I + 2
        ELSE
C  CHARGED HYPERON/ANTIHYPERON PAIR
          NHC = NHC + 1
          I   = I + 2
        ENDIF
        IF     ( I .LT. NCH ) THEN
          GOTO 1101
        ELSEIF ( I .EQ. NCH ) THEN
C  ONLY 1 CHARGED PARTICLE TO BE PRODUCED WHICH IS PI(+-)
          NPC = NPC + 1
        ENDIF
C  CORRECT CHARGED PION NUMBER FOR DECAY OF ETA'S
        NCORR = 2 * ( NETA(2,3) + NETA(2,4) + NETA(3,3) + NETA(3,4) )
        NPC   = MAX( 0, NPC - NCORR )
        IF ( DEBUG ) WRITE(MDEBUG,*) '   NPC,NPN,NCORR,LASTPI=',
     *                                   NPC,NPN,NCORR,LASTPI
      ENDIF
C  CORRECT NUMBER OF CHARGED AND NEUTRAL PIONS FOR RESONANCE DECAY
C  (NRESPC, NRESPN)
      NPC = MAX( 0, NPC - NRESPC + LASTPI )
C  INCREASE NPN ADDITIONALLY BY 1 TO MEET UA5 DATA, WHICH REPRODUCE ON
C  AVERAGE ONE EXCHANGED CHARGE (LASTPI = +1).
      NPN = MAX( 0, NPN - NRESPN - LASTPI + 1 )
C  TOTAL NUMBER OF CHARGED PARTICLES
      NCH = (NNC + NKC + NHC) * 2 + NPC
C  NOW ALL PARTICLES ARE DETERMINED
      IF ( DEBUG ) WRITE(MDEBUG,*)
     *             'PARNUM: TOT.CHARGED=',2*NNC,2*NKC,2*NHC,NPC,
     *             'PARNUM: TOT.NEUTRAL=',2*NNN,2*NKN,2*NHN,NET,NPN
 
      RETURN
      END
