      SUBROUTINE RESDEC
 
C-----------------------------------------------------------------------
C  RES(ONANCE) DEC(AY)
C
C  ROUTINE TREATES DECAY OF THE RESONANCES RHO, K*, AND DELTA
C  THE DECAY MODE IS SELECTED BY THE RANDOM NUMBER RESRAN, WHICH IS
C  SET IN THE ROUTINE HDPM/LEPACX, WHERE THE RESONANCE IS FORMED
C  DECAY WITH FULL KINEMATIC, ENERGY AND MOMENTA CONSERVED
C  THIS SUBROUTINE IS CALLED FROM BOX3
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,GENER.
      COMMON /GENER/   GEN,ALEVEL
      DOUBLE PRECISION GEN,ALEVEL
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
*KEEP,RESON.
      COMMON /RESON/   RDRES,RESRAN,IRESPAR
      REAL             RDRES(2),RESRAN(1000)
      INTEGER          IRESPAR
 
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
 
      INTEGER          I,KK,M3,M4
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,8)
  444 FORMAT(' RESDEC: CURPAR=',1P,8E10.3)
 
C  CURRENT PARTICLE MUST BE SAVED IN STORE
      DO 141  KK = 5,8
        SECPAR(KK) = CURPAR(KK)
 141  CONTINUE
      SECPAR( 9)  = GEN
      SECPAR(10)  = ALEVEL
 
      BETA   = SQRT( GAMMA**2 - 1.D0 ) / GAMMA
 
      IF ( IRESPAR .LE. 0 ) THEN
        WRITE(MONIOU,*)'RESDEC: IRESPAR .LE. 0, CORRECTED'
        IRESPAR = 1
      ENDIF
C-----------------------------------------------------------------------
C  TREAT FIRST NUCLEON RESONANCES, AS MOST FREQUENT
 
      IF     ( ITYPE .EQ. 54 ) THEN
C  DECAY    DELTA(++)  ---->  P + PI(+)
        CALL DECAY1( ITYPE, 14, 8 )
 
      ELSEIF ( ITYPE .EQ. 55  .OR.  ITYPE .EQ. 56 ) THEN
C  DECAY    DELTA(+)   OR       DECAY    DELTA(0)
 
        IF ( RESRAN(IRESPAR) .LE. TB3 ) THEN
C  DECAY    DELTA(+)  ---->  P + PI(0)
C  DECAY    DELTA(0)  ---->  N + PI(0)
          M3 = 69 - ITYPE
          CALL DECAY1( ITYPE, M3, 7 )
 
        ELSE
C  DECAY    DELTA(+)  ---->  N + PI(+)
C  DECAY    DELTA(0)  ---->  P + PI(-)
          M3 = ITYPE - 42
          M4 = M3 - 5
          CALL DECAY1( ITYPE, M3, M4 )
        ENDIF
 
      ELSEIF ( ITYPE .EQ. 57 ) THEN
C  DECAY    DELTA(-)  ---->  N + PI(-)
        CALL DECAY1( ITYPE, 13, 9 )
 
C-----------------------------------------------------------------------
C  RHO RESONANCES
 
      ELSEIF ( ITYPE .EQ. 51 ) THEN
C  DECAY    RHO(0)  ---->  PI(+) + PI(-)
        CALL DECAY1( ITYPE, 8, 9 )
 
      ELSEIF ( ITYPE .EQ. 52  .OR.  ITYPE .EQ. 53 ) THEN
C  DECAY    RHO(+,-)  ---->  PI(+,-) + PI(0)
        M3 = ITYPE - 44
        CALL DECAY1( ITYPE, M3, 7 )
 
C-----------------------------------------------------------------------
C  EXCITED KAON RESONANCES
 
      ELSEIF ( ITYPE .EQ. 62 ) THEN
C  DECAY    K*(0)          ---->  2/3: K(+)    + PI(-)
C                          ---->  1/3: K0(L,S) + PI(0)
        IF     ( RESRAN(IRESPAR) .LE.      TB3 ) THEN
          CALL DECAY1( ITYPE, 11, 9 )
        ELSEIF ( RESRAN(IRESPAR) .LE. .8333333 ) THEN
          CALL DECAY1( ITYPE, 10, 7 )
        ELSE
          CALL DECAY1( ITYPE, 16, 7 )
        ENDIF
 
      ELSEIF ( ITYPE .EQ. 65 ) THEN
C  DECAY    ANTI-K*(0)     ---->  2/3: K(-)    + PI(+)
C                          ---->  1/3: K0(L,S) + PI(0)
        IF     ( RESRAN(IRESPAR) .LE.      TB3 ) THEN
          CALL DECAY1( ITYPE, 12, 8 )
        ELSEIF ( RESRAN(IRESPAR) .LE. .8333333 ) THEN
          CALL DECAY1( ITYPE, 10, 7 )
        ELSE
          CALL DECAY1( ITYPE, 16, 7 )
        ENDIF
 
      ELSEIF ( ITYPE .EQ. 63  .OR.  ITYPE .EQ. 64 ) THEN
C  DECAY    K*(+-)          ---->  2/3: K(+-) + PI(0)
C                           ---->  1/3: K0(L,S) + PI(+-)
        IF     ( RESRAN(IRESPAR) .LE.      TB3 ) THEN
          CALL DECAY1( ITYPE, ITYPE-52, 7 )
        ELSEIF ( RESRAN(IRESPAR) .LE. .8333333 ) THEN
          CALL DECAY1( ITYPE, 10, ITYPE-55 )
        ELSE
          CALL DECAY1( ITYPE, 16, ITYPE-55 )
        ENDIF
 
C-----------------------------------------------------------------------
C  ANTI-NUCLEON RESONANCES
 
      ELSEIF ( ITYPE .EQ. 58 ) THEN
C  DECAY    ANTI-DELTA(--)  ---->  ANTI-P + PI(-)
        CALL DECAY1( ITYPE, 15, 9 )
 
      ELSEIF ( ITYPE .EQ. 59  .OR.  ITYPE .EQ. 60 ) THEN
C  DECAY    ANTI-DELTA(-)   OR       DECAY    ANTI-DELTA(0)
 
        IF ( RESRAN(IRESPAR) .LE. TB3 ) THEN
C  DECAY    ANTI-DELTA(-)  ---->  ANTI-P + PI(0)
C  DECAY    ANTI-DELTA(0)  ---->  ANTI-N + PI(0)
          M3 = 15 + (ITYPE - 59) * 10
          CALL DECAY1( ITYPE, M3, 7 )
 
        ELSE
C  DECAY    ANTI-DELTA(-)  ---->  ANTI-N + PI(-)
C  DECAY    ANTI-DELTA(0)  ---->  ANTI-P + PI(+)
          M3 = 15 + (60 - ITYPE) * 10
          M4 = 68 - ITYPE
          CALL DECAY1( ITYPE, M3, M4 )
        ENDIF
 
      ELSEIF ( ITYPE .EQ. 61 ) THEN
C  DECAY    ANTI-DELTA(+)  ---->  ANTI-N + PI(+)
        CALL DECAY1( ITYPE, 25, 8 )
 
C-----------------------------------------------------------------------
      ELSE
        WRITE(MONIOU,*) 'RESDEC: UNFORESEEN PARTICLE CODE =',ITYPE
      ENDIF
      IRESPAR = IRESPAR -1
 
      RETURN
      END
