      SUBROUTINE SINGLE( E,KIND,AMASS,ASMASS )
 
C-----------------------------------------------------------------------
C  SINGLE (PARTICLE)
C
C  NUCLEON, ANTINUCLEON, PION OR KAON INITIATED
C  HANDLES SINGLE PARTICLE CASE
C  THIS SUBROUTINE IS CALLED FROM MANY BOX ROUTINES
C  ARGUMENTS:
C   E      = AVAILABLE ENERGY IN CM
C   KIND   = 1  BACKWARD PARTICLE
C          = 0  FORWARD  PARTICLE
C   AMASS  = MASS OF SINGLE PARTICLE
C   ASMASS = MASS TO BE LEFT OVER FOR OTHER PARTICLES
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,ELASTY.
      COMMON /ELASTY/  ELAST,IELIS,IELHM,IELNU,IELPI
      DOUBLE PRECISION ELAST
      INTEGER          IELIS(20),IELHM(20),IELNU(20),IELPI(20)
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
 
      IF ( DEBUG ) WRITE(MDEBUG,201)E,KIND,AMASS,ASMASS
 201  FORMAT(' SINGLE: E,KIND,AMASS,ASMASS=',1P,E10.4,I3,2E10.4)
 
      IF ( KIND .NE. 0 ) GOTO 100
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FORWARD PARTICLE
C  NUCLEON, ANTINUCLEON, PION, OR KAON
C  PIONS AND / OR KAONS ARE ALWAYS FORWARD
 
      GFCM   = ( E**2 + AMASS**2 - ASMASS**2 ) * 0.5D0 / (E*AMASS)
      GFCM   = MAX( 1.D0, GFCM )
      BEFCM  = SQRT( GFCM**2 - 1.D0 ) / GFCM
      GFLAB  = GCM * GFCM * ( 1.D0 + BETACM * BEFCM )
      PT     = PTRANS(DUMMY)
      PLLAB2 = MAX( 1.D-6, AMASS**2*(GFLAB**2-1.D0) )
      CTHETA = SQRT( PLLAB2 / (PT**2+PLLAB2) )
      IF ( CTHETA .LT. C(27) ) GOTO 3
      CALL RMMAR( RD,2,1 )
      CALL ADDANG( COSTHE,PHI, CTHETA,RD(1)*PI2, SECPAR(3),SECPAR(4) )
      IF ( SECPAR(3) .LT. C(29) ) GOTO 3
 
C  CHARGE ASSIGNMENT
      IF ( ITYPE .EQ. 10  .OR.  ITYPE .EQ. 11  .OR.
     *     ITYPE .EQ. 12  .OR.  ITYPE .EQ. 16      ) THEN
        SECPAR(1) = CURPAR(1)
      ELSE
        IF ( RD(2) .LT. 0.5 ) THEN
          IADD = 1
        ELSE
          IADD = 0
        ENDIF
        IF     ( ITYPE .EQ. 13  .OR.  ITYPE .EQ. 14 ) THEN
          SECPAR(1) = 13 + IADD
        ELSEIF ( ITYPE .EQ.  8  .OR.  ITYPE .EQ.  9 ) THEN
          SECPAR(1) =  8 + IADD
        ELSEIF ( ITYPE .EQ. 15  .OR.  ITYPE .EQ. 25 ) THEN
          SECPAR(1) = 15 + IADD * 10
        ENDIF
      ENDIF
      SECPAR(2) = GFLAB
      DO  1  J = 5,8
        SECPAR(J) = CURPAR(J)
    1 CONTINUE
      CALL TSTACK
 
    3 CONTINUE
 
C  STATISTICS ON ELASTICITY
      IN = 1.D0 + SECPAR(2) / GAMMA * 20.D0
      IN = MIN( IN, 20 )
      IF ( ITYPE .EQ. 13  .OR.  ITYPE .EQ. 14  .OR.
     *     ITYPE .EQ. 15  .OR.  ITYPE .EQ. 25      ) THEN
        IELNU(IN) = IELNU(IN) + 1
      ELSE
        IELPI(IN) = IELPI(IN)  + 1
      ENDIF
      RETURN
 
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  RECOIL PARTICLE, NUCLEON ONLY
C  FROM SIMPLE RECOIL SPECTRUM COMPUTED
  100 CONTINUE
      HELP   = MIN( C(10), GAMMA*0.5D0 )
      CALL RMMAR( RD,3,1 )
      GRLAB  = RD(1)*(HELP-1.D0)+ 1.D0
      PT     = PTRANS(DUMMY)
      PLLAB2 = MAX( 1.D-6, PAMA(14)**2*(GRLAB**2-1.D0) )
      CTHETA = SQRT( PLLAB2 / (PT**2+PLLAB2) )
      IF ( CTHETA .LT. C(27) ) RETURN
      CALL ADDANG( COSTHE,PHI, CTHETA,RD(2)*PI2, SECPAR(3),SECPAR(4) )
      IF ( SECPAR(3) .LT. C(29) ) RETURN
      SECPAR(2)=GRLAB
 
C  CHARGE ASSIGNEMENT
      IF ( RD(3) .GE. 0.5 ) THEN
        SECPAR(1) = 14.D0
      ELSE
        SECPAR(1) = 13.D0
      ENDIF
 
      DO 103  J = 5,8
        SECPAR(J) = CURPAR(J)
  103 CONTINUE
      CALL TSTACK
 
      RETURN
      END
