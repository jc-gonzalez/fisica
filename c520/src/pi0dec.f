      SUBROUTINE PI0DEC
 
C-----------------------------------------------------------------------
C  PI 0 DEC(AY)
C
C  DECAY OF PI0 INTO 2 GAMMAS OR INTO E(+) + E(-) + GAMMA
C  THIS SUBROUTINE IS CALLED FROM BOX3
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
*KEEP,GENER.
      COMMON /GENER/   GEN,ALEVEL
      DOUBLE PRECISION GEN,ALEVEL
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
*KEND.
 
      DOUBLE PRECISION AUX1,AUX2,COSTH1,COSTH2,EPITO2,FI1
      INTEGER          I
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,9)
  444 FORMAT (' PI0DEC: CURPAR=',1P,9E10.3)
 
C  COPY VERTEX COORDINATES INTO SECPAR
      DO  1  I = 5,8
        SECPAR(I) = CURPAR(I)
  1   CONTINUE
      SECPAR( 9)  = GEN
      SECPAR(10)  = ALEVEL
 
C  LOOK FOR DECAY MODE
      CALL RMMAR (RD,3,1)
 
C  DECAY    PI(0)  ---->  GAMMA + GAMMA
      IF ( RD(3) .LT. 0.98802 ) THEN
C  HALF OF TOTAL ENERGY OF THE PION = EPITO2
        EPITO2 = 0.5D0 * GAMMA * PAMA(7)
        AUX1   = 1.D0 + BETA * RD(1)
        AUX2   = 1.D0 - BETA * RD(1)
        COSTH1 = (BETA + RD(1)) / AUX1
        COSTH2 = (BETA - RD(1)) / AUX2
 
C  FIRST GAMMA  (WITH HIGHER ENERGY)
        FI1 = PI2 * RD(2)
        CALL ADDANG( COSTHE,PHI, COSTH1,FI1, SECPAR(3),SECPAR(4) )
        IF ( SECPAR(3) .GT. C(29) ) THEN
          SECPAR(1) = 1.D0
C  ENERGY OF GAMMA
          SECPAR(2) = AUX1 * EPITO2
          CALL TSTACK
        ENDIF
 
C  SECOND GAMMA   (WITH LOWER ENERGY)
        CALL ADDANG( COSTHE,PHI, COSTH2,FI1+PI, SECPAR(3),SECPAR(4) )
        IF ( SECPAR(3) .GT. C(29) ) THEN
          SECPAR(1) = 1.D0
C  ENERGY OF GAMMA
          SECPAR(2) = AUX2 * EPITO2
          CALL TSTACK
        ENDIF
 
C  DECAY    PI(0)  ---->  E(+) + E(-) + GAMMA      (DALITZ DECAY)
C  (UNIFORM PHASE SPACE DISTRIBUTION IS ASSUMED FOR THIS DECAY)
      ELSE
        CALL DECAY6( PAMA(7), PAMA(3), PAMA(2), 0.D0,
     *                0.D0,0.D0,0.D0, 1.D0, 2)
        DO 11 I = 1,3
          CALL ADDANG( COSTHE,PHI, COS345(I),PHI345(I),
     *                                SECPAR(3),SECPAR(4) )
          IF ( SECPAR(3) .GT. C(29) ) THEN
            SECPAR(1) = FLOAT(4 - I)
            SECPAR(2) = GAM345(I)
            CALL TSTACK
          ENDIF
 11    CONTINUE
 
      ENDIF
 
      RETURN
      END
