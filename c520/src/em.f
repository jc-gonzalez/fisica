      SUBROUTINE EM
 
C-----------------------------------------------------------------------
C  E(LECTRO) M(AGNETIC PARTICLES)
C
C  ROUTINE FOR TREATING EM PARTICLES
C  THIS SUBROUTINE IS CALLED FROM BOX3
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
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
 
      DOUBLE PRECISION ENER
      INTEGER          I
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,9)
  444 FORMAT(' EM    : CURPAR=',1P,9E10.3)
 
C  GET CORRECT PARTICLE ENERGY
      IF     ( ITYPE  .EQ. 1 ) THEN
        ENER = CURPAR(2)
      ELSEIF ( ITYPE .EQ. 2  .OR.  ITYPE .EQ. 3 ) THEN
C  Line commented due to a bug. Aitor 14-Feb-2000
c        ENER  = SECPAR(2) * PAMA(2)
        ENER  = CURPAR(2) * PAMA(2)
      ELSE
        WRITE(MONIOU,*) 'EM    : WRONG PARTICLE CODE =',ITYPE
        RETURN
      ENDIF
 
C  LOOK FOR ENERGY OF EM PARTICLE
*     IF ( ENER .LE. 1.D7 ) THEN
C  EM-PARTICLE ENERGY IS BELOW LPM EFFECT, STORE IT TO SECPAR
C  LPM LIMIT IS SET AT 1.*10**16 EV = 1.*10**7 GEV
        DO 101  I = 1,8
          SECPAR(I) = CURPAR(I)
  101   CONTINUE
        SECPAR( 9)  = GEN
        SECPAR(10)  = ALEVEL
 
C  CALL NKG IF SELECTED
        IF ( FNKG ) THEN
          CALL NKG( ENER )
        ENDIF
 
C  CALL EGS4 IF SELECTED ( PARTICLE IS TAKEN IN EGS FROM COMMON )
        IF ( FEGS ) THEN
          CALL EGS4( ENER )
        ENDIF
 
*     ELSE
C  EM-PARTICLE ENERGY IS ABOVE LPM EFFECT AND MUST BE TREATED BY LPM,
C  IF EM_PARTICLES ARE REQUESTED BY EGS OR NKG
*       IF ( FNKG .OR. FEGS ) CALL LPM(ENER)
*     ENDIF
 
      RETURN
      END
