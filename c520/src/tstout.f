      SUBROUTINE TSTOUT
 
C-----------------------------------------------------------------------
C  T(O) ST(ACK) OUT
C
C  MAKE REAL OUTPUT AFTER ONE INTERACTION HAS FINISHED
C  ADDS PARTICLE TO STACK AND WRITES IT TO DISK IF NECESSARY
C  THIS SUBROUTINE IS CALLED FORM MPPROP, PIGEN1, PIGEN2, AND TSTEND
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,BUFFS.
      COMMON /BUFFS/   RUNH,RUNE,EVTH,EVTE,DATAB,LH
      INTEGER          MAXBUF,MAXLEN
      PARAMETER        (MAXBUF=39*7)
      PARAMETER        (MAXLEN=12)
      REAL             RUNH(MAXBUF),EVTH(MAXBUF),EVTE(MAXBUF),
     *                 RUNE(MAXBUF),DATAB(MAXBUF)
      INTEGER          LH
      CHARACTER*4      CRUNH,CRUNE,CEVTH,CEVTE
      EQUIVALENCE      (RUNH(1),CRUNH), (RUNE(1),CRUNE)
      EQUIVALENCE      (EVTH(1),CEVTH), (EVTE(1),CEVTE)
*KEEP,ELABCT.
      COMMON /ELABCT/  ELCUT
      DOUBLE PRECISION ELCUT(4)
*KEEP,ETHMAP.
      COMMON /ETHMAP/  ECTMAP,ELEFT
      DOUBLE PRECISION ECTMAP,ELEFT
*KEEP,MUPART.
      COMMON /MUPART/  AMUPAR,BCUT,CMUON,FMUBRM,FMUORG
      DOUBLE PRECISION AMUPAR(14),BCUT,CMUON(11)
      LOGICAL          FMUBRM,FMUORG
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
*KEEP,STACKF.
      COMMON /STACKF/  STACK,STACKP,EXST,NSHIFT,NOUREC,ICOUNT,NTO,NFROM
      INTEGER          MAXSTK
      PARAMETER        (MAXSTK = 12*340*2)
      DOUBLE PRECISION STACK(MAXSTK)
      INTEGER          STACKP,EXST,NSHIFT,NOUREC,ICOUNT,NTO,NFROM
*KEND.
 
      DOUBLE PRECISION GLCUT
      INTEGER          I,ISTK,J
      DATA             ISTK / MAXSTK /
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,666) ICOUNT,(SECPAR(J),J=1,9)
 666  FORMAT(' TSTOUT:',I7,1X,1P,9E10.3)
 
C  CALCULATE APPROPRIATE KINETIC ENERGY CUT AND APPLY IT
      IF     ( SECPAR(1) .EQ. 5.D0 .OR. SECPAR(1) .EQ. 6.D0 ) THEN
C  MUONS
        GLCUT = ELCUT(2) / PAMA(NINT(SECPAR(1))) + 1.D0
      ELSEIF ( SECPAR(1) .EQ. 2.D0 .OR. SECPAR(1) .EQ. 3.D0 ) THEN
C  ELECTRONS
        GLCUT = ELCUT(3) / PAMA(NINT(SECPAR(1))) + 1.D0
      ELSEIF ( SECPAR(1) .EQ. 1.D0 ) THEN
C  GAMMAS
        GLCUT = ELCUT(4)
      ELSEIF (SECPAR(1) .GE. 100.D0 ) THEN
C  NUCLEI, CUTTED IF ENERGY/NUCLEON BELOW CUT
        GLCUT = ELCUT(1) * INT(SECPAR(1)/100 )
     *                   / PAMA(NINT(SECPAR(1))) + 1.D0
      ELSE
C  HADRONS
        GLCUT = ELCUT(1) / PAMA(NINT(SECPAR(1))) + 1.D0
      ENDIF
      IF ( SECPAR(2) .LT. GLCUT ) THEN
        IF ( SECPAR(1).EQ.5.D0 .OR. SECPAR(1).EQ.6.D0 ) FMUORG = .FALSE.
        IF (DEBUG) WRITE(MDEBUG,*) 'TSTOUT: PARTICLE BELOW ',
     *     'ENERGY CUT'
        RETURN
      ENDIF
 
      IF ( STACKP .GE. ISTK ) THEN
        WRITE(EXST,REC=NOUREC+1) (STACK(I),I=       1,ISTK/2)
        WRITE(EXST,REC=NOUREC+2) (STACK(I),I=ISTK/2+1,ISTK )
        NOUREC = NOUREC + 2
        NSHIFT = NSHIFT + 2
        STACKP = 0
      ENDIF
 
      NTO    = NTO + 1
      ICOUNT = ICOUNT + 1
 
      DO  2  J = 1,MAXLEN
        STACK(STACKP+J) = SECPAR(J)
 2    CONTINUE
      STACKP = STACKP + MAXLEN
      IF ( SECPAR(1) .LE.  1.D0 ) THEN
        ELEFT  = ELEFT + SECPAR(2)
      ELSE
        ELEFT  = ELEFT + SECPAR(2) * PAMA(NINT(SECPAR(1)))
      ENDIF
 
      RETURN
      END
