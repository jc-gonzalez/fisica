      SUBROUTINE TSTINI
 
C-----------------------------------------------------------------------
C  T(O INTERMEDIATE) ST(ACK) INI(TIALIZATION)
C
C  INITIALIZE THE INTERMEDIATE STACK FOR A SINGLE REACTION
C  THIS SUBROUTINE IS CALLED FROM AAMAIN, BOX3, AND PIGEN
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
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
*KEEP,REJECT.
      COMMON /REJECT/  AVNREJ,
     *                 ALTMIN,ANEXP,THICKA,THICKD,CUTLN,EONCUT,
     *                 FNPRIM
      DOUBLE PRECISION AVNREJ(10)
      REAL             ALTMIN(10),ANEXP(10),THICKA(10),THICKD(10),
     *                 CUTLN,EONCUT
      LOGICAL          FNPRIM
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
*KEEP,THNVAR.
      COMMON /THNVAR/  STACKINT,INT_ICOUNT,THINNING
      INTEGER          MAXICOUNT
      PARAMETER        (MAXICOUNT=20000)
      DOUBLE PRECISION STACKINT(MAXICOUNT,13)
      INTEGER          INT_ICOUNT
      LOGICAL          THINNING
*KEND.
 
      DOUBLE PRECISION EEPP
      INTEGER          I,J
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,1)
 1    FORMAT(' TSTINI: RESET INTERNAL REACTION STACK')
 
      INT_ICOUNT = 0
 
      THINNING = .FALSE.
 
      RETURN
      END
