      SUBROUTINE BOX61
 
C-----------------------------------------------------------------------
C
C  NUCLEON OR ANTINUCLEON INTERACTIONS
C  LIGHT ISOBAR (FORWARD OR BACKWARD), NUCLEON
C  INCLUDES ANNIHILATION
C  THIS SUBROUTINE IS CALLED FROM NUCINT
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*KEEP,NCOUNT.
      COMMON /NCOUNT/  NCOUN
      INTEGER          NCOUN(8)
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
*KEEP,SIGM.
      COMMON /SIGM/    SIGMA,SIGANN,SIGAIR,FRACTN,FRCTNO
      DOUBLE PRECISION SIGMA,SIGANN,SIGAIR,FRACTN,FRCTNO
*KEND.
 
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,9)
  444 FORMAT(' BOX61 : CURPAR=',1P,9E10.3)
 
C  ANNIHILATION
      IF ( ITYPE .EQ. 15  .OR.  ITYPE .EQ. 25 ) THEN
        NCOUN(3) = NCOUN(3) + 1
        CALL RMMAR( RD,1,1 )
        IF ( RD(1)*SIGMA .LE. SIGANN ) THEN
          NCOUN(4) = NCOUN(4) + 1
          CALL NIHILA
          RETURN
        ENDIF
      ENDIF
 
C  INTERACTION (ISOBAR EXCITATION)
C  SELECT LIGHT ISOBAR AND NUCLEON MASSES
      CA = C(3)
      CB = PAMA(14)
 
C  DECIDE WHETHER ISOBAR GOES FORWARD OR BACKWARD
C  FORWARD IF KIND = 0, BACKWORD IF KIND = 1
      CALL RMMAR( RD,1,1 )
      IF ( RD(1) .LE. 0.5 ) THEN
        KIND = 0
      ELSE
        KIND = 1
      ENDIF
 
C  LIGHT ISOBAR
      CALL ISOBAR( ECM,KIND,CA,CB,1 )
 
C  NUCLEON
      CALL SINGLE( ECM,1-KIND,CB,CA )
 
      RETURN
      END
