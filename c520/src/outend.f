      SUBROUTINE OUTEND
 
C-----------------------------------------------------------------------
C  OUT(PUT AT) END (OF SHOWER)
C
C  WRITE REST OF PARTICLES TO OUTPUT BUFFER
C  PRINTS INTERACTION LENGTHS STATISTICS
C  THIS SUBROUTINE IS CALLED FROM MAIN
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,BAL.
      COMMON /BAL/     EBAL
      DOUBLE PRECISION EBAL(10)
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
*KEEP,CHISTA.
      COMMON /CHISTA/  IHYCHI,IKACHI,IMUCHI,INNCHI,INUCHI,IPICHI
      INTEGER          IHYCHI(124),IKACHI(124),IMUCHI(124),
     *                 INNCHI(124),INUCHI(124),IPICHI(124)
*KEEP,ELADPM.
      COMMON /ELADPM/  ELMEAN,ELMEAA,IELDPM,IELDPA
      DOUBLE PRECISION ELMEAN(37),ELMEAA(37)
      INTEGER          IELDPM(37,13),IELDPA(37,13)
*KEEP,MULT.
      COMMON /MULT/    EKINL,MSMM,MULTMA,MULTOT
      DOUBLE PRECISION EKINL
      INTEGER          MSMM,MULTMA(37,13),MULTOT(37,13)
*KEEP,NCOUNT.
      COMMON /NCOUNT/  NCOUN
      INTEGER          NCOUN(8)
*KEEP,NPARTI.
      COMMON /NPARTI/  NPARTO,MUOND
      DOUBLE PRECISION NPARTO(10,25),NPHOTO(10),NPOSIT(10),NELECT(10),
     *                 NNU(10),NMUP(10),NMUM(10),NPI0(10),NPIP(10),
     *                 NPIM(10),NK0L(10),NKPL(10),NKMI(10),NNEUTR(10),
     *                 NPROTO(10),NPROTB(10),NK0S(10),NHYP(10),
     *                 NNEUTB(10),NDEUT(10),NTRIT(10),NALPHA(10),
     *                 NOTHER(10),MUOND
      EQUIVALENCE (NPARTO(1, 1),NPHOTO(1)), (NPARTO(1, 2),NPOSIT(1)),
     *            (NPARTO(1, 3),NELECT(1)), (NPARTO(1, 4),NNU(1))   ,
     *            (NPARTO(1, 5),NMUP(1))  , (NPARTO(1, 6),NMUM(1))  ,
     *            (NPARTO(1, 7),NPI0(1))  , (NPARTO(1, 8),NPIP(1))  ,
     *            (NPARTO(1, 9),NPIM(1))  , (NPARTO(1,10),NK0L(1))  ,
     *            (NPARTO(1,11),NKPL(1))  , (NPARTO(1,12),NKMI(1))  ,
     *            (NPARTO(1,13),NNEUTR(1)), (NPARTO(1,14),NPROTO(1)),
     *            (NPARTO(1,15),NPROTB(1)), (NPARTO(1,16),NK0S(1))  ,
     *            (NPARTO(1,18),NHYP(1))  , (NPARTO(1,19),NDEUT(1)) ,
     *            (NPARTO(1,20),NTRIT(1)) , (NPARTO(1,21),NALPHA(1)),
     *            (NPARTO(1,22),NOTHER(1)), (NPARTO(1,25),NNEUTB(1))
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
*KEEP,PBALA.
      COMMON /PBALA/   PBAL
      DOUBLE PRECISION PBAL(10)
*KEEP,RECORD.
      COMMON /RECORD/  IRECOR
      INTEGER          IRECOR
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
*KEEP,STATI.
      COMMON /STATI/   SABIN,SBBIN,INBIN,IPBIN,IKBIN,IHBIN
      DOUBLE PRECISION SABIN(37),SBBIN(37)
      INTEGER          INBIN(37),IPBIN(37),IKBIN(37),IHBIN(37)
*KEND.
 
      INTEGER I,J,K,NELMEA
C-----------------------------------------------------------------------
 
      IF ( LH .GT. 0 ) THEN
        CALL TOBUF( DATAB,0 )
        DO  2  I = 1,MAXBUF
          DATAB(I) = 0.
    2   CONTINUE
      ENDIF
      LH = 0
 
      IF ( FPRINT .OR. DEBUG ) THEN
        WRITE(MONIOU,101) NSHIFT,NOPART
  101   FORMAT(' ',I10,' SHIFTS TO EXTERNAL STACK'/
     *         ' ',I10,' PARTICLES WRITTEN TO PATAPE')
 
        IF ( .NOT. GHEISH ) THEN
          WRITE(MONIOU,103) (EBAL(I),I=1,10)
  103     FORMAT (/' ENERGY BALANCE OF PARTICLE PRODUCTION PROCESSES'//
     *             1P,5E20.8/5E20.8/)
 
          WRITE(MONIOU,203) (PBAL(I),I=1,10)
  203     FORMAT (' MOMENTUM BALANCE OF PARTICLE PRODUCTION PROCESSES'//
     *          1P,5E20.8/5E20.8/)
 
          WRITE(MONIOU,104) (NCOUN(K),K=1,8)
  104     FORMAT(//
     *      I10,' / ',I6,'  ANTINUCLEONS ENTER / ANNIHILATE IN BOX 60'/
     *      I10,' / ',I6,'  ANTINUCLEONS ENTER / ANNIHILATE IN BOX 61'/
     *      I10,' / ',I6,'  ANTINUCLEONS ENTER / ANNIHILATE IN BOX 62'/
     *      I10,' / ',I6,'  ANTINUCLEONS ENTER / ANNIHILATE IN BOX 63'/)
        ENDIF
      ENDIF
 
      IF ( FPRINT ) THEN
C  PRINT ENERGY - MULTIPLICITY MATRIX
        WRITE(MONIOU,209) SHOWNO,(K,K=1,13),
     *  (J,(MULTMA(J,K),K=1,13),10**((J-4.)/3.),10**((J-3.)/3.),J=1,37),
     *     1,(INT(10**((K-1.)/3.)+1 ),K = 2,13),
     *     2,(INT(10**((K   )/3.)   ),K = 2,13)
  209   FORMAT(//' ENERGY - MULTIPLICITY MATRIX OF SHOWER NO ',I10/
     *           ' ENERGY RUNS VERTICALLY, MULTIPLICITY HORIZONTALLY'//
     *           ' ',5X,5I9,3I8,5I7,'    ENERGY RANGE (GEV)'/
     *           37(/' ',I4,1X,5I9,3I8,5I7,2X,1P,2E10.1,0P)//
     *           ' MULT.',5I9,3I8,5I7,5X,'LOWER BIN LIMIT'/
     *           ' RANGE',5I9,3I8,5I7,5X,'UPPER BIN LIMIT')
      ENDIF
 
C  GET MEAN OF ELASTICITY FOR ENERGY BINS
      DO 3377  J = 1,37
        NELMEA = 0
        DO 3378  K = 1,10
          NELMEA = NELMEA + IELDPM(J,K)
 3378   CONTINUE
        IF ( NELMEA .NE. 0 ) ELMEAN(J) = ELMEAN(J) / NELMEA
 3377 CONTINUE
 
      IF ( FPRINT ) THEN
C  PRINT ENERGY - ELASTICITY MATRIX
        WRITE(MONIOU,408) SHOWNO,(K,K=1,10),
     *        (J,(IELDPM(J,K),K=1,10),
     *        ELMEAN(J),10**((J-4.)/3.),10**((J-3.)/3.),J=1,37),
     *        ((K-1)*0.1,K=1,10),(K*0.1,K=1,10)
  408   FORMAT (//' ENERGY - ELASTICITY MATRIX OF SHOWER NO ',I10/
     *            ' ENERGY RUNS VERTICALLY, ELASTICITY HORIZONTALLY'//
     *            ' ',5X,10I9,'   MEAN EL.   ENERGY RANGE (GEV)'/
     *            37(/' ',I4,1X,10I9,2X,1P,E10.3,2E10.1,0P)//
     *            ' ELA. ',10F9.2,5X,'LOWER BIN LIMIT'/
     *            ' RANGE',10F9.2,5X,'UPPER BIN LIMIT')
 
        WRITE(MONIOU,204) SHOWNO
  204   FORMAT(//' INTERACTIONS PER KINETIC ENERGY INTERVAL OF SHOWER',
     *         ' NO ',I10//)
 
        WRITE(MONIOU,205)
  205   FORMAT('   BIN    LOWER LIMIT    UPPER LIMIT    ',
     *         ' NUCLEON      PIONS      KAONS  S.BARYONS      TOTAL'/
     *         '             IN GEV         IN GEV      ',
     *         '  EVENTS     EVENTS     EVENTS     EVENTS  '/)
        WRITE(MONIOU,207) (I,SABIN(I),SBBIN(I),INBIN(I),IPBIN(I),
     *    IKBIN(I),IHBIN(I),INBIN(I)+IPBIN(I)+IKBIN(I)+IHBIN(I),I=1,37)
  207   FORMAT(' ',I5,1P,2E15.4,0P,1X,5I11)
 
        WRITE(MONIOU,301)
  301   FORMAT (//' INTERACTION LENGTH STATISTICS: ',
     *        '    1 BIN CORRESPONDS TO 10 G/CM**2 OR 1KM FOR MUONS'//
     *        '  BIN      LAMBDA NU   LAMBDA PI   LAMBDA KA   ',
     *                   'LAMBDA HY   LAMBDA MU   LAMBDA NUCLEUS'/)
        WRITE(MONIOU,303) (I,INUCHI(I),IPICHI(I),IKACHI(I),IHYCHI(I),
     *                      IMUCHI(I),INNCHI(I),I=1,124)
  303   FORMAT (' ',I4,6I12)
 
        WRITE(MONIOU,105) IRECOR
  105   FORMAT (/' NO OF WORDS WRITTEN TO PARTICLE TAPE UP TO NOW =',
     *           I10)
      ENDIF
 
      RETURN
      END
