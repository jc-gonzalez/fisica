      SUBROUTINE TOBUF( A,IFL )
 
C-----------------------------------------------------------------------
C  (WRITE) TO BUF(FER)
C
C  WRITES UP TO NSUBBL DATA BLOCKS TO OUTPUT BUFFER AND PUTS THE FULL
C  BUFFER TO TAPE
C  THIS SUBROUTINE IS CALLED FROM MAIN, ELECTR, PHOTON, INPRM, OUTEND,
C  OUTPUT, OUTPT2, AND PHOTON
C  ARGUMENTS:
C   A      = ARRAY TO BE WRITTEN TO TAPE
C   IFL    = STARTING OF FINAL OUTPUT
C          = 0  NORMAL BLOCK
C          = 1  NORMAL BLOCK WITH END OF OUTPUT
C          = 2  ONLY END OF OUTPUT
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
*KEND.
 
      INTEGER   NSUBBL
      PARAMETER (NSUBBL=21)
      REAL      A(*)
C  NSUBBL IS NUMBER OF SUBBLOCKS IN ONE OUTPUT RECORD
C  (OUTPUT RECORD LENGTH = NSUBBL * 39 * 7 * 4 BYTES  <= 22932 )
C  IBLK  IS  COUNTER FOR SUBBLOCKS
C  OUTPUT BUFFER FOR PARTICLE OUTPUT
      REAL      OUTBUF(MAXBUF,NSUBBL)
      INTEGER   I,IBLK,IFL,K
      SAVE      OUTBUF
      DATA      IBLK / 0 /
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'TOBUF : IFL =',IFL
 
 
C  COPY TO BUFFER
      IF ( IFL .LE. 1 ) THEN
        IBLK = IBLK + 1
        DO  1  I = 1,MAXBUF
          OUTBUF(I,IBLK) = A(I)
 1      CONTINUE
      ENDIF
 
C  WRITE TO TAPE IF BLOCK IS FULL OR IF IFL IS 1
      IF ( IFL .GE. 1  .OR.  IBLK .EQ. NSUBBL ) THEN
        NRECS = NRECS + 1
        NBLKS = NBLKS + IBLK
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c        WRITE(PATAPE)           ((OUTBUF(I,K),I=1,MAXBUF),K=1,NSUBBL)
        call jcdatsave(outbuf)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        IRECOR = IRECOR + MAXBUF * NSUBBL
        IBLK   = 0
        DO  2  K = 1,NSUBBL
        DO  2  I = 1,MAXBUF
          OUTBUF(I,K) = 0.0
 2      CONTINUE
      ENDIF
 
      RETURN
      END
