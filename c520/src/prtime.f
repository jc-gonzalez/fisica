      SUBROUTINE PRTIME(TTIME)
 
C-----------------------------------------------------------------------
C  PR(INT) TIME
C
C  PRINTS PRESENT DATE AND TIME AND GIVES IT IN A FORMAT SUITED FOR THE
C  RUNHEADER AND EVENTHEADER
C  THIS SUBROUTINE IS CALLED FROM MAIN AND START
C  ARGUMENT:
C   TTIME  = TIME (YYMMDD)
C
C  IF OUR DATE ROUTINE DOES NOT FIT TO YOUR COMPUTER, PLEASE REPLACE
C  IT BY A SUITABLE ROUTINE OF YOUR SYSTEM
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
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

      DOUBLE PRECISION TTIME
      INTEGER ID, IT, ISL1, ISL2, ISL3, ISL4, ISL5, ISL6

C-----------------------------------------------------------------------
 
C call jcdate (modified version 13-10-98)
      CALL jcdate(ID, IT, ISL1, ISL2, ISL3, ISL4, ISL5, ISL6)
      ttime = DBLE(ID)
      
C format dd, mm, yy, hh, mm, ss
      WRITE(MONIOU,100) ISL3,ISL2,ISL1,ISL4,ISL5,ISL6
 100  FORMAT(' PRESENT TIME : ',
     *     I2,'.',I2.2,'.',I2.2,I4,':',I2.2,':',I2.2)

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 
      RETURN
      END








