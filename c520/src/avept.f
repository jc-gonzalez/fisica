      SUBROUTINE AVEPT( ECM,SLOG )
 
C-----------------------------------------------------------------------
C  AVE(RAGE) PT (TRANSVERSE MOMENTUM)
C
C  CALCULATES AVERAGE RATIO PT(PARTICLE)/PT(PION) DEPENDING ON ENERGY
C  THE DEPENDENCE OF PT ON ENERGY IS DONE IN ROUTINE PTRAM/PTRAN
C  THIS SUBROUTINE IS CALLED FROM HDPM
C  ARGUMENTS:
C   ECM    = ENERGY IN THE CM SYSTEM
C   SLOG   = LOG(S) ( = LOG(ECM**2) )
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,AVPT.
      COMMON /AVPT/    AVPT,AVPK,AVPN,AVPH,AVPE
      DOUBLE PRECISION AVPT,AVPK,AVPN,AVPH,AVPE
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
 
      DOUBLE PRECISION ECM,SLOG
C-----------------------------------------------------------------------
 
CC    IF ( DEBUG ) WRITE(MDEBUG,*) 'AVEPT : ECM =',SNGL(ECM)
 
C  AVERAGE TRANSVERSE MOMENTUM
 
C  ... FOR PIONS (=AVPT)
      IF ( ECM .LT. 132.D0 ) THEN
        AVPT = 0.3D0 + 6.272D-3 * SLOG
      ELSE
        AVPT = ( 0.442105D0 + 0.016276D0 * SLOG )**2
      ENDIF
 
C  ... FOR KAONS (=AVPK)
      IF ( ECM .LT. 131.D0 ) THEN
        AVPK = 1.27D0 * AVPT
      ELSE
        AVPK = (0.403146D0 + 0.0281D0 * SLOG)**2
      ENDIF
 
C  ... FOR NUCLEONS (=AVPN)
      IF ( ECM .LT. 102.D0 ) THEN
        AVPN = 1.39D0 * AVPT
      ELSE
        AVPN = (0.389873D0 + 0.034127D0 * SLOG)**2
      ENDIF
 
C  SET AVERAGE PT RELATED TO AVERAGE PT FOR PIONS
C  ... FOR STRANGE BARYONS (=AVPH)
      AVPH = 1.3D0 * (1.45D0 * AVPN - 0.45D0 * AVPK) / AVPT
C  ... FOR ETA MESONS (=AVPE)
      AVPE = 1.3D0 * (0.88D0 * AVPK + 0.12D0 * AVPN) / AVPT
      AVPK = 1.3D0 * AVPK / AVPT
      AVPN = 1.3D0 * AVPN / AVPT
      AVPT = 1.3D0
 
      IF ( DEBUG ) WRITE(MDEBUG,100)
     *       SNGL(AVPT),SNGL(AVPK),SNGL(AVPN),SNGL(AVPH),SNGL(AVPE)
 100  FORMAT(' AVEPT : AVPT,AVPK,AVPN,AVPH,AVPE=',5F12.5)
 
      RETURN
      END
