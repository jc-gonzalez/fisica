      SUBROUTINE ADDANI( COST0,PHI0, COST1,PHI1, DCTH,DPHI )
 
C-----------------------------------------------------------------------
C  ADD(ITION OF) AN(GLES) I(NVERTED)
C
C  GIVEN TWO DIRECTIONS (0 AND 1) IN A COMMON SYSTEM OF REFERENCE.
C  FIND DCTH AND DPHI SUCH, THAT THE ROUTINE ADDANG TRANSFORMS
C  (COST0,PHI0) BY ADDING (DCTH,DPHI) INTO (COST1,PHI1).
C  CALCULATION IS DONE BY SEQUENTIAL ROTATIONS :
C    1. ROTATE VECTOR AROUND Z AXIS BY -PHI1
C    2. ROTATE VECTOR AROUND Y AXIS BY -THETA1
C  NOW VECTOR IS (X,Y,Z) WITH DCTH      = Z
C                         AND TAN(DPHI) = Y/X
C  THIS SUBROUTINE IS CALLED FROM MUDECY
C  ARGUMENTS:
C   COST0  = COSINE THETA OF PARTICLE BEFORE
C   PHI0   = PHI          OF PARTICLE BEFORE
C   COST1  = COSINE THETA OF PARTICLE
C   PHI1   = PHI          OF PARTICLE
C   DCTH   = COSINE THETA OF ANGLE
C   DPHI   = PHI          OF ANGLE
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
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
 
      DOUBLE PRECISION COST0,COST1,CP,CP1,CT,CT1,DCTH,DPHI,PHI0,PHI1,
     *                 SP,SP1,ST,ST1,X,XX,Y,YY,Z,ZZ
C-----------------------------------------------------------------------
 
CC    IF ( DEBUG ) WRITE(MDEBUG,*) 'ADDANI:'
 
      CT  = COST0
      ST  = SQRT(1.D0-CT**2)
      CP  = COS(PHI0)
      SP  = SIN(PHI0)
      CT1 = COST1
      ST1 = SQRT(1.D0-CT1**2)
      CP1 = COS(PHI1)
      SP1 = SIN(PHI1)
 
      X = ST1 * CP1
      Y = ST1 * SP1
      Z = CT1
 
      XX =  CT*CP*X + CT*SP*Y - ST*Z
      YY = -SP   *X + CP   *Y
      ZZ =  ST*CP*X + ST*SP*Y + CT*Z
 
C  GET NEW COSINE(THETA) AND PHI
      DCTH   = ZZ
      IF ( YY .NE. 0.D0  .OR.  XX .NE. 0.D0 ) THEN
        DPHI = ATAN2( YY, XX )
      ELSE
        DPHI = 0.D0
      ENDIF
 
      RETURN
      END
