      DOUBLE PRECISION FUNCTION AMOTRY(P,Y,PSUM,MP,NP,NDIM,FUNK,IHI,FAC)
 
C-----------------------------------------------------------------------
C
C  REFERENCE : NUMERICAL RECIPES, W.H. PRESS ET AL.,
C              CAMBRIDGE UNIVERSITY PRESS, 1992  ISBN 0 521 43064 X
C  ADAPTED FOR DOUBLE PRECISION
C  THIS SUBROUTINE IS CALLED FROM AMOEBA
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
 
      INTEGER          MP,NP,NMAX
      PARAMETER        (NMAX=20)
      DOUBLE PRECISION FAC,P(MP,NP),PSUM(NP),Y(MP),FUNK
      DOUBLE PRECISION FAC1,FAC2,YTRY,PTRY(NMAX)
      INTEGER          IHI,NDIM,J
      EXTERNAL         FUNK
CU  USES FUNK
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'AMOTRY:'
 
      FAC1 = (1.D0-FAC)/NDIM
      FAC2 = FAC1-FAC
      DO 11 J=1,NDIM
        PTRY(J) = PSUM(J) * FAC1 - P(IHI,J) * FAC2
 11   CONTINUE
      YTRY = FUNK(PTRY)
      IF ( YTRY .LT. Y(IHI) ) THEN
        Y(IHI) = YTRY
        DO 12 J=1,NDIM
          PSUM(J)  = PSUM(J) - P(IHI,J) + PTRY(J)
          P(IHI,J) = PTRY(J)
 12     CONTINUE
      ENDIF
      AMOTRY = YTRY
      RETURN
      END
