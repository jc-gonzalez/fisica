      SUBROUTINE AMOEBA(P,Y,MP,NP,NDIM,FTOL,FUNK,ITER,IFLAG)
 
C-----------------------------------------------------------------------
C
C  FITTING ROUTINE
C  REFERENCE : NUMERICAL RECIPES, W.H. PRESS ET AL.,
C              CAMBRIDGE UNIVERSITY PRESS, 1992  ISBN 0 521 43064 X
C  ADAPTED FOR DOUBLE PRECISION
C  THIS SUBROUTINE IS CALLED FROM LONGFT
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
 
      INTEGER          ITMAX,MP,NMAX,NP
C  MAXIMUM NUMBER OF TRIAL PER CALL
      PARAMETER        (ITMAX=5000)
      PARAMETER        (NMAX=20)
      DOUBLE PRECISION AMOTRY,FTOL,FUNK,P(MP,NP),PSUM(NMAX),
     *                 RTOL,SUM,SWAP,Y(MP),YSAVE,YTRY
      INTEGER          I,IFLAG,IHI,ILO,INHI,ITER,J,M,N,NDIM
      EXTERNAL         FUNK
 
CU  USES AMOTRY,FUNK
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'AMOEBA:'
 
      IFLAG = 0
      ITER  = 0
 1    DO 12 N=1,NDIM
        SUM = 0.D0
        DO 11 M=1,NDIM+1
          SUM = SUM + P(M,N)
 11     CONTINUE
        PSUM(N) = SUM
 12   CONTINUE
 2    ILO=1
      IF ( Y(1) .GT. Y(2) ) THEN
        IHI  = 1
        INHI = 2
      ELSE
        IHI  = 2
        INHI = 1
      ENDIF
      DO 13 I=1,NDIM+1
        IF ( Y(I) .LE. Y(ILO) ) ILO = I
        IF     ( Y(I) .GT. Y(IHI)  ) THEN
          INHI = IHI
          IHI  = I
        ELSEIF ( Y(I) .GT. Y(INHI) ) THEN
          IF ( I .NE. IHI ) INHI = I
        ENDIF
 13   CONTINUE
      RTOL = 2.D0*ABS(Y(IHI)-Y(ILO))/(ABS(Y(IHI))+ABS(Y(ILO)))
      IF ( RTOL .LT. FTOL ) THEN
        SWAP   = Y(1)
        Y(1)   = Y(ILO)
        Y(ILO) = SWAP
        DO 14 N=1,NDIM
          SWAP     = P(1,N)
          P(1,N)   = P(ILO,N)
          P(ILO,N) = SWAP
 14     CONTINUE
        RETURN
      ENDIF
      IF ( ITER .GE.ITMAX ) THEN
        IF(DEBUG) WRITE(MDEBUG,*) 'AMOEBA: ITMAX EXCEEDED IN AMOEBA'
        IFLAG = 1
        RETURN
      ENDIF
      ITER = ITER + 2
      YTRY = AMOTRY(P,Y,PSUM,MP,NP,NDIM,FUNK,IHI,-1.0D0)
      IF     ( YTRY .LE. Y(ILO)  ) THEN
        YTRY = AMOTRY(P,Y,PSUM,MP,NP,NDIM,FUNK,IHI,2.0D0)
      ELSEIF ( YTRY .GE. Y(INHI) ) THEN
        YSAVE = Y(IHI)
        YTRY = AMOTRY(P,Y,PSUM,MP,NP,NDIM,FUNK,IHI,0.5D0)
        IF ( YTRY .GE. YSAVE ) THEN
          DO 16 I=1,NDIM+1
            IF ( I .NE. ILO ) THEN
              DO 15 J=1,NDIM
                PSUM(J) = 0.5D0 * (P(I,J) + P(ILO,J))
                P(I,J)  = PSUM(J)
 15           CONTINUE
              Y(I) = FUNK(PSUM)
            ENDIF
 16       CONTINUE
          ITER = ITER + NDIM
          GOTO 1
        ENDIF
      ELSE
        ITER = ITER - 1
      ENDIF
      GOTO 2
      END
C=======================================================================
 
