      SUBROUTINE PTRAN( ZN,FACT,PTX,PTY )
 
C-----------------------------------------------------------------------
C  TRAN(SVERSE MOMENTUM)
C
C  GENERATION OF TRANSVERSE MOMENTUM FOR PARTICLES IN HDPM
C  THIS SUBROUTINE IS CALLED FROM PPARAM
C  ARGUMENTS:
C   ZN    = POWER OF TRANSV. MOMENTUM FUNCTION, DEP. ON CENT.RAP.DENSITY
C   FACT  = FACTOR TAKING INTO ACCOUNT PARTICLE SPECIFIC TRANSV.MOMENTUM
C   PTX   = TRANSVERSE MOMENTUM IN X DIRECTION
C   PTY   = TRANSVERSE MOMENTUM IN Y DIRECTION
C
C  DESIGN  : T. THOUW   IK3  FZK KARLSRUHE
C  CHANGES : D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
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
*KEND.
 
C-----------------------------------------------------------------------
 
CC    IF ( DEBUG ) WRITE(MDEBUG,*) 'PTRAN : ZN=',SNGL(ZN)
 
C  TWO RANDOM NUMBERS
      CALL RMMAR( RD,2,1 )
C  GENERATE <P_T>   (REFERENCE??)
      B   = ZN * (ZN - 1.D0)
      ZZ  = SQRT(1.D0/RD(1) - 1.D0)
      XPT = ZZ * SQRT(2.D0/B)
   11 CONTINUE
      IF ( XPT .LT. 0.5D-3 ) GOTO 22
      X1  = 1.D0 + XPT
      XB  = X1**ZN
      XC  = 1.D0 + ZN * XPT
      ZA  = SQRT(XB/XC - 1.D0)
      XD  = (ZZ - ZA) * (X1 * 2.D0 * ZA * XC**2 ) / ( B * XPT * XB )
      XPT = XPT + XD
      IF ( ABS(XD) .GT. 1.D-3 ) GOTO 11
   22 CONTINUE
 
C  2*PI*RANDOM NUMBER FOR ANGLE PHI
      Z   = PI2 * RD(2)
      PTX = XPT * FACT * COS(Z)
      PTY = XPT * FACT * SIN(Z)
 
CC    IF ( DEBUG ) WRITE(MDEBUG,*) 'PTRAN : RD(1,2),XPT=',
CC   *                                      RD(1),RD(2),SNGL(XPT)
 
      RETURN
      END
