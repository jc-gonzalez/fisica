      DOUBLE PRECISION FUNCTION PTRANS( DUMMY )
 
C-----------------------------------------------------------------------
C  TRANS(VERSE MOMENTUM)
C
C  RANDOM SELECTION OF TRANSVERSE MOMENTUM
C  DISTRIBUTION IS OF FORM X*EXP(-X)
C  THIS FUNCTION IS CALLED FROM BOX60, BOX65, BOX70, HMESON, ISOBAR,
C  NIHILA, PIGEN1, PIGEN2, SINGLE, AND VHMESO
C  ARGUMENT:
C   DUMMY  = DUMMY (FOR HISTORICAL REASONS)
C
C  CHANGES : J. KNAPP   IK1  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
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
*KEND.
 
      REAL GX(0:50),HX(0:50)
      SAVE GX,HX,DX,FIRST
      LOGICAL FIRST
C  DX IS STEPSIZE FOR APPROXIMATING CURVE
      DATA FIRST / .TRUE. /, DX / 0.5D0 /
C-----------------------------------------------------------------------
 
C     IF ( DEBUG ) WRITE(MDEBUG,*) 'PTRANS:'
 
C  COMPUTE FUNCTION VALUES AND INTEGRAL OF STEP FUNCTION H(X)
C  APPROXIMATING Y(X) = X * EXP(1-X) WITH H(X) > Y(X)
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        IMAX  = C(34) / DX
        GX(0) = 0.D0
        HX(0) = DX*EXP(1.D0-DX)
        DO  2  I = 1,IMAX
          X     = I*DX
          IF ( X .LT. 1.D0 ) X = X + DX
          HX(I) = X*EXP(1.D0-X)
          GX(I) = GX(I-1) + HX(I-1)
    2   CONTINUE
        SUMI = 1.D0 / GX(IMAX)
        DO  3  I = 1,IMAX
          GX(I) = GX(I) * SUMI
    3   CONTINUE
      ENDIF
 
C-----------------------------------------------------------------------
C  GET RANDOM VARIABLE DISTRIBUTED AS HX(X)
   11 CONTINUE
      CALL RMMAR( RD,2,1 )
      I  = 0
    1 CONTINUE
      I  = I+1
      IF ( GX(I) .LT. RD(1) ) GOTO 1
      XX = ( (RD(1)-GX(I-1))/(GX(I)-GX(I-1)) + I-1 ) * DX
      ZZ = HX(I-1)
C  GET RANDOM VARIABLE DISTRIBUTED AS Y(X) BY REJECTION METHOD
      TT = XX * EXP(1.-XX)
      IF ( RD(2)*ZZ .GT. TT ) GOTO 11
 
C  GET REQUIRED PEAK VALUE
      PTRANS = XX * C(12)
      IF ( DEBUG ) WRITE(MDEBUG,*) 'PTRANS: PT = ',SNGL(PTRANS)
 
      RETURN
      END
