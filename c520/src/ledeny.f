      SUBROUTINE LEDENY( LEDEFL )
 
C-----------------------------------------------------------------------
C  LE(A)D(ER'S) EN(ERG)Y
C
C  SELECTS THE FEYNMAN X OF THE ANTILEADING PARTICLES FROM A THEORETICAL
C  DISTRIBUTION AND CALCULATES THE RAPIDITY FROM IT
C  CALCULATE THE RAPIDITY OF THE LEADER FROM THE REMAINDER OF ENERGY
C  THIS SUBROUTINE IS CALLED FROM HDPM
C  ARGUMENT:
C   LEDEFL = 0  CORRECT ENDING OF LEDENY
C          = 1  NOT CORRECT ENDING OF LEDENY
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
*KEEP,INTER.
      COMMON /INTER/   AVCH,AVCH3,DC0,DLOG,DMLOG,ECMDIF,ECMDPM,ELAB,
     *                 FNEUT,FNEUT2,GNU,PLAB,POSC2,POSC3,POSN2,POSN3,
     *                 RC3TO2,S,SEUGF,SEUGP,SLOG,SLOGSQ,SMLOG,
     *                 WIDC2,WIDC3,WIDN2,WIDN3,YCM,YY0,ZN,
     *                 IDIF,ITAR
      DOUBLE PRECISION AVCH,AVCH3,DC0,DLOG,DMLOG,ECMDIF,ECMDPM,ELAB,
     *                 FNEUT,FNEUT2,GNU,PLAB,POSC2,POSC3,POSN2,POSN3,
     *                 RC3TO2,S,SEUGF,SEUGP,SLOG,SLOGSQ,SMLOG,
     *                 WIDC2,WIDC3,WIDN2,WIDN3,YCM,YY0,ZN
      INTEGER          IDIF,ITAR
*KEEP,LEPAR.
      COMMON /LEPAR/   LEPAR1,LEPAR2,LASTPI,NRESPC,NRESPN,NCPLUS
      INTEGER          LEPAR1,LEPAR2,LASTPI,NRESPC,NRESPN,NCPLUS
*KEEP,NEWPAR.
      COMMON /NEWPAR/  EA,PT2,PX,PY,TMAS,YR,ITYP,
     *                 IA1,IA2,IB1,IB2,IC1,IC2,ID1,ID2,IE1,IE2,IF1,IF2,
     *                 IG1,IG2,IH1,IH2,II1,II2,IJ1,NTOT
      DOUBLE PRECISION EA(3000),PT2(3000),PX(3000),PY(3000),TMAS(3000),
     *                 YR(3000)
      INTEGER          ITYP(3000),
     *                 IA1,IA2,IB1,IB2,IC1,IC2,ID1,ID2,IE1,IE2,IF1,IF2,
     *                 IG1,IG2,IH1,IH2,II1,II2,IJ1,NTOT
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
*KEEP,VKIN.
      COMMON /VKIN/    BETACM
      DOUBLE PRECISION BETACM
*KEND.
 
      DATA SL / 3.D0 /
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'LEDENY: ITYPE,ITAR=',ITYPE,ITAR
 
C  BETACM IS AVAILABLE IN COMMON /VKIN/ BUT NOT FOR PHOTOPRODUCTION
      IF ( ITYPE .EQ. 7 ) BETACM = SQRT( 1.D0 - 1.D0 / GCM**2 )
 
C  MOMENTUM OF INCOMING TARGET IN CM SYSTEM
      PNT = PAMA(ITAR) * GCM * BETACM
      IF ( DEBUG ) WRITE(MDEBUG,*) 'LEDENY: PNT=',SNGL(PNT)
 
C  GET FEYNMAN X FOR ANTILEADER DEPENDING ON ENERGY
C  DISCRIPTION OF THE FEYNMAN X DISTRIBUTION DEPENDING ON ENERGY
C    DN/DXF = SL*XF                               0 < XF < X1
C    DN/DXF = SL*X1                              X1 < XF < X2
C    DN/DXF = SL*X1 * EXP(-AL*(XF-X2))           X2 < XF <  1
 
      IF     ( ECMDPM .LT. 13.76D0 ) THEN
        X1 = 0.20D0
        X2 = 0.65D0
        AL = 1.265D0
      ELSEIF ( ECMDPM .LT. 5580.D0 ) THEN
        X1 = 0.716D0   + 0.00543D0 * SMLOG
        X2 = 0.8175D0  - 0.032D0   * SMLOG
        AL = 1.14D0    + 0.022D0   * SMLOG
      ELSE
        X1 = 0.265D0
        X2 = 0.265D0
        AL = 1.14D0 + 0.022D0*SMLOG
      ENDIF
 
C  CALCULATE THE INTEGRALS OVER THE THREE PARTS OF THE FUNCTION
      AA = 0.5D0 * SL * X1**2
      BB = SL * X1 * (X2 - X1)
      CC = SL * X1 / AL * ( 1.D0 - EXP( AL*(X2-1.D0) ) )
C  NORMALIZE TO 1
      TT = 1.D0 / (AA + BB + CC)
      CC = CC * TT
      AA = AA * TT
      BB = BB * TT
      AB = AA + BB
 
      CALL RMMAR( RD,1,1 )
C  GET XF FOR ANTILEADER
        IF     ( RD(1) .LE. AA ) THEN
          XF = SQRT( RD(1)*2.D0 / (SL*TT) )
        ELSEIF ( RD(1) .LE. AB ) THEN
          XF = (RD(1)-AA) / (SL*X1*TT) + X1
        ELSE
          XF = X2 - LOG( 1.D0 - (RD(1)-AB)*AL/(SL*X1*TT) ) / AL
        ENDIF
      IF ( DEBUG ) WRITE(MDEBUG,*) 'LEDENY: XF(TARGET)=',SNGL(XF)
 
C  CONVERT FEYNMAN X INTO RAPIDITY FOR ANTILEADER
      PLAL  = PNT * XF * PAMA(LEPAR2) / PAMA(ITAR)
      EA(2) = SQRT(PLAL**2 + TMAS(2)**2)
*     YR(2) = -0.5D0 * LOG( (EA(2)+PLAL)/(EA(2)-PLAL) )
      YR(2) = - LOG( (EA(2)+PLAL)/TMAS(2) )
 
C  CALCULATE THE REMAINDER OF ENERGY AND LONG. MOMENTUM OF LEADER
C  THIS HOLDS ALSO FOR MULTIPLE COLLISIONS (GNU > 1)
      ESUM  = 0.D0
      DO 10  I = 2,NTOT
        EA(I) = TMAS(I) * COSH( YR(I) + YCM )
        ESUM  = ESUM + EA(I)
  10  CONTINUE
      EA(1) = ELAB + PAMA(ITAR) - ESUM
      IF ( EA(1) .LE. TMAS(1) ) THEN
        LEDEFL = 1
        RETURN
      ENDIF
      PLLBSQ = EA(1)**2 - TMAS(1)**2
      PLLB   = SQRT( PLLBSQ )
*     YR(1)  = 0.5D0 * LOG( (EA(1) + PLLB) / (EA(1) - PLLB) ) - YCM
      YR(1)  = LOG( (EA(1) + PLLB) / TMAS(1) ) - YCM
      IF ( DEBUG ) WRITE(MDEBUG,*) 'LEDENY: EA(1),YR(2),YR(1)=',
     *                     SNGL(EA(1)),SNGL(YR(2)),SNGL(YR(1))
      LEDEFL = 0
      RETURN
      END
