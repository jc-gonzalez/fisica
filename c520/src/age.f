      SUBROUTINE AGE( R,S )
 
C-----------------------------------------------------------------------
C  AGE
C
C  CALCULATES LONGITUDINAL AGE PARAMETER
C  ORIGINALLY DEVELOPED BY: J. KEMPA, UNIVERSITY OF LODZ, POLAND
C  THIS SUBROUTINE IS CALLED FROM AVAGE
C  ARGUMENTS:
C   R      = AVERAGED AGE PARAMETER
C   S      = LONGITUDINAL AGE PARAMETER OF TOTAL SHOWER
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
 
      DOUBLE PRECISION A,B,C,R,R1,R2,R3,R4,R5,S
      DATA R1 / 1.9096D-02 /, R2 / 1.7964D-01 /, R3 / 5.3644D-01 /,
     *     R4 / 1.0332D0   /, R5 / 1.4856D0   /
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'AGE   : R=',SNGL(R)
 
      R = MAX( R, R1 )
      R = MIN( R, R5 )
 
      IF     ( R .LT. R2 ) THEN
        A =  3.109121D-1
        B =  2.146465D-1
        C = -5.451040D-3
      ELSEIF ( R .LT. R3 ) THEN
        A =  3.666449D-1
        B =  1.639189D-1
        C =  5.970362D-3
      ELSEIF ( R .LT. R4 ) THEN
        A =  1.459842D-1
        B =  6.317027D-1
        C = -2.420241D-1
      ELSEIF ( R .LE. R5 ) THEN
        A = -3.375703D-1
        B =  2.090333D0
        C = -1.343802D0
      ENDIF
 
      S = ( SQRT(B**2 - 4.D0 * A * (C-R)) - B ) / ( 2.D0 * A )
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'AGE   : S=',SNGL(S)
 
      RETURN
      END
