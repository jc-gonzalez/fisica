      SUBROUTINE STRDEC
 
C-----------------------------------------------------------------------
C  STR(ANGE BARYON) DEC(AY)
C
C  ROUTINE TREATES DECAY OF STRANGE BARYONS (LAMBDA, SIGMA, XI, OMEGA)
C  DECAY WITH FULL KINEMATIC, ENERGY AND MOMENTA CONSERVED
C  THIS SUBROUTINE IS CALLED FORM NUCINT
C
C  DESIGN  : D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,IRET.
      COMMON /IRET/    IRET1,IRET2
      INTEGER          IRET1,IRET2
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
*KEEP,STRBAR.
      COMMON /STRBAR/  CSTRBA
      DOUBLE PRECISION CSTRBA(11)
*KEND.
 
      INTEGER  I,J
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,9)
 444  FORMAT(' STRDEC: CURPAR=',1P,9E10.3)
 
C  COPY COORDINATES INTO SECPAR
      DO  1  J = 5,8
        SECPAR(J) = CURPAR(J)
   1  CONTINUE
 
      IF     ( ITYPE .EQ. 18 ) THEN
        CALL RMMAR( RD,1,1 )
        IF ( RD(1) .LT. CSTRBA(5) ) THEN
C  DECAY LAMBDA  --->  P + PI(-)
          CALL DECAY1( ITYPE, 14, 9 )
        ELSE
C  DECAY LAMBDA  --->  N + PI(0)
          CALL DECAY1( ITYPE, 13, 7 )
        ENDIF
 
      ELSEIF ( ITYPE .EQ. 19 ) THEN
        CALL RMMAR( RD,1,1 )
        IF ( RD(1) .LT. CSTRBA(6) ) THEN
C  DECAY SIGMA(+)  --->  P + PI(0)
          CALL DECAY1( ITYPE, 14, 7 )
        ELSE
C  DECAY SIGMA(+)  --->  N + PI(+)
          CALL DECAY1( ITYPE, 13, 8 )
        ENDIF
 
      ELSEIF ( ITYPE .EQ. 20  .OR.  ITYPE .EQ. 28 ) THEN
C  DECAY      SIGMA(0)  --->       LAMBDA + GAMMA
C  DECAY ANTI-SIGMA(0)  --->  ANTI-LAMBDA + GAMMA
        CALL DECAY1( ITYPE, ITYPE-2, 1 )
 
      ELSEIF ( ITYPE .EQ. 21 ) THEN
C  DECAY SIGMA(-)  --->  N + PI(-)
        CALL DECAY1( ITYPE, 13, 9 )
 
      ELSEIF ( ITYPE .EQ. 22  .OR.  ITYPE .EQ. 30 ) THEN
C  DECAY      XI(0)  --->       LAMBDA + PI(0)
C  DECAY ANTI-XI(0)  --->  ANTI-LAMBDA + PI(0)
        CALL DECAY1( ITYPE, ITYPE-4, 7 )
 
      ELSEIF ( ITYPE .EQ. 23 ) THEN
C  DECAY XI(-)  --->  LAMBDA + PI(-)
        CALL DECAY1( ITYPE, 18, 9 )
 
      ELSEIF ( ITYPE .EQ. 24  .OR.  ITYPE .EQ. 32 ) THEN
        CALL RMMAR( RD,1,1 )
        IF     ( RD(1) .LT. CSTRBA(10) ) THEN
C  DECAY      OMEGA(-)  --->       LAMBDA + K(-)
C  DECAY ANTI-OMEGA(+)  --->  ANTI-LAMBDA + K(+)
          CALL DECAY1( ITYPE, ITYPE-6, 15-ITYPE/8 )
        ELSEIF ( RD(1) .LT. CSTRBA(11) ) THEN
C  DECAY      OMEGA(-)  --->       XI(0) + PI(-)
C  DECAY ANTI-OMEGA(+)  --->  ANTI-XI(0) + PI(+)
          CALL DECAY1( ITYPE, ITYPE-2, 12-ITYPE/8 )
        ELSE
C  DECAY      OMEGA(-)  --->       XI(-) + PI(0)
C  DECAY ANTI-OMEGA(+)  --->  ANTI-XI(+) + PI(0)
          CALL DECAY1( ITYPE, ITYPE-1, 7 )
        ENDIF
 
      ELSEIF ( ITYPE .EQ. 26 ) THEN
        CALL RMMAR( RD,1,1 )
        IF ( RD(1) .LT. CSTRBA(5) ) THEN
C  DECAY ANTI-LAMBDA  --->  ANTI-P + PI(+)
          CALL DECAY1( ITYPE, 15, 8 )
        ELSE
C  DECAY ANTI-LAMBDA  --->  ANTI-N + PI(0)
          CALL DECAY1( ITYPE, 25, 7 )
        ENDIF
 
      ELSEIF ( ITYPE .EQ. 27 ) THEN
        CALL RMMAR( RD,1,1 )
        IF ( RD(1) .LT. CSTRBA(6) ) THEN
C  DECAY ANTI-SIGMA(-)  --->  ANTI-P + PI(0)
          CALL DECAY1( ITYPE, 15, 7 )
        ELSE
C  DECAY ANTI-SIGMA(-)  --->  ANTI-N + PI(-)
          CALL DECAY1( ITYPE, 25, 9 )
        ENDIF
 
      ELSEIF ( ITYPE .EQ. 29 ) THEN
C  DECAY ANTI-SIGMA(+)  --->  ANTI-N + PI(+)
        CALL DECAY1( ITYPE, 25, 8 )
 
      ELSEIF ( ITYPE .EQ. 31 ) THEN
C  DECAY ANTI-XI(+)  --->  ANTI-LAMBDA + PI(+)
        CALL DECAY1( ITYPE, 26, 8 )
 
      ELSE
        WRITE(MONIOU,*) 'STRDEC: UNFORESEEN PARTICLE CODE =',ITYPE
      ENDIF
      IRET1 = 1
      RETURN
      END
