      SUBROUTINE BOX3
 
C-----------------------------------------------------------------------
C
C  CHECKS PASSAGE THROUGH OBSERVATION LEVEL(S)
C  IRET1=1 KILLS PARTICLE
C  IRET2=1 PARTICLE HAS BEEN CUTTED IN UPDATE
C  THIS SUBROUTINE IS CALLED FROM MAIN
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,GENER.
      COMMON /GENER/   GEN,ALEVEL
      DOUBLE PRECISION GEN,ALEVEL
*KEEP,IRET.
      COMMON /IRET/    IRET1,IRET2
      INTEGER          IRET1,IRET2
*KEEP,LONGI.
      COMMON /LONGI/   APLONG,HLONG,PLONG,SPLONG,THSTEP,THSTPI,
     *                 NSTEP,LLONGI,FLGFIT
      DOUBLE PRECISION APLONG(0:1040,9),HLONG(0:1024),PLONG(0:1040,9),
     *                 SPLONG(0:1040,9),THSTEP,THSTPI
      INTEGER          NSTEP
      LOGICAL          LLONGI,FLGFIT
*KEEP,OBSPAR.
      COMMON /OBSPAR/  OBSLEV,THCKOB,XOFF,YOFF,THETAP,PHIP,
     *                 THETPR,PHIPR,NOBSLV
      DOUBLE PRECISION OBSLEV(10),THCKOB(10),XOFF(10),YOFF(10),
     *                 THETAP,THETPR(2),PHIP,PHIPR(2)
      INTEGER          NOBSLV
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
*KEND.
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c   Simulate more precisely muons Cherenkov light
c------------------------------------------------------------
      integer          k
      double precision chloop,savpar(8),oldchi,oldthk,oldh
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      DOUBLE PRECISION HEIGH,HNEW,PROPAR(8),THCKHN
      INTEGER          I,IRET3,J,L,LPCT1,LPCT2
      EXTERNAL         HEIGH
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,9)
  444 FORMAT(' BOX3  : CURPAR=',1P,9E10.3)
 
      IF     ( ITYPE .EQ. 7 ) THEN
C  PI 0 DECAYS INTO 2 PHOTONS IN SUBROUTINE PI0DEC
        CALL TSTINI
        CALL PI0DEC
        CALL TSTEND
        IRET1 = 1
        RETURN
 
      ELSEIF ( ITYPE .EQ. 5  .OR.  ITYPE .EQ. 6 ) THEN
C  MUONS ARE TRACKED WITHIN ROUTINE MUTRAC
        CALL TSTINI
        CALL MUTRAC
        CALL TSTEND
        IRET1 = 1
        RETURN
 
      ELSEIF ( ITYPE .LE. 3 ) THEN
C  ELECTRONS OR PHOTONS ARE TREATED IN SUBROUTINE EM
        CALL EM
        IRET1 = 1
        RETURN
 
      ELSEIF (     ITYPE .EQ. 17  .OR.
     *        (ITYPE .GE. 71  .AND.  ITYPE .LE. 74)) THEN
C  ETA DECAYS WITHIN ROUTINE ETADEC
        CALL TSTINI
        CALL ETADEC
        CALL TSTEND
        IRET1 = 1
        RETURN
 
      ELSEIF ( ITYPE .GE. 51  .AND.  ITYPE .LE. 65 ) THEN
C  RESONANCES DECAY WITHIN ROUTINE RESDEC
        CALL TSTINI
        CALL RESDEC
        CALL TSTEND
        IRET1 = 1
        RETURN
 
      ENDIF
 
C  FOR ALL THE OTHER PARTICLES THE PLACE OF NEXT INTERACTION WAS
C  DETERMINED IN BOX2
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c   This is just a first approach to the problem
c------------------------------------------------------------

c      goto 5991
      if ( itype .eq. 5 .or. itype .eq. 6 ) then
        oldthk = thickh     
        do 5101  i = 1,9
          savpar(i) = curpar(i)
 5101   continue
        chi = 0.2d0 * chi
        do 5100  k = 1,5
c  calculate hight difference in cm from given chi in g/cm**2
          thckhn = thickh + costhe * chi
          hnew   = heigh(thckhn)
c  update particle to interaction point (if it reaches so far)
c  and store coordinates in propar
          call update( hnew, thckhn, 0 )
          if ( iret2 .ne. 0 ) goto 5104
          do 5103  i = 1,8
            curpar(i) = outpar(i)
 5103     continue
          thickh = thckhn 
 5100   continue
 5104   continue
        thickh = oldthk
        do 5102  i = 1,9
          curpar(i) = savpar(i)
 5102   continue
        
      else
c  calculate hight difference in cm from given chi in g/cm**2
        thckhn = thickh + costhe * chi
        hnew   = heigh(thckhn)
c  update particle to interaction point (if it reaches so far)
c  and store coordinates in propar
        call update( hnew, thckhn, 0 )
          
      endif
        
      goto 5992
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c 5991 continue
c
cC     CALCULATE HIGHT DIFFERENCE IN CM FROM GIVEN CHI IN G/CM**2
c      THCKHN = THICKH + COSTHE * CHI
c      HNEW   = HEIGH(THCKHN)
cC     UPDATE PARTICLE TO INTERACTION POINT (IF IT REACHES SO FAR)
cC     AND STORE COORDINATES IN PROPAR
c      CALL UPDATE( HNEW, THCKHN, 0 )
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 5992 continue
c>>> it was : >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cC  CALCULATE HIGHT DIFFERENCE IN CM FROM GIVEN CHI IN G/CM**2
c      THCKHN = THICKH + COSTHE * CHI
c      HNEW   = HEIGH(THCKHN)
c      IF (DEBUG) WRITE(MDEBUG,*)'BOX3  : THICKH,THCKHN,HNEW=',
c     *                    SNGL(THICKH),SNGL(THCKHN),SNGL(HNEW)
cC  UPDATE PARTICLE TO INTERACTION POINT (IF IT REACHES SO FAR)
cC  AND STORE COORDINATES IN PROPAR
c      CALL UPDATE( HNEW, THCKHN, 0 )
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 

      IF ( DEBUG ) THEN
        WRITE(MDEBUG,455) IRET1,IRET2
  455   FORMAT(' BOX3  : IRET1..2=',2I5)
        IF ( IRET2 .EQ. 0 ) WRITE(MDEBUG,454) (OUTPAR(I),I=1,8)
  454   FORMAT(' BOX3  : OUTPAR=',1P,8E10.3)
      ENDIF
C  STORE PARTICLE FOR FURTHER TREATMENT
      IF ( IRET2 .EQ. 0 ) THEN
        DO  3  I = 1,8
          PROPAR(I) = OUTPAR(I)
   3    CONTINUE
        IRET3 = 0
      ELSE
C  PARTICLE CUTTED AT INTERACTION POINT; IT MAY HOWEVER PASS SOME OF THE
C  OBSERVATION LEVELS
        IRET3 = 1
      ENDIF
 
C  HERE THE ENDPOINT OF THE CURRENT TRACKING STEP IS WELL DEFINED.
C  THE PARTICLE IS TRACKED FROM THICKH DOWN TO THCKHN
C  COUNT THE PARTICLES FOR THE LONGITUDINAL DEVELOPMENT
      IF ( LLONGI ) THEN
        LPCT1 = INT(THICKH*THSTPI + 1.D0)
        LPCT2 = INT(THCKHN*THSTPI)
        LPCT2 = MIN(NSTEP,LPCT2)
C  ALL HADRONS
        IF     ( ITYPE .GE. 7 .AND. ITYPE .LE. 41 ) THEN
          DO 5004 L = LPCT1,LPCT2
            PLONG(L,6) = PLONG(L,6) + 1.D0
 5004     CONTINUE
C  CHARGED HADRONS
          IF ( SIGNUM(ITYPE) .NE. 0.D0 ) THEN
            DO 5005 L = LPCT1,LPCT2
              PLONG(L,7) = PLONG(L,7) + 1.D0
 5005       CONTINUE
          ENDIF
C  NUCLEI
        ELSEIF ( ITYPE .GT. 100 ) THEN
          DO 5006 L = LPCT1,LPCT2
            PLONG(L,8) = PLONG(L,8) + 1.D0
 5006     CONTINUE
        ENDIF
      ENDIF
 
C  CHECK OBSERVATION LEVEL PASSAGE AND UPDATE PARTICLE COORDINATES
      DO  1  J = 1,NOBSLV
        IF ( HNEW .GT. OBSLEV(J) ) GOTO 2
        IF ( H    .LT. OBSLEV(J) ) GOTO 1
C  REMEMBER NUMBER OF LEVEL FOR OUTPUT
        LEVL  = J
        CALL UPDATE( OBSLEV(J), THCKOB(J), J )
        IF (DEBUG) WRITE(MDEBUG,456) J,IRET1,IRET2
  456   FORMAT(' BOX3  : LEVEL ',I5,' IRET1,2=',2I5)
 
C  IF PARTICLE IS NOT CUTTED, BRING IT TO OUTPUT
        IF ( IRET2 .EQ. 0 ) THEN
          CALL OUTPUT
        ENDIF
   1  CONTINUE
 
C  KILL PARTICLE AS IT DECAYS OR INTERACTS BELOW LOWEST OBSLEVEL
      IRET1 = 1
      RETURN
 
C  PARTICLE INTERACTS OR DECAYS BEFORE PASSING OBSLEVEL
   2  CONTINUE
 
C  PARTICLE IS NOW UPDATED TO POINT OF INTERACTION
      IF ( IRET3 .EQ. 0 ) THEN
        DO  5  J = 1,8
          CURPAR(J) = PROPAR(J)
   5    CONTINUE
        ALEVEL = H
        BETA   = SQRT( GAMMA**2 - 1.D0 ) / GAMMA
      ELSE
C  ELIMINATE PARTICLE IF BELOW CUTS
        IRET1 = 1
      ENDIF
 
      RETURN
      END
