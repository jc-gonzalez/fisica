      SUBROUTINE MUTRAC
 
C-----------------------------------------------------------------------
C  MU(ON) TRAC(KING)
C
C  TRACKS THE MUON REGARDING MAX. STEP LENGTH FOR MULTIPLE SCATTERING
C  CHECKS PASSAGE THROUGH OBSERVATION LEVELS
C  IRET1=1 KILLS PARTICLE
C  IRET2=1 PARTICLE HAS BEEN CUTTED IN UPDATE
C  THIS SUBROUTINE IS CALLED FROM BOX3
C
C  DESIGN  : D. HECK    IK3  FZK KARLSRUHE
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
*KEEP,MUPART.
      COMMON /MUPART/  AMUPAR,BCUT,CMUON,FMUBRM,FMUORG
      DOUBLE PRECISION AMUPAR(14),BCUT,CMUON(11)
      LOGICAL          FMUBRM,FMUORG
*KEEP,NPARTI.
      COMMON /NPARTI/  NPARTO,MUOND
      DOUBLE PRECISION NPARTO(10,25),NPHOTO(10),NPOSIT(10),NELECT(10),
     *                 NNU(10),NMUP(10),NMUM(10),NPI0(10),NPIP(10),
     *                 NPIM(10),NK0L(10),NKPL(10),NKMI(10),NNEUTR(10),
     *                 NPROTO(10),NPROTB(10),NK0S(10),NHYP(10),
     *                 NNEUTB(10),NDEUT(10),NTRIT(10),NALPHA(10),
     *                 NOTHER(10),MUOND
      EQUIVALENCE (NPARTO(1, 1),NPHOTO(1)), (NPARTO(1, 2),NPOSIT(1)),
     *            (NPARTO(1, 3),NELECT(1)), (NPARTO(1, 4),NNU(1))   ,
     *            (NPARTO(1, 5),NMUP(1))  , (NPARTO(1, 6),NMUM(1))  ,
     *            (NPARTO(1, 7),NPI0(1))  , (NPARTO(1, 8),NPIP(1))  ,
     *            (NPARTO(1, 9),NPIM(1))  , (NPARTO(1,10),NK0L(1))  ,
     *            (NPARTO(1,11),NKPL(1))  , (NPARTO(1,12),NKMI(1))  ,
     *            (NPARTO(1,13),NNEUTR(1)), (NPARTO(1,14),NPROTO(1)),
     *            (NPARTO(1,15),NPROTB(1)), (NPARTO(1,16),NK0S(1))  ,
     *            (NPARTO(1,18),NHYP(1))  , (NPARTO(1,19),NDEUT(1)) ,
     *            (NPARTO(1,20),NTRIT(1)) , (NPARTO(1,21),NALPHA(1)),
     *            (NPARTO(1,22),NOTHER(1)), (NPARTO(1,25),NNEUTB(1))
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
 
      DOUBLE PRECISION CHITOT,HEIGH,HNEW,PROPAR(8),THCKHN
      INTEGER          I,IRET3,J,L,LPCT1,LPCT2
      LOGICAL          FSCAT
      EXTERNAL         HEIGH
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,444) (CURPAR(I),I=1,9)
  444 FORMAT(' MUTRAC: CURPAR=',1P,9E10.3)
 
C  THE PLACE OF NEXT INTERACTION WAS DETERMINED IN BOX2
C  KEEP TOTAL STEP LENGTH UNTIL DECAY OR INTERACTION OCCURS
      CHITOT = CHI
 
 10   CONTINUE
 
C  CALCULATE MAX STEP SIZE (10 RAD. LENGTH) FOR MULTIPLE SCATTERING
      CHI = MIN( C(20), CHITOT )
      IF ( CHI .EQ. CHITOT ) THEN
        FSCAT  = .FALSE.
        IF (DEBUG) WRITE(MDEBUG,*)'MUTRAC: CHI=',SNGL(CHI)
      ELSE
        FSCAT  = .TRUE.
        IF (DEBUG) WRITE(MDEBUG,*)'MUTRAC: C(20)=',SNGL(C(20))
      ENDIF
 
 
C  CALCULATE HIGHT DIFFERENCE IN CM FROM GIVEN CHI IN G/CM**2
      THCKHN = THICKH + COSTHE * CHI
      HNEW   = HEIGH(THCKHN)
      IF (DEBUG) WRITE(MDEBUG,*)'MUTRAC: THICKH,THCKHN,HNEW=',
     *                    SNGL(THICKH),SNGL(THCKHN),SNGL(HNEW)
C  UPDATE MUON TO INTERACTION POINT (IF IT REACHES SO FAR)
C  AND STORE COORDINATES IN PROPAR
      CALL UPDATE( HNEW, THCKHN, 0 )
      IF ( DEBUG ) THEN
        WRITE(MDEBUG,455) IRET1,IRET2
  455   FORMAT(' MUTRAC: IRET1..2=',2I5)
        IF ( IRET2 .EQ. 0 ) WRITE(MDEBUG,454) (OUTPAR(I),I=1,8)
  454   FORMAT(' MUTRAC: OUTPAR=',1P,8E10.3)
      ENDIF
C  STORE MUON FOR FURTHER TREATMENT
      IF ( IRET2 .EQ. 0 ) THEN
        DO  3  I = 1,8
          PROPAR(I) = OUTPAR(I)
  3     CONTINUE
        IRET3 = 0
      ELSE
C  MUON CUTTED AT INTERACTION POINT; IT MAY HOWEVER PASS SOME OF THE
C  OBSERVATION LEVELS
        IRET3 = 1
      ENDIF
 
C  HERE THE ENDPOINT OF THE CURRENT TRACKING STEP IS WELL DEFINED.
C  THE MUON IS TRACKED FROM THICKH DOWN TO THICKHN
C  COUNT THE MUONS FOR THE LONGITUDINAL DEVELOPMENT
      IF ( LLONGI ) THEN
        LPCT1 = INT(THICKH*THSTPI + 1.D0)
        LPCT2 = INT(THCKHN*THSTPI)
        LPCT2 = MIN(NSTEP,LPCT2)
        IF     ( ITYPE .EQ. 6 ) THEN
          DO 5003 L = LPCT1,LPCT2
            PLONG(L,4) = PLONG(L,4) + 1.D0
 5003     CONTINUE
        ELSEIF ( ITYPE .EQ. 5 ) THEN
          DO 5013 L = LPCT1,LPCT2
            PLONG(L,5) = PLONG(L,5) + 1.D0
 5013     CONTINUE
        ENDIF
      ENDIF
 
C  CHECK OBSERVATION LEVEL PASSAGE AND UPDATE MUON COORDINATES
      DO  1  J = 1,NOBSLV
        IF ( HNEW .GT. OBSLEV(J) ) GOTO 2
        IF ( H    .LT. OBSLEV(J) ) GOTO 1
C  REMEMBER NUMBER OF LEVEL FOR OUTPUT
        LEVL  = J
        CALL UPDATE( OBSLEV(J), THCKOB(J), J )
        IF (DEBUG) WRITE(MDEBUG,456) J,IRET1,IRET2
  456   FORMAT(' MUTRAC: OBSLEV=',I5,' IRET1,2=',2I5)
 
C  IF MUON IS NOT CUTTED, BRING IT TO OUTPUT
        IF ( IRET2 .EQ. 0 ) THEN
          CALL OUTPUT
        ENDIF
  1   CONTINUE
 
C  KILL MUON AS IT DECAYS OR INTERACTS BELOW LOWEST OBSLEVEL
      IRET1  = 1
      FMUORG = .FALSE.
      RETURN
 
C  MUON SCATTERS, DECAYS OR INTERACTS BEFORE PASSING OBSLEVEL
  2   CONTINUE
 
      IF ( IRET3 .NE. 0 ) THEN
C  ELIMINATE MUON IF BELOW CUTS
        IRET1  = 1
        FMUORG = .FALSE.
        RETURN
      ENDIF
C  MUON IS NOW UPDATED TO POINT OF INTERACTION
      DO  5  J = 1,8
        CURPAR(J) = PROPAR(J)
  5   CONTINUE
      BETA = SQRT( GAMMA**2 - 1.D0 ) / GAMMA
      IF ( FSCAT ) THEN
C  MUON HAS MADE MULTIPLE SCATTERING AND MUST NOW BE TRACKED FURTHER ON
        CHITOT = CHITOT - C(20)
        IF ( CHITOT .GT. 0.D0 ) THEN
          THICKH = THCKHN
          IF ( DEBUG ) WRITE(MDEBUG,457) (CURPAR(I),I=1,9)
 457      FORMAT(' MUTRAC: SCATTER',1P,9E10.3)
          GOTO 10
        ENDIF
      ENDIF
C  MUONS HAVE TO DECAY IMMEDIATELY OR TO UNDERGO BREMSSTR./PAIRPR.
      IF ( FDECAY ) THEN
        ALEVEL = H
        CALL MUDECY
        MUOND  = MUOND + 1.D0
        FMUORG = .FALSE.
C  MUDECY WRITES EM-PARTICLE TO STACK
      ELSE
        IF ( FMUBRM ) THEN
          CALL MUBREM
        ELSE
          CALL MUPRPR
        ENDIF
C  MUBREM AND MUPRPR WRITE EM-PARTICLES AND MUON TO STACK
      ENDIF
      IRET1 = 1
 
      RETURN
      END
