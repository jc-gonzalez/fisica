      SUBROUTINE EGS4( EEIN )
 
C---------------------------------------------------------------------
C  E(LECTRON) G(AMMA) S(HOWER)
C
C  TREATES ELECTROMAGNETIC SUBSHOWER
C  THIS SUBROUTINE PACKAGE IS CALLED FROM EM
C  ARGUMENT:
C   EEIN   = (R8) INCOMING PARTICLE ENERGY (GEV)
C
C  DESIGN  : D. HECK   IK3  FZK KARLSRUHE
C---------------------------------------------------------------------
 
      DOUBLE PRECISION EEIN
*KEEP,GENER.
      COMMON /GENER/   GEN,ALEVEL
      DOUBLE PRECISION GEN,ALEVEL
*KEND.
      COMMON/GEOM/ZALTIT,BOUND(6),NEWOBS,OBSLVL(10)
*KEEP,LONGI.
      COMMON /LONGI/   APLONG,HLONG,PLONG,SPLONG,THSTEP,THSTPI,
     *                 NSTEP,LLONGI,FLGFIT
      DOUBLE PRECISION APLONG(0:1040,9),HLONG(0:1024),PLONG(0:1040,9),
     *                 SPLONG(0:1040,9),THSTEP,THSTPI
      INTEGER          NSTEP
      LOGICAL          LLONGI,FLGFIT
*KEND.
      COMMON/MISC/KMPI,KMPO,DUNIT,NOSCAT,MED(6),RHOR(6),IRAYLR(6)
*KEEP,OBSPAR.
      COMMON /OBSPAR/  OBSLEV,THCKOB,XOFF,YOFF,THETAP,PHIP,
     *                 THETPR,PHIPR,NOBSLV
      DOUBLE PRECISION OBSLEV(10),THCKOB(10),XOFF(10),YOFF(10),
     *                 THETAP,THETPR(2),PHIP,PHIPR(2)
      INTEGER          NOBSLV
*KEEP,PARPAR.
      COMMON /PARPAR/  CURPAR,SECPAR,PRMPAR,OUTPAR,C,
     *                 E00,E00PN,PTOT0,PTOT0N,THICKH,ITYPE,LEVL
      DOUBLE PRECISION CURPAR(14),SECPAR(14),PRMPAR(14),OUTPAR(14),
     *                 C(50),E00,E00PN,PTOT0,PTOT0N,THICKH
      INTEGER          ITYPE,LEVL
*KEND.
      DOUBLE PRECISION PI0MSQ
      COMMON/PION/PI0MSQ,PITHR,PICMAS,PI0MAS,AMASK0,AMASKC,AMASPR,AMASNT
*KEEP,REJECT.
      COMMON /REJECT/  AVNREJ,
     *                 ALTMIN,ANEXP,THICKA,THICKD,CUTLN,EONCUT,
     *                 FNPRIM
      DOUBLE PRECISION AVNREJ(10)
      REAL             ALTMIN(10),ANEXP(10),THICKA(10),THICKD(10),
     *                 CUTLN,EONCUT
      LOGICAL          FNPRIM
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
*KEEP,STACKE.
      COMMON/STACKE/   E,TIME,X,Y,Z,U,V,W,DNEAR,IQ,IGEN,IR,IOBS,LPCTE,NP
      DOUBLE PRECISION E(60),TIME(60)
      REAL             X(60),Y(60),Z(60),U(60),V(60),W(60),DNEAR(60)
      INTEGER          IQ(60),IGEN(60),IR(60),IOBS(60),LPCTE(60),NP
*KEND.
      COMMON/THRESH/RMT2,RMSQ,ESCD2,AP,API,AE,UP,UE,TE,THMOLL
      DOUBLE PRECISION PZERO,PRM,PRMT2,RMI,VC
      COMMON/USEFUL/PZERO,PRM,PRMT2,RMI,VC,RM,MEDIUM,MEDOLD,IBLOBE,ICALL
      COMMON/ACLOCK/NCLOCK,JCLOCK
      DOUBLE PRECISION THICK
      CHARACTER MEDARR*24
      DATA MEDARR/'AIR-NTP                 '/
C---------------------------------------------------------------------
 
      IF((DEBUG))WRITE(MDEBUG,* )'EGS4  :'
C***  CONVERSION GEV --> MEV
      E(1)= EEIN*1000.D0
C***  CHECK ENERGY RANGE
      IQ(1)=NINT(SECPAR(1))
      IF ( IQ(1) .EQ. 1 ) THEN
       IF ( E(1) .GT. UP ) THEN
        CALL AUSGB2
        WRITE(KMPO,91) EEIN
91      FORMAT(' EGS4  : ENERGY OF GAMMA =',1P,E10.3,' GEV TOO HIGH')
        STOP
       ENDIF
      ELSE
       IF ( E(1) .GT. UE ) THEN
        CALL AUSGB2
        WRITE(KMPO,92) EEIN
92      FORMAT(' EGS4  :ENERGY OF ELECTRON/POSITRON =',1P,E10.3,
     *         ' GEV TOO HIGH')
        STOP
       ENDIF
      ENDIF
C***  FILL IN STARTING COORDINATES
      NP=1
      TIME(1)=SECPAR(6)
      X(1)=SECPAR(7)
      Y(1)=-SECPAR(8)
C***  STARTS IN HEIGHT 'Z' DOWNWARDS
      Z(1)=-SECPAR(5)
      IF (LLONGI) LPCTE(1)=MIN(NSTEP,INT(THICK(SECPAR(5))*THSTPI)+1)
      SITHET=SQRT(1.D0-SECPAR(3)**2)
C***  START DIRECTION COSINES
      U(NP)=SITHET*COS(-SECPAR(4))
      V(NP)=SITHET*SIN(-SECPAR(4))
      W(NP)=SECPAR(3)
C*** RENORMALIZATION OF DIRECTION COSINES
      RADINV=1.5-0.5*(U(NP)**2+V(NP)**2+W(NP)**2)
      U(NP)=U(NP)*RADINV
      V(NP)=V(NP)*RADINV
      W(NP)=W(NP)*RADINV
      DNEAR(1)=0.
      IGEN(1)=GEN
       DO 101 K=1,5
C ***  DETERMINE START REGION
       IF (-BOUND(K).LE.Z(1) .AND. -BOUND(K+1).GT.Z(1)) THEN
        IR(1)=K+1
        GO TO 110
       END IF
101   CONTINUE
102   CONTINUE
      CALL AUSGB2
      WRITE(KMPO,120) -Z(1)*0.00001
120   FORMAT (' EGS4  : START VALUE OF Z=',1P,E10.3,
     *' KM  NOT IN ATMOSPHERE')
      STOP
110   CONTINUE
       DO 111 IDET=1,NOBSLV
C ***  DETERMINE NEXT OBSERVATION LEVEL
       IF (-Z(1).GE.OBSLVL(IDET)) THEN
        IOBS(1)=IDET
        GO TO 130
       END IF
111   CONTINUE
112   CONTINUE
      CALL AUSGB2
      WRITE(KMPO,140) -Z(1)*0.01,OBSLVL(NOBSLV)*0.01
140   FORMAT(' EGS4  : START VALUE OF Z= ',E10.3, ' M  BELOW LOWEST DET'
     *, 'ECTOR AT',E10.3,' M')
      STOP
C***  NEWOBS IS THE NEXT OBSERVATION LEVEL
130   NEWOBS=IOBS(NP)
      CALL SHOWER
      IF(DEBUG) WRITE(MDEBUG,*) 'EGS4  : EGS-STACK EMPTY, EXIT'
      RETURN
      END
