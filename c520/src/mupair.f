      SUBROUTINE MUPAIR
C
C*********************************************************************
C  DESIGN  : D. HECK   IK3  FZK KARLSRUHE
C  DATE    : JUL  15, 1988
C*********************************************************************
C  IN ANALOGY WITH THE SUBROUTINE PAIR.
C  FOR A PHOTON ENERGY LESS THAN 434 MEV, THE APPROXIMATION IS
C  MADE THAT THE ENERGY OF ONE POSITIVE OR NEGATIVE MUON IS
C  UNIFORMLY DISTRIBUTED IN THE INTERVAL (RMMU, EIG/2)  =
C  (MUON REST MASS, PHOTON ENERGY/2).
C  FOR PHOTON ENERGY ABOVE 434 MEV THE
C  COULOMB CORRECTED BETHE-HEITLER CROSS SECTION IS USED.
C  (BUTCHER AND MESSEL, OP. CIT., P. 17-19, 22).
C  ========== THIS MAY BE INCORRECT ==========
C*********************************************************************
      DOUBLE PRECISION PEIG,PESE1,PESE2
      DOUBLE PRECISION ENERN
      COMMON/BREMPR/DL1(6),DL2(6),DL3(6),DL4(6),DL5(6),DL6(6),DELCM, ALP
     *HI(2),BPAR(2),DELPOS(2),PWR2I(50)
      DOUBLE PRECISION PRRMMU
      COMMON/MUON/PRRMMU,RMMU,RMMUT2
*KEEP,PARPAR.
      COMMON /PARPAR/  CURPAR,SECPAR,PRMPAR,OUTPAR,C,
     *                 E00,E00PN,PTOT0,PTOT0N,THICKH,ITYPE,LEVL
      DOUBLE PRECISION CURPAR(14),SECPAR(14),PRMPAR(14),OUTPAR(14),
     *                 C(50),E00,E00PN,PTOT0,PTOT0N,THICKH
      INTEGER          ITYPE,LEVL
*KEEP,POLAR.
      COMMON /POLAR/   POLART,POLARF
      DOUBLE PRECISION POLART,POLARF
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
*KEEP,STACKE.
      COMMON/STACKE/   E,TIME,X,Y,Z,U,V,W,DNEAR,IQ,IGEN,IR,IOBS,LPCTE,NP
      DOUBLE PRECISION E(60),TIME(60)
      REAL             X(60),Y(60),Z(60),U(60),V(60),W(60),DNEAR(60)
      INTEGER          IQ(60),IGEN(60),IR(60),IOBS(60),LPCTE(60),NP
*KEND.
      COMMON/THRESH/RMT2,RMSQ,ESCD2,AP,API,AE,UP,UE,TE,THMOLL
      COMMON/UPHIOT/THETA,SINTHE,COSTHE,SINPHI, COSPHI,PI,TWOPI,PI5D2
      DOUBLE PRECISION PZERO,PRM,PRMT2,RMI,VC
      COMMON/USEFUL/PZERO,PRM,PRMT2,RMI,VC,RM,MEDIUM,MEDOLD,IBLOBE,ICALL
      COMMON/ACLOCK/NCLOCK,JCLOCK
C_____IF (NCLOCK.GT.JCLOCK) THEN
C______WRITE(MDEBUG,* )' MUPAIR:NP=',NP,' IR=',IR(NP),' IOBS=',IOBS(NP)
C______CALL AUSGB2
C_____END IF
      IF(DEBUG)WRITE(MDEBUG,*)'MUPAIR: E=',E(NP)
      IGEN(NP) = IGEN(NP) + 1
C***  PRECISE ENERGY OF INCIDENT GAMMA
      PEIG=E(NP)
C ***  SUBTRACT EM SUBSHOWER FROM NKG CALCULATION
      IF ( FNKG ) THEN
        SECPAR(3) = W(NP)
        IF (U(NP)**2+V(NP)**2.GT.3.E-38) THEN
          ANGLEX = -ATAN2(V(NP),U(NP))
        ELSE
          ANGLEX = 0.
        END IF
        SECPAR(4) = ANGLEX
        SECPAR(5) = -Z(NP)
        ENERN = -PEIG*1.D-3
        CALL NKG(ENERN)
      ENDIF
C***  ENERGY OF INCIDENT GAMMA
      EIG=PEIG
      IF (EIG.LE.434.) THEN
C ***  BELOW 434.MEV, WE ASSUME UNIFORM ENERGY
C ***  DISTRIBUTION OF THE MUON #2 IN THE INTERVAL (RMMU, EIG/2).
C ***  SEE ALSO SLAC-265, P.49  FOR FURTHER DISCUSSION.
       CALL RMMAR(RNNO29,1,2)
       ESE2=(EIG*0.5-RMMU)*RNNO29+RMMU
      ELSE
C ***  ABOVE 434.MEV, MUST SAMPLE
C ***  COULOMB CORRECTED(LVX=2,LVL=4,6) CROSS SECTIONS.
C ***  SEE RELATED COMMENTS IN BREMS.
       LVX=2
       LVL0=3
181    CONTINUE
C  ***  RETRY IF REJECTED BECAUSE DEL OUT OF RANGE, OR BY SCREENING
C  ***  WE'LL NEED AT LEAST ONE RANDOM NUMBER
        CALL RMMAR(RD,2,2)
        RNNO30=RD(1)
C  ***  NOW DECIDE WHICH OF THE TWO SUBDISTRIBUTIONS TO USE.
        RNNO31=RD(2)
        IF (RNNO31.GE.BPAR(LVX)) THEN
C   ***  USE THE SUBDISTRIBUTION THAT IS PROPORTIONAL TO
C   ***  12*(BR-0.5)**2. IT USES A(DELTA) FOR SCREENING FUNCTION
         LVL=LVL0+1
         CALL RMMAR(RD,2,2)
         RNNO32=RD(1)
         RNNO33=RD(2)
C   ***  FROM SYMMETRY, ONLY NEED TO SAMPLE BR IN INTERVAL (0,.5)
         BR=0.5*(1.0-MAX(RNNO32,RNNO33,RNNO30))
        ELSE
C   ***  USE THE SUBDISTRIBUTION THAT IS PROPORTIONAL TO 1,I.E.
C   ***  UNIFORM.IT USES C(DELTA) FOR A SCREENING REJECT FUNCTION
         LVL=LVL0+3
         BR=RNNO30*0.5
        END IF
C  ***  THE SCREENING FUNCTIONS ARE FUNCTIONS OF DELTA=DELCM*DEL,
C  ***  WHERE DELCM= 136.0*EXP(ZG)*RM (SAME AS FOR BREMS)
C  ***  AND WHERE DEL=1./(EG0*BR*(1.0-BR))
C  ***  WITH EG0 = INCIDENT PHOTON ENERGY AND BR=ENERGY FRACTION.
        IF((BR.EQ.0.0))GO TO181
C  ***  TO AVOID DIVISION BY ZERO
        DEL=1.0/(EIG*BR*(1.0-BR))
        IF((DEL.GE.(RM/RMMU)*DELPOS(LVX)))GO TO181
C  ***  NEXT TRY
C  ***  THE PRECEDING CONDITION ENSURES THAT A(DELTA) AND C(DELTA)
C  ***  WILL BE POSITIVE. IF IT IS NOT SATISFIED,LOOP BACK AND TRY
C  ***  ANOTHER SAMPLE.
        DELTA=(RMMU*RMI)*DELCM*DEL
        IF (DELTA.LT.1.0) THEN
         REJF=DL1(LVL)+DELTA*(DL2(LVL) +DELTA*DL3(LVL))
        ELSE
         REJF=DL4(LVL)+DL5(LVL) *LOG(DELTA+DL6(LVL))
        END IF
C  ***  RANDOM NUMBER FOR SCREENING REJECTION
        CALL RMMAR(RNSCRN,1,2)
C  ***  RETRY UNTIL ACCEPTED
        IF((RNSCRN.LE.REJF))GO TO182
       GO TO 181
182    CONTINUE
C ***  BR=PRODUCT ENERGY FRACTION
C ***  ENERGY OF SECONDARY 'MUON' #2
       ESE2=BR*EIG
C ***  END OF EIG.GT.434 ELSE
      END IF
C***  ENERGY GOING TO LOWER SECONDARY HAS NOW BEEN DETERMINED
C***  PRECISE ENERGY OF SECONDARY 'MUON' 2
      PESE2=ESE2
C***  PRECISE ENERGY OF SECONDARY 'MUON' 1
      PESE1=PEIG-PESE2
      E(NP)=PESE1
      E(NP+1)=PESE2
C***  THIS AVERAGE ANGLE OF EMISSION FOR BOTH PAIR PRODUCTION AND
C***  BREMSSTRAHLUNG IS MUCH SMALLER THAN THE AVERAGE ANGLE OF
C***  MULTIPLE SCATTERING FOR DELTA T TRANSPORT=0.01 R.L.
C***  THE INITIAL AND FINAL MOMENTA ARE COPLANAR
C***  SET UP A NEW 'MUON'
      THETA=RMMU/EIG
      CALL UPHI(1,1)
C***  SET UP A NEW 'MUON'
      NP=NP+1
      SINTHE=-SINTHE
      CALL UPHI(3,2)
C***  NOW RANDOMLY DECIDED WHICH IS POSITIVE MUON, AND SET
C***  CHARGES ACCORDINGLY
      CALL RMMAR(RNNO34,1,2)
      IF (RNNO34.LE.0.5) THEN
C ***  POSITIVE MUON ON TOP
       IQ(NP)=5
       IQ(NP-1)=6
      ELSE
C ***  NEGATIVE MUON ON TOP
       IQ(NP)=6
       IQ(NP-1)=5
      END IF
      CALL RMMAR(RD,2,2)
      RNPOLT=RD(1)
      RNPOLF=RD(2)
      POLART=2.*RNPOLT-1.
      POLARF=TWOPI*RNPOLF
      RETURN
      END