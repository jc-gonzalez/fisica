      SUBROUTINE BREMS
C                                VERSION 4.00  --  26 JAN 1986/1900
C******************************************************************
C   FOR ELECTRON ENERGY GREATER THAN 5.0 MEV, THE BETHE-HEITLER
C   CROSS SECTION IS EMPLOYED.
C******************************************************************
      DOUBLE PRECISION PEIE,PESG,PESE
      COMMON/BREMPR/DL1(6),DL2(6),DL3(6),DL4(6),DL5(6),DL6(6),DELCM, ALP
     *HI(2),BPAR(2),DELPOS(2),PWR2I(50)
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
      DATA AI2LN2/0.7213475/
C_____IF (NCLOCK.GT.JCLOCK) THEN
C______WRITE(MDEBUG,* )' BREMS: NP=',NP,' IR=',IR(NP),' IOBS=',IOBS(NP)
C______CALL AUSGB2
C_____END IF
      PEIE=E(NP)
      EIE=PEIE
      NP=NP+1
      IF (EIE.LT.50.0) THEN
       LVX=1
       LVL0=0
      ELSE
       LVX=2
       LVL0=3
      END IF
      ABREMS=REAL(IFIX(1.44269*ALOG(EIE*API)))
351   CONTINUE
       CALL RMMAR(RNNO06,1,2)
       IF (0.5.LT.((ABREMS*ALPHI(LVX)+0.5)*RNNO06)) THEN
        CALL RMMAR(RD,2,2)
        RNNO07=RD(1)
        RNNO08=RD(2)
        IDISTR=ABREMS*RNNO07
        P=PWR2I(IDISTR+1)
        LVL=LVL0+1
        IF (RNNO08.GE.AI2LN2) THEN
361      CONTINUE
          CALL RMMAR(RD,3,2)
          RNNO09=RD(1)
          RNNO10=RD(2)
          RNNO11=RD(3)
          H=MAX(RNNO10,RNNO11)
          BR=1.0-0.5*H
          IF((BR*RNNO09.LE.0.5))GO TO362
         GO TO 361
362      CONTINUE
        ELSE
         CALL RMMAR(RNNO12,1,2)
         BR=RNNO12*0.5
        END IF
        BR=BR*P
       ELSE
        CALL RMMAR(RD,2,2)
        RNNO13=RD(1)
        RNNO14=RD(2)
        BR=MAX(RNNO13,RNNO14)
        LVL=LVL0+2
       END IF
       ESG=EIE*BR
       IF((ESG.LT.AP))GO TO351
       PESG=ESG
       PESE=PEIE-PESG
       ESE=PESE
       IF((ESE.LT.RM))GO TO351
       DEL = BR/ESE
       IF((DEL.GE.DELPOS(LVX)))GO TO351
       DELTA = DELCM*DEL
       IF (DELTA.LT.1.0) THEN
        REJF=DL1(LVL)+DELTA*(DL2(LVL)+DELTA*DL3(LVL))
       ELSE
        REJF=DL4(LVL)+DL5(LVL)*LOG(DELTA+DL6(LVL))
       END IF
       CALL RMMAR(RNSCRN,1,2)
       IF((RNSCRN.LE.REJF))GO TO352
      GO TO 351
352   CONTINUE
      THETA=RM/EIE
      CALL UPHI(1,3)
      IF (ESG.LE.ESE) THEN
       IQ(NP)=1
       E(NP)=PESG
       E(NP-1)=PESE
      ELSE
       IQ(NP)=IQ(NP-1)
       IQ(NP-1)=1
       E(NP)=PESE
       E(NP-1)=PESG
       T=U(NP)
       U(NP)=U(NP-1)
       U(NP-1)=T
       T=V(NP)
       V(NP)=V(NP-1)
       V(NP-1)=T
       T=W(NP)
       W(NP)=W(NP-1)
       W(NP-1)=T
      END IF
      RETURN
      END
