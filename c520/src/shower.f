      SUBROUTINE SHOWER
C
C*********************************************************************
C  DESIGN  : D. HECK   IK3  FZK KARLSRUHE
C  DATE    : AUG  11, 1988
C*********************************************************************
C  THIS ROUTINE LOOKS, WHAT IS ON TOP OF STACK, AND CALLS THE
C  APPROPRIATE ROUTINE TO TREAT THIS PARTICLE.
C*********************************************************************
      COMMON/MISC/KMPI,KMPO,DUNIT,NOSCAT,MED(6),RHOR(6),IRAYLR(6)
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
C***  TAKE FIRST PARTICLE IN STACK
      NP=1
      IF((DEBUG))CALL AUSGB2
251   CONTINUE
C ***  DECIDE WHAT IS ON TOP OF STACK
261    CONTINUE
C  ***  JUMP TO PARTICLE IN QUESTION
C  ***    THE FOLLOWING PARTICLE IDENTIFICATION IS MADE BY THE
C  ***    VALUE OF IQ(NP) (ACCORDING TO PROGRAM 'GEANT')
C  ***    IQ    =  1         PHOTON
C  ***          =  2         POSITRON E (+)
C  ***          =  3         ELECTRON E (-)
C  ***          =  5         POSITIVE MUON (+)
C  ***          =  6         NEGATIVE MUON (-)
C  ***          =  7         NEUTRAL  PION (0)
C  ***          =  8         POSITIVE PION (+)
C  ***          =  9         NEGATIVE PION (-)
C  ***    IF IQ =  OTHER VALUE, JUMP TO ERROR MESSAGE
        GO TO(270,280,280,290, 300,300,300,300,300) (IQ(NP))
C  ***  IQ OUT OF RANGE?
290     WRITE(KMPO,320) IQ(NP)
320     FORMAT(' SHOWER: PARTICLE TYPE ',I5,' NOT IDENTIFIED')
        CALL AUSGB2
        NP=NP-1
        GO TO262
300     CALL MPPROP
        GO TO262
270     CALL PHOTON(IRCODE)
C  ***  PHOTON DISCARDED ?
        IF((IRCODE.EQ.2))GO TO262
        IF((IQ(NP).LT.2 .OR. IQ(NP).GT.3))GO TO261
280     CALL ELECTR(IRCODE)
C  ***  ELECTRON DISCARDED ?
        IF((IRCODE.EQ.2))GO TO262
        IF((IQ(NP).EQ.1))GO TO 270
C  ***  LOOP BACK UP TO PARTICLE SELECTION
       GO TO 261
262    CONTINUE
C ***  CHECK TO SEE IF ANYTHING LEFT ON STACK
C ***  NOTHING ON STACK, SO JUMP OUT OF LOOP
       IF((NP.LE.0))GO TO252
      GO TO 251
252   CONTINUE
C***  TOP STACK LOOP END
      RETURN
      END
