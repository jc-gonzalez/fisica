      SUBROUTINE CGHINI
 
C-----------------------------------------------------------------------
C  C(ORSIKA) GH(EISHA) INI(TIALISATION)
C  INITIALIZATION OF RELEVANT GHEISHA VARIABLES
C  THIS SUBROUTINE IS CALLED FROM START
C
C  ORIGIN  : GHEISHA ROUTINE "GHEINI", F.CARMINATI
C  REDESIGN: P. GABRIEL IK1  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
*KEEP,AIR.
      COMMON /AIR/     COMPOS,PROBTA,AVERAW,AVOGAD
      DOUBLE PRECISION COMPOS(3),PROBTA(3),AVERAW,AVOGAD
*KEEP,CGCOMP.
      PARAMETER (KK=3)
      COMMON/CGCOMP/ ACOMP,ZCOMP,WCOMP
      REAL           ACOMP(KK),ZCOMP(KK),WCOMP(KK)
*KEEP,PAM.
      COMMON /PAM/     PAMA,SIGNUM
      DOUBLE PRECISION PAMA(6000),SIGNUM(6000)
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
 
      COMMON/GSECTI/ AIEL(20),AIIN(20),AIFI(20),AICA(20),ALAM,K0FLAG
      INTEGER K0FLAG
      REAL AIEL,AIIN,AIFI,AICA,ALAM
 
C --- GHEISHA COMMONS ---
C --- INITIALIZATION FLAGS FOR VARIOUS GHEISHA ROUTINES ---
      COMMON /KGINIT/ KGINIT(50)
 
      COMMON/CONSTS/ PI,TWPI,PIBTW,MP,MPI,MMU,MEL,MKCH,MK0,SMP,SMPI,
     $               SMU,CT,CTKCH,CTK0,
     $               ML0,MSP,MS0,MSM,MX0,MXM,CTL0,CTSP,CTSM,CTX0,CTXM,
     $               RMASS(35),RCHARG(35)
 
                     REAL MP,MPI,MMU,MEL,MKCH,MK0,
     *                    ML0,MSP,MS0,MSM,MX0,MXM
 
      PARAMETER (MXGKGH=100)
      PARAMETER (MXEVEN=12*MXGKGH)
      COMMON/EVENT / NSIZE,NCUR,NEXT,NTOT,EVE(MXEVEN)
 
      COMMON/PRNTFL/INBCD,NEWBCD,INBIN,NEWBIN,NPEVT,NEVTP,LPRT,NPRT(10)
                    LOGICAL LPRT,NPRT
 
      PARAMETER (MXGKPV=MXGKGH)
      COMMON /VECUTY/ PV(10,MXGKPV)
 
C --- BOUNDARY LIMITS FOR ARGUMENTS OF INTRINSIC FUNCTIONS ---
C --- XL DENOTES LOWER BOUND WHEREAS XU DENOTES UPPER BOUND ---
      COMMON /LIMITS/ EXPXL,EXPXU
 
 
C --- "NEVENT" CHANGED TO "KEVENT" IN COMMON /CURPAR/ DUE TO CLASH ---
C --- WITH VARIABLE "NEVENT" IN GEANT COMMON ---
 
      PARAMETER (MXGKCU=MXGKGH)
      COMMON /CURPAR/ WEIGHT(10),DDELTN,IFILE,IRUN,NEVT,KEVENT,SHFLAG,
     $                ITHST,ITTOT,ITLST,IFRND,TOFCUT,CMOM(5),CENG(5),
     $                RS,S,ENP(10),NP,NM,NN,NR,NO,NZ,IPA(MXGKCU),
     $                ATNO2,ZNO2
 
      DATA CLIGHT /2.99792458E10/
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'CGHINI:'
 
C --- INITIALZE COMPOSITION OF AIR
      WCOMP(1) = COMPOS(1)
      WCOMP(2) = COMPOS(2)
      WCOMP(3) = COMPOS(3)
      ACOMP(1) = 14.
      ACOMP(2) = 16.
      ACOMP(3) = 40.
      ZCOMP(1) =  7.
      ZCOMP(2) =  8.
      ZCOMP(3) = 18.
 
C --- SET GHEISHA I/O UNITS TO THE SAME AS FOR CORSIKA --
      INBCD=MONIIN
      NEWBCD=MONIOU
      IF ( DEBUG ) NEWBCD=MDEBUG
 
C --- INITIALISE ALL GHEISHA PRINT FLAGS AS FALSE ---
C --- ACTIVATION IS DONE BY "DEBUG" STEERING CARD ---
      DO 11 J=1,10
         NPRT(J)=.FALSE.
 11   CONTINUE
      IF ( DEBUG ) THEN
         NPRT(4)=.TRUE.
         NPRT(9)=.TRUE.
      ENDIF
      LPRT=.FALSE.
      DO 12 I=1,MXGKPV
         DO 12 J=1,10
            PV(J,I)=0.
 12   CONTINUE
 
C --- INITIALISE KGINIT ARRAY ---
      DO 20 J=1,50
         KGINIT(J)=0
 20   CONTINUE
 
C --- INITIALIZE SOME CUT-OFF PARAMETERS WITH GEANT VALUES ---
      TOFCUT=1.0E+20
      NSIZE=MXEVEN
      K0FLAG=0
      CENG(3)=0.
      CENG(4)=0.
 
C --- INITIALIZE PI, 2*PI, PI/2 AND PARTICLE PARAMETERS ---
      PI=ACOS(-1.0)
      TWPI=2.0*PI
      PIBTW=PI/2.0
C *** GAMMA ***
      RMASS(1)=PAMA(1)
      RCHARG(1)=0.0
C *** NEUTRINO ***
      RMASS(2)=PAMA(4)
      RCHARG(2)=0.0
C *** E+ ***
      RMASS(3)=PAMA(2)
      RCHARG(3)=1.0
C *** E- ***
      RMASS(4)=PAMA(3)
      RCHARG(4)=-1.0
C *** MU+ ***
      RMASS(5)=PAMA(5)
      RCHARG(5)=1.0
C *** MU- ***
      RMASS(6)=PAMA(6)
      RCHARG(6)=-1.0
C *** PI+ ***
      RMASS(7)=PAMA(8)
      RCHARG(7)=1.0
      CT=780.4
C *** PI0 ***
      RMASS(8)=PAMA(7)
      RCHARG(8)=0.0
C *** PI- ***
      RMASS(9)=PAMA(9)
      RCHARG(9)=-1.0
C *** K+ ***
      RMASS(10)=PAMA(11)
      RCHARG(10)=1.0
      CTKCH=370.9
C *** K0 SHORT (==> K0) ***
      RMASS(11)=PAMA(16)
      RCHARG(11)=0.0
      CTK0=2.675
C *** K0 LONG (==> K0 BAR) ***
      RMASS(12)=-PAMA(10)
      RCHARG(12)=0.0
C *** K- ***
      RMASS(13)=PAMA(12)
      RCHARG(13)=-1.0
C *** P ***
      RMASS(14)=PAMA(14)
      RCHARG(14)=1.0
C *** P BAR ***
      RMASS(15)=-PAMA(15)
      RCHARG(15)=-1.0
C *** N ***
      RMASS(16)=PAMA(13)
      RCHARG(16)=0.0
C *** N BAR ***
      RMASS(17)=-PAMA(25)
      RCHARG(17)=0.0
C *** L0 ***
      RMASS(18)=PAMA(18)
      RCHARG(18)=0.0
      CTL0=7.89
C *** L0 BAR ***
      RMASS(19)=-PAMA(26)
      RCHARG(19)=0.0
C *** S+ ***
      RMASS(20)=PAMA(19)
      RCHARG(20)=1.0
      CTSP=2.40
C *** S0 ***
      RMASS(21)=PAMA(20)
      RCHARG(21)=0.0
C *** S- ***
      RMASS(22)=PAMA(21)
      RCHARG(22)=-1.0
      CTSM=4.44
C *** S+ BAR ***
      RMASS(23)=-PAMA(27)
      RCHARG(23)=-1.0
C *** S0 BAR ***
      RMASS(24)=-PAMA(28)
      RCHARG(24)=0.0
C *** S- BAR ***
      RMASS(25)=-PAMA(29)
      RCHARG(25)=1.0
C *** XI0 ***
      RMASS(26)=PAMA(22)
      RCHARG(26)=0.0
      CTX0=8.69
C *** XI- ***
      RMASS(27)=PAMA(23)
      RCHARG(27)=-1.0
      CTXM=4.92
C *** XI0 BAR ***
      RMASS(28)=-PAMA(30)
      RCHARG(28)=0.0
      CTX0=8.69
C *** XI- BAR ***
      RMASS(29)=-PAMA(31)
      RCHARG(29)=1.0
C *** DEUTERON ***
      RMASS(30)=PAMA(45)
      RCHARG(30)=1.0
C *** TRITON ***
      RMASS(31)=PAMA(46)
      RCHARG(31)=1.0
C *** ALPHA ***
      RMASS(32)=PAMA(47)
      RCHARG(32)=2.0
C *** OMEGA- ***
      RMASS(33)=PAMA(24)
      RCHARG(33)=-1.0
C *** OMEGA- BAR ***
      RMASS(34)=-PAMA(32)
      RCHARG(34)=1.0
C *** NEW PARTICLE (GEANTINO) ***
      RMASS(35)=0.0
      RCHARG(35)=0.0
 
      IF (NPRT(9))
     $ WRITE(MDEBUG,1000) (I,RMASS(I),RCHARG(I),I=1,33),
     $            CT,CTKCH,CTK0,CTL0,CTSP,CTSM,CTX0,CTXM
 1000 FORMAT(' *CGHINI* === GHEISHA PARTICLE PROPERTIES ==='/
     $ '0INDEX',5X,'MASS (GEV)',5X,'CHARGE'/1H /
     $ 33(1H ,1X,I3,5X,F11.6,6X,F5.2/),
     $ '0PI +-  CT = ',G12.5,' K  +-  CT = ',G12.5/
     $ ' K0     CT = ',G12.5,' L0     CT = ',G12.5/
     $ ' S+     CT = ',G12.5,' S-     CT = ',G12.5/
     $ ' X0     CT = ',G12.5,' X-     CT = ',G12.5)
 
      MP=RMASS(14)
      MPI=RMASS(7)
      MMU=RMASS(5)
      MEL=RMASS(3)
      MKCH=RMASS(10)
      MK0=RMASS(11)
      SMP=MP**2
      SMPI=MPI**2
      SMU=MMU**2
      ML0=RMASS(18)
      MSP=RMASS(20)
      MS0=RMASS(21)
      MSM=RMASS(22)
      MX0=RMASS(26)
      MXM=RMASS(27)
 
C --- LOAD LIMITS FOR INTRINSIC FUNCTION ARGUMENTS ---
      EXPXL = - 82.0
      EXPXU =   82.0
 
      IF (NPRT(9)) WRITE(MDEBUG,1001) EXPXL,EXPXU
 1001 FORMAT(' *GHEINI* === INTRINSIC FUNCTION BOUNDARIES ==='/
     $ ' EXPXL,EXPXU = ',2(G12.5,1X))
 
      RETURN
      END
