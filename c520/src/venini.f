      SUBROUTINE VENINI
 
C-----------------------------------------------------------------------
C  VEN(US) INI(TIALISATION)
C
C  FIRST INITIALIZATION OF VENUS ARRAYS AND PARAMETERS
C  THIS SUBROUTINE IS CALLED FROM START
C
C  DESIGN   : D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
*KEEP,AIR.
      COMMON /AIR/     COMPOS,PROBTA,AVERAW,AVOGAD
      DOUBLE PRECISION COMPOS(3),PROBTA(3),AVERAW,AVOGAD
*KEEP,PAM.
      COMMON /PAM/     PAMA,SIGNUM
      DOUBLE PRECISION PAMA(6000),SIGNUM(6000)
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
*KEEP,VENPAR.
      COMMON /VENPAR/  PARVAL,NPARAM,PARCHA
      REAL             PARVAL(100)
      INTEGER          NPARAM
      CHARACTER*6      PARCHA(100)
*KEEP,VENUS.
      COMMON /VENUS/   ISH0,IVERVN,MTAR99,FVENUS,FVENSG
      INTEGER          ISH0,IVERVN,MTAR99
      LOGICAL          FVENUS,FVENSG
*KEND.
 
      PARAMETER (KOLLMX=2500)
      PARAMETER (MXEPS=10)
      PARAMETER (MXTAU=4)
      PARAMETER (MXVOL=10)
      PARAMETER (NGAU=129)
      PARAMETER (NDEP=129)
      PARAMETER (NDET=129)
      PARAMETER (NPTF=129)
      PARAMETER (NPTJ=129)
      PARAMETER (NSTRU=2049)
      COMMON /ACCUM/   AMSAC,ILAMAS,IMSG,INOIAC,IPAGE,JERR,NAEVT,NREVT
     *                ,NRPTL,NRSTR,NTEVT
      COMMON /CDEN/    MASSNR,RMX,R0
      COMMON /CGAU/    QGAU(NGAU),XGAU(NGAU)
      COMMON /CIUTOT/  IUTOTC,IUTOTE
      COMMON /CJINTC/  CLUST(MXTAU,MXVOL,MXEPS)
      COMMON /CJINTD/  VOLSUM(MXTAU),VO2SUM(MXTAU),NCLSUM(MXTAU)
      COMMON /CLEP/    ICINPU,IDSCAT
      COMMON /CNSTA/   AINFIN,PI,PIOM,PROM
      COMMON /COL/     BIMP,BMAX,COORD(4,KOLLMX),DISTCE(KOLLMX)
     *                ,QDEP(NDEP),QDET14(NDET),QDET16(NDET),QDET40(NDET)
     *                ,QDET99(NDET),RMPROJ,RMTARG(4),XDEP(NDEP)
     *                ,XDET14(NDET),XDET16(NDET),XDET40(NDET)
     *                ,XDET99(NDET)
     *                ,KOLL,LTARG,NORD(KOLLMX),NPROJ,NRPROJ(KOLLMX)
     *                ,NRTARG(KOLLMX),NTARG
      COMMON /CPTF/    FPTFS,FPTFSS,FPTFU,FPTFUS,FPTFUU
     *                ,QPTFS(NPTF),QPTFSS(NPTF),QPTFU(NPTF),QPTFUS(NPTF)
     *                ,QPTFUU(NPTF),XPTF(NPTF)
      COMMON /CPTJ/    QPTJ(NPTJ),XPTJ(NPTJ)
      COMMON /CPTLU/   NPTLU
      COMMON /CQUAMA / QUAMA
      DOUBLE PRECISION SEEDC,SEEDI
      COMMON /CSEED/   SEEDC,SEEDI
      COMMON /CVSN/    IVERSN
      COMMON /EPSCR/   EPSCRI
      COMMON /FILES/   IFCH,IFDT,IFHI,IFMT,IFOP
      COMMON /NEVNT/   NEVNT
      COMMON /PARO1/   AMPRIF,AMSIAC,BMAXIM,BMINIM,CORE,CUTMSQ,CUTMSS
     *                ,DELMSS,DELREM,FCTRMX,GAUMX,OVERLP,PAREA,PDIQUA
     *                ,PHARD,PSPINL,PSPINH,PISPN,PTF,PTH,PTMX,PTQ,PUD
     *                ,PVALEN,QSEPC,QSETC,QMUST,QVAPC,QVATC,RADIAC
     *                ,RADIAS,RSTRAS,SIGJ,SIGPPI,TAUMAX,TAUMIN
     *                ,TAUMX,TAUNLL,TENSN,THEMAS,WPROJ,WTARG,WTMINI
     *                ,WTSTEP,XCUT
     *                ,IAQU,IFRADE,IOJINT,IOPBRK,IOPENT,IOPENU
     *                ,IOPTF,IOPTQ,IRESCL,IWCENT,KENTRO,KO1KO2
     *                ,LABSYS,MAXRES,NCLEAN,NCOLMX,NDECAW,NEQMN,NEQMX
     *                ,NSTTAU,NTRYMX,NUMTAU
      COMMON /PARO2/   AMPROJ,AMTARG,ANGMUE,ELEPTI,ELEPTO,ENGY
     *                ,PNLL,PNLLX,PROB(99),PROSEA,RHOPHI,TAUREA
     *                ,YHAHA,YMXIMI,YPJTL
     *                ,ICBAC(99,2),ICFOR(99,2),ICHOIC,ICLHIS,IDPM
     *                ,IDPROJ,IDTARG,IENTRO,IJPHIS,IMIHIS,IPAGI,ISH
     *                ,ISHEVT,ISHSUB,ISPALL,ISPHIS,ISTMAX,ISUP,IVI
     *                ,JPSI,JPSIFI,KUTDIQ,LAPROJ,LATARG,MAPROJ,MATARG
     *                ,MODSHO,NDECAX,NDECAY,NEVENT
      COMMON /PARO3/   ASUHAX(7),ASUHAY(7),OMEGA,SIGPPD,SIGPPE,UENTRO
     *                ,IWZZZZ
      COMMON /PARO4/   GRICEL,GRIDEL,GRIGAM,GRIRSQ,GRISLO
      COMMON /PARO5/   DELEPS,DELVOL
      COMMON /QUARKM/  SMAS,SSMAS,USMAS,UUMAS
      COMMON /STRU2/   DELTA0,DELTA1,QSEH(NSTRU),QSEPI(NSTRU)
     *                ,QVAH(NSTRU),QVAPI(NSTRU),XSE(NSTRU),XVA(NSTRU)
      COMMON /VENLIN/  PTQ1,PTQ2,PTQ3,QMUST1,QMUST2,QMUST3
     *                ,IDTABL(100)
 
      EXTERNAL         SDENSI,SGAU,SPTF,SPTJ
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'VENINI:'
 
      IFMT = MONIOU
      IFCH = MDEBUG
      ICHOIC = 2
      NEVNT = 0
 
C     VERSION NUMBER
C     --------------
         IVERSN=4125
         IVERVN=IVERSN
 
C     FRAGMENTATION PARAMETERS/OPTIONS
C     --------------------------------
C     PROB. FOR U OR D QUARK PRODUCTION ( =(1-P_STRANGE)/2 ):
         PUD=0.455
C     QQ-QQBAR PROBABILITY
         PDIQUA=0.12
C     SPIN PROBABILITIES (FOR LIGHT AND HEAVY FLAVOURS):
         PSPINL=0.50
         PSPINH=0.75
C     ISOSPIN PROBABILITY:
         PISPN=0.50
C     OPTION FOR P_T DISTRIBUTION (1=EXPONENTIAL,2=GAUSSIAN):
         IOPTF=1
C     AVERAGE P_TRANSVERSE
         PTF=0.40
C     STRING TENSION:
         TENSN=1.0
C     STRING DECAY PARAMETER
         PAREA=.60
C     THRESHOLD RESONANCE -> STRING
         DELREM=1.0
C     CUTOFF FOR KMAXOR BEYOND WHICH PDIQ=0 IN SR JSPLIT
         KUTDIQ=4
C     OPTION FOR BREAKING PROCEDURE (1=AMOR,2=SAMBA)
         IOPBRK=1
 
C     PROTON-PROTON PARAMETERS/OPTIONS
C     --------------------------------
C     OPTION FOR QUARK P_T DISTRIBUTION (1=EXPONENTIAL,2=GAUSSIAN,3=POWE
         IOPTQ=2
C     MEAN TRANSVERSE MOMENTUM OF QUARKS
C      (Q1+Q2*LN(E)+Q3*LN(E)**2, E=SQRT(S)):
         PTQ1=0.260
         PTQ2=0.
         PTQ3=0.
C     PROBABILITY FOR SEMIHARD INTERACTION (NOT USED IF NEGATIVE):
C        PHARD=-1.0
C     CUTOFF PARAMETER FOR P_T DISTR. FOR SEMIHARD INTERACTIONS:
         PTH=1.0
C     EFFECTIVE RATIO OF STRANGE SEA OVER U SEA:
         RSTRAS=0.
C     EFFECTIVE CUTOFF MASS IN STRUCTURE FUNCTIONS:
         CUTMSQ=2.0
         CUTMSS=0.001
C     VALENCE QUARK FRACTION IN CASE OF DIFFRACTIVE INTERACTION
         PVALEN=0.30
C     PHASE SPACE PARAMETERS:
         DELMSS=0.300
 
C     GRIBOV-REGGE-THEORY PARAMETERS
C     ------------------------------
C     GAMMA (IN FM**2):
         GRIGAM=3.64*0.04
C     R**2(IN FM**2):
         GRIRSQ=3.56*0.04
C     DELTA=INTERCEPT OF REGGE TRAJECTORY-1:
         GRIDEL=0.07
C     SLOPE OF REGGE TRAJECTORY (IN FM**2):
         GRISLO=0.25*0.04
C     C (DETERMINES RELATIVE WEIGHT OF ELASTIC AND DIFFR CROSS SCTN):
         GRICEL=1.5
 
C     NUCLEUS-NUCLEUS PARAMETERS
C     --------------------------
C     HARD CORE DISTANCE:
         CORE=0.8
C     JPSI NUCLEON CROSS SECTION (FM**2):
         SIGJ=0.2
 
C     RESCATTERING PARAMETERS
C     -----------------------
C     REACTION TIME:
         TAUREA=1.5
C     OVERLAP PARAMETER (NOT USED IF NEGATIVE)
         OVERLP=-1.0
C     BARYON RADIUS:
         RADIAC=0.65
C     MESON RADIUS:
         RADIAS=0.35
C     CRITICAL ENERGY DENSITY (<0 TO AVOID SECONDARY INTERACTIONS):
         EPSCRI=1.0
C     BARYON ENERGY DENSITY
         EPSBAR=2.0
C     INTERACTION MASS:
         AMSIAC=0.8
C     OPTION TO CALL JINTA1 (1) OR JINTA2 (2)
         IOJINT=2
C     PRINT OPTIONS
         AMPRIF=0.
         DELVOL=1.0
         DELEPS=1.0
 
C     CLUSTER DECAY PARAMETERS/OPTIONS
C     --------------------------------
C     CORRELATION LENGTH
C     (CORLEN>1.0: FIRST FIX SHORT CLUSTER BREAKING
         CORLEN=1.0
C     MINIMUM MASS
         AMUSEG=3.0
C     BAG CONSTANT -1/4
         BAG4RT=0.200
C     OPTION FOR ENTROPY CALCULATION:
C      IOPENT=0: ZERO ENTROPY
C      IOPENT=1: OSCILLATOR MODEL (0 FOR K.LE.UENTRO)
C      IOPENT=2: FERMI GAS WITH CONST VOLUME (0 FOR K.LE.UENTRO)
C      IOPENT=3: FERMI GAS WITH CONST DENSITY (0 FOR K.LE.UENTRO)
C      IOPENT=4: FERMI GAS WITH CONST VOLUME - NEW (0 FOR K.LE.UENTRO)
C      IOPENT=5: RESONANCE GAS (HAGEDORN) (0 FOR U.LE.UENTRO)
         IOPENT=5
         UENTRO=4.0
         KENTRO=100000
C     DECAY TIME (COMOVING FRAME):
         TAUNLL=1.0
C     OSCILLATOR QUANTUM
         OMEGA=0.500
 
C     PRESENTLY NOT USED
C     ------------------
 
C     CLUSTER DECAY INITIALIZATIONS
C     -----------------------------
C     AVERAGE HADRON MASSES, TWO LOWEST MULTIPLETS (IF POSSIBLE):
C     N/DELTA,LAMBDA/SIGMA,XI,OMEGA,PI/RHO,KAON,DELTA:
         ASUHAX(1)=1.134
         ASUHAX(2)=1.301
         ASUHAX(3)=1.461
         ASUHAX(4)=1.673
         ASUHAX(5)=0.6125
         ASUHAX(6)=0.7915
         ASUHAX(7)=1.2320
C     LOWEST MASSES:
         ASUHAY(1)=0.940
         ASUHAY(2)=1.200
         ASUHAY(3)=1.322
         ASUHAY(4)=1.673
         ASUHAY(5)=0.1400
         ASUHAY(6)=0.4977
         ASUHAY(7)=1.2320
 
C     TECHNICAL PARAMETERS
C     --------------------
C     DELTA_ZETA FOR /C4PTL/...WEIPTL()
         DLZETA=0.5
C     MIN TAU FOR SPACE-TIME EVOLUTION:
         TAUMIN=0.
C     MAX TAU FOR SPACE-TIME EVOLUTION
         TAUMAX=10.0
C     TAU STEPS FOR SPACE-TIME EVOTUTION (46+40)
         NUMTAU=51
C     RANGE FOR PT DISTRIBUTION
         PTMX=6.0
C     RANGE FOR GAUSS DISTRIBUTION
         GAUMX=8.0
C     PARAMETER DETERMINING RANGE FOR DENSITY DISTRIBUTION
         FCTRMX=10.0
C     TRY-AGAIN PARAMETER
         NTRYMX=10
C     MAX TIME FOR JPSI EVOLUTION
         TAUMX=20.0
C     TIME STEPS FOR JPSI EVOLUTION
         NSTTAU=100
 
C     OPTIONS
C     -------
C     OPTION FOR MINIMUM ENERGY IN SJCGAM:
C       IOPENU = 1 : SUM OF HADRON MASSES
C       IOPENU = 2 : BAG MODEL CURVE WITH MINIMUM AT NONZERO STRANGEN.
         IOPENU=1
C     PARAMETER THETA IN BERGER/JAFFE MASS FORMULA
         THEMAS=0.51225
C     SEA PROBABILITY (IF .LT. 0. THEN CALCULATED FROM STRUCTURE FNCTS)
         PROSEA=-1.0
C     INELASTIC PP CROSS SECTION (FM**2)
C      (IF NEGATIVE: CALCULATED FROM GRIBOV-REGGE-THEORY):
CDH      SIGPPI=-1.0
C     MULTISTRING PARAMETER (Q1+Q2*LN(E)+Q3*LN(E)**2, E=SQRT(S)):
C      (NOT USED IF RACPRO IS CALLED WITH 'GRI'-OPTION (DEFAULT))
         QMUST1=0.50
         QMUST2=0.
         QMUST3=0.
C     ENTRO() CALCULATED (1) OR FROM DATA (2)
         IENTRO=2
C     DUAL PARTON MODEL (1) OR NOT (ELSE)
         IDPM=0
C     ANTIQUARK COLOR EXCHANGE (1) OR NOT (0):
         IAQU=1
C     MINIMUM NUMBER OF VALENCE QUARKS:
         NEQMN=-5
C     MAXIMUM NUMBER OF VALENCE QUARKS:
         NEQMX=5
C     UPPER LIMIT FOR RAPIDITY INTERVAL FOR INTERMITTENCY ANALYSIS
         YMXIMI=2.0
C     CLEAN /CPTL/ IF NCLEAN > 0 (EVERY NCLEAN_TH TIME STEP)
         NCLEAN=0
C     TRAFO FROM PP-CM INTO LAB-SYSTEM (1) OR NOT (.NE.1)
         LABSYS=1
C     MAXIMUM NUMBER OF COLLISIONS:
         NCOLMX=1000
C     MAXIMUM RESONANCE SPIN (SPIN IN A GENARAL SENSE: MOD(/ID/,10))
         MAXRES=99999
C     MOMENTUM RESCALING (1=YES):
         IRESCL=1
C     NUE ENERGY
         ELEPTI=43.00
C     MUE  ENERGY
         ELEPTO=26.24
C     MUE ANGLE
         ANGMUE=3.9645/180.*3.1415926
C     JPSI TO BE PRODUCED (1) OR NOT (0):
         JPSI=0
C     JPSI FINAL STATE INTERACTION (1) OR NOT (0):
         JPSIFI=0
C     COLLISION TRIGGER (ONLY COLL BETWEEN KO1 AND KO2 ARE USED):
         KO1KO2=00009999
C     PRINT OPTION:
C     ISH=14: CALL UTTIMA
C     ISH=15: PRINTS PTLS READ FROM DATA FILE IN SR VEANLY
C     ISH=16: PRINTS SEA PROB.
C     ISH=17: PRINTS RANDOM NUMBERS
C     ISH=18: SR JCLUDE, NO-PHASE-SPACE CLUSTERS
C     ISH=19: SR AINITL, CALL SMASSP
C     ISH=20: SR VEANLY, PRINTS EVT NR IF EVT IS ACCEPTED
C     ISH=21: CREATES HISTOGRAM FOR SEA DISTRIBUTION
C     ISH=22: SR JFRADE, MSG AFTER CALL UTCLEA
C     ISH=23: CALL JINTFP
C     ISH=24: CALL JINTCL
C     ISH=25: CALL JCHPRT
C     ISH=90,91,92,93,94,95: MORE AND MORE DETAILED MESSAGES.
      IF ( DEBUG ) THEN
        ISH  = ISH0
      ELSE
        ISH  = 0
      ENDIF
C     PRINT OPTION:
C     ISHSUB=IJMN, IJ SPECIFIES LOCATION WHERE ISH=MN.
C     IJ=01: SR JCLUDE
C     IJ=02: SR JETGEN
C     IJ=03: SR JFRADE, STARTING BEFORE FRAGMENTATION
C     IJ=04: SR JDECAY
C     IJ=05: SR JDECAX
C     IJ=06: SR NUCOLL
C     IJ=07: SR NUCOGE+-
C     IJ=08: SR ASTORE
C     IJ=09: SR JFRADE, STARTING AFTER FRAGMENTATION
C     IJ=10: SR JFRADE, STARTING BEFORE DECAY
C     IJ=11: SR JFRADE, STARTING AFTER INTERACTIONS
C     IJ=12: SR JCENTR, ENTRO() IN DATA FORMAT
C     IJ=13: SR JCENTP
C     IJ=14: SR JDECAX IF CLUSTER DECAY
C     IJ=15: SR JSPLIT
C     IJ=16: SR JFRADE
C     IJ=17: SR RACPRO
C     IJ=18: SR UTCLEA
C     IJ=19: SR JINTA1, JINTA2, AFTER CALL UTCLEA
C     IJ=20: SR JDECAS
C     IJ=21: SR JDECAS (WITHOUT JDECAX)
         ISHSUB=0
C     PRINT OPTION:
C     IF ISHEVT.NE.0: FOR EVT#.NE.ISHEVT ISH IS SET TO 0
         ISHEVT=0
C     PRINT MARKS BETWEEN WHOM ISH IS SET TO ISH(INIT):
         IPAGI=0
C     VERIFY OPTION FOR INPUT READING:
         IVI=1
C     MAXIMUM IMPACT PARAMETER (BMAXIM=0=>CENTRAL):
         BMAXIM=10000.
C     MINIMUM IMPACT PARAMETER:
         BMINIM=0.
C     STORE ONLY STABLE PTL (0) OR ALSO PARENTS (1):
         ISTMAX=0
C     RANDOM GENERATOR SEED
         SEEDI=ISEED(1,1)
         SEEDC=ISEED(2,1)+1.D9*ISEED(3,1)
C     SUPPRESSION (1) OR NOT OF MESSAGES
         ISUP=0
C     SUPPRESSION OF CALLING JFRADE (0). JFRADE=FRAGM+DECAY+RESCATTERING
         IFRADE=1
C..   DECAY SUPPRESSION. NDECAY SPECIFIES WHICH RESONANCES ARE NOT DECAY
C..   0000001 : ALL RESONANCES
C..   0000010 : K_SHORT/LONG (+-20)
C..   0000100 : LAMBDA (+-2130)
C..   0001000 : SIGMA (+-1130,+-2230)
C..   0010000 : CASCADE (+-2330,+-1330)
C..   0100000 : OMEGA (+-3331)
C..   1000000 : PI0 (110)
         NDECAY=1111110
C..   DECAY SUPPRESSION. NDECAX SPECIFIES WHICH RESONANCES ARE NOT DECAY
C..   0000001 : JPSI
C..   0000010 : K_ZERO (+-230)
C..   0000100 : DELTA (+-1111,+-1121,+-1221,+-2221)
C..   0001000 : RHO,OMEGA,PHI (111,+-121,221,331)
C..   0010000 : ETA (220)
C..   0100000 : ETAPRIME (330)
C..   1000000 : A0 (112), A+- (+-122)
         NDECAX=0010000
C..   DECAY SUPPRESSION. NDECAW SPECIFIES WHICH RESONANCES ARE NOT DECAY
C..   0000001 : F0 (332)
C..   0000010 : K* (+-131,+-231)
         NDECAW=0
C     FILL ZZZZ HISTOGRAMS (1) OR NOT (0)
C        IWZZZZ=0
C     FILL INTERMITTENCY HISTOGRAMS (1) OR NOT (0)
C        IMIHIS=0
C     FILL SPACE-TIME HISTOGRAMS (1) OR NOT (0)
         ISPHIS=0
C     FILL CLUSTER HISTOGRAMS (1) OR NOT (0)
C        ICLHIS=0
C     FILL JPSI HISTOGRAMS (1) OR NOT (0)
C        IJPHIS=0
C     RHO/RHO+PHI RATIO
         RHOPHI=0.5
C     WSPA: ALL PTLS (1) OR ONLY INTERACTING PTLS (ELSE)
         ISPALL=1
C     TMIN IN WSPA
         WTMINI=-3.0
C     T-STEP IN WSPA
         WTSTEP=1.0
C     ONLY CENTRAL POINT (1) OR LONGITUDINAL DISTRIBUTION (ELSE) IN WSPA
         IWCENT=0
C     QUARK MASSES
         SMAS=0.
         UUMAS=0.
         USMAS=0.
         SSMAS=0.
 
C  CONSTANTS (PROTON MASS, PION MASS, PI, INFINITE)
C  ---------
C     PROM=0.94
      PROM=PAMA(14)
C     PIOM=0.14
      PIOM=PAMA(8)
      PI=3.141592654
      AINFIN=1.E+30
 
C  INITIALIZATIONS
C  ---------------
      LAPROJ=0
      MAPROJ=0
      LATARG=0
      MAPROJ=0
      IDPROJ=1120
      IDTARG=1120
      DO 6 I=1,99
        PROB(I)=0.
        ICBAC(I,1)=0
        ICBAC(I,2)=0
        ICFOR(I,1)=0
        ICFOR(I,2)=0
 6    CONTINUE
      PNLL=0.
 
C  FEW INITIALIZATIONS FOR CROSS SECTION CALCULATIONS
C  --------------------------------------------------
 
      IMSG=0
      JERR=0
      NTEVT=0
      NREVT=0
      NAEVT=0
      NRSTR=0
      NRPTL=0
      INOIAC=0
      ILAMAS=0
      NPTLU=0
      DO 44 ITAU = 1,MXTAU
        VOLSUM(ITAU)=0.
        VO2SUM(ITAU)=0.
        NCLSUM(ITAU)=0
 44   CONTINUE
      DO 43 IEPS=1,MXEPS
        DO 43 IVOL=1,MXVOL
          DO 43 ITAU=1,MXTAU
            CLUST(ITAU,IVOL,IEPS) = 0.
 43   CONTINUE
      IUTOTC=0
      IUTOTE=0
 
      IF ( NPARAM .GT. 0 ) THEN
        DO 3 N=1,NPARAM
          CALL UTLOW6(PARCHA(N))
          IF ( DEBUG ) WRITE(MDEBUG,*) PARCHA(N),PARVAL(N)
          IF    (PARCHA(N).EQ.'AMPRIF')THEN
                                       AMPRIF=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'AMSIAC')THEN
                                       AMSIAC=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'AMUSEG')THEN
                                       AMUSEG=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'ANGMUE')THEN
                                       ANGMUE=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'BAG4RT')THEN
                                       BAG4RT=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'BMAXIM')THEN
                                       BMAXIM=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'BMINIM')THEN
                                       BMINIM=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'CORE  ')THEN
                                       CORE  =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'CORLEN')THEN
                                       CORLEN=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'CUTMSQ')THEN
                                       CUTMSQ=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'CUTMSS')THEN
                                       CUTMSS=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'DELEPS')THEN
                                       DELEPS=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'DELMSS')THEN
                                       DELMSS=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'DELREM')THEN
                                       DELREM=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'DELVOL')THEN
                                       DELVOL=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'ELEPTI')THEN
                                       ELEPTI=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'ELEPTO')THEN
                                       ELEPTO=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'EPSCRI')THEN
                                       EPSCRI=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'FCTRMX')THEN
                                       FCTRMX=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'GAUMX ')THEN
                                       GAUMX =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'GRICEL')THEN
                                       GRICEL=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'GRIDEL')THEN
                                       GRIDEL=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'GRIGAM')THEN
                                       GRIGAM=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'GRIRSQ')THEN
                                       GRIRSQ=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'GRISLO')THEN
                                       GRISLO=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IAQU  ')THEN
                                       IAQU  =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'ICLHIS')THEN
                                       ICLHIS=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IDPM  ')THEN
                                       IDPM  =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IENTRO')THEN
                                       IENTRO=NINT(PARVAL(N))
          ELSEIF(PARCHA(N).EQ.'IFRADE')THEN
                                       IFRADE=NINT(PARVAL(N))
          ELSEIF(PARCHA(N).EQ.'IJPHIS')THEN
                                       IJPHIS=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IMIHIS')THEN
                                       IMIHIS=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IOJINT')THEN
                                       IOJINT=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IOPBRK')THEN
                                       IOPBRK=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IOPENT')THEN
                                       IOPENT=PARVAL(N)
                                       IOPENT = MOD(IOPENT,10)
          ELSEIF(PARCHA(N).EQ.'IOPENU')THEN
                                       IOPENU=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IOPTF ')THEN
                                       IOPTF =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IOPTQ ')THEN
                                       IOPTQ =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IPAGI ')THEN
                                       IPAGI =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IRESCL')THEN
                                       IRESCL=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'ISH   ')THEN
                                       ISH   =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'ISHEVT')THEN
                                       ISHEVT=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'ISHSUB')THEN
                                       ISHSUB=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'ISPALL')THEN
                                       ISPALL=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'ISPHIS')THEN
                                       ISPHIS=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'ISTMAX')THEN
                                       ISTMAX=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'ISUP  ')THEN
                                       ISUP  =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IVERSN')THEN
                                       IVERSN=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IVI   ')THEN
                                       IVI   =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'IWCENT')THEN
                                       IWCENT=NINT(PARVAL(N))
          ELSEIF(PARCHA(N).EQ.'IWZZZZ')THEN
                                       IWZZZZ=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'JPSI  ')THEN
                                       JPSI  =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'JPSIFI')THEN
                                       JPSIFI=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'KENTRO')THEN
                                       KENTRO=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'KO1KO2')THEN
                                       KO1KO2=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'KUTDIQ')THEN
                                       KUTDIQ=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'LABSYS')THEN
                                       LABSYS=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'MAXRES')THEN
                                       MAXRES=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'NCLEAN')THEN
                                       NCLEAN=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'NCOLMX')THEN
                                       NCOLMX=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'NDECAW')THEN
                                       NDECAW=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'NDECAX')THEN
                                       NDECAX=NINT(PARVAL(N))
          ELSEIF(PARCHA(N).EQ.'NDECAY')THEN
                                       NDECAY=NINT(PARVAL(N))
          ELSEIF(PARCHA(N).EQ.'NEQMN ')THEN
                                       NEQMN =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'NEQMX ')THEN
                                       NEQMX =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'NSTTAU')THEN
                                       NSTTAU=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'NTRYMX')THEN
                                       NTRYMX=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'NUMTAU')THEN
                                       NUMTAU=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'OVERLP')THEN
                                       OVERLP=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PAREA ')THEN
                                       PAREA =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PDIQUA')THEN
                                       PDIQUA=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PISPN ')THEN
                                       PISPN =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PROSEA')THEN
                                       PROSEA=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PSPINH')THEN
                                       PSPINH=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PSPINL')THEN
                                       PSPINL=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PTF   ')THEN
                                       PTF   =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PTH   ')THEN
                                       PTH   =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PHARD ')THEN
                                       PHARD =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PTMX  ')THEN
                                       PTMX  =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PTQ1  ')THEN
                                       PTQ1  =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PTQ2  ')THEN
                                       PTQ2  =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PTQ3  ')THEN
                                       PTQ3  =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PUD   ')THEN
                                       PUD   =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'PVALEN')THEN
                                       PVALEN=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'QMUST ')THEN
            CALL UTSTOP('VENINI: *** QMUST NOT USED ANYMORE! *** ')
          ELSEIF(PARCHA(N).EQ.'QMUST1')THEN
                                       QMUST1=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'QMUST2')THEN
                                       QMUST2=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'QMUST3')THEN
                                       QMUST3=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'RADIAC')THEN
                                       RADIAC=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'RADIAS')THEN
                                       RADIAS=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'RHOPHI')THEN
                                       RHOPHI=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'RSTRAS')THEN
                                       RSTRAS=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'SEEDI ')THEN
                                       SEEDI =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'SIGJ  ')THEN
                                       SIGJ  =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'SIGPPI')THEN
                                       SIGPPI=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'SMAS  ')THEN
                                       SMAS  =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'SSMAS ')THEN
                                       SSMAS =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'TAUMAX')THEN
                                       TAUMAX=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'TAUMIN')THEN
                                       TAUMIN=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'TAUMX ')THEN
                                       TAUMX =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'TAUNLL')THEN
                                       TAUNLL=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'TAUREA')THEN
                                       TAUREA=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'TENSN ')THEN
                                       TENSN =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'THEMAS')THEN
                                       THEMAS=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'UENTRO')THEN
                                       UENTRO=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'USMAS ')THEN
                                       USMAS =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'UUMAS ')THEN
                                       UUMAS =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'WPROJ ')THEN
                                       WPROJ =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'WTARG ')THEN
                                       WTARG =PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'WTMINI')THEN
                                       WTMINI=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'WTSTEP')THEN
                                       WTSTEP=PARVAL(N)
          ELSEIF(PARCHA(N).EQ.'YMXIMI')THEN
                                       YMXIMI=PARVAL(N)
          ENDIF
 3      CONTINUE
      ENDIF
 
      IF ( ISPHIS .EQ. 1 ) LABSYS = 0
      IF ( IDPM .EQ. 1 ) THEN
        IAQU = 0
        NEQMN = 2
        NEQMX = 3
      ENDIF
      IF ( IOPENU .EQ. 2 ) THEN
        CALL SMASSI(THEMAS)
        IF ( ISH .EQ. 19 ) THEN
          CALL SMASSP
          CALL UTSTOP(' VENLNK:                                ')
        ENDIF
      ENDIF
 
      IF ( IOJINT .EQ. 2 ) THEN
        IF     ( EPSCRI .LT. 0. ) THEN
          RADIAC = 0.
          RADIAS = 0.
        ELSEIF ( EPSCRI .GT. 0. ) THEN
          VOLBAR = PROM/EPSBAR*PI*0.25
CDH       RADIAC = (VOLBAR*0.5/PI)**0.3333333
          VOLMES = 0.455/EPSCRI*PI*0.25
CDH       RADIAS = (VOLMES*0.5/PI)**0.3333333
        ELSE
          CALL UTSTOP('EPSCRI MUST NOT BE 0.                   ')
        ENDIF
      ENDIF
 
      CALL JDECIN(.FALSE.)
C  INITIALIZE ALL PT DISTRIBUTIONS
      CX = PTMX
      QUAMA = 0.
      IF ( IOPTF .EQ. 1 ) THEN
        ROOT  = SQRT(PTMX**2+QUAMA**2)
        AUXIL = 2./PTF
        BPTFU = +0.25*PTF**2*EXP(-AUXIL*QUAMA)*(AUXIL*QUAMA+1.)
        FPTFU = -0.25*PTF**2*EXP(-AUXIL*ROOT)*(AUXIL*ROOT+1.)+BPTFU
        CALL UTQUAF(SPTF,NPTF,XPTF,QPTFU,0.,.33*CX,.66*CX,CX)
C       DO 199 N=1,NPTF
C         WRITE(IFCH,*)'N,X,Q=',N,XPTF(N),QPTFU(N)
C199    CONTINUE
      ELSE
        AUXIL = 0.25*PI/PTF**2
        BPTFU = +EXP(-AUXIL* QUAMA**2)*0.5/AUXIL
        FPTFU = -EXP(-AUXIL*(QUAMA**2+PTMX**2))*0.5/AUXIL+BPTFU
      ENDIF
 
      QUAMA = SMAS
      IF ( QUAMA .NE. 0. ) THEN
        IF ( IOPTF .EQ. 1 ) THEN
          ROOT  = SQRT(PTMX**2+SMAS**2)
          AUXIL = 2./PTF
          BPTFS = +0.25*PTF**2*EXP(-AUXIL*SMAS)*(AUXIL*SMAS+1.)
          FPTFS = -0.25*PTF**2*EXP(-AUXIL*ROOT)*(AUXIL*ROOT+1.)+BPTFS
          CALL UTQUAF(SPTF,NPTF,XPTF,QPTFS,0.,.33*CX,.66*CX,CX)
        ELSE
          AUXIL = 0.25*PI/PTF**2
          BPTFS = +EXP(-AUXIL* SMAS**2)*0.5/AUXIL
          FPTFS = -EXP(-AUXIL*(SMAS**2+PTMX**2))*0.5/AUXIL+BPTFS
        ENDIF
      ELSE
        DO 201 N = 1,NPTF
          QPTFS(N) = QPTFU(N)
 201    CONTINUE
        FPTFS = FPTFU
      ENDIF
 
      QUAMA = UUMAS
      IF ( QUAMA .NE. 0. ) THEN
        IF ( IOPTF .EQ. 1 ) THEN
          ROOT   = SQRT(PTMX**2+UUMAS**2)
          AUXIL  = 2./PTF
          BPTFUU = +0.25*PTF**2*EXP(-AUXIL*UUMAS)*(AUXIL*UUMAS+1.)
          FPTFUU = -0.25*PTF**2*EXP(-AUXIL*ROOT)*(AUXIL*ROOT+1.)+BPTFUU
          CALL UTQUAF(SPTF,NPTF,XPTF,QPTFUU,0.,.33*CX,.66*CX,CX)
        ELSE
          AUXIL  = 0.25*PI/PTF**2
          BPTFUU =  EXP(-AUXIL* UUMAS**2)*0.5/AUXIL
          FPTFUU = -EXP(-AUXIL*(UUMAS**2+PTMX**2))*0.5/AUXIL+BPTFUU
        ENDIF
      ELSE
        DO 202 N = 1,NPTF
          QPTFUU(N) = QPTFU(N)
 202    CONTINUE
        FPTFUU = FPTFU
      ENDIF
 
      QUAMA = USMAS
      IF ( QUAMA .NE. 0. ) THEN
        IF ( IOPTF .EQ. 1 ) THEN
          ROOT   = SQRT(PTMX**2+USMAS**2)
          AUXIL  = 2./PTF
          BPTFUS =  0.25*PTF**2*EXP(-AUXIL*USMAS)*(AUXIL*USMAS+1.)
          FPTFUS = -0.25*PTF**2*EXP(-AUXIL*ROOT)*(AUXIL*ROOT+1.)+BPTFUS
          CALL UTQUAF(SPTF,NPTF,XPTF,QPTFUS,0.,.33*CX,.66*CX,CX)
        ELSE
          AUXIL  = 0.25*PI/PTF**2
          BPTFUS =  EXP(-AUXIL* USMAS**2)*0.5/AUXIL
          FPTFUS = -EXP(-AUXIL*(USMAS**2+PTMX**2))*0.5/AUXIL+BPTFUS
        ENDIF
      ELSE
        DO 203 N = 1,NPTF
          QPTFUS(N) = QPTFU(N)
 203    CONTINUE
        FPTFUS = FPTFU
      ENDIF
 
      QUAMA = SSMAS
      IF ( QUAMA .NE. 0. ) THEN
        IF ( IOPTF .EQ. 1 ) THEN
          ROOT   = SQRT(PTMX**2+SSMAS**2)
          AUXIL  = 2./PTF
          BPTFSS = +0.25*PTF**2*EXP(-AUXIL*SSMAS)*(AUXIL*SSMAS+1.)
          FPTFSS = -0.25*PTF**2*EXP(-AUXIL*ROOT)*(AUXIL*ROOT+1.)+BPTFSS
          CALL UTQUAF(SPTF,NPTF,XPTF,QPTFSS,0.,.33*CX,.66*CX,CX)
        ELSE
          AUXIL  = 0.25*PI/PTF**2
          BPTFSS =  EXP(-AUXIL* SSMAS**2)*0.5/AUXIL
          FPTFSS = -EXP(-AUXIL*(SSMAS**2+PTMX**2))*0.5/AUXIL+BPTFSS
        ENDIF
      ELSE
        DO 204 N = 1,NPTF
          QPTFSS(N) = QPTFU(N)
 204    CONTINUE
        FPTFSS = FPTFU
      ENDIF
 
C  INITIALIZE FUNCTIONS FOR JPSI GENERATION
      IF ( JPSI .EQ. 1 ) THEN
        CX = GAUMX
        CALL UTQUAF(SGAU,NGAU,XGAU,QGAU,0.,.33*CX,.66*CX,CX)
        CX = PTMX
        CALL UTQUAF(SPTJ,NPTJ,XPTJ,QPTJ,0.,.33*CX,.66*CX,CX)
      ENDIF
 
C INITIALIZE DENSITY DISTRIBUTION INTEGRALS FOR NITROGEN, OXYGEN, ARGON
      MASSNR = 14.
      R0 = 1.19*MASSNR**(.3333333) -1.61*MASSNR**(-.3333333)
      CX = R0+FCTRMX*0.54
      RMTARG(1) = CX
      CALL UTQUAF(SDENSI,NDET,XDET14,QDET14,0.,.33*CX,.66*CX,CX)
 
      MASSNR = 16.
      R0 = 1.19*MASSNR**(.3333333) -1.61*MASSNR**(-.3333333)
      CX = R0+FCTRMX*0.54
      RMTARG(2) = CX
      CALL UTQUAF(SDENSI,NDET,XDET16,QDET16,0.,.33*CX,.66*CX,CX)
 
      MASSNR = 40.
      R0 = 1.19*MASSNR**(.3333333) -1.61*MASSNR**(-.3333333)
      CX = R0+FCTRMX*0.54
      RMTARG(3) = CX
      CALL UTQUAF(SDENSI,NDET,XDET40,QDET40,0.,.33*CX,.66*CX,CX)
 
C  QDET99 AND XDET99 ARE NOT INITIALIZED
      MTAR99 = 0
 
      OPEN(UNIT=14,FILE='VENUSDAT',STATUS='OLD')
      READ(14,*)(IDUMMY, XVA(I), QVAH(I), QVAPI(I), I=1,2049)
      CLOSE(UNIT=14)
 
      WRITE(IFMT,105) FLOAT(IVERSN)/1000.
 105  FORMAT(
     * ' !-----------------------------------------------------!'
     */' !      V(ERY) E(NERGETIC) NU(CLEAR) S(CATTERING)      !'
     */' !      VENUS',F6.3,5X,'-          K. WERNER           !'
     */' !      SUBROUTINE TURBOVERSION    D. HECK             !'
     */' !-----------------------------------------------------!')
      RETURN
      END
