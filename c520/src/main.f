
      PROGRAM MAIN
 
C-----------------------------------------------------------------------
C  MAIN PROGRAM
C
C  SIMULATION OF EXTENSIVE AIR SHOWERS
C  PREPARES INITIALIZATIONS
C  GENERATES SHOWERS IN THE SHOWER LOOP
C  TREATES PARTICLES IN THE PARTICLE LOOP
C  PERFORMS PRINTING OF TABLES AT END OF SHOWER AND AT END OF RUN
C-----------------------------------------------------------------------
 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      parameter (xct=1)
      parameter (yct=2)
      parameter (zct=3)
      parameter (ctthet=4)
      parameter (ctphi=5)
      parameter (ctdiam=6)
      parameter (ctfoc=7)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

*KEEP,BAL.
      COMMON /BAL/     EBAL
      DOUBLE PRECISION EBAL(10)
*KEEP,BUFFS.
      COMMON /BUFFS/   RUNH,RUNE,EVTH,EVTE,DATAB,LH
      INTEGER          MAXBUF,MAXLEN
      PARAMETER        (MAXBUF=39*7)
      PARAMETER        (MAXLEN=12)
      REAL             RUNH(MAXBUF),EVTH(MAXBUF),EVTE(MAXBUF),
     *                 RUNE(MAXBUF),DATAB(MAXBUF)
      INTEGER          LH
      CHARACTER*4      CRUNH,CRUNE,CEVTH,CEVTE
      EQUIVALENCE      (RUNH(1),CRUNH), (RUNE(1),CRUNE)
      EQUIVALENCE      (EVTH(1),CEVTH), (EVTE(1),CEVTE)
*KEEP,CHISTA.
      COMMON /CHISTA/  IHYCHI,IKACHI,IMUCHI,INNCHI,INUCHI,IPICHI
      INTEGER          IHYCHI(124),IKACHI(124),IMUCHI(124),
     *                 INNCHI(124),INUCHI(124),IPICHI(124)
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,CURVE.
      COMMON /CURVE/   CHAPAR,DEP,ERR,NSTP
      DOUBLE PRECISION CHAPAR(1100),DEP(1100),ERR(1100)
      INTEGER          NSTP
*KEEP,ELADPM.
      COMMON /ELADPM/  ELMEAN,ELMEAA,IELDPM,IELDPA
      DOUBLE PRECISION ELMEAN(37),ELMEAA(37)
      INTEGER          IELDPM(37,13),IELDPA(37,13)
*KEEP,ELASTY.
      COMMON /ELASTY/  ELAST,IELIS,IELHM,IELNU,IELPI
      DOUBLE PRECISION ELAST
      INTEGER          IELIS(20),IELHM(20),IELNU(20),IELPI(20)
*KEEP,GENER.
      COMMON /GENER/   GEN,ALEVEL
      DOUBLE PRECISION GEN,ALEVEL
*KEEP,IRET.
      COMMON /IRET/    IRET1,IRET2
      INTEGER          IRET1,IRET2
*KEEP,ISTA.
      COMMON /ISTA/    IFINET,IFINNU,IFINKA,IFINPI,IFINHY
      INTEGER          IFINET,IFINNU,IFINKA,IFINPI,IFINHY
*KEEP,LONGI.
      COMMON /LONGI/   APLONG,HLONG,PLONG,SPLONG,THSTEP,THSTPI,
     *                 NSTEP,LLONGI,FLGFIT
      DOUBLE PRECISION APLONG(0:1040,9),HLONG(0:1024),PLONG(0:1040,9),
     *                 SPLONG(0:1040,9),THSTEP,THSTPI
      INTEGER          NSTEP
      LOGICAL          LLONGI,FLGFIT
*KEEP,MPARTI.
      COMMON /MPARTI/  MPARTO
      DOUBLE PRECISION MPARTO(10,25),MPHOTO(10),MPOSIT(10),MELECT(10),
     *                 MNU(10),MMUP(10),MMUM(10),MPI0(10),MPIP(10),
     *                 MPIM(10),MK0L(10),MKPL(10),MKMI(10),MNEUTR(10),
     *                 MPROTO(10),MPROTB(10),MK0S(10),MHYP(10),
     *                 MNEUTB(10),MDEUT(10),MTRIT(10),MALPHA(10),
     *                 MOTHER(10)
      EQUIVALENCE (MPARTO(1, 1),MPHOTO(1)), (MPARTO(1, 2),MPOSIT(1)),
     *            (MPARTO(1, 3),MELECT(1)), (MPARTO(1, 4),MNU(1))   ,
     *            (MPARTO(1, 5),MMUP(1))  , (MPARTO(1, 6),MMUM(1))  ,
     *            (MPARTO(1, 7),MPI0(1))  , (MPARTO(1, 8),MPIP(1))  ,
     *            (MPARTO(1, 9),MPIM(1))  , (MPARTO(1,10),MK0L(1))  ,
     *            (MPARTO(1,11),MKPL(1))  , (MPARTO(1,12),MKMI(1))  ,
     *            (MPARTO(1,13),MNEUTR(1)), (MPARTO(1,14),MPROTO(1)),
     *            (MPARTO(1,15),MPROTB(1)), (MPARTO(1,16),MK0S(1))  ,
     *            (MPARTO(1,18),MHYP(1))  , (MPARTO(1,19),MDEUT(1)) ,
     *            (MPARTO(1,20),MTRIT(1)) , (MPARTO(1,21),MALPHA(1)),
     *            (MPARTO(1,22),MOTHER(1)), (MPARTO(1,25),MNEUTB(1))
*KEEP,MULT.
      COMMON /MULT/    EKINL,MSMM,MULTMA,MULTOT
      DOUBLE PRECISION EKINL
      INTEGER          MSMM,MULTMA(37,13),MULTOT(37,13)
*KEEP,MUPART.
      COMMON /MUPART/  AMUPAR,BCUT,CMUON,FMUBRM,FMUORG
      DOUBLE PRECISION AMUPAR(14),BCUT,CMUON(11)
      LOGICAL          FMUBRM,FMUORG
*KEEP,NCOUNT.
      COMMON /NCOUNT/  NCOUN
      INTEGER          NCOUN(8)
*KEEP,NKGI.
      COMMON /NKGI/    SEL,SELLG,STH,ZEL,ZELLG,ZSL,DIST,
     *                 DISX,DISY,DISXY,DISYX,DLAX,DLAY,DLAXY,DLAYX,
     *                 OBSATI,RADNKG,RMOL,TLEV,TLEVCM,IALT
      DOUBLE PRECISION SEL(10),SELLG(10),STH(10),ZEL(10),ZELLG(10),
     *                 ZSL(10),DIST(10),
     *                 DISX(-10:10),DISY(-10:10),
     *                 DISXY(-10:10,2),DISYX(-10:10,2),
     *                 DLAX (-10:10,2),DLAY (-10:10,2),
     *                 DLAXY(-10:10,2),DLAYX(-10:10,2),
     *                 OBSATI(2),RADNKG,RMOL(2),TLEV(10),TLEVCM(10)
      INTEGER          IALT(2)
*KEEP,NKGS.
      COMMON /NKGS/    CZX,CZY,CZXY,CZYX,SAH,SL,ZNE
      DOUBLE PRECISION CZX(-10:10,2),CZY(-10:10,2),CZXY(-10:10,2),
     *                 CZYX(-10:10,2),SAH(10),SL(10),ZNE(10)
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
*KEEP,PBALA.
      COMMON /PBALA/   PBAL
      DOUBLE PRECISION PBAL(10)
*KEEP,PRIMSP.
      COMMON /PRIMSP/  PSLOPE,LLIMIT,ULIMIT,LL,UL,SLEX,ISPEC
      DOUBLE PRECISION PSLOPE,LLIMIT,ULIMIT,LL,UL,SLEX
      INTEGER          ISPEC
*KEEP,RANDPA.
      COMMON /RANDPA/  FAC,U1,U2,RD,NSEQ,ISEED,KNOR
      DOUBLE PRECISION FAC,U1,U2
      REAL             RD(3000)
      INTEGER          ISEED(103,10),NSEQ
      LOGICAL          KNOR
*KEEP,RECORD.
      COMMON /RECORD/  IRECOR
      INTEGER          IRECOR
*KEEP,REJECT.
      COMMON /REJECT/  AVNREJ,
     *                 ALTMIN,ANEXP,THICKA,THICKD,CUTLN,EONCUT,
     *                 FNPRIM
      DOUBLE PRECISION AVNREJ(10)
      REAL             ALTMIN(10),ANEXP(10),THICKA(10),THICKD(10),
     *                 CUTLN,EONCUT
      LOGICAL          FNPRIM
*KEEP,RESON.
      COMMON /RESON/   RDRES,RESRAN,IRESPAR
      REAL             RDRES(2),RESRAN(1000)
      INTEGER          IRESPAR
 
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
*KEEP,STACKF.
      COMMON /STACKF/  STACK,STACKP,EXST,NSHIFT,NOUREC,ICOUNT,NTO,NFROM
      INTEGER          MAXSTK
      PARAMETER        (MAXSTK = 12*340*2)
      DOUBLE PRECISION STACK(MAXSTK)
      INTEGER          STACKP,EXST,NSHIFT,NOUREC,ICOUNT,NTO,NFROM
*KEEP,STATI.
      COMMON /STATI/   SABIN,SBBIN,INBIN,IPBIN,IKBIN,IHBIN
      DOUBLE PRECISION SABIN(37),SBBIN(37)
      INTEGER          INBIN(37),IPBIN(37),IKBIN(37),IHBIN(37)
*KEEP,THNVAR.
      COMMON /THNVAR/  STACKINT,INT_ICOUNT,THINNING
      INTEGER          MAXICOUNT
      PARAMETER        (MAXICOUNT=20000)
      DOUBLE PRECISION STACKINT(MAXICOUNT,13)
      INTEGER          INT_ICOUNT
      LOGICAL          THINNING
*KEEP,VERS.
      COMMON /VERS/    VERNUM,MVDATE,VERDAT
      DOUBLE PRECISION VERNUM
      INTEGER          MVDATE
      CHARACTER*18     VERDAT
*KEEP,CEREN1.
      COMMON /CEREN1/  CERELE,CERHAD,ETADSN,WAVLGL,WAVLGU,CYIELD,
     *                 CERSIZ,LCERFI
      DOUBLE PRECISION CERELE,CERHAD,ETADSN,WAVLGL,WAVLGU,CYIELD
      REAL             CERSIZ
      LOGICAL          LCERFI
*KEEP,CEREN2.
      COMMON /CEREN2/  PHOTCM,XCER,YCER,UEMIS,VEMIS,CARTIM,ZEMIS,
     *                 DCERX,DCERY,ACERX,ACERY,
     *                 XCMAX,YCMAX,EPSX,EPSY,
     *                 DCERXI,DCERYI,FCERX,FCERY,
     *                 XSCATT,YSCATT,CERXOS,CERYOS,
     *                 NCERX,NCERY,ICERML
      REAL             PHOTCM,XCER,YCER,UEMIS,VEMIS,CARTIM,ZEMIS,
     *                 DCERX,DCERY,ACERX,ACERY,
     *                 XCMAX,YCMAX,EPSX,EPSY,
     *                 DCERXI,DCERYI,FCERX,FCERY,
     *                 XSCATT,YSCATT,CERXOS(20),CERYOS(20)
      INTEGER          NCERX,NCERY,ICERML
*KEEP,CEREN3.
      COMMON /CEREN3/  CERCNT,DATAB2,LHCER
      INTEGER          MAXBF2
      PARAMETER        (MAXBF2 = 39 * 7)
      DOUBLE PRECISION CERCNT
      REAL             DATAB2(MAXBF2)
      INTEGER          LHCER
*KEEP,CEREN4.
      COMMON /CEREN4/  NRECER
      INTEGER          NRECER
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*keep,certel.
      common /certel/  cormxd,cord,coralp,ctpars,omega,
     +                 photn,photnp,phpt,pht,vphot,
     +                 vchi,veta,vzeta,vchim,vetam,vzetam,
     +                 lambda,mu,nu,nctels,ncph
      double precision cormxd,cord,coralp,ctpars(20,7),omega(20,3,3),
     +                 photn(3),photnp(3),phpt(3),pht,vphot(3),
     +                 vchi(3),veta(3),vzeta(3),vchim,vetam,vzetam,
     +                 lambda,mu,nu
      integer          nctels,ncph(5)
      double precision xg,yg,zg,xgp,ygp,zgp,up,vp,wp,xpcut,ypcut,zpcut
      equivalence (photn(1) ,xg)   ,(photn(2) ,yg)   ,(photn(3) ,zg)  ,
     +            (photnp(1),xgp)  ,(photnp(2),ygp)  ,(photnp(3),zgp),
     +            (phpt(1)  ,xpcut),(phpt(2)  ,ypcut),(phpt(3)  ,zpcut),
     +            (vphot(1) ,up)   ,(vphot(2) ,vp)   ,(vphot(3) ,wp)    
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C     Modificacion hecha por Aitor (5-feb-98)
      common /aitor/   aitoth
      double precision aitoth
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c     Angles for the "spinning" of a particle around the 
c     main axis of the CT
      common /spinang/ spinxi
      double precision spinxi
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


*KEND.
 
      INTEGER JNBIN(37),JPBIN(37),JKBIN(37),JHBIN(37)
      DOUBLE PRECISION CHI2,FPARAM(6)
      DOUBLE PRECISION MPART2(10,25),MPHOT2(10),MPOSI2(10),MELEC2(10),
     *                 MNU2(10),MMUP2(10),MMUM2(10),MPI02(10),MPIP2(10),
     *                 MPIM2(10),MK0L2(10),MKPL2(10),MKMI2(10),
     *                 MNETR2(10),MPROT2(10),MPRTB2(10),MK0S2(10),
     *                 MHYP2(10),MNETB2(10),MDEUT2(10),MTRIT2(10),
     *                 MALPH2(10),MOTH2(10)
      EQUIVALENCE (MPART2(1, 1),MPHOT2(1)), (MPART2(1, 2),MPOSI2(1)),
     *            (MPART2(1, 3),MELEC2(1)), (MPART2(1, 4),MNU2(1))  ,
     *            (MPART2(1, 5),MMUP2(1)) , (MPART2(1, 6),MMUM2(1)) ,
     *            (MPART2(1, 7),MPI02(1)) , (MPART2(1, 8),MPIP2(1)) ,
     *            (MPART2(1, 9),MPIM2(1)) , (MPART2(1,10),MK0L2(1)) ,
     *            (MPART2(1,11),MKPL2(1)) , (MPART2(1,12),MKMI2(1)) ,
     *            (MPART2(1,13),MNETR2(1)), (MPART2(1,14),MPROT2(1)),
     *            (MPART2(1,15),MPRTB2(1)), (MPART2(1,16),MK0S2(1)) ,
     *            (MPART2(1,18),MHYP2(1)) , (MPART2(1,19),MDEUT2(1)),
     *            (MPART2(1,20),MTRIT2(1)), (MPART2(1,21),MALPH2(1)),
     *            (MPART2(1,22),MOTH2 (1)), (MPART2(1,25),MNETB2(1))
C  VARIABLES BEING USED FOR RUNTIME
      REAL     TDIFF
      INTEGER  ILEFTA,ILEFTB,TIME
      EXTERNAL TIME

C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      double precision ctdiams(20)
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      double precision theprim, phiprim
      double precision spinthe, spinphi
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


C-----------------------------------------------------------------------
 
 
      CERELE = 0.D0
      CERHAD = 0.D0
      NRECER = 0
C  INITIALIZE AND READ RUN STEERING CARDS
      CALL START
 
      IF ( CERSIZ .LE. 0. ) THEN
        ICRSIZ = 0
      ELSE
        ICRSIZ = 1
      ENDIF
 
C  RESET COUNTER FOR WORDS WRITTEN TO TAPE
      IRECOR = 0
 
C  RESET COUNTER FOR AVERAGE HIGHT OF 1ST INTERACTION
      CHISUM = 0.D0
      CHISM2 = 0.D0
 
C  SET ARRAYS FOR SCALES OF KINETIC ENERGY-INTERACTION TABLE
      SABIN(1) = 0.D0
      SBBIN(1) = 0.1D0
      DO 13  J = 2,37
        SABIN(J) = 10.D0**((J-4.D0)/3.D0)
        SBBIN(J) = 10.D0**((J-3.D0)/3.D0)
  13  CONTINUE
 
C  CHECK AND SET PRIMARY PARAMETERS
      CALL INPRM

      do 161 i=1,nctels
        ctdiams(i) = ctpars(i,ctdiam)
 161  continue

C  INITIALIZE NKG ROUTINES
      CALL ININKG
 
C  RESET COUNTERS FOR NUCLEON, PION AND KAON TABLE FOR ALL SHOWERS
C  RESET ENERGY-MULTIPLICITY & ENERGY-ELASTICITY MATRIX FOR ALL SHOWERS
      DO 17  J = 1,37
        JNBIN(J)  = 0
        JPBIN(J)  = 0
        JKBIN(J)  = 0
        JHBIN(J)  = 0
        ELMEAA(J) = 0.D0
        DO 17  L = 1,13
          MULTOT(J,L) = 0
          IELDPA(J,L) = 0
  17  CONTINUE
 
C  RESET OTHER ARRAYS FOR ALL SHOWERS
      DO 99  J = 1,20
        IELNU(J) = 0
        IELPI(J) = 0
        IELIS(J) = 0
        IELHM(J) = 0
  99  CONTINUE
 
C  RESET ARRAYS FOR INTERACTION LENGTH STATISTICS
      DO 90  J = 1,124
        IHYCHI(J) = 0
        IKACHI(J) = 0
        IMUCHI(J) = 0
        INUCHI(J) = 0
        IPICHI(J) = 0
        INNCHI(J) = 0
  90  CONTINUE
 
C  RESET ARRAY FOR MEAN VALUES AND STANDARD DEVIATION
      DO 477  K = 1,25
      DO 477  J = 1,10
        MPARTO(J,K) = 0.D0
        MPART2(J,K) = 0.D0
 477  CONTINUE
 
C  RESET ARRAYS FOR LONGITUDINAL DISTRIBUTION
      IF ( LLONGI ) THEN
        DO 478  K = 1,9
          DO 4781  J = 0,NSTEP
            APLONG(J,K) = 0.D0
            SPLONG(J,K) = 0.D0
 4781     CONTINUE
 478    CONTINUE
      ENDIF
 
C  STEERING OF PRINTOUT OF RANDOM GENERATOR SEEDS
      IPROUT = MIN(100,NSHOW/20)
      IPROUT = MAX(1,IPROUT)
 
C  TIME AT BEGINNING
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c      ILEFTA = TIME()
      ILEFTA = 0
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      print *,'JCIO::========================================'
      print *,'JCIO:: Initializing JCIO system for advanced'
      print *,'JCIO:: saving of data.'
      print *,'JCIO::========================================'
c- initialize jcio system
      call jcinitio(dsn,nrrun)
c- create file run######
      call jcstartrun(runh)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

C-----------------------------------------------------------------------
C  LOOP OVER SHOWERS
      DO 2  ISHW = 1,NSHOW
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c   Next block of code has been modified, and comes from INPRM 
c----------------------------------------------------------------------
C  SCATTERING OF CENTER OF CHERENKOV ARRAY RELATIVE TO SHOWER AXIS
c>> Actually, XSCATT and YSCATT should be RminSCATT and RmaxSCATT
        ICERML = MIN(MAX(ICERML,1),20)
        XSCATT = ABS(XSCATT)
        YSCATT = ABS(YSCATT)
        WRITE(MONIOU,5225)ICERML,XSCATT,YSCATT
 5225   FORMAT(' ** USING EACH SHOWER SEVERAL TIMES:'/ 
     +     ' USE EACH EVENT ',I2,' TIMES'/
     +     ' THE EVENTS ARE SCATTERED RANDOMLY IN A SECTOR OF RADII:'/
     +     '   Rmin = ',F10.0,'   Rmax = ',F10.0)
        DO 4438 I=1,ICERML
 5226     CALL RMMAR( RD,2,3 )
          CERXOS(I) = 2.0*YSCATT*(RD(1)-0.5)
          CERYOS(I) = 2.0*YSCATT*(RD(2)-0.5)
          R=SQRT(CERXOS(I)**2+CERYOS(I)**2)
          IF ((R.LT.XSCATT).OR.(R.GT.YSCATT)) GOTO 5226
          WRITE(MONIOU,4437) I,CERXOS(I),CERYOS(I)
 4437     FORMAT('    CORE OF EVENT ',I2,'  AT  ',2F12.2)
 4438   CONTINUE
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c**********************************************************************
c NEW
c  fs: now correct offset cerxos and ceryos for adding to
c  fs: detector coordinates
c  fs: rotate 'impact coordinate system' with thetap and phip
c  fs: need flat distribution in inmpact system just for gammas!
c  fs: ctt = cos(thetap)
          if ((prmpar(1).eq.1).and.(r.gt.0.0)) then
            do i=1,icerml
              r=sqrt(cerxos(i)**2+ceryos(i)**2)
              sdphi = ceryos(i)/r
              cdphi = cerxos(i)/r
              sphi0 = sin(phip)
              cphi0 = cos(phip)
              cerxos(i) = r*(cdphi*ctt*cphi0-sdphi*sphi0)
              ceryos(i) = r*(cdphi*ctt*sphi0+sdphi*cphi0)
            enddo
          endif
c**********************************************************************

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c   Next block of code comes from INPRM 
c----------------------------------------------------------------------
        EVTH(98) = FLOAT(ICERML)
        DO  480 I=1,20
          EVTH( 98+I) = CERXOS(I)
          EVTH(118+I) = CERYOS(I)
 480    CONTINUE
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

        SHOWNO = SHOWNO + 1
        I      = ISHW
        IF ( ISHW .LE. MAXPRT ) THEN
          FPRINT = .TRUE.
        ELSE
          FPRINT = .FALSE.
        ENDIF
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c   Create cer######,dat######,sta###### files
c------------------------------------------------------------
        call jcnewshower
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

C  RESET COUNTERS
        DO 447  K = 1,25
        DO 447  J = 1,10
          NPARTO(J,K) = 0.D0
 447    CONTINUE
        MUOND = 0.D0
 
C  RESET ARRAY FOR LONGITUDINAL DISTRIBUTION PER SHOWER
        IF ( LLONGI ) THEN
          DO 479  K = 1,9
            DO 4791  J = 0,NSTEP
              PLONG(J,K) = 0.D0
 4791       CONTINUE
 479      CONTINUE
        ENDIF
 
        NRECS = 0
        NBLKS = 0
        DO 922  KKK = 1,10
          AVNREJ(KKK) = 0.D0
 922    CONTINUE
        IRESPAR = 0
 
 
C  FIRST INTERACTION DATA
        FIRSTI = .TRUE.
        IFINET = 0
        IFINNU = 0
        IFINKA = 0
        IFINPI = 0
        IFINHY = 0
        ELAST  = 0.D0
 
C  RESET COUNTERS FOR NUCLEON, PION AND KAON TABLE FOR SHOWER
C  RESET ENERGY-MULTIPLICITY & ENERGY-ELASTICITY MATRIX FOR SHOWER
        DO 11  J = 1,37
          INBIN(J) = 0
          IPBIN(J) = 0
          IKBIN(J) = 0
          IHBIN(J) = 0
          ELMEAN(J) = 0.D0
          DO 11  L = 1,13
            MULTMA(J,L) = 0
            IELDPM(J,L) = 0
  11    CONTINUE
 
        DO 12  J = 1,10
          PBAL(J) = 0.D0
          EBAL(J) = 0.D0
  12    CONTINUE
 
C  INITIALIZE PARTICLE STACK
        CALL ISTACK
C  RESET STACKINT
        DO J=1,MAXICOUNT
          DO K=1,MAXLEN
            STACKINT(J,K) = 0.D0
          ENDDO
        ENDDO
 
C  INITIALIZE EVENT HEADER AND END FOR EACH EVENT
        DO 2123  L = 2,43
          EVTH(L) = 0.
 2123   CONTINUE
        DO 123  L = 2,MAXBUF
          EVTE(L) = 0.
 123    CONTINUE
 
C  SHOWER BEGIN PRINTOUT
        IF ( FPRINT .OR. DEBUG ) WRITE(MONIOU,105) SHOWNO
 105    FORMAT ('1',10('='),' SHOWER NO ',I10,' ',47('=')/)
 
C  RANDOM GENERATOR STATUS AT BEGINNING OF SHOWER CALCULATION
        EVTH(13) = NSEQ
        DO 45  L = 1,NSEQ
          CALL RMMAQ( ISEED(1,L), L, 'R' )
C  SEED
          EVTH(11+L*3) = ISEED(1,L)
C  NUMBER OF CALLS
          EVTH(12+L*3) = MOD ( ISEED(2,L), 1000000 )
C  NUMBER OF MILLIONS
          EVTH(13+L*3) = ISEED(3,L)*1000 + INT( ISEED(2,L)/1000000 )
  45    CONTINUE
        IF ( FPRINT  .OR.  DEBUG  .OR.  MOD(ISHW-1,IPROUT).EQ.0 ) THEN
          CALL PRTIME(TTIME)
          WRITE(MONIOU,158) SHOWNO,(L,(ISEED(J,L),J=1,3),L=1,NSEQ)
 158      FORMAT(' AND RANDOM NUMBER GENERATOR AT BEGIN OF EVENT :',I8,
     *            /,(' SEQUENCE = ',I2,'  SEED = ',I9 ,'  CALLS = ',I9,
     *               '  BILLIONS = ',I9))
        ENDIF
C  RESET KNOR
        KNOR = .TRUE.
 
C  GET FULL RANDOM GENERATOR STATUS (103 WORDS PER SEQUENCE)
CC      DO 495  L = 1,NSEQ
CC        CALL RMMAQ( ISEED(1,L), L, 'RV' )
CC        WRITE(MONIOU,658) L,(ISEED(J,L),J=1,103)
CC658     FORMAT ( ' FULL RANDOM NUMBER GENERATOR STATUS ',
CC   *             'FOR SEQUENCE ',I2,/(' ',10I11))
CC495   CONTINUE
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> *** ATENTION *** ATENTION *** ATENTION *** ATENTION *** ATENTION >>
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>>                                                                  >>
c>> In the next block (between this ATENTION comments) CORSIKA makes >>
c>> three things, in this order:                                     >>
c>>                                                                  >>
c>>   i. Set ANGLES OF INCIDENCE (different distributions of theta   >>
c>>      for gammas -flat- and hadrons -standard.                    >>
c>>  ii. Set HEIGHT for start at THICK0 (normally = 0 => 112.8 Km)   >>
c>> iii. Set ENERGY of the primary.                                  >>
c>>                                                                  >>
c>> (The original order was ii., iii. and i.)                        >>
c>>                                                                  >>
c>> JCG Wed Sep 21 10:49:14 MET DST 1998 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C  GET PRIMARY ANGLES OF INCIDENCE
        IF ( FIXINC ) THEN

          THETAP = THETPR(1)
          PHIP   = PHIPR(1)
          PRMPAR(3) = COS(THETAP)

        ELSE

          if ( prmpar(1).eq.1 ) then

C>> GAMMAS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C NOTE!! We will use a FLAT distribution for THETA: 
C Then, next block (original block) must be commented.
c The modificated code follows this block
c
            CALL RMMAR( RD,3,1 )
            CT1 = THETPR(1)
            CT2 = THETPR(2)
            THETAP = RD(2)*(CT2 - CT1) + CT1
            CTT  = COS(THETAP)
            PRMPAR(3) = CTT

          else

C>> HADRONS AND ELECTRONS (AND ANY OTHER BUT GAMMAS) >>>>>>>>>>>>>>>>
c  Choose angles at random with equal flux for all directions
c  with horizontal detector array (see: O.C. Allkofer & P.K.F. Grieder,
c  Cosmic Rays on Earth, in: Physics Data 25/1, H.Behrens & G.Ebel Ed.,
c  (Fachinformationszentrum Karlsruhe, Germany, 1983) chpt. 1.1.2)
c
c            CALL RMMAR( RD,3,1 )
c            CT1 = SIN(THETPR(1))**2
c            CT2 = SIN(THETPR(2))**2
c            CTT  = SQRT( 1.D0 - RD(2)*(CT2 - CT1) - CT1 )
c            PRMPAR(3) = CTT
c            THETAP = ACOS(CTT)
c
c Use equal flux from all directions. This assumes a volume detector
c insensitive on the incidence angle.
c
            call rmmar( rd,2,1 )
            ct1 = cos(thetpr(1))
            ct2 = cos(thetpr(2))
            ctt  = rd(2)*(ct2 - ct1) + ct1
            prmpar(3) = ctt
            thetap = acos(ctt)

          endif

          PHIP = RD(1) * ( PHIPR(2) - PHIPR(1) ) + PHIPR(1)

        ENDIF

        PRMPAR(4) = PHIP


c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C  DEFINE HEIGHT FOR START AT THICK0 (IN G/CM**2) (112.8 KM FOR THICK0=0)
        PRMPAR(5) = HEIGH(THICK0)
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C  GET PRIMARY ENERGY INTO PRMPAR(2)
        IF ( ISPEC .EQ. 0 ) THEN
          PRMPAR(2) = LLIMIT
        ELSE
          CALL RMMAR( RD,1,1 )
          IF ( PSLOPE .NE. -1.D0 ) THEN
            PRMPAR(2) = ( RD(1)*UL + ( 1.D0-RD(1) )*LL )**SLEX
          ELSE
            PRMPAR(2) = LLIMIT * LL**RD(1)
          ENDIF
        ENDIF

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> *** ATENTION *** ATENTION *** ATENTION *** ATENTION *** ATENTION >>
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> Modification: this is no longer needed >>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> (Superseeded by Sphere algorithm, see cerenkov.f) >>>>>>>>>>>>>>>>>
c>> JCG Wed Sep 21 10:49:14 MET DST 1998 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cc Btw, we now modify the "shadow area" of the telescopes,
cc to cover the angle theta. 
c        do 160 i=1,nctels
c          ctpars(i,ctdiam) = ctdiams(i)/cos(thetap)
c          write (MONIOU,*) 
c     *        'New region for CT',i,' = ',ctpars(i,ctdiam)
c 160    continue
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c Here we calculate the angles spinphi and spinthe, which are the 
c phi and theta angles of the particle, with respect to the direction
c where the CT is pointing to. spinthe is the angular displacement
c of the new direction respect to the original (CT); spinphi=0 means
c that the new direction is towards the zenith, spinphi=+-180 means 
c towards the horizont.
c See the document "simulation.tex" 

c First, save the "CT" orientation 
c (moved from a couple of lines below, marked with [*])
        EVTH(11) = THETAP
        EVTH(12) = PHIP
        
        CALL RMMAR( RD,3,1 )

c Then, calculate the new direction relative to the CT direction
        spinphi = RD(1)*PI
        spinthe = RD(2)*spinxi*pi/180.

c And then, RE-calculate the GLOBAL direction in CORSIKA
c We use formulae for spherical triangles
        theprim = acos( cos(THETAP)*cos(spinthe)+
     $                  sin(THETAP)*sin(spinthe)*cos(spinphi) )
        phiprim = asin( sin(spinthe)*sin(spinphi)/sin(theprim) )
        THETAP = theprim
        EVTH(140) = spinthe
        if (RD(3).gt.0.5) then
          PHIP = PHIP - phiprim 
          EVTH(141) = -spinphi
        else
          PHIP = PHIP + phiprim 
          EVTH(141) = spinphi
        endif
        PRMPAR(3) = COS(THETAP)
        PRMPAR(4) = PHIP

C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> Modification (HZA trick) cancelled >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> JCG Wed Sep 21 10:49:14 MET DST 1998 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C     Modificacion hecha por Aitor
c         aitoth = THETAP
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

C  CALCULATE COORDINATE CORRECTION FOR TOP OF ATMOSPHERE
        CALL COORIN( PRMPAR(5) )
 
C  COUNTER FOR PARTICLE OUTPUT
        LH = 0
C  COUNTER FOR CERENKOV OUTPUT
        IF ( LCERFI ) LHCER = 0
C  CALCULATE BUNCH SIZE FOR CERENKOV PHOTONS IF NOT SET IN DATAC
        IF ( ICRSIZ .EQ. 0 ) THEN
          CALL GETBUS( NINT(PRMPAR(1)),SNGL(PRMPAR(2)),SNGL(PRMPAR(3)),
     *                  CERSIZ )
          WRITE(MONIOU,*)'CERENKOV BUNCH SIZE IS CALCULATED TO=',CERSIZ
        ENDIF
C  GET GAMMA FACTOR FROM ENERGY
C  FOR GAMMAS PRMPAR(2) STAYS = ENERGY
        IF ( PRMPAR(1) .NE. 1.D0 )
     *              PRMPAR(2) = PRMPAR(2) / PAMA(NINT(PRMPAR(1)))
 
C  SET PRIMARY TO CURRENT PARTICLE
        DO 3  J = 1,8
          CURPAR(J) = PRMPAR(J)
          NCOUN(J)  = 0
  3     CONTINUE
C  SET WEIGHT
 
C  CALCULATE FIRST INTERACTION POINT IF HADRONIC
        GEN = 0.D0
 
        H = HEIGH(THICK0)
        CALL BOX2
        IF ( FIX1I ) THEN
          CHI = THICK(FIXHEI) / PRMPAR(3)
          H = FIXHEI
          FDECAY = .FALSE.
        ELSE
          H = HEIGH ( CHI*PRMPAR(3) + THICK0 )
        ENDIF
        CHISUM = CHISUM + CHI
        CHISM2 = CHISM2 + CHI**2
        ALEVEL = H
C  INITIALIZE COORDINATE CORRECTIONS FOR HADRONIC PRIMARIES
C  FOR EM PRIMARIES IT IS DONE IN EGS
        HH = MAX( H, 0.D0 )
        IF ( CURPAR(1) .GT. 3.D0 ) CALL COORIN( HH )
 
        IF ( FMUADD ) THEN
          IF ( CURPAR(1) .EQ. 5  .OR.  CURPAR(1) .EQ. 6) THEN
            DO J = 1,MAXLEN
              AMUPAR(J) = CURPAR(J)
            ENDDO
            AMUPAR(5) = PRMPAR(5)
            IF(DEBUG)WRITE(MDEBUG,*)'MAIN  : MUON STORED IN AMUPAR'
            FMUORG = .TRUE.
          ENDIF
        ENDIF
 
C  SET TARGET FLAG IF SELECTED FOR FIRST INTERACTION
        IF ( N1STTR .GT. 0 ) THEN
          FIXTAR  = .TRUE.
          FDECAY  = .FALSE.
          EVTH(6) = REAL(N1STTR)
        ELSE
          FIXTAR  = .FALSE.
          EVTH(6) = 0.0
        ENDIF
 
C  INITIALIZE ARRAYS FOR NKG FOR EACH SHOWER
        IF ( FNKG ) CALL STANKG
 
C  STORE FIRST PARTICLE IN HEADER AND PRINT IT OUT
        EVTH( 2) = REAL(SHOWNO)
        EVTH( 3) = CURPAR(1)
        IF ( CURPAR(1) .EQ. 1.D0 ) THEN
C  PRIMARY ENERGY FOR PHOTONS
          E00    = GAMMA
          E00PN  = GAMMA
          INUCL  = 1
        ELSE
          E00    = GAMMA * PAMA(NINT(CURPAR(1)))
          INUCL  = INT(MAX(1.D0,CURPAR(1)/100.D0))
          E00PN  = E00 / INUCL
        ENDIF
        EVTH(147) = 0.
 
        IF ( FEGS ) THEN
C  PARAMETER FOR ELECTRON AND PHOTON REJECT (CONVERT ENERGY TO MEV)
          EONCUT = .5E-9*SQRT(E00*1000.D0)
          CUTLN  = LOG(EONCUT)
        ENDIF
        EVTH( 4) = E00
        EVTH( 5) = THICK0
        EVTH( 7) = H
        PTOT0    = SQRT( E00**2 - PAMA(NINT(CURPAR(1)))**2 )
        PTOT0N   = PTOT0 / INUCL
        ST       = SQRT(1.D0-COSTHE**2)
        EVTH( 8) = PTOT0 * ST * COS(PHI)
        EVTH( 9) = PTOT0 * ST * SIN(PHI)
        EVTH(10) = PTOT0 * COSTHE
c
c [*] one block from here sent above
c
        EVTH(85) = CERSIZ
 
        IF ( CURPAR(1) .GT. 3.D0 ) THEN
          IF ( FPRINT .OR. DEBUG ) WRITE(MONIOU,102) (CURPAR(J),J = 1,8)
 102      FORMAT (/' PRIMARY PARAMETERS AT FIRST INTERACTION POINT'/
     *               16X,1P,8E10.3)
        ELSE
          IF ( FPRINT .OR. DEBUG ) WRITE(MONIOU,132)
 132      FORMAT (/' PRIMARY PARTICLE IS ELECTROMAGNETIC')
        ENDIF
 
C  WRITE EVENT HEADER INTO BUFFER
C  FOR EM PARTICLES EVTH IS WRITTEN TO BUFFER IN EGS (IF ACTIVE)
        IF ( EVTH(3) .GT. 3.0  .OR.  .NOT. FEGS ) THEN
          CALL TOBUF ( EVTH,0 )
          IF ( LCERFI ) CALL TOBUFC( EVTH,0 )
        ENDIF
 
C  PRINT HEADER FOR HIGH ENERGY PARTICLES
        IF ( FPRINT .OR. DEBUG ) WRITE(MONIOU,103)
 103    FORMAT(/'                   TYPE      GAMMA   COSTHETA ',
     *          '    PHI     HEIGHT     TIME      X-CM      Y-CM   ',
     *          '    GEN      LEVEL  E ON STACK'/)
        NOPART = 0
 
 
        IF ( CURPAR(1) .LE. 3.D0  .OR.
     *      (CURPAR(1) .GE. 5.D0  .AND.  CURPAR(1) .LE. 7.D0) ) THEN
C  GIVE PARTICLE TO EGS OR NKG IF ELECTROMAGNETIC
C  AND TAKE THEN NEXT PARTICLE FROM STACK
C  FLAG FOR NO PRIMARY INTERACTION IS SET FOR ALL BUT ELM. PRIMARIES
          IF ( CURPAR(1) .LE. 3.D0 ) THEN
            FNPRIM = .FALSE.
          ELSE
            FNPRIM = .TRUE.
            H = PRMPAR(5)
          ENDIF
C********* Aitor Ibarra Ibaibarriaga 7-4-2000***********************
C  CORSIKA Bug detected by Dieter Heck. This error was important for 
C  the longitudinal development (variable llongi).
          if ( llongi ) lpct1 = 1
C*********** END ************
          CALL BOX3
          IF ( FEGS ) THEN
            CHISUM = CHISUM + THICK(DBLE(EVTH(7)))
            CHISM2 = CHISM2 + THICK(DBLE(EVTH(7)))**2
          ENDIF
          FIRSTI = .FALSE.
          GOTO 4
 
        ELSE
C  HADRONIC PARTICLES
          FNPRIM = .TRUE.
 
C  FILL LONGITUDINAL DISTRIBUTION FOR THE PRIMARY PARTICLE
C  THE PARTICLE IS TRACKED FROM THICK0 DOWN TO THICK0+CHI*PRMPAR(3)
C  COUNT THE PARTICLES FOR THE LONGITUDINAL DEVELOPMENT
          IF ( LLONGI ) THEN
            LPCT1 = INT( THICK0 * THSTPI )
            LPCT2 = INT( (THICK0 + PRMPAR(3)*CHI) * THSTPI )
            LPCT2 = MIN(NSTEP,LPCT2)
C  GAMMAS, ELECTRONS AND POSITRONS ARE NOT TRANSPORTED HERE, SEE EGS
C  MUONS ARE TRANSPORTED IN MUTRAC
C  HADRONS
            IF     ( ITYPE .GE. 7 .AND. ITYPE .LE. 41 ) THEN
              DO 5004 L = LPCT1,LPCT2
                PLONG(L,6) = PLONG(L,6) + 1.
 5004         CONTINUE
C  CHARGED HADRONS
              IF ( SIGNUM(ITYPE) .NE. 0.D0 ) THEN
                DO 5005 L = LPCT1,LPCT2
                  PLONG(L,7) = PLONG(L,7) + 1.
 5005           CONTINUE
              ENDIF
C  NUCLEI
            ELSEIF ( ITYPE .GT. 100 ) THEN
              DO 5006 L = LPCT1,LPCT2
                PLONG(L,8) = PLONG(L,8) + 1.
 5006         CONTINUE
            ENDIF
          ENDIF
 
C  CHECK OBSERVATION LEVEL PASSAGE AND UPDATE PARTICLE COORDINATES
          HNEW = H
C  FOR UPDATE WE NEED THE START ALTITUDE H
          H = HEIGH(THICK0)
          DO  251  J = 1,NOBSLV
C  JUMP INTO NORMAL PARTICLE TREATMENT FOR HADRONS
            IF ( HNEW .GT. OBSLEV(J) ) THEN
              H = HNEW
              GOTO 6
            ENDIF
            IF ( H .LT. OBSLEV(J) ) GOTO 251
C  REMEMBER NUMBER OF LEVEL FOR OUTPUT
            LEVL   = J
            CALL UPDATE( OBSLEV(J),THCKOB(J),J )
            IF (DEBUG) WRITE(MDEBUG,256) J,IRET1,IRET2
 256        FORMAT(' MAIN  : LEVEL ',I5,' IRET1,2=',2I5)
C  IF PARTICLE IS NOT CUTTED, BRING IT TO OUTPUT
            IF ( IRET2 .EQ. 0 ) THEN
              CALL OUTPUT
            ENDIF
 251      CONTINUE
          IF (DEBUG) WRITE(MDEBUG,*)
     *       'MAIN  : PRIMARY REACHED LOWEST OBSERVATION LEVEL'
          GOTO 4
        ENDIF
 
C-----------------------------------------------------------------------
C  NORMAL CYCLE
  7     CONTINUE
 
C  IF ENERGY TOO SMALL TAKE NEXT PARTICLE
        IF ( GAMMA .LE. 1.D0 ) THEN
          IF ( CURPAR(1) .NE. 1.D0 ) THEN
            IF ( CURPAR(1).EQ.5.D0 .OR. CURPAR(1).EQ.6.D0 )
     *                                        FMUORG = .FALSE.
            GOTO 4
          ENDIF
C  SPECIAL TREATMENT FOR PHOTONS
          ITYPE = 1
          CHI   = 0.D0
          GOTO 5
        ENDIF
 
C  DETERMINE PLACE OF NEXT INTERACTION
        CALL BOX2
 
C  CHECK PASSAGE THROUGH OBSERVATION LEVELS AND TRACK PARTICLES TO THE
C  PLACE OF INTERACTION
  5     CONTINUE
        IRET1 = 0
        CALL BOX3
        IF ( IRET1 .NE. 0 ) GOTO 4
 
  6     CONTINUE
        IRET1 = 0
        MSMM  = 0
 
C  INCREMENT PARTICLE GENERATION AND PROCESS NUCLEAR INTERACTION
        GEN   = GEN + 1.D0
C  INITIALIZE INTERMEDIATE STACK FOR ONE REACTION
        CALL TSTINI
        CALL NUCINT
C  TRANSFER INTERMEDIATE STACK FOR ONE REACTION
        CALL TSTEND
 
C  ENERGY - MULTIPLICITY STATISTICS
        IF ( EKINL .LE. 0.1D0 ) THEN
          MEN = 1
        ELSE
          MEN = 4.D0 + 3.D0 * LOG10(EKINL)
          MEN = MIN( MEN, 37 )
        ENDIF
        IF ( MSMM .LE. 1 ) THEN
          MMU = 1
        ELSE
          MMU = 1.D0 + 3.D0 * LOG10(DBLE(MSMM))
          MMU = MIN( MMU, 13 )
        ENDIF
        MULTMA(MEN,MMU) = MULTMA(MEN,MMU) + 1
        MULTOT(MEN,MMU) = MULTOT(MEN,MMU) + 1
        IF ( DEBUG ) WRITE(MDEBUG,*) 'MAIN  : EKINL,MSMM=',
     *                                  SNGL(EKINL),MSMM
 
        IF ( IRET1 .EQ. 0 ) THEN
          IF ( DEBUG ) WRITE(MDEBUG,666) (CURPAR(II),II=1,11)
 666      FORMAT(' MAIN  : CURPAR=',1P,11E10.3)
          GOTO 7
        ENDIF
 
C  GET NEXT PARTICLE FROM STACK, IF IRET=1 ALL PARTICLES ARE DONE
  4     CONTINUE
        IRET1 = 0
        CALL FSTACK
        IF ( FMUADD ) THEN
          IF ( (CURPAR(1) .EQ. 5  .OR.  CURPAR(1) .EQ. 6)
     *         .AND.  IRET1 .EQ. 0  .AND.  .NOT. FMUORG ) THEN
            DO J = 1,MAXLEN
              AMUPAR(J) = CURPAR(J)
            ENDDO
            IF(DEBUG)WRITE(MDEBUG,*)'MAIN  : MUON STORED IN AMUPAR'
            FMUORG = .TRUE.
          ENDIF
        ENDIF
 
        IF ( IRET1 .EQ. 0 ) GOTO 7
 
C-----------------------------------------------------------------------
C  FINISH SHOWER AND PRINT INFORMATION
        CALL OUTEND
 
 
*       IF ( DEBUG ) WRITE(MDEBUG,442) NPARTO
*442    FORMAT(' MAIN  : NPARTO='/(' ',10F10.0))
 
        IF ( FPRINT .OR. DEBUG ) THEN
          IOBSLV = MIN( 5, NOBSLV )
          WRITE(MONIOU,54) (K,K=1,IOBSLV)
  54      FORMAT (/' PARTICLES AT DETECTOR LEVEL :'/
     *             ' FOR LEVEL         ', 5I13)
          WRITE(MONIOU,55) (OBSLEV(K),K=1,IOBSLV)
  55      FORMAT ( ' HEIGHT IN CM        ',1P, 5E13.3/)
          WRITE(MONIOU,776) 'PROTONS      ',(NPROTO(K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'ANTIPROTONS  ',(NPROTB(K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'NEUTRONS     ',(NNEUTR(K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'ANTINEUTRONS ',(NNEUTB(K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'PHOTONS      ',(NPHOTO(K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'ELECTRONS    ',(NELECT(K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'POSITRONS    ',(NPOSIT(K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'NEUTRINOS    ',(NNU   (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'MU -         ',(NMUM  (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'MU +         ',(NMUP  (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'PI 0         ',(NPI0  (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'PI -         ',(NPIM  (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'PI +         ',(NPIP  (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'K0L          ',(NK0L  (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'K0S          ',(NK0S  (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'K -          ',(NKMI  (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'K +          ',(NKPL  (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'STR. BARYONS ',(NHYP  (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'DEUTERONS    ',(NDEUT (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'TRITONS      ',(NTRIT (K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'ALPHAS       ',(NALPHA(K),K=1,IOBSLV)
          WRITE(MONIOU,776) 'OTHER PARTIC.',(NOTHER(K),K=1,IOBSLV)
          WRITE(MONIOU,*)
          WRITE(MONIOU,776) 'DECAYED MUONS',MUOND
 776      FORMAT(' NO OF ',A13, '= ',5F13.0)
 
          IF ( NOBSLV .GT. 5 ) THEN
            IOBSLV =  NOBSLV
            WRITE(MONIOU,54) (K,K=6,IOBSLV)
            WRITE(MONIOU,55) (OBSLEV(K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'PROTONS      ',(NPROTO(K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'ANTIPROTONS  ',(NPROTB(K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'NEUTRONS     ',(NNEUTR(K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'ANTINEUTRONS ',(NNEUTB(K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'PHOTONS      ',(NPHOTO(K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'ELECTRONS    ',(NELECT(K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'POSITRONS    ',(NPOSIT(K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'NEUTRINOS    ',(NNU   (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'MU -         ',(NMUM  (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'MU +         ',(NMUP  (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'PI 0         ',(NPI0  (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'PI -         ',(NPIM  (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'PI +         ',(NPIP  (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'K0L          ',(NK0L  (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'K0S          ',(NK0S  (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'K -          ',(NKMI  (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'K +          ',(NKPL  (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'STR. BARYONS ',(NHYP  (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'DEUTERONS    ',(NDEUT (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'TRITONS      ',(NTRIT (K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'ALPHAS       ',(NALPHA(K),K=6,IOBSLV)
            WRITE(MONIOU,776) 'OTHER PARTIC.',(NOTHER(K),K=6,IOBSLV)
            WRITE(MONIOU,*)
          ENDIF
        ENDIF
 
C  ADD UP FOR MEAN VALUES
        DO 779  K = 1,25
        DO 779  J = 1,10
          MPARTO(J,K) = MPARTO(J,K) + NPARTO(J,K)
          MPART2(J,K) = MPART2(J,K) + NPARTO(J,K)**2
 779    CONTINUE
        EVTE(2) = SHOWNO
        DO 335  K = 1,NOBSLV
          EVTE(3) = EVTE(3) + NPHOTO(K)
          EVTE(4) = EVTE(4) + NELECT(K) + NPOSIT(K)
          EVTE(5) = EVTE(5) + NPROTO(K) + NPROTB(K) + NNEUTR(K) +
     *              NNEUTB(K) + NPI0(K) + NPIM(K) + NPIP(K) + NK0L(K) +
     *              NK0S(K) + NKMI(K) + NKPL(K) + NHYP(K) +
     *              NDEUT(K) + NTRIT(K) + NALPHA(K) + NOTHER(K)
          EVTE(6) = EVTE(6) + NMUP(K) + NMUM(K)
 335    CONTINUE
        EVTE(7)   = NOPART
 
        IF ( FPRINT .OR. DEBUG ) WRITE(MONIOU,110)
     *                  IFINNU,IFINPI,IFINET,IFINKA,IFINHY,
     *                  IFINNU+IFINPI+IFINET+IFINKA+IFINHY,ELAST
 110    FORMAT(/' NO OF NUCLEONS  PRODUCED IN FIRST INTERACTION =',I10/
     *          ' NO OF PIONS     PRODUCED IN FIRST INTERACTION =',I10/
     *          ' NO OF ETAS      PRODUCED IN FIRST INTERACTION =',I10/
     *          ' NO OF KAONS     PRODUCED IN FIRST INTERACTION =',I10/
     *          ' NO OF S.BARYONS PRODUCED IN FIRST INTERACTION =',I10/
     *          ' TOTAL MULTIPLICITY       OF FIRST INTERACTION =',I10/
     *        ' ELASTICITY               OF FIRST INTERACTION =',F10.4)
 
C  PRINT OUT NKG RESULT FOR ONE SHOWER IF SELECTED
        IF ( FNKG ) CALL AVAGE
 
        IF ( LLONGI ) THEN
C  TREAT LONGITUDINAL DISTRIBUTIONS
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c   calculated here again, 'cos it's rewrite I dont know where
          LPCT1 = INT( THICK0 * THSTPI )
          LPCT2 = INT( (THICK0 + PRMPAR(3)*CHI) * THSTPI )
          LPCT2 = MIN(NSTEP,LPCT2)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          DO 980  J = LPCT1,NSTEP
C  ADD ELECTRONS, POSITRONS, MUONS AND NUCLEI TO THE CHARGED PARTICLES
            PLONG(J,7) = PLONG(J,7) + PLONG(J,2) + PLONG(J,3)
     *                 + PLONG(J,4) + PLONG(J,5) + PLONG(J,8)
C  ADD UP FOR MEAN VALUES OF LONGITUDINAL DISTRIBUTION
            DO 979  K = 1,9
              APLONG(J,K) = APLONG(J,K) + PLONG(J,K)
              SPLONG(J,K) = SPLONG(J,K) + PLONG(J,K)**2
 979        CONTINUE
 980      CONTINUE
 
C  PRINT LONGITUDINAL DISTRIBUTIONS PER SHOWER
          IF ( FPRINT .OR. DEBUG )  WRITE(MONIOU,910) THSTEP,
     *      'GAMMAS','POSITRONS','ELECTRONS','MU-','MU+','HADRONS',
     *      'CHARGED','NUCLEI','CERENKOV',
     *      (J*THSTEP,(PLONG(J,K),K=1,9),J=LPCT1,NSTEP)
 910      FORMAT(/' ---------- LONGITUDINAL DISTRIBUTION IN STEPS OF ',
     *        F5.0,' G/CM**2 ----------------'/
     *        '  DEPTH ',3A12,3A11,A12,A11,A12/
     *        (' ',F6.0,F13.0,2F12.0,3F11.0,F12.0,F11.0,1P,E12.5,0P) )
CJOK  ADAPTED FOR HEAT CALCULATION
C910      FORMAT(/
C    *    ' LONGITUDINAL DISTRIBUTION IN STEPS OF ',F5.0,' G/CM**2'
C    *      /' ',92('=')/'  DEPTH',8A10,A12/1P
C    *      (' ',0P,F6.0,1P,9E11.4))
CJOK
 
          IF ( FLGFIT ) THEN
C  PERFORM FIT TO THE LONGITUDINAL DISTRIBUTION OF ALL CHARGED PARTICLES
C  IF EGS IS SELECTED THIS IS THE DISTRIBUTION WHICH IS TO BE TAKEN
            IF ( FEGS ) THEN
              DO 930 J=0,NSTEP-LPCT1
                DEP(J+1)    = (J+LPCT1)*THSTEP
                CHAPAR(J+1) = PLONG(J+LPCT1,7)
 930          CONTINUE
              NSTP = NSTEP + 1 - LPCT1
              WRITE(MONIOU,8229) 'ALL CHARGED PARTICLES'
 8229         FORMAT(/' FIT OF THE CURVE   ',
     *      ' N(T) = P1*((T-P2)/(P3-P2))**((P3-T)/(P4+P5*T+P6*T**2))'/
     *      ' TO LONGITUDINAL DISTRIBUTION OF ',A35)
C  IF NKG IS SELECTED ONLY THE ELECTRON DISTRIBUTION IS AVAILABLE
            ELSEIF ( FNKG ) THEN
              DEP(1)    = 0.D0
              CHAPAR(1) = 0.D0
              DO 931 J = 1,IALT(1)
                DEP(J+1)    = TLEV(J)
                CHAPAR(J+1) = SL(J)
 931          CONTINUE
              NSTP = IALT(1) + 1
              WRITE(MONIOU,8229) 'NKG ELECTRONS'
C  IF NONE IS SELECTED IT DOES NOT REALLY MAKE SENSE TO FIT
C  BUT LET'S TAKE THEN ALL CHARGED WHICH ARE MUONS AND HADRONS
            ELSE
              DO 932 J=0,NSTEP-LPCT1
                DEP(J+1)    = (J+LPCT1)*THSTEP
                CHAPAR(J+1) = PLONG(J+LPCT1,7)
 932          CONTINUE
              NSTP = NSTEP + 1 - LPCT1
              WRITE(MONIOU,8229) 'MUONS AND CHARGED HADRONS'
            ENDIF
            IF ( NSTP .GT. 6 ) THEN
C  THERE ARE MORE THAN 6 STEP VALUES, A FIT SHOULD BE POSSIBLE.
C  DO THE FIT: NPAR AND FPARAM GIVE THE NUMBER OF PARAMETERS USED
C  AND THE FINAL VALUES FOR THE PARAMETERS. CHISQ GIVES THE CHI**2/DOF
C  FOR THE FIT.
              CALL LONGFT(FPARAM,CHI2)
              WRITE(MONIOU,8230) FPARAM,CHI2,CHI2/SQRT(FPARAM(1))*100.D0
 8230         FORMAT(' PARAMETERS         = ',1P,6E12.4,0P/
     *               ' CHI**2/DOF         = ',F10.1/
     *               ' AV. DEVIATION IN % = ',F10.4)
C  STORE RESULT IN END EVENT BLOCK
              DO 933 K = 1,6
                EVTE(255+K) = FPARAM(K)
 933          CONTINUE
              EVTE(262) = CHI2
            ELSE
              WRITE(MONIOU,*) 'NO LONGI. FIT POSSIBLE, ',
     *          ' NSTP = ',NSTP,'  TOO SMALL.'
              DO 934 K = 1,6
                EVTE(255+K) = 0.
 934          CONTINUE
              EVTE(262) = 0.
            ENDIF
          ENDIF
        ENDIF
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c   Saves statistics to sta###### file
        call jcstadata(EVTH,EVTE,
     +      NPROTO,NPROTB,NNEUTR,NNEUTB,NPHOTO,NELECT,NPOSIT,
     +      NNU   ,NMUM  ,NMUP  ,NPI0  ,NPIM  ,NPIP  ,NK0L  ,
     +      NK0S  ,NKMI  ,NKPL  ,NHYP  ,NDEUT ,NTRIT ,NALPHA,
     +      NOTHER,IFINNU,IFINPI,IFINET,IFINKA,IFINHY,
     +      CERELE,CERHAD,PLONG,LPCT1,NSTEP,THSTEP)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C  WRITE SHOWER END TO OUTPUT BUFFER
c        CALL TOBUF( EVTE,0 )
        CALL TOBUF( EVTE,1 )
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        IF ( LCERFI ) THEN
          CALL OUTND2
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c          CALL TOBUFC( EVTE,0 )
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        ENDIF
 
        IF ( FPRINT .OR. DEBUG ) WRITE(MONIOU,*)
     *                 'CERENKOV PH. FROM ELECTRONS = ',SNGL(CERELE),
     *                 '  CERENKOV PH. FROM HADRONS = ',SNGL(CERHAD)
        CERELE = 0.D0
        CERHAD = 0.D0
        NRECER = 0
 
        IF ( FPRINT .OR. DEBUG ) WRITE(MONIOU,210) SHOWNO
 210    FORMAT(/'   END OF SHOWER NO ',I10)
 
        DO 19  J = 1,37
          JNBIN(J) = JNBIN(J) + INBIN(J)
          JPBIN(J) = JPBIN(J) + IPBIN(J)
          JKBIN(J) = JKBIN(J) + IKBIN(J)
          JHBIN(J) = JHBIN(J) + IHBIN(J)
  19    CONTINUE
 
  2   CONTINUE
C  END OF SHOWER LOOP
 
C-----------------------------------------------------------------------
 992  CONTINUE
 
C  RESET NUMBER OF SHOWERS TO CORRECT VALUE
      ISHW = I
 
      RUNE(3) = REAL(ISHW)
C  WRITE RUN END TO OUTPUT BUFFER AND FINISH OUTPUT
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c      CALL TOBUF ( RUNE,1 )
      call jcendrun(rune)
c      IF ( LCERFI ) CALL TOBUFC( RUNE,1 )
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C  TIME SINCE BEGINNING
c      ILEFTB = TIME()
      ILEFTB = 1
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      TDIFF  = ILEFTB - ILEFTA
 
C  MEAN VALUE FOR FIRST INTERACTION ALTITUDE (G/CM**2)
      IF ( ISHW .GT. 1 ) THEN
        CHISM2 = SQRT( ABS(CHISM2-CHISUM**2/ISHW) / (ISHW-1) )
        CHISUM = CHISUM / ISHW
      ELSE
        CHISM2 = 0.D0
      ENDIF
 
C  OUTPUTS FOR ALL SHOWERS
      WRITE(MONIOU,201) ISHW,TDIFF,TDIFF/ISHW,IRECOR,IRECOR/ISHW,
     *                   CHISUM,CHISM2
 201  FORMAT('1',10('='),' RUN SUMMARY ',56('=')//
     *  ' NUMBER OF GENERATED EVENTS = ',I10,/
     *  ' TOTAL TIME USED            = ',E10.3,' SEC'/
     *  ' TIME PER EVENT             = ',E10.3,' SEC'/
     *  ' TOTAL SPACE ON PATAPE USED = ',I10,' WORDS'/
     *  ' SPACE PER EVENT ON PATAPE  = ',I10,' WORDS'/
     *  ' AVERAGE HEIGHT OF 1ST INT. = ',F10.3,' +-',F10.3,' G/CM**2'/)
 
C  ENERGY - MULTIPLICITY MATRIX FOR ALL SHOWERS
      WRITE(MONIOU,209) (K,K=1,13),
     *  (J,(MULTOT(J,K),K=1,13),10**((J-4.)/3.),10**((J-3.)/3.),J=1,37),
     *   1,(INT(10**((K-1.)/3.)+1),K = 2,13),
     *   2,(INT(10**((K   )/3.)  ),K = 2,13)
 209  FORMAT(//' ENERGY - MULTIPLICITY MATRIX FOR ALL SHOWERS'/
     *       ' ENERGY RUNS VERTICALLY, MULTIPLICITY HORIZONTALLY'//,
     *       ' ',6X,5I9,3I8,5I7,'   ENERGY RANGE (GEV)'/
     *       37(/' ',I4,1X,I10,4I9,3I8,5I7,1X,1P,2E10.1,0P)//
     *       ' MULT. ',5I9,3I8,5I7,4X,'LOWER BIN LIMIT'/
     *       ' RANGE ',5I9,3I8,5I7,4X,'UPPER BIN LIMIT')
 
C  GET MEAN OF ELASTICITY FOR ENERGY BINS
      DO 3377  J = 1,37
        NELMEA = 0
        DO 3378  K = 1,10
          NELMEA = NELMEA + IELDPA(J,K)
 3378   CONTINUE
        IF ( NELMEA .NE. 0 ) ELMEAA(J) = ELMEAA(J) / NELMEA
 3377 CONTINUE
 
C  PRINT ENERGY - ELASTICITY MATRIX FOR ALL SHOWERS
      WRITE(MONIOU,408) (K,K=1,10),  (J,(IELDPA(J,K),K=1,10),
     *  ELMEAA(J),10**((J-4.D0)/3.D0),10**((J-3.)/3.D0),J=1,37),
     *      ((K-1)*0.1D0,K=1,10),(K*0.1D0,K=1,10)
 408  FORMAT (//' ENERGY - ELASTICITY MATRIX FOR ALL SHOWERS'/
     *          ' ENERGY RUNS VERTICALLY, ELASTICITY HORIZONTALLY'//
     *          ' ',5X,10I9,'   MEAN EL.   ENERGY RANGE (GEV)'/
     *          37(/' ',I4,1X,10I9,2X,1P,E10.3,2E10.1,0P)//
     *          ' ELA. ',10F9.2,5X,'LOWER BIN LIMIT'/
     *          ' RANGE',10F9.2,5X,'UPPER BIN LIMIT')
 
      WRITE(MONIOU,204)
 204  FORMAT (//' INTERACTIONS PER KINETIC ENERGY INTERVAL FOR ALL ',
     *    'SHOWERS'//'   BIN    LOWER LIMIT    UPPER LIMIT     ',
     *    'NUCLEON     PIONS     KAONS S.BARYONS      TOTAL'/
     *    12X,'IN GEV',9X,'IN GEV',7X,
     *     '  EVENTS    EVENTS    EVENTS    EVENTS    '//)
      WRITE(MONIOU,207) (I,SABIN(I),SBBIN(I),JNBIN(I),JPBIN(I),JKBIN(I)
     *            ,JHBIN(I),JNBIN(I)+JPBIN(I)+JKBIN(I)+JHBIN(I),I=1,37)
 207  FORMAT(' ',I5,1P,2E15.4,0P,I12,3I10,I11)
 
      IF ( .NOT.GHEISH ) THEN
C  PRINT ELASTICITY STATISTICS
        WRITE(MONIOU,89) (I,(I-1)*.05,I*.05,
     *                   IELIS(I),IELHM(I),IELNU(I),IELPI(I),I = 1,20)
  89    FORMAT (//' ELASTICITY STATISTICS '//
     *          ' BIN   LOW  HIGH EDGE   FOR ISOBARS     HEAVY MESONS',
     *          '  SINGLE NUCLEONS        AND PIONS'/
     *         (' ',I3,'  ',F4.2,'  ',F4.2,'  ',4I17))
      ENDIF
 
C  CALCULATE MEAN VALUES AND STANDARD DEVIATIONS OF PARTICLE NUMBERS
      IF ( ISHW .GT. 1 ) THEN
        DO 879  K = 1,25
        DO 879  J = 1,NOBSLV
          MPART2(J,K) = SQRT( abs(MPART2(J,K)-MPARTO(J,K)**2/ISHW)
     *                                                  /(ISHW-1) )
          MPARTO(J,K) = MPARTO(J,K)/ISHW
 879    CONTINUE
      ELSE
        DO 880  K = 1,25
        DO 880  J = 1,NOBSLV
          MPART2(J,K) = 0.D0
 880    CONTINUE
      ENDIF
 
C  PRINT MEAN VALUES AND STANDARD DEVIATIONS OF PARTICLE NUMBERS
      IOBSLV = MIN( 3, NOBSLV )
      WRITE(MONIOU,854) (K,K=1,IOBSLV)
 854  FORMAT (/ ' AVERAGE NUMBER OF PARTICLES PER EVENT :'/
     *          ' FROM LEVEL NUMBER ', 3(10X,I10,10X) )
      WRITE(MONIOU,855) (OBSLEV(K),K=1,IOBSLV)
 855  FORMAT (  ' HEIGHT IN CM',1P,3(20X,E10.3)/)
 
      WRITE(MONIOU,778)'PROTONS     ',(MPROTO(K),MPROT2(K),K=1,IOBSLV)
      WRITE(MONIOU,778)'ANTIPROTONS ',(MPROTB(K),MPRTB2(K),K=1,IOBSLV)
      WRITE(MONIOU,778)'NEUTRONS    ',(MNEUTR(K),MNETR2(K),K=1,IOBSLV)
      WRITE(MONIOU,778)'ANTINEUTRONS',(MNEUTB(K),MNETB2(K),K=1,IOBSLV)
      WRITE(MONIOU,778)'PHOTONS     ',(MPHOTO(K),MPHOT2(K),K=1,IOBSLV)
      WRITE(MONIOU,778)'ELECTRONS   ',(MELECT(K),MELEC2(K),K=1,IOBSLV)
      WRITE(MONIOU,778)'POSITRONS   ',(MPOSIT(K),MPOSI2(K),K=1,IOBSLV)
      WRITE(MONIOU,778)'NEUTRINOS   ',(MNU   (K),MNU2  (K),K=1,IOBSLV)
      WRITE(MONIOU,778)'MU -        ',(MMUM  (K),MMUM2 (K),K=1,IOBSLV)
      WRITE(MONIOU,778)'MU +        ',(MMUP  (K),MMUP2 (K),K=1,IOBSLV)
      WRITE(MONIOU,778)'PI 0        ',(MPI0  (K),MPI02 (K),K=1,IOBSLV)
      WRITE(MONIOU,778)'PI -        ',(MPIM  (K),MPIM2 (K),K=1,IOBSLV)
      WRITE(MONIOU,778)'PI +        ',(MPIP  (K),MPIP2 (K),K=1,IOBSLV)
      WRITE(MONIOU,778)'K0L         ',(MK0L  (K),MK0L2 (K),K=1,IOBSLV)
      WRITE(MONIOU,778)'K0S         ',(MK0S  (K),MK0S2 (K),K=1,IOBSLV)
      WRITE(MONIOU,778)'K -         ',(MKMI  (K),MKMI2 (K),K=1,IOBSLV)
      WRITE(MONIOU,778)'K +         ',(MKPL  (K),MKPL2 (K),K=1,IOBSLV)
      WRITE(MONIOU,778)'STR. BARYONS',(MHYP  (K),MHYP2 (K),K=1,IOBSLV)
      WRITE(MONIOU,778)'DEUTERONS   ',(MDEUT (K),MDEUT2(K),K=1,IOBSLV)
      WRITE(MONIOU,778)'TRITONS     ',(MTRIT (K),MTRIT2(K),K=1,IOBSLV)
      WRITE(MONIOU,778)'ALPHAS      ',(MALPHA(K),MALPH2(K),K=1,IOBSLV)
      WRITE(MONIOU,778)'OTHER PART. ',(MOTHER(K),MOTH2 (K),K=1,IOBSLV)
      WRITE(MONIOU,*)
 778  FORMAT(' NO OF ',A12,' = ',3(F13.1,' +-',F13.1,' '))
 
      IF ( NOBSLV .GT. 3 ) THEN
        IOBSLV = MIN( 6, NOBSLV )
        WRITE(MONIOU,854) (K,K=4,IOBSLV)
        WRITE(MONIOU,855) (OBSLEV(K),K=4,IOBSLV)
        WRITE(MONIOU,778)'PROTONS     ',(MPROTO(K),MPROT2(K),K=4,IOBSLV)
        WRITE(MONIOU,778)'ANTIPROTONS ',(MPROTB(K),MPRTB2(K),K=4,IOBSLV)
        WRITE(MONIOU,778)'NEUTRONS    ',(MNEUTR(K),MNETR2(K),K=4,IOBSLV)
        WRITE(MONIOU,778)'ANTINEUTRONS',(MNEUTB(K),MNETB2(K),K=4,IOBSLV)
        WRITE(MONIOU,778)'PHOTONS     ',(MPHOTO(K),MPHOT2(K),K=4,IOBSLV)
        WRITE(MONIOU,778)'ELECTRONS   ',(MELECT(K),MELEC2(K),K=4,IOBSLV)
        WRITE(MONIOU,778)'POSITRONS   ',(MPOSIT(K),MPOSI2(K),K=4,IOBSLV)
        WRITE(MONIOU,778)'NEUTRINOS   ',(MNU   (K),MNU2  (K),K=4,IOBSLV)
        WRITE(MONIOU,778)'MU -        ',(MMUM  (K),MMUM2 (K),K=4,IOBSLV)
        WRITE(MONIOU,778)'MU +        ',(MMUP  (K),MMUP2 (K),K=4,IOBSLV)
        WRITE(MONIOU,778)'PI 0        ',(MPI0  (K),MPI02 (K),K=4,IOBSLV)
        WRITE(MONIOU,778)'PI -        ',(MPIM  (K),MPIM2 (K),K=4,IOBSLV)
        WRITE(MONIOU,778)'PI +        ',(MPIP  (K),MPIP2 (K),K=4,IOBSLV)
        WRITE(MONIOU,778)'K0L         ',(MK0L  (K),MK0L2 (K),K=4,IOBSLV)
        WRITE(MONIOU,778)'K0S         ',(MK0S  (K),MK0S2 (K),K=4,IOBSLV)
        WRITE(MONIOU,778)'K -         ',(MKMI  (K),MKMI2 (K),K=4,IOBSLV)
        WRITE(MONIOU,778)'K +         ',(MKPL  (K),MKPL2 (K),K=4,IOBSLV)
        WRITE(MONIOU,778)'STR. BARYONS',(MHYP  (K),MHYP2 (K),K=4,IOBSLV)
        WRITE(MONIOU,778)'DEUTERONS   ',(MDEUT (K),MDEUT2(K),K=4,IOBSLV)
        WRITE(MONIOU,778)'TRITONS     ',(MTRIT (K),MTRIT2(K),K=4,IOBSLV)
        WRITE(MONIOU,778)'ALPHAS      ',(MALPHA(K),MALPH2(K),K=4,IOBSLV)
        WRITE(MONIOU,778)'OTHER PART. ',(MOTHER(K),MOTH2 (K),K=4,IOBSLV)
        WRITE(MONIOU,*)
 
        IF ( NOBSLV .GT. 6 ) THEN
          IOBSLV = MIN( 9, NOBSLV )
          WRITE(MONIOU,854) (K,K=7,IOBSLV)
          WRITE(MONIOU,855) (OBSLEV(K),K=7,IOBSLV)
        WRITE(MONIOU,778)'PROTONS     ',(MPROTO(K),MPROT2(K),K=7,IOBSLV)
        WRITE(MONIOU,778)'ANTIPROTONS ',(MPROTB(K),MPRTB2(K),K=7,IOBSLV)
        WRITE(MONIOU,778)'NEUTRONS    ',(MNEUTR(K),MNETR2(K),K=7,IOBSLV)
        WRITE(MONIOU,778)'ANTINEUTRONS',(MNEUTB(K),MNETB2(K),K=7,IOBSLV)
        WRITE(MONIOU,778)'PHOTONS     ',(MPHOTO(K),MPHOT2(K),K=7,IOBSLV)
        WRITE(MONIOU,778)'ELECTRONS   ',(MELECT(K),MELEC2(K),K=7,IOBSLV)
        WRITE(MONIOU,778)'POSITRONS   ',(MPOSIT(K),MPOSI2(K),K=7,IOBSLV)
        WRITE(MONIOU,778)'NEUTRINOS   ',(MNU   (K),MNU2  (K),K=7,IOBSLV)
        WRITE(MONIOU,778)'MU -        ',(MMUM  (K),MMUM2 (K),K=7,IOBSLV)
        WRITE(MONIOU,778)'MU +        ',(MMUP  (K),MMUP2 (K),K=7,IOBSLV)
        WRITE(MONIOU,778)'PI 0        ',(MPI0  (K),MPI02 (K),K=7,IOBSLV)
        WRITE(MONIOU,778)'PI -        ',(MPIM  (K),MPIM2 (K),K=7,IOBSLV)
        WRITE(MONIOU,778)'PI +        ',(MPIP  (K),MPIP2 (K),K=7,IOBSLV)
        WRITE(MONIOU,778)'K0L         ',(MK0L  (K),MK0L2 (K),K=7,IOBSLV)
        WRITE(MONIOU,778)'K0S         ',(MK0S  (K),MK0S2 (K),K=7,IOBSLV)
        WRITE(MONIOU,778)'K -         ',(MKMI  (K),MKMI2 (K),K=7,IOBSLV)
        WRITE(MONIOU,778)'K +         ',(MKPL  (K),MKPL2 (K),K=7,IOBSLV)
        WRITE(MONIOU,778)'STR. BARYONS',(MHYP  (K),MHYP2 (K),K=7,IOBSLV)
        WRITE(MONIOU,778)'DEUTERONS   ',(MDEUT (K),MDEUT2(K),K=7,IOBSLV)
        WRITE(MONIOU,778)'TRITONS     ',(MTRIT (K),MTRIT2(K),K=7,IOBSLV)
        WRITE(MONIOU,778)'ALPHAS      ',(MALPHA(K),MALPH2(K),K=7,IOBSLV)
        WRITE(MONIOU,778)'OTHER PART. ',(MOTHER(K),MOTH2 (K),K=7,IOBSLV)
          WRITE(MONIOU,*)
 
          IF ( NOBSLV .GT. 9 ) THEN
            IOBSLV = MIN( 10, NOBSLV )
            WRITE(MONIOU,854) (K,K=9,IOBSLV)
            WRITE(MONIOU,855) (OBSLEV(K),K=9,IOBSLV)
        WRITE(MONIOU,778)'PROTONS     ',(MPROTO(K),MPROT2(K),K=9,IOBSLV)
        WRITE(MONIOU,778)'ANTIPROTONS ',(MPROTB(K),MPRTB2(K),K=9,IOBSLV)
        WRITE(MONIOU,778)'NEUTRONS    ',(MNEUTR(K),MNETR2(K),K=9,IOBSLV)
        WRITE(MONIOU,778)'ANTINEUTRONS',(MNEUTB(K),MNETB2(K),K=9,IOBSLV)
        WRITE(MONIOU,778)'PHOTONS     ',(MPHOTO(K),MPHOT2(K),K=9,IOBSLV)
        WRITE(MONIOU,778)'ELECTRONS   ',(MELECT(K),MELEC2(K),K=9,IOBSLV)
        WRITE(MONIOU,778)'POSITRONS   ',(MPOSIT(K),MPOSI2(K),K=9,IOBSLV)
        WRITE(MONIOU,778)'NEUTRINOS   ',(MNU   (K),MNU2  (K),K=9,IOBSLV)
        WRITE(MONIOU,778)'MU -        ',(MMUM  (K),MMUM2 (K),K=9,IOBSLV)
        WRITE(MONIOU,778)'MU +        ',(MMUP  (K),MMUP2 (K),K=9,IOBSLV)
        WRITE(MONIOU,778)'PI 0        ',(MPI0  (K),MPI02 (K),K=9,IOBSLV)
        WRITE(MONIOU,778)'PI -        ',(MPIM  (K),MPIM2 (K),K=9,IOBSLV)
        WRITE(MONIOU,778)'PI +        ',(MPIP  (K),MPIP2 (K),K=9,IOBSLV)
        WRITE(MONIOU,778)'K0L         ',(MK0L  (K),MK0L2 (K),K=9,IOBSLV)
        WRITE(MONIOU,778)'K0S         ',(MK0S  (K),MK0S2 (K),K=9,IOBSLV)
        WRITE(MONIOU,778)'K -         ',(MKMI  (K),MKMI2 (K),K=9,IOBSLV)
        WRITE(MONIOU,778)'K +         ',(MKPL  (K),MKPL2 (K),K=9,IOBSLV)
        WRITE(MONIOU,778)'STR. BARYONS',(MHYP  (K),MHYP2 (K),K=9,IOBSLV)
        WRITE(MONIOU,778)'DEUTERONS   ',(MDEUT (K),MDEUT2(K),K=9,IOBSLV)
        WRITE(MONIOU,778)'TRITONS     ',(MTRIT (K),MTRIT2(K),K=9,IOBSLV)
        WRITE(MONIOU,778)'ALPHAS      ',(MALPHA(K),MALPH2(K),K=9,IOBSLV)
        WRITE(MONIOU,778)'OTHER PART. ',(MOTHER(K),MOTH2 (K),K=9,IOBSLV)
            WRITE(MONIOU,*)
          ENDIF
 
        ENDIF
      ENDIF
 
C  PRINT OUT NKG RESULT FOR ALL SHOWERS IF SELECTED
      IF ( FNKG ) CALL MITAGE
 
C  CALCULATE MEAN VALUES AND SIGMAS OF LONGITUDINAL DISTRIBUTION
      IF ( LLONGI ) THEN
        IF ( ISHW .GT. 1 ) THEN
          DO 790  K = 1,9
            DO 789  J = LPCT1,NSTEP
              SPLONG(J,K) = SQRT( abs(SPLONG(J,K)-APLONG(J,K)**2/ISHW)
     *                                                   /(ISHW-1) )
              APLONG(J,K) = APLONG(J,K)/ISHW
 789        CONTINUE
 790      CONTINUE
        ELSE
          DO 990  K = 1,9
            DO 989  J = LPCT1,NSTEP
              SPLONG(J,K) = 0.D0
 989        CONTINUE
 990      CONTINUE
        ENDIF
 
C  PRINT AVERAGE LONGITUDINAL DISTRIBUTIONS
        WRITE(MONIOU,911) THSTEP,
     *     'GAMMAS ','POSITRONS','ELECTRONS','MU-  ','MU+  ',
     *     (J*THSTEP,(APLONG(J,K),SPLONG(J,K),K=1,5),J=LPCT1,NSTEP)
 911    FORMAT(/' AVERAGE LONGITUDINAL DISTRIBUTION IN STEPS OF ',
     *      F5.0,' G/CM**2 '/' ',131('=')/
     *      ' DEPTH',8X,3(A10,16X),A9,15X,A9 //
     *     (' ',F5.0,2X,1P,E11.4,'+-',E11.4,0P,1X,F12.0,'+-',F11.0,
     *                1X,F12.0,'+-',E11.4,1X,F11.1,'+-',F10.1,
     *                1X,F11.1,'+-',F10.1 ))
        WRITE(MONIOU,912) THSTEP,
     *     'HADRONS','CHARGED','NUCLEI','CERENKOV',
     *     (J*THSTEP,(APLONG(J,K),SPLONG(J,K),K=6,9),J=LPCT1,NSTEP)
 912    FORMAT(/' AVERAGE LONGITUDINAL DISTRIBUTION IN STEPS OF ',
     *      F5.0,' G/CM**2 '/' ',115('=')/
     *      ' DEPTH',8X,A9,16X,A10,16X,A9,21X,A9 //
     *     (' ',F5.0,1X,F11.1,'+-',F11.1,1X,F12.0,'+-',F12.0,
     *                2X,F10.1,'+-',F10.1,1X,1P,E16.6,'+-',E16.6,0P))
      ENDIF
 
      IF ( FLGFIT ) THEN
C  PERFORM FIT TO THE LONGITUDINAL DISTRIBUTION OF ALL CHARGED PARTICLES
C  IF EGS IS SELECTED THIS IS THE DISTRIBUTION WHICH IS TO BE TAKEN
        IF ( FEGS ) THEN
          DO 730 J=0,NSTEP-LPCT1
            DEP(J+1)    = (J+LPCT1)*THSTEP
            CHAPAR(J+1) = APLONG(J+LPCT1,7)
 730      CONTINUE
          NSTP = NSTEP + 1 - LPCT1
          WRITE(MONIOU,8229) 'AVERAGE ALL CHARGED PARTICLES'
C  IF NKG IS SELECTED ONLY THE ELECTRON DISTRIBUTION IS AVAILABLE
        ELSEIF ( FNKG ) THEN
          DEP(1)    = 0.D0
          CHAPAR(1) = 0.D0
          DO 731 J = 1,IALT(1)
            DEP(J+1)    = TLEV(J)
            CHAPAR(J+1) = SEL(J)/ISHW
 731      CONTINUE
          NSTP = IALT(1) + 1
          WRITE(MONIOU,8229) 'AVERAGE NKG ELECTRONS'
C  IF NONE IS SELECTED IT DOES NOT REALLY MAKE SENSE TO FIT
C  BUT LET'S TAKE THEN ALL CHARGED WHICH ARE MUONS AND HADRONS
        ELSE
          DO 732 J=0,NSTEP-LPCT1
            DEP(J+1)    = (J+LPCT1)*THSTEP
            CHAPAR(J+1) = APLONG(J+LPCT1,7)
 732      CONTINUE
          NSTP = NSTEP + 1 - LPCT1
          WRITE(MONIOU,8229) 'AVERAGE MUONS AND CHARGED HADRONS'
        ENDIF
        IF ( NSTP .GT. 6 ) THEN
C  THERE ARE MORE THAN 6 STEP VALUES, A FIT SHOULD BE POSSIBLE.
C  DO THE FIT: NPAR AND FPARAM GIVE THE NUMBER OF PARAMETERS USED
C  AND THE FINAL VALUES FOR THE PARAMETERS. CHISQ GIVES THE CHI**2/DOF
C  FOR THE FIT.
          CALL LONGFT(FPARAM,CHI2)
          WRITE(MONIOU,8230) FPARAM,CHI2,CHI2/SQRT(FPARAM(1))*100.D0
        ELSE
          WRITE(MONIOU,*) 'NO LONGI. FIT POSSIBLE, ',
     *                      ' NSTP = ',NSTP,'  TOO SMALL.'
        ENDIF
      ENDIF
 
 
C  CONTROL PRINT OUTPUT OF CONSTANTS
      IF ( DEBUG ) THEN
        CALL STAEND
        WRITE(MDEBUG,*) 'MAIN  : STAEND CALLED'
      ENDIF
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      call jcenddata(runh,rune)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      WRITE(MONIOU,*)' '
      CALL PRTIME(TTIME)
      WRITE(MONIOU,101)
 101  FORMAT (/' ',10('='),' END OF RUN ',67('='))
 
C  CLOSE ALL OPEN UNITS
      IF ( MONIOU .NE. 6 ) CLOSE( MONIOU )
      IF ( MDEBUG .NE. 6 ) CLOSE( MDEBUG )
      CLOSE( EXST )
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c      CLOSE( PATAPE )
c      IF ( LCERFI ) CLOSE( CETAPE )
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 
      STOP
      END
