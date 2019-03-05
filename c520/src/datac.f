      SUBROUTINE DATAC
 
C-----------------------------------------------------------------------
C  DATA C(ARDS)
C
C  READS DATA CARDS FROM UNIT 5 TO STEER RUN.
C  READING IS FREE FORMAT WITH BLANK AS SEPARATOR.
C  EACH KEYWORD STARTS ON A NEW LINE LEFTSHIFTED.
C  THIS SUBROUTINE IS CALLED FROM START
C
C  AUTHOR  : J. KNAPP   IK1  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
c      IMPLICIT NONE

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c All this lines are under test
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      parameter (xct=1)
      parameter (yct=2)
      parameter (zct=3)
      parameter (ctthet=4)
      parameter (ctphi=5)
      parameter (ctdiam=6)
      parameter (ctfoc=7)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

*KEEP,DPMFLG.
      COMMON /DPMFLG/  NFLAIN,NFLDIF,NFLPI0,NFLCHE,NFLPIF,NFRAGM
      INTEGER          NFLAIN,NFLDIF,NFLPI0,NFLCHE,NFLPIF,NFRAGM
*KEEP,ELABCT.
      COMMON /ELABCT/  ELCUT
      DOUBLE PRECISION ELCUT(4)
*KEEP,ETHMAP.
      COMMON /ETHMAP/  ECTMAP,ELEFT
      DOUBLE PRECISION ECTMAP,ELEFT
*KEEP,LONGI.
      COMMON /LONGI/   APLONG,HLONG,PLONG,SPLONG,THSTEP,THSTPI,
     *                 NSTEP,LLONGI,FLGFIT
      DOUBLE PRECISION APLONG(0:1040,9),HLONG(0:1024),PLONG(0:1040,9),
     *                 SPLONG(0:1040,9),THSTEP,THSTPI
      INTEGER          NSTEP
      LOGICAL          LLONGI,FLGFIT
*KEEP,MAGANG.
      COMMON /MAGANG/  ARRANG,ARRANR,COSANG,SINANG
      DOUBLE PRECISION ARRANG,ARRANR,COSANG,SINANG
*KEEP,MAGNET.
      COMMON /MAGNET/  BX,BZ,BVAL,BNORMC,BNORM,COSB,SINB,BLIMIT
      DOUBLE PRECISION BX,BZ,BVAL,BNORMC
      REAL             BNORM,COSB,SINB,BLIMIT
*KEEP,MUMULT.
      COMMON /MUMULT/  CHC,OMC,FMOLI
      DOUBLE PRECISION CHC,OMC
      LOGICAL          FMOLI
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
*KEEP,VENPAR.
      COMMON /VENPAR/  PARVAL,NPARAM,PARCHA
      REAL             PARVAL(100)
      INTEGER          NPARAM
      CHARACTER*6      PARCHA(100)
*KEEP,VENUS.
      COMMON /VENUS/   ISH0,IVERVN,MTAR99,FVENUS,FVENSG
      INTEGER          ISH0,IVERVN,MTAR99
      LOGICAL          FVENUS,FVENSG
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
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c All this lines are under test
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
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
      character *72 ctfile

      character *6  keyw
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c     Angles for the "spinning" of a particle around the 
c     main axis of the CT
      common /spinang/ spinxi
      double precision spinxi
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*KEND.
 
      DOUBLE PRECISION R1,R2
      INTEGER          I,IE,IOBSLV,IS,ISEQ,MMM,MONNEW,NNTYP
      INTEGER          IPARAM
      CHARACTER        LINE*80, CFMTF*7, CFMTI*5, CFMTL*4
      DATA             CFMTF/'(F10.0)'/, CFMTI/'(I11)'/, CFMTL/'(L1)'/

      character*100 cifile
      character*100 cofile
      integer iargc
      intrinsic getarg
      intrinsic iargc
     
C-----------------------------------------------------------------------
 
C  WRITE TITEL
      WRITE(MONIOU,999)
 999  FORMAT(' ',10('='),' USERS RUN DIRECTIVES FOR THIS SIMULATION ',
     *    27('=')/)
 
C  DEFAULT VALUES FOR ALL RUN PARAMETERS
      ISEQ = 0
      NSEQ = 2
      ISEED(1,1) = 1
      ISEED(2,1) = 0
      ISEED(3,1) = 0
      ISEED(1,2) = 2
      ISEED(2,2) = 0
      ISEED(3,2) = 0
      ISEED(1,3) = 3
      ISEED(2,3) = 0
      ISEED(3,3) = 0
      NRRUN     = 1
      SHOWNO    = 0
      LLIMIT    = 1.D4
      ULIMIT    = 1.D4
      PSLOPE    = 0.D0
      PRMPAR(1) = 14.D0
      THETPR(1) = 0.D0
      THETPR(2) = 0.D0
      PHIPR(1)  = 0.D0
      PHIPR(2)  = 0.D0
      NSHOW     = 10
      IOBSLV    = 0
      NOBSLV    = 1
      OBSLEV(1) = 110.D2
      ELCUT(1)  = 0.3D0
      ELCUT(2)  = 0.3D0
      ELCUT(3)  = 0.003D0
      ELCUT(4)  = 0.003D0
      ECTMAP  = 1.D4
      NFLAIN  = 0
      NFLDIF  = 0
      NFLPI0  = 0
      NFLPIF  = 0
      NFLCHE  = 0
      NFRAGM  = 0
      FNKG    = .TRUE.
      FMOLI   = .TRUE.
      FMUADD  = .FALSE.
      FEGS    = .FALSE.
      STEPFC  = 10.
      MAXPRT  = 10
      BX      = 20.D0
      BZ      = 42.8D0
      ARRANG  =  0.D0
      LLONGI  = .FALSE.
      FLGFIT  = .FALSE.
      THSTEP  = 20.D0
      RADNKG  = 200.D2
C  BORDER BETWEEN LOW AND HIGH ENERGY INTERACTION MODELS
C  SET BY DEFAULT TO ELAB = 80 GEV
      HILOELB = 80.D0
      GHEISH  = .TRUE.
      FDBASE  = .TRUE.
      DEBUG   = .FALSE.
      DEBDEL  = .FALSE.
      NDEBDL  = 100000000
      THICK0  = 0.D0
      FIX1I   = .FALSE.
      FIXHEI  = 0.D0
      DSN     =
     *'ANYNAMEUPTO64CHARACTERS                                         '
      HOST    = '                    '
      USER    = '                    '
      WAVLGL  = 300.D0
      WAVLGU  = 450.D0
      CERSIZ  = 0.
      NCERX   = 27
      NCERY   = 27
      DCERX   = 1500.
      DCERY   = 1500.
      ACERX   = 100.
      ACERY   = 100.
      LCERFI  = .TRUE.
      ICERML  = 1
      XSCATT  = 0.
      YSCATT  = 0.
      DO  554  I = 1,20
        CERXOS(I) = 0.
        CERYOS(I) = 0.
 554  CONTINUE
      FVENUS  =.TRUE.
      ISH0    = 91
      IPARAM  = 0
      NPARAM  = 0
      DO  555  I = 1,100
        PARVAL(I) = 0.
 555  CONTINUE
      FVENSG  =.FALSE.
 
C-----------------------------------------------------------------------
C  OPEN DATASET FOR COMMANDS
c      IF ( MONIIN .NE. 5 ) THEN
c        OPEN(UNIT=MONIIN,FILE='INPUTS',STATUS='OLD',FORM='FORMATTED')
c        WRITE(MONIOU,*) 'DATA CARDS FOR RUN STEERING ARE ',
c     *                  'EXPECTED FROM UNIT',MONIIN
c      ELSE
c        WRITE(MONIOU,*) 'DATA CARDS FOR RUN STEERING ARE ',
c     *                  'EXPECTED FROM STANDARD INPUT'
c      ENDIF

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c All this lines are under test
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      if (iargc() .gt. 0) then
        write(*,*) 'Getting command line parameters (cifile)...'
        call getarg(1, cifile)
        moniin=21
        if (iargc() .gt. 1) then
          write(*,*) 'Getting command line parameters (cofile)...'
          call getarg(2, cofile)
          moniou=22
        endif
        OPEN(UNIT=MONIIN,FILE=cifile,STATUS='OLD',FORM='FORMATTED')
        OPEN(UNIT=MONIOU,FILE=cofile,STATUS='UNKNOWN')
        WRITE(MONIOU,*) 'DATA CARDS FOR RUN STEERING ARE ',
     *                  'EXPECTED FROM FILE ',cifile
        WRITE(MONIOU,*) 'LOG OUTPUT WILL BE WRITTEN INTO FILE ',cofile
      else
        IF ( MONIIN .NE. 5 ) THEN
          OPEN(UNIT=MONIIN,FILE='INPUTS',STATUS='OLD',FORM='FORMATTED')
          WRITE(MONIOU,*) 'DATA CARDS FOR RUN STEERING ARE ',
     *        'EXPECTED FROM UNIT',MONIIN
        ELSE
          WRITE(MONIOU,*) 'DATA CARDS FOR RUN STEERING ARE ',
     *        'EXPECTED FROM STANDARD INPUT'
        ENDIF
      endif
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 
C-----------------------------------------------------------------------
 1    CONTINUE
 
C  ERASE 'LINE' BY FILLING WITH BLANKS
      DO 2 I=1,80
        LINE(I:I) = ' '
 2    CONTINUE
 
C  GET A NEW INPUT LINE AND PRINT IT
      READ(MONIIN,500,END=1000) LINE
 500  FORMAT(A80)
      IF (DEBUG) THEN
        WRITE(MDEBUG,501) LINE
 501    FORMAT(' DATAC : ',A80)
      ELSE
        WRITE(MONIOU,502) LINE
 502    FORMAT(' ',A80)
      ENDIF
 
C  CONVERT LOWER CASE CHARACTERS TO UPPER CASE
      DO 3 I=1,5
        CALL LOWUP(LINE(I:I))
 3    CONTINUE
      IF ( LINE(1:4).NE.'HOST' .AND. LINE(1:4).NE.'USER' ) THEN
        CALL LOWUP(LINE(6:6))
        IF ( LINE(1:6).NE.'DIRECT' .AND. LINE(1:6).NE.'HISTDS' ) THEN
          DO 4 I=7,80
            CALL LOWUP(LINE(I:I))
 4        CONTINUE
        ENDIF
      ENDIF
 
C-----------------------------------------------------------------------
C  INTERPRET KEYWORD AND READ PARAMETERS
 
C  DUMMY LINE (MAY BE USED FOR COMMENTS) NO ACTION
      IF     ( LINE(1:6) .EQ. '      ' ) THEN
      ELSEIF ( LINE(1:1) .EQ. '*'      ) THEN
      ELSEIF ( LINE(1:2) .EQ. 'C '     ) THEN
 
C  GET ANGLE (DEGREES) BETWEEN ARRAY X-DIRCTION AND MAGNETIC NORD
      ELSEIF ( LINE(1:6) .EQ. 'ARRANG' ) THEN
        IS = 6
 11     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 11
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) ARRANG
 
C  GET CERENKOV ARRAY SPECIFICATIONS
      ELSEIF ( LINE(1:6) .EQ. 'CERARY' ) THEN
        IS = 6
 21     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 21
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NCERX
        IS = IE
 22     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 22
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NCERY
        IS = IE
 23     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 23
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) DCERX
        IS = IE
 24     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 24
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) DCERY
        IS = IE
 25     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 25
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) ACERX
        IS = IE
 26     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 26
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) ACERY
 
C  GET CERENKOV OUTPUT FLAG
      ELSEIF ( LINE(1:6) .EQ. 'CERFIL' ) THEN
        IS = 6
 31     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 31
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I1)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) LCERFI
 
C  GET MAXIMUM BUNCH SIZE FOR CERENKOV PHOTONS
      ELSEIF ( LINE(1:6) .EQ. 'CERSIZ' ) THEN
        IS = 6
 36     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 36
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) CERSIZ
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C  get maximum "xi" angle, respecto to the CT direction
      ELSEIF ( LINE(1:3) .EQ. 'XIP' ) THEN
        IS = 3
 41     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 41
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) spinxi
 
C  GET CERENKOV EVENT SCATTERING INFORMATION
      ELSEIF ( LINE(1:5) .EQ. 'CSCAT' ) THEN
        IS = 5
 43     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 43
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) ICERML
        IS = IE
 44     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 44
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) XSCATT
        IS = IE
 45     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 45
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) YSCATT
 
C  GET CERENKOV WAVELENGTH BAND
      ELSEIF ( LINE(1:6) .EQ. 'CWAVLG' ) THEN
        IS = 6
 46     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 46
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) R1
        IS = IE
 47     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 47
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) R2
        WAVLGL = MIN( R1, R2 )
        WAVLGU = MAX( R1, R2 )
 
C  GET DATABASE FLAG
      ELSEIF ( LINE(1:6) .EQ. 'DATBAS' ) THEN
        IS = 6
 50     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 50
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) FDBASE
 
C  GET DEBUG FLAG AND DELAYED DEBUG PARAMETERS
      ELSEIF ( LINE(1:5) .EQ. 'DEBUG' ) THEN
        IS = 5
 51     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 51
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I1)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) DEBUG
        IS = IE
 52     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 52
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) MMM
        IS = IE
 53     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 53
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I1)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) DEBDEL
        IS = IE
 54     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 54
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NDEBDL
        IF ( MMM .LE. 0  .OR.  MMM .GT. 99 ) THEN
          MDEBUG = 6
        ELSE
          MDEBUG = MMM
        ENDIF
 
C  GET OUTPUT DIRECTORY FOR CALCULATIONS ON DEC-STATION OR TRANSPUTER
      ELSEIF ( LINE(1:6) .EQ. 'DIRECT' ) THEN
        DO 70 I=1,79
          DSN(I:I) = ' '
 70     CONTINUE
        IS = 6
        IF ( LINE(IS+1:80) .NE. ' ' ) THEN
 71       CONTINUE
          IS = IS + 1
          IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 71
          IE = INDEX(LINE(IS:),' ') + IS - 2
          IF ( IE-IS .GT. 63 ) THEN
            IE = IS + 63
            DSN(1:IE-IS+1) = LINE(IS:IE)
            WRITE(MONIOU,*)
     *        'DATAC: DATASETNAME TOO LONG AND TRUNCATED TO:',DSN(1:64)
          ELSE
            DSN(1:IE-IS+1) = LINE(IS:IE)
          ENDIF
        ENDIF
 
C  GET ENERGY CUTS FOR PARTICLE PRINTOUT
      ELSEIF ( LINE(1:6) .EQ. 'ECTMAP' ) THEN
        IS = 6
 81     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 81
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) ECTMAP
 
C  GET ENERGY CUTS FOR HADRONS, MUONS, ELECTRONS, AND PHOTONS
      ELSEIF ( LINE(1:5) .EQ. 'ECUTS' ) THEN
        IS = 5
 91     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 91
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) ELCUT(1)
        IS = IE
 92     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 92
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) ELCUT(2)
        IS = IE
 93     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 93
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) ELCUT(3)
        IS = IE
 94     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 94
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) ELCUT(4)
 
C  GET FLAGS FOR ELECTROMAGNETIC OPTIONS (NKG, EGS)
      ELSEIF ( LINE(1:6) .EQ. 'ELMFLG' ) THEN
        IS = 6
201     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 201
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I1)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) FNKG
        IS = IE
202     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 202
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I1)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) FEGS
 
C  GET ENERGY RANGE OF PRIMARY PARTICLE
      ELSEIF ( LINE(1:6) .EQ. 'ERANGE' ) THEN
        IS = 6
211     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 211
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) R1
        IS = IE
212     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 212
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) R2
        LLIMIT = MIN( R1, R2 )
        ULIMIT = MAX( R1, R2 )
 
C  GET SLOPE OF ENERGY SPECTRUM OF PRIMARY PARTICLE
      ELSEIF ( LINE(1:6) .EQ. 'ESLOPE' ) THEN
        IS = 6
221     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 221
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) PSLOPE
 
C  GET EVENT NUMBER
      ELSEIF ( LINE(1:5) .EQ. 'EVTNR' ) THEN
        IS = 5
231     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 231
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) SHOWNO
        SHOWNO = MAX( SHOWNO-1, 0 )
 
C  END OF DATA CARD INPUT
      ELSEIF ( LINE(1:4) .EQ. 'EXIT' ) THEN
        IF ( DEBUG ) THEN
          WRITE(MONIOU,*) 'DATAC : END OF DATACARD INPUT'
        ELSE
          WRITE(MONIOU,*)
          WRITE(MONIOU,*) 'END OF DATACARD INPUT'
        ENDIF
        RETURN
 
C  GET FIXED HEIGHT (G/CM**2) OF PARTICLE START
      ELSEIF ( LINE(1:6) .EQ. 'FIXCHI' ) THEN
        IS = 6
241     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 241
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) THICK0
 
C  GET FIXED HEIGHT OF FIRST INTERACTION AND FIRST TARGET
      ELSEIF ( LINE(1:6) .EQ. 'FIXHEI' ) THEN
        IS = 6
251     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 251
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) FIXHEI
        IS = IE
252     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 252
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) N1STTR
        IF ( FIXHEI .GT. 0. ) FIX1I = .TRUE.
 
C  GET FLAG FOR GHEISHA LOW ENERGY HADRONIC INTERACTION MODEL
      ELSEIF ( LINE(1:6) .EQ. 'GHEISH' ) THEN
        IS = 6
261     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 261
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I1)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) GHEISH
 
C  GET FLAGS FOR HADRON INTERACTION OPTIONS
      ELSEIF ( LINE(1:6) .EQ. 'HADFLG' ) THEN
        IS = 6
271     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 271
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NFLAIN
        IS = IE
272     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 272
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NFLDIF
        IS = IE
273     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 273
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NFLPI0
        IS = IE
274     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 274
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NFLPIF
        IS = IE
275     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 275
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NFLCHE
        IS = IE
276     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 276
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NFRAGM
 
C  GET NAME OF HOST COMPUTER
      ELSEIF ( LINE(1:4) .EQ. 'HOST' ) THEN
        DO 286 I=1,20
          HOST(I:I) = ' '
 286    CONTINUE
        IS = 4
 287    CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 287
        IE = INDEX(LINE(IS:),' ') + IS - 2
        IF ( IE-IS .GT. 19 ) THEN
          IE = IS + 19
          HOST(1:IE-IS+1) = LINE(IS:IE)
          WRITE(MONIOU,*)
     *        'DATAC: HOSTNAME TOO LONG AND TRUNCATED TO:',HOST(1:20)
        ELSE
          HOST(1:IE-IS+1) = LINE(IS:IE)
        ENDIF
 
C  GET PARAMETER FOR LONGITUDINAL DEVELOPMENT
      ELSEIF ( LINE(1:5) .EQ. 'LONGI' ) THEN
        IS = 5
301     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 301
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I1)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) LLONGI
        IS = IE
302     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 302
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) THSTEP
        IS = IE
303     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 303
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I1)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) FLGFIT
 
C  GET PARAMETERS OF MAGNETIC FIELD
      ELSEIF ( LINE(1:6) .EQ. 'MAGNET' ) THEN
        IS = 6
311     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 311
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) BX
        IS = IE
312     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 312
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) BZ
 
C  GET NUMBER OF EVENTS TO BE PRINTED
      ELSEIF ( LINE(1:6) .EQ. 'MAXPRT' ) THEN
        IS = 6
321     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 321
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) MAXPRT
        IF ( MAXPRT .LT. 0 ) MAXPRT = 10
 
C  GET FLAG FOR ADDITIONAL MUON INFORMATION ON PATAPE
      ELSEIF ( LINE(1:6) .EQ. 'MUADDI' ) THEN
        IS = 6
331     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 331
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I1)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) FMUADD
 
C  GET FLAG FOR MUON MULTIPLE SCATTERING (T=MOLIERE, F=GAUSS)
      ELSEIF ( LINE(1:6) .EQ. 'MUMULT' ) THEN
        IS = 6
336     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 336
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I1)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) FMOLI
 
C  GET NUMBER OF SHOWERS TO BE PRODUCED
      ELSEIF ( LINE(1:5) .EQ. 'NSHOW' ) THEN
        IS = 5
341     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 341
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NSHOW
        IF ( NSHOW .LE. 0 ) NSHOW = 1
 
C  GET HEIGHT OF OBSERVATION LEVELS
      ELSEIF ( LINE(1:6) .EQ. 'OBSLEV' ) THEN
        IOBSLV = IOBSLV + 1
        IF ( IOBSLV .LE. 10 )  THEN
          IS = 6
351       CONTINUE
          IS = IS + 1
          IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 351
          IE = INDEX(LINE(IS:),' ') + IS - 2
          WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
          READ(LINE(IS:IE),CFMTF) OBSLEV(IOBSLV)
          NOBSLV = IOBSLV
        ELSE
          WRITE(MONIOU,*) 'DATAC : TOO MUCH OBSERVATION LEVELS,',
     *                    ' IGNORE IT'
        ENDIF
 
C  GET NEW MONITOR OUTPUT UNIT
      ELSEIF ( LINE(1:6) .EQ. 'OUTPUT' ) THEN
        IS = 6
361     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 361
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) MONNEW
        WRITE(MONIOU,593) MONIOU,MONNEW
 593    FORMAT(' ATTENTION'/' ========='/
     *         ' PRINTER OUTPUT REDIRECTED FROM UNIT ',I3,
     *         ' TO UNIT ',I3)
        MONIOU = MONNEW
 
C  GET PHI OF PRIMARY PARTICLE
      ELSEIF ( LINE(1:4) .EQ. 'PHIP' ) THEN
        IS = 4
371     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 371
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) R1
        IS = IE
372     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 372
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) R2
        PHIPR(1) = MIN( R1, R2 )
        PHIPR(2) = MAX( R1, R2 )
 
C  GET TYPE OF PRIMARY PARTICLE
      ELSEIF ( LINE(1:6) .EQ. 'PRMPAR' ) THEN
        IS = 6
381     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 381
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NNTYP
        PRMPAR(1) = NNTYP
 
C  GET WIDTH OF NKG LATERAL DISTRIBUTION
      ELSEIF ( LINE(1:6) .EQ. 'RADNKG' ) THEN
        IS = 6
389     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 389
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) RADNKG
 
C  GET RUN NUMBER
      ELSEIF ( LINE(1:5) .EQ. 'RUNNR' ) THEN
        IS = 5
391     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 391
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) NRRUN
        NRRUN = ABS(NRRUN)
 
C  GET SEEDS OF RANDOM NUMBER SEQUENCES
      ELSEIF ( LINE(1:4) .EQ. 'SEED' ) THEN
        ISEQ = ISEQ + 1
        IF ( ISEQ .LE. 10 )  THEN
          IS = 4
401       CONTINUE
          IS = IS + 1
          IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 401
          IE = INDEX(LINE(IS:),' ') + IS - 2
          WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
          READ(LINE(IS:IE),CFMTI) ISEED(1,ISEQ)
          IS = IE
402       CONTINUE
          IS = IS + 1
          IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 402
          IE = INDEX(LINE(IS:),' ') + IS - 2
          WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
          READ(LINE(IS:IE),CFMTI) ISEED(2,ISEQ)
          IS = IE
403       CONTINUE
          IS = IS + 1
          IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 403
          IE = INDEX(LINE(IS:),' ') + IS - 2
          WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
          READ(LINE(IS:IE),CFMTI) ISEED(3,ISEQ)
          NSEQ = ISEQ
        ELSE
          WRITE(MONIOU,*) 'DATAC : TOO MUCH RANDOM GENERATOR SEEDS,',
     *                    ' IGNORE IT'
        ENDIF
 
C  GET FACTOR FOR ELECTRON'S MULTIPLE SCATTERING LENGTH
      ELSEIF ( LINE(1:6) .EQ. 'STEPFC' ) THEN
        IS = 6
406     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 406
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) STEPFC
 
C  GET THETA OF PRIMARY PARTICLE
      ELSEIF ( LINE(1:6) .EQ. 'THETAP' ) THEN
        IS = 6
411     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 411
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) R1
        IS = IE
412     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 412
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTF) R2
        THETPR(1) = MIN( R1, R2 ) 
        THETPR(2) = MAX( R1, R2 )
 
C  GET NAME OF USER
      ELSEIF ( LINE(1:4) .EQ. 'USER' ) THEN
        DO 416 I=1,20
          USER(I:I) = ' '
416     CONTINUE
        IS = 4
417     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 417
        IE = INDEX(LINE(IS:),' ') + IS - 2
        IF ( IE-IS .GT. 19 ) THEN
          IE = IS + 19
          USER(1:IE-IS+1) = LINE(IS:IE)
          WRITE(MONIOU,*)
     *        'DATAC: USERNAME TOO LONG AND TRUNCATED TO:',USER(1:20)
        ELSE
          USER(1:IE-IS+1) = LINE(IS:IE)
        ENDIF
 
C  GET PARAMETER ISH0 FOR AMOUNT OF VENUS DEBUG
      ELSEIF ( LINE(1:6) .EQ. 'VENDBG' ) THEN
        IS = 6
421     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 421
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTI(3:4),'(I2)') IE - IS + 1
        READ(LINE(IS:IE),CFMTI) ISH0
 
C  GET VENUS PARAMETER WITH CODE WORD AND VALUE
      ELSEIF ( LINE(1:6) .EQ. 'VENPAR' ) THEN
        IPARAM = IPARAM + 1
        IF ( IPARAM .LE. 100 ) THEN
          PARCHA(IPARAM) = '      '
          IS = 6
431       CONTINUE
          IS = IS + 1
          IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 431
          IE = INDEX(LINE(IS:),' ') + IS - 2
          IF ( IE-IS .GT. 5 ) THEN
            WRITE(MONIOU,*)
     *        'DATAC: VENUS PARAMETER NAME TOO LONG AND IGNORED'
            GOTO 1
          ELSE
            PARCHA(IPARAM) = LINE(IS:IE)
          ENDIF
          IS = IE
432       CONTINUE
          IS = IS + 1
          IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 432
          IE = INDEX(LINE(IS:),' ') + IS - 2
          WRITE(CFMTF(3:4),'(I2)') IE - IS + 1
          READ(LINE(IS:IE),CFMTF) PARVAL(IPARAM)
          NPARAM = IPARAM
        ELSE
          WRITE(MONIOU,*) 'DATAC : TOO MUCH VENUS PARAMETERS,',
     *                    ' IGNORE IT'
        ENDIF
 
C  GET FLAG FOR VENUS HIGH ENERGY HADRONIC INTERACTION MODEL
      ELSEIF ( LINE(1:5) .EQ. 'VENUS' ) THEN
        IS = 5
441     CONTINUE
        IS = IS + 1
        IF ( LINE(IS:IS) .EQ. ' ' ) GOTO 441
        IE = INDEX(LINE(IS:),' ') + IS - 2
        WRITE(CFMTL(3:3),'(I1)') IE - IS + 1
        READ(LINE(IS:IE),CFMTL) FVENUS
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  
c  get cerenkov file name with cts array specifications
      elseif ( LINE(1:6) .eq. 'CERTEL' ) then
        read(line(7:),'(I10)') nctels
        d2r = 3.1415926535897932385/180.0
        do 10 nct=1,nctels
          read(moniin,*) (ctpars(nct,m),m=1,7)
          ct = cos(ctpars(nct,ctthet)*d2r)
          st = sin(ctpars(nct,ctthet)*d2r)
          cp = cos(ctpars(nct,ctphi)*d2r)
          sp = sin(ctpars(nct,ctphi)*d2r)
          omega(nct,1,1) = cp
          omega(nct,1,2) = sp 
          omega(nct,1,3) = 0.0
          omega(nct,2,1) = -ct*sp 
          omega(nct,2,2) = ct*cp 
          omega(nct,2,3) = st 
          omega(nct,3,1) = st*sp 
          omega(nct,3,2) = -st*cp 
          omega(nct,3,3) = ct 
c          write(moniou,*) nct,(ctpars(nct,m),m=1,7)
 10     continue
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  

C  ILLEGAL KEYWORD
      ELSE
        WRITE(MONIOU,*) 'DATAC : UNKNOWN KEYWORD :',(LINE(I:I),I=1,6)
      ENDIF
 
      GOTO 1
 
C-----------------------------------------------------------------------
 1000 CONTINUE
      IF ( DEBUG ) THEN
        WRITE(MDEBUG,*) 'DATAC : NO MORE DIRECTIVES FOUND'
      ELSE
        WRITE(MONIOU,*) '*** NO MORE DIRECTIVES FOUND ***'
      ENDIF
 
      RETURN
      END
