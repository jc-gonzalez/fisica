C=======================================================================

      SUBROUTINE CERENE( STEPCR )

C-----------------------------------------------------------------------
C  CEREN(KOV RADIATION FROM) E(LECTRONS)
C
C  CREATION OF CERENKOV PHOTONS ALONG A TRACK OF ELECTRONS
C  CERENKOV RADIATION IS ONLY CALCULATED FOR LOWEST OBSERVATION LEVEL
C  THE COORDINATES ON EGS-STACK ARE AT THE END OF STEP EXCEPT E(NP),
C  WHICH IS AT THE BEGINNING OF STEP
C  THIS SUBROUTINE IS CALLED FROM ELECTR
C  ARGUMENT:
C   STEPCR = STEP LENGTH FOR ELECTRON OR POSITRON (REAL*4)
C
C  AUTHOR  : M. ROZANSKA  UNIVERSITY OF KRAKOW
C            S. MARTINEZ  UNIVERSITY OF MADRID
C            F. ARQUEROS  UNIVERSITY OF MADRID
C  CHANGES : D. HECK    IK3  FZK KARLSRUHE
C            R. ATTALLAH  UNIVERSITY OF PERPIGNAN
C-----------------------------------------------------------------------
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      parameter (xct=1)
      parameter (yct=2)
      parameter (zct=3)
      parameter (ctthet=4)
      parameter (ctphi=5)
      parameter (ctdiam=6)
      parameter (ctfoc=7)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c      IMPLICIT NONE
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
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,EPCONT.
      COMMON/EPCONT/   EDEP,RATIO,TSTEP,TUSTEP,USTEP,TVSTEP,VSTEP,IDISC,
     *                 IROLD,IRNEW,RHOFAC, EOLD,ENEW,EKE,ELKE,BETA2,GLE,
     *                 TSCAT,IAUSFL
      DOUBLE PRECISION EDEP,RATIO
      REAL             TSTEP,TUSTEP,USTEP,TVSTEP,VSTEP,RHOFAC,EOLD,ENEW,
     *                 EKE,ELKE,BETA2,GLE,TSCAT
      INTEGER          IDISC,IROLD,IRNEW,IAUSFL(29)
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
*keep,graal1.
      common /graal1/ wavelength ! (nm)
      real wavelength
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*KEND.

      COMMON /ACLOCK/  NCLOCK,JCLOCK
      DOUBLE PRECISION BETAE,BETAF,BETAI,CTHETA,DBETA,ECR,
     *                 ETA1,E1,STHETA,STHETF,STHETI
      DOUBLE PRECISION RHOF,THICK
      REAL             A,B,CC,COSCR,COSDEL,DVCOR,DXXX,DYYY,FSTEPI,
     *                 HTOP,H2,PATHCR,PHICER,PHOTCT,RADINV,
     *                 SINCR,SINDEL,SINPSI,SINPS2,STEPCR,UEMIS2,US,VCOR,
     *                 VEMIS2,VS,WEMIS,XCER1,XCER2,XEMIS,XXX,
     *                 YCER1,YCER2,YEMIS,YYY
      INTEGER          I,ISTC,I1,JCLOCK,LPCT1,NCLOCK,NSTEPC
      EXTERNAL         RHOF,THICK
C-----------------------------------------------------------------------
C_____IF (NCLOCK.GT.JCLOCK) THEN
C______WRITE(MDEBUG,*)'CERENE: NP=',NP,' IR=',IR(NP),' IOBS=',IOBS(NP)
C______CALL AUSGB2
C_____ENDIF

C-----------------------------------------------------------------------

C  SKIP PARTICLES OUT OF ZENITH ANGULAR CUT
      IF ( W(NP) .LT. C(29) ) RETURN
C  E(NP) IS ENERGY AT BEGIN OF STEP
      E1 = E(NP)

C  LOOK WETHER CERENKOV CONDITION IS FULFILLED AT BEGIN OF STEP
      BETAI  = SQRT( 1.D0 - (PAMA(2)*1.D3/E1)**2 )
C  REFRACTIVE INDEX PARAMETRISATION: N=1+ETA = ETA1
      H2     = -Z(NP)
      HTOP   = H2 + VSTEP * W(NP)
      ETA1   = 1.D0 + ETADSN * RHOF(DBLE(HTOP))
      CTHETA = 1.D0 /( ETA1 * BETAI)
      STHETI = 1.D0 - CTHETA**2
      IF ( STHETI .GT. 0.D0 ) THEN
C  PARTICLE IS ABOVE ENERGY THRESHOLD IF EMISSION ANGLE IS >0
        PHOTCT = CYIELD * STEPCR * STHETI
        NSTEPC = PHOTCT / CERSIZ + 1
        IF ( NSTEPC .LT. 1 ) RETURN
        FSTEPI = 1. / REAL(NSTEPC)
C  CALCULATE INCREMENTS AND START VALUES FOR POSITION AND VELOCITY
        DVCOR  = -VSTEP * FSTEPI
        VCOR   = VSTEP - 0.5 * DVCOR
        DBETA  = -2.D0*FSTEPI*EDEP*(PAMA(2)*1.D3)**2 / (E1**3*BETAI)
        BETAE  = BETAI - 0.5D0 * DBETA
      ELSE
        
C  LOOK WETHER CERENKOV CONDITION IS FULFILLED AT END OF STEP, BUT NOT
C  AT THE BEGINNING. THIS MAY HAPPEN ONLY ABOVE ABOUT 22 KM
        IF ( HTOP .LT. 22.E5 ) RETURN
C  ENERGY AT END OF STEP IS ENEW (FROM COMMON EPCONT)
        BETAF  = SQRT( 1.D0 - (PAMA(2)*1.D3/ENEW)**2 )
C  REFRACTIVE INDEX PARAMETRISATION: N=1+ETA = ETA1
        ETA1   = 1.D0 + ETADSN * RHOF( DBLE(H2) )
        CTHETA = 1.D0 /( ETA1 * BETAF)
        STHETF = 1.D0 - CTHETA**2
C  PARTICLE IS BELOW ENERGY THRESHOLD IF EMISSION ANGLE IS 0
        IF ( STHETF .LE. 0.D0 ) RETURN
        PHOTCT = CYIELD * STEPCR * STHETF
        NSTEPC = PHOTCT / CERSIZ + 1
        IF ( NSTEPC .LT. 1 ) RETURN
        FSTEPI = 1. / REAL(NSTEPC)
C  CALCULATE INCREMENTS AND START VALUES FOR POSITION AND VELOCITY
C  LOOP 1000 RUNS FROM BOTTOM TO TOP OF STEP
        DVCOR  = VSTEP * FSTEPI
        VCOR   = -0.5 * DVCOR
        DBETA  = 2.D0*FSTEPI*EDEP*(PAMA(2)*1.D3)**2 / (ENEW**3*BETAF)
        BETAE  = BETAF - 0.5D0 * DBETA
      ENDIF

C  LOOP OVER SUBSTEPS
      DO 1000  ISTC = 1,NSTEPC
        VCOR   = VCOR + DVCOR
        ZEMIS  = H2 + VCOR * W(NP)
        ETA1   = 1.D0 + ETADSN * RHOF(DBLE(ZEMIS))
C  VELOCITY IN THE MIDDLE OF SUBSTEP
        BETAE  =  BETAE + DBETA
        CTHETA = 1.D0 / (ETA1*BETAE)
        STHETA = 1.D0 - CTHETA**2
C  PARTICLE IS AT ENERGY THRESHOLD IF EMISSION ANGLE BECOMES 0
        IF ( STHETA .LE. 0.D0 ) RETURN
C  NUMBER OF EMITTED PHOTONS ON DISTANCE DVCOR
        PHOTCM = CYIELD * STHETA * STEPCR * FSTEPI
        STHETA = SQRT(STHETA)
C  ASSUME EMISSION POINT OF ALL PHOTONS IN THE MIDDLE OF THE STEP
        XEMIS  =  X(NP) - VCOR * U(NP)
        YEMIS  = -Y(NP) + VCOR * V(NP)
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C GENERATE RANDOM WAVELENGTH FOR SINGLE C-PHOTON.
        CALL RMMAR( RD,1,3 )
        WAVELENGTH = 1. / (1/WAVLGL - 
     +       RD(1)/(WAVLGL*WAVLGU/(WAVLGU-WAVLGL)))
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C  CALCULATE PHOTON DIRECTION IN THE CORSIKA COORDINATE FRAME
        CALL RMMAR( RD,1,3 )
        PHICER = RD(1) * PI2
        SINCR  = SIN(PHICER)
        COSCR  = COS(PHICER)
        A =  U(NP)
        B = -V(NP)
        CC = W(NP)
        SINPS2 = A**2 + B**2
        IF ( SINPS2 .LT. 1.E-10 ) THEN
          UEMIS  = STHETA * COSCR
          VEMIS  = STHETA * SINCR
          WEMIS  = CTHETA * CC
        ELSE
          SINPSI = SQRT(SINPS2)
          US     = STHETA * COSCR
          VS     = STHETA * SINCR
          SINDEL = B * (1./SINPSI)
          COSDEL = A * (1./SINPSI)
          UEMIS  = CC * COSDEL * US - SINDEL * VS + A * CTHETA
          VEMIS  = CC * SINDEL * US + COSDEL * VS + B * CTHETA
          WEMIS  =     -SINPSI * US              + CC * CTHETA
        ENDIF
C  EMISSION ANGLE WITHIN ZENITH ANGULAR CUT?
        IF ( WEMIS .LT. C(29) ) GOTO 1000
        RADINV = 1.5 - 0.5 * ( UEMIS**2 + VEMIS**2 + WEMIS**2 )
        UEMIS2 = UEMIS * RADINV
        VEMIS2 = VEMIS * RADINV
        WEMIS  = WEMIS * RADINV

C  CALCULATE DISTANCE FROM SHOWER AXIS AT THE DETECTOR LEVEL
        PATHCR = ( ZEMIS - OBSLEV(NOBSLV) ) / WEMIS
        XCER2  = XEMIS + PATHCR * UEMIS2 - XOFF(NOBSLV)
        YCER2  = YEMIS + PATHCR * VEMIS2 - YOFF(NOBSLV)

C  ADD THE CERENKOV PHOTONS TO THE LONGITUDINAL DEVELOPMENT
        IF ( LLONGI ) THEN
C  IF STARTING POINT BELOW LOWEST LEVEL THEN DON'T CHECK
          IF ( HLONG(NSTEP) .LE. ZEMIS ) THEN
C  FIND FIRST THE EQUIVALENT LEVELS
            LPCT1 = LPCTE(NP)
C  ZEMIS IS ONLY LITTLE BELOW Z OLD, THEREFORE INCREMENTAL SEARCH
C  (REMEBER: LPCTE IS AT START OF ELECTRON STEP)
            DO 6002 I1 = LPCT1,NSTEP
              IF ( HLONG(I1) .LT. ZEMIS ) GOTO 6003
 6002       CONTINUE
            I1 = NSTEP + 1
 6003       CONTINUE
            DO 4862 I=I1,NSTEP
                PLONG(I,9) = PLONG(I,9) + PHOTCM
 4862       CONTINUE
          ENDIF
        ENDIF

C  TAKE INTO ACCOUNT A ROTATION OF ARRAY RELATIVE TO MAGNETIC NORD
        XCER  = XCER2  * COSANG + YCER2  * SINANG
        YCER  = YCER2  * COSANG - XCER2  * SINANG
        UEMIS = UEMIS2 * COSANG + VEMIS2 * SINANG
        VEMIS = VEMIS2 * COSANG - UEMIS2 * SINANG
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        CERELE = CERELE + PHOTCM
        DO 7001 I=1,ICERML
          DO 101 NCT=1,NCTELS
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> Modification to implement sphere algorithm >>>>>>>>>>>>>>>>>>>>>>>>
c>> JCG Wed Sep 21 10:49:14 MET DST 1998 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>>>>> this is the last (simple) check <<<<<
C  changes to ct frame
c            XG = XCER - CERXOS(I) - CTPARS(NCT,XCT) 
c            YG = YCER - CERYOS(I) - CTPARS(NCT,YCT) 
c            ZG =  0.0 - CTPARS(NCT,ZCT)
c            DIST2 = SQRT( XG**2 + YG**2 )
c            IF ( DIST2 .LT. (CTPARS(NCT,CTDIAM)/2.) ) GOTO 102
c>> New check >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            XG = XCER - CERXOS(I) 
            YG = YCER - CERYOS(I)
            ZG = 0.0
            DIST2 = 
     >          SQRT((VEMIS*(-CTPARS(NCT,XCT) + XG) - 
     >          UEMIS*(-CTPARS(NCT,YCT) + YG))**2 + 
     >          (-(SQRT(1 - UEMIS**2 - VEMIS**2)*
     >          (-CTPARS(NCT,XCT) + XG)) + 
     >          UEMIS*(-CTPARS(NCT,ZCT) + ZG))**2 + 
     >          (SQRT(1 - UEMIS**2 - VEMIS**2)*
     >          (-CTPARS(NCT,YCT) + YG) - 
     >          VEMIS*(-CTPARS(NCT,ZCT) + ZG))**2)
            IF ( DIST2 .LT. (CTPARS(NCT,CTDIAM)/2.) ) GOTO 102
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            
 101      CONTINUE
          
          GOTO 7001
          
C   BUNCH FALLS ON A DETECTOR, CALCULATE ARRIVAL TIME (NSEC)
          
 102      CARTIM = ((ETADSN*(THCKOB(NOBSLV)-THICK(DBLE(ZEMIS)))
     *         /WEMIS+PATHCR-VCOR/BETAE)/C(25)+TIME(NP))* 1.E9
          
c          CALL OUTPT2(IQ(NP),I)
          CALL OUTPT2(NCT,I)

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          call jctime(cartim)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

          GOTO 1000
          
 7001   CONTINUE
 1000 CONTINUE
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      RETURN
      END

C=======================================================================

      SUBROUTINE CERENH( STEPCR,BETACR )

C-----------------------------------------------------------------------
C  CEREN(KOV RADIATION FROM) H(ADRONS)
C
C  CERENKOV RADIATION FROM HADRONS
C  CERENKOV RADIATION IS ONLY CALCULATED FOR LOWEST OBSERVATION LEVEL
C  THIS SUBROUTINE IS CALLED FROM UPDATE
C  ARGUMENTS:
C   STEPCR = STEP LENGTH FOR ELECTRON OR POSITRON
C   BETACR = VELOCITY OF PARTICLE IN UNITS OF SPEED OF LIGHT
C
C  AUTHOR  : M. ROZANSKA  UNIVERSITY OF KRAKOW
C            S. MARTINEZ  UNIVERSITY OF MADRID
C            F. ARQUEROS  UNIVERSITY OF MADRID
C  CHANGES : D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      parameter (xct=1)
      parameter (yct=2)
      parameter (zct=3)
      parameter (ctthet=4)
      parameter (ctphi=5)
      parameter (ctdiam=6)
      parameter (ctfoc=7)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c      IMPLICIT NONE
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
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,EPCONT.
      COMMON/EPCONT/   EDEP,RATIO,TSTEP,TUSTEP,USTEP,TVSTEP,VSTEP,IDISC,
     *                 IROLD,IRNEW,RHOFAC, EOLD,ENEW,EKE,ELKE,BETA2,GLE,
     *                 TSCAT,IAUSFL
      DOUBLE PRECISION EDEP,RATIO
      REAL             TSTEP,TUSTEP,USTEP,TVSTEP,VSTEP,RHOFAC,EOLD,ENEW,
     *                 EKE,ELKE,BETA2,GLE,TSCAT
      INTEGER          IDISC,IROLD,IRNEW,IAUSFL(29)
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
*KEEP,CERHDR.
      COMMON/CERHDR/   TPART,UPART,VPART,WPART,XPART,YPART,ZPART
      DOUBLE PRECISION TPART,UPART,VPART,WPART,XPART,YPART,ZPART
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
      character *72 ctfile
*keep,graal1.
      common /graal1/ wavelength ! (nm)
      real wavelength
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*KEND.

      DOUBLE PRECISION BETACR,CINTEN,CTHETA,ETA,ETA1,HMID,PHOTCT,
     *                 RHOF,STEPCR,STHETA,THICK
      REAL             A,B,CC,COSCR,COSDEL,DVCOR,DXXX,DYYY,FSTEPI,
     *                 PATHCR,PHICER,RADINV,SINCR,SINDEL,SINPSI,SINPS2,
     *                 UEMIS2,US,VEMIS2,VCOR,VS,WEMIS,XCER1,XCER2,XEMIS,
     *                 XXX,YCER1,YCER2,YEMIS,YYY
      INTEGER          I,II,ISTC,I1,I2,NSTEPC
      EXTERNAL         RHOF,THICK
C-----------------------------------------------------------------------

c      IF ( DEBUG ) WRITE(MDEBUG,*) 'CERENH: ZPART=',SNGL(ZPART),
c     *    ' STEPCR=',SNGL(STEPCR),' BETACR=',SNGL(BETACR)

C  SKIP PARTICLE OUT OF ANGULAR ACCEPTANCE RANGE
      IF ( WPART .LT. C(29) ) RETURN
C  CERENKOV INTENSITY FACTOR DEPENDS ON CHARGE STATE OF HEAVY IONS
      CINTEN = CYIELD * ABS(SIGNUM(INT(CURPAR(1))))

C  REFRACTIVE INDEX PARAMETRISATION: N=1+ETA
      HMID   = ZPART + 0.5D0 * STEPCR * WPART
      ETA1   = 1.D0 + ETADSN * RHOF(DBLE(HMID))
      CTHETA = 1.D0 / ( ETA1 * BETACR )
      STHETA = 1.D0 - CTHETA**2
      IF ( STHETA .LE. 0.D0 ) RETURN

      PHOTCT = CINTEN * STHETA * STEPCR
      NSTEPC = PHOTCT / CERSIZ + 1
      IF ( NSTEPC .LT. 1 ) RETURN
      FSTEPI = 1. / REAL(NSTEPC)
      VCOR   = -0.5 * STEPCR * FSTEPI
      DVCOR  = -2. * VCOR
C  CERENKOV RADIATION IS ONLY CALCULATED FOR LOWEST OBSERVATION LEVEL
      DO  1000  ISTC = 1,NSTEPC
        VCOR   = VCOR + DVCOR
        ZEMIS  = ZPART + VCOR * WPART
        ETA    = ETADSN * RHOF(DBLE(ZEMIS))
        ETA1   = 1.D0 + ETA
        CTHETA = 1.D0 / ( ETA1 * BETACR )
        STHETA = 1.D0 - CTHETA**2
        IF ( STHETA .LE. 0.D0 ) RETURN

C  NUMBER OF EMITTED PHOTONS ON DISTANCE STEPCR
        PHOTCM = CINTEN * STHETA * STEPCR * FSTEPI
        STHETA = SQRT(STHETA)

C  ASSUME EMISSION POINT OF ALL PHOTONS IN THE MIDDLE OF THE STEP
C  HAS TO BE CHECKED IF STEPS ARE NOT TOO LONG
        XEMIS = XPART - VCOR * UPART
        YEMIS = YPART - VCOR * VPART

C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C GENERATE RANDOM WAVELENGTH FOR SINGLE C-PHOTON.
        CALL RMMAR( RD,1,3 )
        WAVELENGTH = 1. / (1/WAVLGL - 
     +       RD(1)/(WAVLGL*WAVLGU/(WAVLGU-WAVLGL)))
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

C  CALCULATE PHOTON DIRECTION IN THE OVERALL COORDINATE FRAME
        CALL RMMAR( RD,1,3 )
        PHICER = RD(1) * PI2
        SINCR  = SIN(PHICER)
        COSCR  = COS(PHICER)
        A  = UPART
        B  = VPART
        CC = WPART
        SINPS2 = A**2 + B**2
        IF ( SINPS2 .LT. 1.E-10 ) THEN
          UEMIS  = STHETA * COSCR
          VEMIS  = STHETA * SINCR
          WEMIS  = CTHETA * CC
        ELSE
          SINPSI = SQRT(SINPS2)
          US = STHETA * COSCR
          VS = STHETA * SINCR
          SINDEL = B * (1./SINPSI)
          COSDEL = A * (1./SINPSI)
          UEMIS  = CC * COSDEL * US - SINDEL * VS + A * CTHETA
          VEMIS  = CC * SINDEL * US + COSDEL * VS + B * CTHETA
          WEMIS  =     -SINPSI * US              + CC * CTHETA
        ENDIF
C  EMISSION ANGLE WITHIN ZENITH ANGULAR CUT?
        IF ( WEMIS .LT. C(29) ) GOTO 1000
        RADINV = 1.5 - 0.5 * ( UEMIS**2 + VEMIS**2 + WEMIS**2 )
        UEMIS2 = UEMIS * RADINV
        VEMIS2 = VEMIS * RADINV
        WEMIS  = WEMIS * RADINV

C  CALCULATE DISTANCE FROM SHOWER AXIS AT THE DETECTOR LEVEL
        PATHCR = ( ZEMIS - OBSLEV(NOBSLV) ) / WEMIS
        XCER2  = XEMIS + PATHCR * UEMIS2 - XOFF(NOBSLV)
        YCER2  = YEMIS + PATHCR * VEMIS2 - YOFF(NOBSLV)

C  ADD THE CERENKOV PHOTONS TO THE LONGITUDINAL DEVELOPMENT
        IF ( LLONGI ) THEN
C  IF STARTING POINT BELOW LOWEST LEVEL THEN DON'T CHECK
          IF ( HLONG(NSTEP) .LE. ZEMIS ) THEN
C  FIND FIRST THE EQUIVALENT LEVELS
            I1 = 0
            I2 = NSTEP
 6001       CONTINUE
            II = (I1+I2)/2
            IF ( HLONG(II) .LT. ZEMIS ) THEN
              I2 = II
            ELSE
              I1 = II
            ENDIF
            IF ( I2-I1 .GT. 1 ) GOTO 6001
            DO 4862 I=I2,NSTEP
                PLONG(I,9) = PLONG(I,9) + PHOTCM
 4862       CONTINUE
          ENDIF
        ENDIF

C  TAKE INTO ACCOUNT A ROTATION OF ARRAY RELATIVE TO MAGNETIC NORD
        XCER  = XCER2  * COSANG + YCER2  * SINANG
        YCER  = YCER2  * COSANG - XCER2  * SINANG
        UEMIS = UEMIS2 * COSANG + VEMIS2 * SINANG
        VEMIS = VEMIS2 * COSANG - UEMIS2 * SINANG
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        CERHAD = CERHAD + PHOTCM
        DO 7001 I=1,ICERML
          DO 101 NCT=1,NCTELS
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> Modification to implement sphere algorithm >>>>>>>>>>>>>>>>>>>>>>>>
c>> JCG Wed Sep 21 10:49:14 MET DST 1998 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>>>>> this is the last (simple) check <<<<<
C  changes to ct frame
c            XG = XCER - CERXOS(I) - CTPARS(NCT,XCT) 
c            YG = YCER - CERYOS(I) - CTPARS(NCT,YCT) 
c            ZG =  0.0 - CTPARS(NCT,ZCT)
c            DIST2 = SQRT( XG**2 + YG**2 )
c            IF ( DIST2 .LT. (CTPARS(NCT,CTDIAM)/2.) ) GOTO 102
c>> New check >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            XG = XCER - CERXOS(I) 
            YG = YCER - CERYOS(I)
            ZG = 0.0
            DIST2 = 
     >          SQRT((VEMIS*(-CTPARS(NCT,XCT) + XG) - 
     >          UEMIS*(-CTPARS(NCT,YCT) + YG))**2 + 
     >          (-(SQRT(1 - UEMIS**2 - VEMIS**2)*
     >          (-CTPARS(NCT,XCT) + XG)) + 
     >          UEMIS*(-CTPARS(NCT,ZCT) + ZG))**2 + 
     >          (SQRT(1 - UEMIS**2 - VEMIS**2)*
     >          (-CTPARS(NCT,YCT) + YG) - 
     >          VEMIS*(-CTPARS(NCT,ZCT) + ZG))**2)
            IF ( DIST2 .LT. (CTPARS(NCT,CTDIAM)/2.) ) GOTO 102
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 101      CONTINUE
          
          GOTO 7001
          
C   BUNCH FALLS ON A DETECTOR, CALCULATE ARRIVAL TIME (NSEC)
          
 102      CARTIM = ((ETADSN*(THCKOB(NOBSLV)-THICK(DBLE(ZEMIS)))
     *         /WEMIS+PATHCR-VCOR/BETACR)/C(25)+TPART)*1.E9

c          CALL OUTPT2(INT(CURPAR(1)),I)
          CALL OUTPT2(NCT,I)

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          call jctime(cartim)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

          GOTO 1000
          
 7001   CONTINUE
 1000 CONTINUE
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      RETURN
      END

C=======================================================================

      SUBROUTINE GETBUS( IPARTI,ENERGY,THETA,CERSZE )

C-----------------------------------------------------------------------
C  GET BU(NCH) S(IZE)
C
C  CALCULATES OPTIMAL BUNCH SIZE FOR CERENKOV PHOTONS. CERENKOV PHOTONS
C  ARE GROUPED IN BUNCHES IN ORDER TO ACCELERATE COMPUTING TIME.
C  HOWEVER, WE SET A MAXIMAL VALUE FOR THE GROUPING OF CERENKOV PHOTONS
C  SO THAT WE GET AT LEAST 100 BUNCHES/M**2 AT A CERENKOV FLUX OF 3000
C  PHOTONS/M**2. THIS IS THE MINIMUM CERENKOV FLUX WHICH CAN BE
C  DISTINGUISHED FROM THE NIGHT SKY LIGHT BACKGROUND IN THE HEGRA
C  EXPERIMENT AT THE ISLAND LA PALMA. SO THE PARAMETRIZATION OF THE
C  CERENKOV BUNCH AS CALCULATED IN THIS SUBROUTINE IS VALID FOR
C  OBSERVATION LEVELS SIMILAR TO THAT OF THE HEGRA EXPERIMENT.
C     FOR A GIVEN PRIMARY PARTICLE, INCIDENT ENERGY AND ANGLE, AN
C  OPTIMAL BUNCH SIZE IS CALCULATED BY INTERPOLATION IN A TABLE,
C  WHERE WE HAVE CHOSEN AN ENERGY RANGE UP TO 1000 TEV, INCIDENT
C  ANGLES 0 AND 40 DEGREES, AND 4 TYPES OF PRIMARIS: GAMMAS,
C  PROTONS, NITROGEN, AND IRON.
C  THIS SUBROUTINE IS CALLED FROM MAIN
C  ARGUMENTS:
C   IPARTI      = TYPE OF PRIMARY PARTICLE
C   ENERGY (R4) = PARTICLES ENERGY IN GEV
C   THETA  (R4) = ANGLE IN RAD
C   CERSZE (R4) = SIZE OF CERENKOV BUNCH
C
C  AUTHORS : S. MARTINEZ  UNIVERSITY OF MADRID
C            F. ARQUEROS  UNIVERSITY OF MADRID
C-----------------------------------------------------------------------

      IMPLICIT NONE
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
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

      REAL    ANGLE(2),ENGAM(3),ENHAD(3),ENNIT(2),
     *        SIFE(3,2),SIGAM(3,2),SINIT(2),SIPRO(3,2)
      REAL    CERS1F,CERS1P,ENERGY,CERSZE,S1,S2,THETA
      INTEGER I,IANFE,IANP,IATNUM,IPARTI,I1,I2

      DATA ANGLE /  0., 40. /
      DATA ENGAM /  100.,  200.,  500. /
      DATA ENHAD /  100.,  200.,  1000. /
      DATA ENNIT /  200.,  1000. /
      DATA ( SIFE (I,1),I=1,3 ) /  30.,  30.,  140. /
      DATA ( SIFE (I,2),I=1,3 ) /  30.,  30.,  110. /
      DATA ( SIGAM(I,1),I=1,3 ) /  30.,  45.,  100. /
      DATA ( SIGAM(I,2),I=1,3 ) /  30.,  40.,  100. /
      DATA SINIT /  30.,  150. /
      DATA ( SIPRO(I,1),I=1,3 ) /  30.,  30.,  120. /
      DATA ( SIPRO(I,2),I=1,3 ) /  30.,  30.,  160. /
      DATA IANP   / 1 /, IANFE / 26 /
C-----------------------------------------------------------------------

c      IF ( DEBUG ) WRITE(MDEBUG,100) IPARTI,SNGL(ENERGY),SNGL(THETA)
c 100  FORMAT(' GETBUS: INPUT PARTICLE = ',I5,1P,2E10.3)

C  DEFAULT VALUE
      CERSZE = 100.

      ENERGY = ENERGY / 1000.
      IF ( ENERGY .LE. 100. ) THEN
        CERSZE = 30.
        IF ( DEBUG ) WRITE(MDEBUG,101) CERSZE
        RETURN
      ENDIF

      THETA  = THETA / PI * 180.

C-----------------------------------------------------------------------
C  PHOTON, ELECTRON OR POSITRON AS PRIMARY PARTICLE
      IF ( IPARTI .LE. 3 ) THEN
C  FIND ENERGY BIN FOR INTERPOLATION
        IF ( ENERGY .LE. ENGAM(2) ) THEN
          I1 = 1
          I2 = 2
        ELSE
          I1 = 2
          I2 = 3
        ENDIF
        S1 = SIGAM(I1,1) + (ENERGY - ENGAM(I1))
     *       / (ENGAM(I2) - ENGAM(I1))
     *       * (SIGAM(I2,1) - SIGAM(I1,1))
        S2 = SIGAM(I1,2) + (ENERGY - ENGAM(I1))
     *       / (ENGAM(I2) - ENGAM(I1))
     *       * (SIGAM(I2,2) - SIGAM(I1,2))
        CERSZE = S1 + (THETA-ANGLE(1))/(ANGLE(2)-ANGLE(1)) * (S2-S1)
        IF ( DEBUG ) WRITE(MDEBUG,101) CERSZE
        RETURN
      ENDIF

C-----------------------------------------------------------------------
C  NITROGEN AS PRIMARY PARTICLE AND VERTICAL INCIDENCE
CJOK WHY SPECIAL TREATMENT FOR NITROGEN ????
CJOK WHY ONLY VERTICAL INCIDENCE ????
      IF ( IPARTI .EQ. 1407  .AND.  ABS(THETA) .LT. 1.E-1 ) THEN
        IF ( ENERGY .LT. 200. ) THEN
          CERSZE = 30.
        ELSE
          CERSZE = SINIT(1) + (ENERGY-ENNIT(1))
     *             / (ENNIT(2)-ENNIT(1)) * (SINIT(2)-SINIT(1))
        ENDIF
        IF ( DEBUG ) WRITE(MDEBUG,101) CERSZE
        RETURN
      ENDIF

C-----------------------------------------------------------------------
C  GET THE ATOMIC NUMBER OF THE NUCLEUS
C  Z IS 1,  IF PROTON
      IF     ( IPARTI .EQ. 14  ) THEN
        IATNUM = 1
C  REST OF POSSIBLE NUCLEI
      ELSEIF ( IPARTI .GT. 100 ) THEN
        IATNUM = MOD(IPARTI,100)
        IF ( IATNUM .GT. 26 ) THEN
          WRITE(MONIOU,*) 'GETBUS: UNEXPECTED PARTICLE CODE'
          RETURN
        ENDIF
      ELSE
        WRITE(MONIOU,*) 'GETBUS: UNEXPECTED PARTICLE CODE'
        RETURN
      ENDIF

C  FIND ENERGY BIN FOR INTERPOLATION IN CASE OF HADRONIC PRIMARY
      IF ( ENERGY .LE. ENHAD(2) ) THEN
        I1 = 1
        I2 = 2
      ELSE
        I1 = 2
        I2 = 3
      ENDIF

C  INTERPOLATION FOR HADRONS
      S1 = SIPRO(I1,1) + (ENERGY-ENHAD(I1))
     *       / (ENHAD(I2)-ENHAD(I1)) * (SIPRO(I2,1)-SIPRO(I1,1))
      S2 = SIPRO(I1,2) + (ENERGY-ENHAD(I1))
     *       / (ENHAD(I2)-ENHAD(I1)) * (SIPRO(I2,2)-SIPRO(I1,2))
      CERS1P = S1 + (THETA-ANGLE(1)) / (ANGLE(2)-ANGLE(1)) * (S2-S1)

      S1 = SIFE(I1,1) + (ENERGY-ENHAD(I1)) / (ENHAD(I2)-ENHAD(I1))
     *                * (SIFE(I2,1)-SIFE(I1,1))
      S2 = SIFE(I1,2) + (ENERGY-ENHAD(I1)) / (ENHAD(I2)-ENHAD(I1))
     *                * (SIFE(I2,2)-SIFE(I1,2))
      CERS1F = S1 + (THETA-ANGLE(1)) / (ANGLE(2)-ANGLE(1)) * (S2-S1)

      CERSZE = CERS1P + (IATNUM-IANP) * (CERS1F-CERS1P) / (IANFE-IANP)

      IF ( DEBUG ) WRITE(MDEBUG,101) CERSZE
 101  FORMAT(' GETBUS: BUNCH SIZE = ',1P,1E10.3)

      RETURN
      END

C=======================================================================

      SUBROUTINE OUTND2

C-----------------------------------------------------------------------
C  OUT(PUT AT E)ND (OF SHOWER)
C
C  WRITE REST OF PARTICLES TO OUTPUT BUFFER
C  OUTND2 IS CALLED FROM MAIN
C
C  AUTHORS : S. MARTINEZ, UNIVERSITY OF MADRID
C            F. ARQUEROS, UNIVERSITY OF MADRID
C-----------------------------------------------------------------------

      IMPLICIT NONE
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
*KEND.

      INTEGER I
C-----------------------------------------------------------------------

      IF ( LHCER .GT. 0 ) THEN
        CALL TOBUFC( DATAB2,1 )
        DO  2  I = 1,MAXBF2
          DATAB2(I) = 0.
    2   CONTINUE
      ELSE
        CALL TOBUFC( DATAB2,2 )
      ENDIF

      WRITE(MONIOU,*) 'CERCNT = ',SNGL( CERCNT )
      CERCNT = 0.D0
      LHCER  = 0

      RETURN
      END

C=======================================================================

      SUBROUTINE OUTPT2(J,IMOV)

C-----------------------------------------------------------------------
C  (WRITE CERENKOV RADIATION) OUTP(U)T
C
C  OUTPUT ROUTINE FOR CERENKOV PHOTONS
C  THIS SUBROUTINE IS CALLED FROM CERENE AND CERENH
C
C  AUTHORS : S. MARTINEZ, UNIVERSITY OF MADRID
C            F. ARQUEROS, UNIVERSITY OF MADRID
C-----------------------------------------------------------------------

      IMPLICIT NONE
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
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      COMMON /GRAAL1/  WAVELENGTH ! (NM)
      REAL WAVELENGTH
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*KEND.

      INTEGER I,J,IMOV
C-----------------------------------------------------------------------

      IF(DEBUG)WRITE(MDEBUG,3)PHOTCM,XCER,YCER,UEMIS,VEMIS,CARTIM,ZEMIS
  3   FORMAT(' OUTPT2: ',1P,8E10.3)
C  WRITE A BLOCK OF 39 PARTICLES TO THE CERENKOV OUTPUT BUFFER AND
C  CLEAR FIELD
      IF ( LCERFI ) THEN
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c        DATAB2(LHCER+1) = PHOTCM
cc        DATAB2(LHCER+1) = WAVELENGTH + J*1000.
        DATAB2(LHCER+1) = J*100000. + IMOV*1000. + WAVELENGTH
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        DATAB2(LHCER+2) = XCER
        DATAB2(LHCER+3) = YCER
        DATAB2(LHCER+4) = UEMIS
        DATAB2(LHCER+5) = VEMIS
        DATAB2(LHCER+6) = CARTIM
        DATAB2(LHCER+7) = ZEMIS
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c        CERCNT = CERCNT + DBLE( PHOTCM )
        CERCNT = CERCNT + 1
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        LHCER  = LHCER + 7
        IF ( LHCER .GE. MAXBF2 ) THEN
          CALL TOBUFC( DATAB2,0 )
          DO  1  I = 1,MAXBF2
            DATAB2(I) = 0.
    1     CONTINUE
          LHCER = 0
        ENDIF
      ELSE
C  WRITE A BLOCK OF 39 PARTICLES TO THE PARTICLE OUTPUT BUFFER AND
C  CLEAR FIELD
        DATAB(LH+1) = 99.E5 + NINT(PHOTCM)*10. + 1.
        DATAB(LH+2) = XCER
        DATAB(LH+3) = YCER
        DATAB(LH+4) = UEMIS
        DATAB(LH+5) = VEMIS
        DATAB(LH+6) = CARTIM
        DATAB(LH+7) = ZEMIS
        LH     = LH + 7
        NOPART = NOPART + 1
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c        CERCNT = CERCNT + DBLE( PHOTCM )
        CERCNT = CERCNT + 1
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        IF ( LH .GE. MAXBUF ) THEN
          CALL TOBUF( DATAB,0 )
          DO  2  I = 1,MAXBUF
            DATAB(I) = 0.
    2     CONTINUE
          LH = 0
        ENDIF
      ENDIF

      RETURN
      END

C=======================================================================

      SUBROUTINE TOBUFC( A,IFL )

C-----------------------------------------------------------------------
C  (WRITE) TO BUF(FER) C(ERENKOV DATA)
C
C  COPY TO BUFFER CERENKOV DATA
C  THIS SUBROUTINE IS CALLED FROM MAIN, INPRM, ELECTR, PHOTON, OUTND2,
C  AND OUTPT2
C  ARGUMENTS:
C   A      = ARRAY TO BE WRITTEN TO TAPE
C   IFL    = STARTING OF FINAL OUTPUT
C          = 0  NORMAL BLOCK
C          = 1  NORMAL BLOCK WITH END OF OUTPUT
C          = 2  ONLY END OF OUTPUT
C-----------------------------------------------------------------------

      IMPLICIT NONE
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
*KEEP,RECORD.
      COMMON /RECORD/  IRECOR
      INTEGER          IRECOR
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
*KEND.

      INTEGER   NSUBBL
      PARAMETER (NSUBBL=21)
      REAL      A(*)
C  NSUBBL IS NUMBER OF SUBBLOCKS IN ONE OUTPUT RECORD
C  (OUTPUT RECORD LENGTH = NSUBBL * 39 * 7 * 4 BYTES  <= 22932 )
C  IBLK2 IS COUNTER FOR SUBBLOCKS OF CERENKOV OUTPUT
C  OUTPUT BUFFER FOR CERENKOV OUTPUT
      REAL      OUTBF2(MAXBF2,NSUBBL)
      SAVE      OUTBF2
      INTEGER   I,IBLK2,IFL,K
      DATA      IBLK2 / 0 /
C-----------------------------------------------------------------------

      IF ( IFL .LE. 1 ) THEN
        IBLK2 = IBLK2 + 1
        DO  3  I = 1,MAXBF2
          OUTBF2(I,IBLK2) = A(I)
    3   CONTINUE
      ENDIF

C  WRITE TO TAPE IF BLOCK IS FULL OR IF IFL IS 1
      IF ( IFL .GE. 1  .OR.  IBLK2 .EQ. NSUBBL ) THEN
        NRECER = NRECER + 1

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c        WRITE(CETAPE)            ((OUTBF2(I,K),I=1,MAXBF2),K=1,NSUBBL)
        call jccersave(outbf2)
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        IBLK2 = 0
        DO  4  K = 1,NSUBBL
        DO  4  I = 1,MAXBF2
          OUTBF2(I,K) = 0.0
   4    CONTINUE
      ENDIF

      RETURN
      END
