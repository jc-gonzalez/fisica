C=======================================================================

      SUBROUTINE INPRM
 
C-----------------------------------------------------------------------
C  IN(PUT) PR(I)M(ARY)
C
C  TAKES INPUT PRIMARY ENERGY FROM SPECIFIED SPECTRUM
C  CHECKS INPUT VARIABLES FOR CONSISTENCY AND LIMITATIONS
C  WRITES DATA BASE FILE
C  THIS SUBROUTINE IS CALLED FROM MAIN
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,ATMOS.
      COMMON /ATMOS/   AATM,BATM,CATM,DATM
      DOUBLE PRECISION AATM(5),BATM(5),CATM(5),DATM(5)
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
*KEEP,PARPAE.
      DOUBLE PRECISION GAMMA,COSTHE,PHI,H,T,X,Y,CHI,BETA,GCM,ECM
      EQUIVALENCE      (CURPAR(2),GAMMA),  (CURPAR(3),COSTHE),
     *                 (CURPAR(4), PHI ),  (CURPAR(5), H    ),
     *                 (CURPAR(6), T   ),  (CURPAR(7), X    ),
     *                 (CURPAR(8), Y   ),  (CURPAR(9), CHI  ),
     *                 (CURPAR(10),BETA),  (CURPAR(11),GCM  ),
     *                 (CURPAR(12),ECM )
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
*KEEP,STACKF.
      COMMON /STACKF/  STACK,STACKP,EXST,NSHIFT,NOUREC,ICOUNT,NTO,NFROM
      INTEGER          MAXSTK
      PARAMETER        (MAXSTK = 12*340*2)
      DOUBLE PRECISION STACK(MAXSTK)
      INTEGER          STACKP,EXST,NSHIFT,NOUREC,ICOUNT,NTO,NFROM
*KEEP,VERS.
      COMMON /VERS/    VERNUM,MVDATE,VERDAT
      DOUBLE PRECISION VERNUM
      INTEGER          MVDATE
      CHARACTER*18     VERDAT
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
*KEND.
 
      DOUBLE PRECISION EFRAC,HEIGH,H0,OOO,THICK
      REAL             VERVEN
      INTEGER          I,IBL,IDPM,ILONG,ISO,J,L
      LOGICAL          LTHIN
      EXTERNAL         HEIGH,THICK
      CHARACTER*1      MARK
      CHARACTER*9      LSTDSN
C-----------------------------------------------------------------------
 
      WRITE(MONIOU,504)
  504 FORMAT(//' ',10('='),' SHOWER PARAMETERS ', 50('=') )
 
C  WRITE ENERGY SPECTRUM TO HEADER
      RUNH(16) = PSLOPE
      RUNH(17) = LLIMIT
      RUNH(18) = ULIMIT
 
      EVTH(58) = PSLOPE
      EVTH(59) = LLIMIT
      EVTH(60) = ULIMIT
 
      IF ( PRMPAR(1) .GE. 6000.D0  .OR.  PRMPAR(1) .LE. 0.D0 ) THEN
        WRITE(MONIOU,*)'INCORRECT SELECTION OF PRIMARY PARTICLE TYPE = '
     *                  ,INT(PRMPAR(1))
        WRITE(MONIOU,*)'PLEASE READ THE MANUALS'
        STOP
      ENDIF
C  CHECK WETHER NUCLEUS IS A SINGLE NUCLEON
      IF (PRMPAR(1) .EQ. 100.D0 ) PRMPAR(1) = 13.D0
      IF (PRMPAR(1) .EQ. 101.D0 ) PRMPAR(1) = 14.D0
      WRITE(MONIOU,*)'PRIMARY PARTICLE IDENTIFICATION IS ',
     *                NINT(PRMPAR(1))
C  CHECK RECOMMENDED ENERGY RANGE
      IF ( FVENUS  .AND.
     *     ULIMIT.GT.2.D7  .AND.  PRMPAR(1).GE.8.D0 ) THEN
        WRITE(MONIOU,502) ULIMIT
  502   FORMAT(' INTERACTION MODEL DOUBTFUL FOR THE SELECTED PRIMARY ',
     *         'ENERGY OF ',E10.3,' GEV'/' PLEASE READ THE MANUALS')
        STOP
      ENDIF
 



c> *** modified by fs (22/09/98) *******************************


c      IF ( PRMPAR(1) .GT. 101.D0 ) THEN
c        IF ( GHEISH ) THEN
cC  GHEISHA CAN TREAT ONLY DEUTERONS, TRITONS, AND ALPHA PARTICLES
c          IF ( PRMPAR(1) .NE. 201.D0  .AND.  PRMPAR(1) .NE. 301.D0
c     *         .AND.  PRMPAR(1) .NE. 402.D0 ) THEN
c            IF ( LLIMIT .LT. HILOELB * INT(PRMPAR(1)/100.D0) ) THEN
c              WRITE(MONIOU,503) INT(PRMPAR(1)/100.D0),LLIMIT
c              STOP
c            ENDIF
c          ENDIF
c        ELSE
c          IF ( LLIMIT .LT. HILOELB * INT(PRMPAR(1)/100.D0) ) THEN
c            WRITE(MONIOU,503) INT(PRMPAR(1)/100.D0),LLIMIT
c  503       FORMAT(' NUCLEUS WITH A =',I2,' AND PRIMARY ENERGY =',
c     *        1PE10.3,' GEV TOO LOW FOR HIGH ENERGY INTERACTION MODEL'/
c     *        ' AND CANNOT BE TREATED BY LOW ENERGY INTERACTION MODEL'/
c     *        ' PLEASE READ THE MANUALS')
c            STOP
c          ENDIF
c        ENDIF
c      ENDIF


c> *** end of modification ****************************************

C  DEFINE ENERGY RANGE AND ENERGY SPECTRUM OF PRIMARY
      IF ( LLIMIT .EQ. ULIMIT ) THEN
        ISPEC = 0
        WRITE(MONIOU,506) LLIMIT
  506   FORMAT(' PRIMARY ENERGY IS FIXED AT           ',1PE10.3,
     *         ' GEV' )
      ELSE
        ISPEC = 1
        WRITE(MONIOU,505) PSLOPE,LLIMIT,ULIMIT
  505   FORMAT(' PRIMARY ENERGY IS TAKEN FROM SPECTRUM VIA MONTE CARLO'/
     *  5X,' SLOPE OF PRIMARY SPECTRUM                = ',1P,E10.3/
     *  5X,' LOWER LIMIT CUT-OFF FOR PRIMARY SPECTRUM = ',E10.3,' GEV'/
     *  5X,' UPPER LIMIT CUT-OFF FOR PRIMARY SPECTRUM = ',E10.3,' GEV'/)
        IF ( PSLOPE .NE. -1.D0 ) THEN
          LL   = LLIMIT ** (PSLOPE + 1.D0)
          UL   = ULIMIT ** (PSLOPE + 1.D0)
          SLEX = 1.D0 / (PSLOPE + 1.D0)
        ELSE
          LL   = ULIMIT / LLIMIT
        ENDIF
      ENDIF
 
C  FIRST INTERACTION TARGET FIXED ?
      IF     ( N1STTR .EQ. 1 ) THEN
        WRITE(MONIOU,508) 'NITROGEN'
 508    FORMAT(' TARGET OF FIRST INTERACTION IS FIXED TO   ',A8)
      ELSEIF ( N1STTR .EQ. 2 ) THEN
        WRITE(MONIOU,508) 'OXYGEN  '
      ELSEIF ( N1STTR .EQ. 3 ) THEN
        WRITE(MONIOU,508) 'ARGON   '
      ELSE
        N1STTR = 0
        WRITE(MONIOU,*)'TARGET OF FIRST INTERACTION IS CHOSEN RANDOMLY'
      ENDIF
 
C  CHECK ANGULAR SETTINGS
      IF ( THETPR(1) .LT. 0.D0 ) THEN
        WRITE(MONIOU,*)'UNALLOWED CHOICE OF THETPR = ',SNGL(THETPR(1)),
     *                  ' DEGREES'
        WRITE(MONIOU,*)'PLEASE READ THE MANUALS'
        STOP
      ENDIF
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>      IF     ( THETPR(2) .GT. 70.D0 ) THEN
c>        WRITE(MONIOU,*)'UNALLOWED CHOICE OF THETPR = ',SNGL(THETPR(2)),
c>     *                  ' DEGREES'
c>        WRITE(MONIOU,*)'PLEASE READ THE MANUALS'
c>        STOP
c>      ELSEIF ( THETPR(2) .GT. 45.D0 ) THEN
c>        WRITE(MONIOU,*)'UNALLOWED CHOICE OF THETPR = ',SNGL(THETPR(2)),
c>     *                  ' DEGREES'
c>        WRITE(MONIOU,*)'#########################################'
c>        WRITE(MONIOU,*)'# IN DOUBTFUL CASES CONTACT THE AUTHORS #'
c>        WRITE(MONIOU,*)'#########################################'
c>        STOP
c>      ENDIF
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C  INCIDENCE ANGLE FIXED ?
      IF ( THETPR(1) .EQ. THETPR(2) .AND. PHIPR(1) .EQ. PHIPR(2) ) THEN
        FIXINC = .TRUE.
        WRITE(MONIOU,517) THETPR(1),PHIPR(1)
  517   FORMAT(' THETA OF INCIDENCE IS FIXED TO ',F10.2,' DEGREES'/
     *         ' PHI   OF INCIDENCE IS FIXED TO ',F10.2,' DEGREES')
      ELSE
        FIXINC = .FALSE.
        WRITE(MONIOU,527) THETPR,PHIPR
  527   FORMAT(' THETA OF INCIDENCE CHOSEN FROM ',F10.2,'...',F10.2,
     *         ' DEGREES'/
     *         ' PHI   OF INCIDENCE CHOSEN FROM ',F10.2,'...',F10.2,
     *         ' DEGREES')
      ENDIF
      EVTH(81) = THETPR(1)
      EVTH(82) = THETPR(2)
      EVTH(83) = PHIPR(1)
      EVTH(84) = PHIPR(2) 
      THETPR(1) = THETPR(1)*PI/180.D0
      THETPR(2) = THETPR(2)*PI/180.D0
      PHIPR(1)  = PHIPR(1) *PI/180.D0
      PHIPR(2)  = PHIPR(2) *PI/180.D0
 
C-----------------------------------------------------------------------
C  PRMPAR, OBSLEV, NOBSLV
      PRMPAR(2) = 0.D0
      PRMPAR(6) = 0.D0
      PRMPAR(7) = 0.D0
      PRMPAR(8) = 0.D0
 
C  ORDERING OF OBSERVATION LEVELS FROM TOP TO BOTTOM
      IF ( NOBSLV .GT. 1 ) THEN
  215 CONTINUE
        DO 11  I = 2,NOBSLV
          IF ( OBSLEV(I) .GT. OBSLEV(I-1) ) THEN
            OOO         = OBSLEV(I)
            OBSLEV(I)   = OBSLEV(I-1)
            OBSLEV(I-1) = OOO
            GOTO 215
          ENDIF
   11   CONTINUE
      ENDIF
C  CHECK WETHER OBSERVATION LEVELS ARE IN ALLOWED RANGE
      DO 12  I = 1,NOBSLV
        IF ( OBSLEV(I) .GE. HEIGH(0.D0) ) THEN
          WRITE(MONIOU,120)I,OBSLEV(I),HEIGH(0.D0)
 120      FORMAT(' UNALLOWED CHOICE OF OBSLEV '/' OBSERVATION LEVEL ',
     *           I2,' IS AT ',F12.3,' CM, WHICH IS ABOVE ',
     *           F12.3,' CM'/' PLEASE READ THE MANUALS')
          STOP
        ENDIF
        IF ( OBSLEV(I) .LE. -1.D5 ) THEN
          WRITE(MONIOU,121)I,OBSLEV(I)
 121      FORMAT(' UNALLOWED CHOICE OF OBSLEV '/' OBSERVATION LEVEL ',
     *          I2,' IS AT ',F12.3,' CM, WHICH IS BELOW ',
     *          '-1.D5 CM'/' PLEASE READ THE MANUALS')
          STOP
        ENDIF
        THCKOB(I) = THICK(OBSLEV(I))
   12 CONTINUE
 
C  WRITE OBSERVATION LEVELS TO HEADER (IN CM)
      RUNH(5) = REAL(NOBSLV)
      EVTH(47) = REAL(NOBSLV)
      DO 114  I = 1,NOBSLV
        RUNH(5+I)  = OBSLEV(I)
        EVTH(47+I) = OBSLEV(I)
  114 CONTINUE
 
C  FIRST INTERACTION HEIGHT FIXED ?
      IF ( FIX1I ) THEN
        IF ( FIXHEI .GE. HEIGH(0.D0) ) THEN
          WRITE(MONIOU,122)FIXHEI,HEIGH(0.D0)
 122      FORMAT(' UNALLOWED CHOICE OF FIXHEI '/' FIRST INTERACTION ',
     *           'IS FIXED AT ',F12.3,' CM, WHICH IS ABOVE ',
     *           F12.3,' CM'/' PLEASE READ THE MANUALS')
          STOP
        ENDIF
        IF ( FIXHEI .LE. OBSLEV(NOBSLV) ) THEN
          WRITE(MONIOU,123)FIXHEI,OBSLEV(NOBSLV)
 123      FORMAT(' UNALLOWED CHOICE OF FIXHEI '/' FIRST INTERACTION ',
     *           'IS FIXED AT ',F12.3,' CM, '/' WHICH IS BELOW ',
     *           'LOWEST OBSERVATION LEVEL AT ',F12.3,' CM'
     *           /' PLEASE READ THE MANUALS')
          STOP
        ENDIF
          WRITE(MONIOU,507) FIXHEI
 507      FORMAT(' HEIGHT OF FIRST INTERACTION IS FIXED TO ',1P,E10.2,
     *         ' CM')
        IF ( N1STTR .GE. 1  .AND.  N1STTR .LE. 3 ) THEN
          IF ( PRMPAR(1) .LE. 3.D0 ) THEN
            WRITE(MONIOU,516) INT(PRMPAR(1))
 516        FORMAT(' TARGET OF FIRST INTERACTION CANNOT BE FIXED FOR ',
     *           'PRIMARY TYPE ',I5/' PLEASE READ THE MANUALS')
            STOP
          ELSEIF ( N1STTR .EQ. 1 ) THEN
            WRITE(MONIOU,*)'TARGET OF FIRST INTERACTION IS NITROGEN'
          ELSEIF ( N1STTR .EQ. 2 ) THEN
            WRITE(MONIOU,*)'TARGET OF FIRST INTERACTION IS OXYGEN'
          ELSEIF ( N1STTR .EQ. 3 ) THEN
            WRITE(MONIOU,*)'TARGET OF FIRST INTERACTION IS ARGON'
          ENDIF
        ELSE
          WRITE(MONIOU,*)
     *       'TARGET OF FIRST INTERACTION IS CHOSEN AT RANDOM'
        ENDIF
      ELSE
        FIXHEI = 0.D0
        WRITE(MONIOU,*) 'HEIGHT OF FIRST INTERACTION IS CHOSEN RANDOMLY'
      ENDIF
 
C  STARTING ALTITUDE WITHIN ATMOSPHERE?
      IF ( THICK0 .LT. 0.D0 ) THEN
        WRITE(MONIOU,130)THICK0
 130    FORMAT(' UNALLOWED STARTING ALTITUDE WITH NEGATIVE MASS OVERLAY'
     *          ,E12.3/' PLEASE READ THE MANUALS')
        STOP
      ENDIF
      IF ( THICK0 .GE. THCKOB(NOBSLV) ) THEN
        WRITE(MONIOU,131) THICK0
 131    FORMAT(' UNALLOWED STARTING ALTITUDE AT ',F12.3,' G/CM**2',
     *         '  WHICH IS BELOW LOWEST OBSERVATION LEVEL'/
     *        ' PLEASE READ THE MANUALS')
        STOP
      ENDIF
      H0 = HEIGH(THICK0)
      IF ( THICK0 .EQ. 0.D0 ) THEN
        WRITE(MONIOU,518) H0, THICK0
        WRITE(MONIOU,*)'                 WHICH IS AT TOP OF ATMOSPHERE'
      ELSE
        WRITE(MONIOU,518) H0, THICK0
      ENDIF
 518  FORMAT(' STARTING ALTITUDE AT ',1P,F13.2,' CM (=',
     *                                             E7.1,' G/CM**2)')
      WRITE(MONIOU,203) (OBSLEV(I),THCKOB(I),I=1,NOBSLV)
  203 FORMAT(/' OBSERVATION LEVELS IN  CM    AND IN    G/CM**2 ',
     *  1P /(5X, 2E20.8 /))
 
C  LONGITUDINAL SHOWER DEVELOPMENT
      IF ( LLONGI ) THEN
        THSTEP = NINT(THSTEP)
        THSTEP = MAX(THSTEP,1.D0)
        THSTEP = MIN(THSTEP,1040.D0)
        THSTPI = 1.D0/THSTEP
        NSTEP  = INT(THCKOB(NOBSLV)*THSTPI)
        IF ( NSTEP .GE. 1040 ) THEN
          NSTEP  = 1040
          THSTEP = THCKOB(NOBSLV)/NSTEP
          WRITE(MONIOU,*)'LONGITUDINAL SHOWER SAMPLING MODIFIED'
        ENDIF
        WRITE(MONIOU,925) NSTEP+1,THSTEP
 925    FORMAT(/' LONGITUDINAL SHOWER DEVELOPMENT:'/
     *          '      SHOWER IS SAMPLED IN ',I4,
     *          ' STEPS OF ',F6.1,' G/CM**2')
C  GET HEIGHT VALUES IN CM FOR USE IN EGS
        DO 478  J = 0,NSTEP
          HLONG(J) = HEIGH(J*THSTEP)
 478    CONTINUE
        IF ( FLGFIT ) THEN
          WRITE(MONIOU,*)
     *      '     FIT TO CHARGED PARTICLE LONG. DISTRIBUTION   ENABLED'
        ELSE
          WRITE(MONIOU,*)
     *      '     FIT TO CHARGED PARTICLE LONG. DISTRIBUTION   DISABLED'
        ENDIF
        WRITE(MONIOU,*)
      ENDIF
 
C-----------------------------------------------------------------------
C  CHECK INPUT OF ENERGY CUTS
      IF     ( GHEISH  .AND.  ELCUT(1) .LT. 0.05D0 ) THEN
        WRITE(MONIOU,*)'ELCUT(1) SELECTED INCORRECT TO ',ELCUT(1),' GEV'
        WRITE(MONIOU,*)'PLEASE READ THE MANUALS '
        STOP
      ELSEIF ( .NOT.GHEISH  .AND.  ELCUT(1) .LT. 0.3D0 ) THEN
        WRITE(MONIOU,*)'ELCUT(1) SELECTED INCORRECT TO ',ELCUT(1),' GEV'
        WRITE(MONIOU,*)'PLEASE READ THE MANUALS '
        STOP
      ENDIF
      IF ( ELCUT(2) .LT. 0.05D0 ) THEN
        WRITE(MONIOU,*)'ELCUT(2) SELECTED INCORRECT TO ',ELCUT(2),' GEV'
        WRITE(MONIOU,*)'PLEASE READ THE MANUALS '
        STOP
      ENDIF
      IF ( ELCUT(3) .LT. 0.003D0 ) THEN
        WRITE(MONIOU,*)'ELCUT(3) SELECTED INCORRECT TO ',ELCUT(3),' GEV'
        WRITE(MONIOU,*)'PLEASE READ THE MANUALS '
        STOP
      ENDIF
      IF ( ELCUT(4) .LT. 0.003D0 ) THEN
        WRITE(MONIOU,*)'ELCUT(4) SELECTED INCORRECT TO ',ELCUT(4),' GEV'
        WRITE(MONIOU,*)'PLEASE READ THE MANUALS '
        STOP
      ENDIF
      WRITE(MONIOU,703) ECTMAP,ELCUT
  703 FORMAT (' PARTICLES WITH LORENTZ FACTOR LARGER THAN',1P,E15.4,
     *        ' ARE PRINTED OUT'/' SHOWER PARTICLES ENERGY CUT :'/
     *        '      FOR HADRONS   : ',E15.4,' GEV'/
     *        '      FOR MUONS     : ',E15.4,' GEV'/
     *        '      FOR ELECTRONS : ',E15.4,' GEV'/
     *        '      FOR PHOTONS   : ',E15.4,' GEV'//)
 
      DO 774  I = 1,4
        RUNH(20+I) = ELCUT(I)
        EVTH(60+I) = ELCUT(I)
  774 CONTINUE
 
C-----------------------------------------------------------------------
C  PARAMETERS OF EARTH MAGNETIC FIELD OF MIDDLE EUROPE
C  +X DIRECTION IS NORTH, +Y DIRECTION IS EAST, +Z DIRECTION IS DOWN
      BVAL   = SQRT( BX**2 + BZ**2 )
C  BNORM HAS DIMENSIONS OF MEV/CM
      BNORM  = BVAL * C(25) * 1.D-16
C  BNORMC HAS DIMENSIONS OF GEV/CM
      BNORMC = BNORM * 1.D-3
      SINB   = BZ / BVAL
      COSB   = BX / BVAL
      WRITE(MONIOU,*)'EARTH MAGNETIC FIELD STRENGTH IS ',SNGL(BVAL),
     *                ' MICROTESLA'
      WRITE(MONIOU,*)'     WITH INCLINATION ANGLE      ',
     *               SNGL(ASIN(SINB)*180./PI),' DEGREES'
      IF ( BVAL .GE. 10000.D0 ) THEN
        WRITE(MONIOU,*)'YOU WANT TO MAGNETIZE THE GALAXY ?'
        WRITE(MONIOU,*)'PLEASE READ THE MANUALS !'
        STOP
      ENDIF
C  LIMITING FACTOR FOR STEP SIZE OF ELECTRON IN MAGNETIC FIELD
      BLIMIT = 0.2 / BNORM
      EVTH(71) = BX
      EVTH(72) = BZ
C  ANGLE BETWEEN ARRAY X-DIRECTION AND MAGNETIC NORD
C  POSITIV, IF X-DIRECTION OF ARRAY POINTS TO EASTERN DIRECTION
      ARRANR = ARRANG * PI / 180.D0
      COSANG = COS(ARRANR)
      SINANG = SIN(ARRANR)
      EVTH(93) = ARRANR
      IF ( ARRANG .NE. 0.D0 ) THEN
        WRITE(MONIOU,*)
        WRITE(MONIOU,*)'DETECTOR COORDINATE SYSTEM IS ROTATED AWAY ',
     *                 'FROM NORTH BY ',SNGL(ARRANG),' DEGREES'
      ENDIF
 
C-----------------------------------------------------------------------
C  DEFINE CERENKOV ARRAY
      NCERX = MAX( NCERX, 1 )
      NCERY = MAX( NCERY, 1 )
      ACERX = ABS(ACERX)
      ACERY = ABS(ACERY)
      DCERX = MAX( ABS(DCERX), 0.001 )
      DCERY = MAX( ABS(DCERY), 0.001 )
      XCMAX = (ACERX + (NCERX-1) * DCERX) * 0.5
      YCMAX = (ACERY + (NCERY-1) * DCERY) * 0.5
      DCERXI = 1./DCERX
      EPSX = ACERX * 0.5 * DCERXI
      DCERYI = 1./DCERY
      EPSY = ACERY * 0.5 * DCERYI
      IF ( MOD(NCERX,2) .EQ. 0 ) THEN
        FCERX = -0.5
      ELSE
        FCERX = 0.0
      ENDIF
      IF ( MOD(NCERY,2) .EQ. 0 ) THEN
        FCERY = -0.5
      ELSE
        FCERY = 0.0
      ENDIF
 
      WRITE(MONIOU,472)
     *          ACERX*.01,ACERY*.01, DCERX*.01,DCERY*.01, NCERX,NCERY
 472  FORMAT(/' CERENKOV ARRAY:'/
     *  5X,' CERENKOV STATIONS ARE ',F6.2,'  X  ',F6.2,' M**2 LARGE'/
     *  5X,' THE GRID SPACING IS   ',F6.2,' AND ',F6.2,' M',/
     *  5X,' THERE ARE ',I3,' X ',I3,' STATIONS IN X/Y DIRECTIONS'/
     *  5X,' THE CERENKOV ARRAY IS CENTERED AROUND (0., 0.)'/)
C  CALCULATE CERENKOV YIELD FACTOR FROM WAVELENGTH BAND
      IF ( WAVLGL .LT. 100.D0  .OR.  WAVLGU .GT. 700.D0
     *                         .OR.  WAVLGL .GE. WAVLGU ) THEN
        WRITE(MONIOU,*)'CERENKOV WAVELENGTH BAND FROM ',SNGL(WAVLGL),
     *              ' TO ',SNGL(WAVLGU),' NANOMETER'
        WRITE(MONIOU,*)' IS OUT OF VALIDITY RANGE'
        WRITE(MONIOU,*)'PLEASE READ THE MANUALS'
        STOP
      ENDIF
      WRITE(MONIOU,*)'CERENKOV WAVELENGTH BAND FROM ',SNGL(WAVLGL),
     *              ' TO ',SNGL(WAVLGU),' NANOMETER'
C  WAVELENGTH IS CONVERTED FROM NM TO CM
      CYIELD = (1.D0/WAVLGL - 1.D0/WAVLGU) * 2.D7 * PI / C(50)
C  CALCULATE FACTOR FOR ETA DENSITY NORML.(ETA AT SEA LEVEL = 0.283D-3)
      ETADSN = 0.283D-3 * CATM(1) / BATM(1)
 
      IF ( CERSIZ .GT. 0. ) THEN
        WRITE(MONIOU,*)'CERENKOV BUNCH SIZE IS SET TO=',CERSIZ
      ELSE
        WRITE(MONIOU,*)'CERENKOV BUNCH SIZE IS CALCULATED FOR EACH ',
     *                 'SHOWER'
      ENDIF
 
      IF ( LCERFI ) THEN
        WRITE(MONIOU,*)'CERENKOV PHOTONS ARE WRITTEN TO SEPARATE FILE'
      ELSE
        WRITE(MONIOU,*)'CERENKOV PHOTONS ARE WRITTEN TO PARTICLE ',
     *                 'OUTPUT FILE'
      ENDIF
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c   Next block of code has been modified, and is passed to MAIN
c----------------------------------------------------------------------
cC  SCATTERING OF CENTER OF CHERENKOV ARRAY RELATIVE TO SHOWER AXIS
c      ICERML = MIN(MAX(ICERML,1),20)
c      XSCATT = ABS(XSCATT)
c      YSCATT = ABS(YSCATT)
c      IF ( ICERML .GE. 1 ) THEN
c        WRITE(MONIOU,5225)ICERML,XSCATT,YSCATT
c 5225   FORMAT(' DEFINE MULTIPLE CERENKOV ARRAYS TO USE EACH',
c     *   ' SHOWER SEVERAL TIMES'/ ' USE EACH EVENT ',I2,' TIMES'/
c     *   ' THE EVENTS ARE SCATTERED QUASI RANDOMLY IN THE RANGE '/
c     *   '   X =  +- ',F10.0,'    Y = +- ',F10.0)
c        DO 4438 I=1,ICERML
c          CALL SELCOR(CERXOS(I),CERYOS(I))
c          WRITE(MONIOU,4437) I,CERXOS(I),CERYOS(I)
c 4437     FORMAT('    CORE OF EVENT ',I2,'  AT  ',2F12.2)
c 4438   CONTINUE
c        XCMAX = XCMAX + XSCATT
c        YCMAX = YCMAX + YSCATT
c      ENDIF
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

C  STORE CERENKOV PARAMETERS IN EVENTHEADER
      EVTH(86) = NCERX
      EVTH(87) = NCERY
      EVTH(88) = DCERX
      EVTH(89) = DCERY
      EVTH(90) = ACERX
      EVTH(91) = ACERY
      IF ( LCERFI ) THEN
        EVTH(92) = 1.
      ELSE
        EVTH(92) = 0.
      ENDIF
      EVTH(96) = WAVLGL
      EVTH(97) = WAVLGU
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c   Next block of code has been passed to MAIN
c----------------------------------------------------------------------
c      EVTH(98) = FLOAT(ICERML)
c      DO  480 I=1,20
c        EVTH( 98+I) = CERXOS(I)
c        EVTH(118+I) = CERYOS(I)
c 480  CONTINUE
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

C-----------------------------------------------------------------------
C  FLAG FOR ADDITIONAL MUON INFORMATION
      IF ( FMUADD ) THEN
        WRITE(MONIOU,*)
        WRITE(MONIOU,*)'ADDITIONAL INFORMATION ON MUON ORIGIN IS',
     *                 ' WRITTEN TO PARTICLE TAPE'
        EVTH(94) = 1.
      ELSE
        EVTH(94) = 0.
      ENDIF
 
C  PRINTOUT OF INFORMATIONS FOR DEBUGGING
      IF ( DEBUG ) WRITE(MONIOU,484) MDEBUG
  484 FORMAT(/' ATTENTION ! DEBUGGING IS ACTIVE'/
     *          ' ====> DEBUG INFORMATION WRITTEN TO UNIT ',I3//)
 
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c   Next block of code is obsolete.
c   Now it's used "jcio" routines (C)
cC-----------------------------------------------------------------------
cC  OPEN OUTPUT DATA SET FOR RUN
c      IBL = INDEX(DSN,' ')
c      DSN(IBL:73) = 'DAT000000'
c      WRITE(DSN(IBL+3:IBL+8),'(I6)') NRRUN
c      DO 274  L = IBL+3,IBL+8
c        IF ( DSN(L:L) .EQ. ' ' ) DSN(L:L) = '0'
c 274  CONTINUE
cC  OPEN DATASET FOR PARTICLE OUTPUT
c      OPEN(UNIT=PATAPE,FILE=DSN,STATUS='NEW',
c     *       FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
c      WRITE(MONIOU,579) DSN
c 579  FORMAT(/' PARTICLE OUTPUT TO DIRECTORY : ',A79)
cC  WRITE RUNHEADER TO OUTPUT BUFFER
c      CALL TOBUF( RUNH,0 )
c
cC  OPEN OUTPUT DATA SET FOR CERENKOV PHOTONS
c      IF ( LCERFI ) THEN
c        DSN(IBL:73) = 'CER000000'
c        WRITE(DSN(IBL+3:IBL+8),'(I6)') NRRUN
c        DO 249  L = IBL+3,IBL+8
c          IF (DSN(L:L) .EQ. ' ' ) DSN(L:L) = '0'
c 249    CONTINUE
c        OPEN(UNIT=CETAPE,FILE=DSN,STATUS='NEW',
c     *       FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
c        WRITE(MONIOU,580) DSN
c 580    FORMAT(' CERENKOV OUTPUT TO DIRECTORY : ',A79)
c        CALL TOBUFC( RUNH,0 )
c      ELSE
c        WRITE(MONIOU,580) DSN
c      ENDIF
cC  RESET DSN
c      DSN(IBL:73) = '         '
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 
C  OPEN ON EXTERNAL STACK
C  BLOCKS OF 32640 BYTES = 4080 REAL*8 = 340 PARTICLES
      OPEN(UNIT=EXST,STATUS='SCRATCH',
     *     FORM='UNFORMATTED',ACCESS='DIRECT',RECL=MAXSTK)
 
 
C-----------------------------------------------------------------------
C  WRITE DATA SET FOR INFORMATION BANK
      IF (FDBASE ) THEN
C  OPEN OUTPUT DATA SET FOR RUN
        IBL = INDEX(DSN,' ')
        DSN(IBL:79) = 'DAT000000.dbase'
        WRITE(DSN(IBL+3:IBL+8),'(I6)') NRRUN
        DO 275  L = IBL+3,IBL+8
          IF ( DSN(L:L) .EQ. ' ' ) DSN(L:L) = '0'
 275    CONTINUE
        OPEN(UNIT=MDBASE,FILE=DSN,STATUS='NEW')
        WRITE(MONIOU,581) DSN
 581    FORMAT(/' DBASE OUTPUT TO DIRECTORY : ',A79)
C  RESET DSN
        DSN(IBL+9:IBL+14) = '      '
 
        LSTDSN(1:3) = 'LST'
        LSTDSN(4:9) = DSN(IBL+3:IBL+8)
        VERVEN=FLOAT(IVERVN)/1000.
        IF ( LLONGI ) THEN
          ILONG = 1
        ELSE
          ILONG = 0
        ENDIF
        IF ( EVTH(75) .NE. 0. ) THEN
          ISO = 0
        ELSE
          ISO = 1
        ENDIF
C  SET DPMFLAG (0=VENUS, 1=HDPM, 2=SIBYLL, 3=QGSJET, 4=DPMJET)
        IF     ( EVTH( 76) .NE. 0. ) THEN
          IDPM = 0
        ELSEIF ( EVTH(139) .NE. 0. ) THEN
          IDPM = 2
        ELSEIF ( EVTH(141) .NE. 0. ) THEN
          IDPM = 3
        ELSEIF ( EVTH(143) .NE. 0. ) THEN
          IDPM = 4
        ELSE
          IDPM = 1
        ENDIF
C  INCREMENT DPMFLAG FOR VARIOUS CROSS SECTIONS
C  BY (0=HDPM-SIG, 10=VENUSSIG, 20=SIBYLLSIG, 30=QGSSIG, 4=DPMJETSIG)
        IF     ( EVTH(145) .NE. 0 ) THEN
          IDPM = IDPM + 10
        ELSEIF ( EVTH(140) .NE. 0 ) THEN
          IDPM = IDPM + 20
        ELSEIF ( EVTH(142) .NE. 0 ) THEN
          IDPM = IDPM + 30
        ELSEIF ( EVTH(144) .NE. 0 ) THEN
          IDPM = IDPM + 40
        ENDIF
        MARK = '1'
        LTHIN = .FALSE.
        EFRAC = 0.D0
 
        WRITE(MDBASE,666)VERNUM,MARK,MVDATE,VERVEN,
     $INT(RUNH(3))+19000000,INT(EVTH(80)),INT(EVTH(79)),INT(EVTH(78)),
     $INT(EVTH(77)),INT(RUNH(2)),INT(PRMPAR(1)),
     $LLIMIT,ULIMIT,PSLOPE,INT(RUNH(20)),
     $INT(RUNH(19)),INT(EVTH(76)),INT(EVTH(75)),ISO,IDPM,
     $NFLAIN,NFLDIF,NFLPI0,NFLPIF,
     $NFLCHE,NFRAGM,ILONG,THSTEP,
     $BX,BZ,NOBSLV,
     $OBSLEV(1),OBSLEV(2),OBSLEV(3),
     $OBSLEV(4),OBSLEV(5),OBSLEV(6),
     $OBSLEV(7),OBSLEV(8),OBSLEV(9),
     $OBSLEV(10),ELCUT(1),ELCUT(2),ELCUT(3),
     $ELCUT(4),EVTH(81),EVTH(82),EVTH(83),
     $EVTH(84),FIXHEI,N1STTR,THICK0,
     $STEPFC,ARRANG,INT(EVTH(94)),NSEQ,
     $ISEED(1,1),ISEED(2,1),ISEED(3,1),ISEED(1,2),
     $ISEED(2,2),ISEED(3,2),ISEED(1,3),
     $ISEED(2,3),ISEED(3,3),0,DSN,
     $LSTDSN,' JDD300.01',' JDD300.01',
     $NSHOW,HOST,USER,LTHIN,EFRAC
 
 666    FORMAT('#version#',F6.3,A1,'#versiondate#',I9,'#venusversion#',
     $F6.3,'#rundate#',I9,/,'#computer#',I2,'#horizont#',I2,'#neutrino#'
     $,I2,'#cerenkov#',I2,'#runnumber#',I7,/,'#primary#',I5,
     $'#e_range_l#',E15.7,'#e_range_u#',E15.7,/,'#slope#',E15.7,'#nkg#',
     $I2,'#egs#',I2,'#venus#',I2,'#gheisha#',I2,'#isobar#',I2,'#hdpm#',
     $I2,/,'#hadflag1#',I2,'#hadflag2#',I2,'#hadflag3#',I2,'#hadflag4#',
     $I2,'#hadflag5#',I2,'#hadflag6#',I2,/,'#longi#',I2,'#longistep#',
     $E15.7,'#magnetx#',E15.7,/,'#magnetz#',E15.7,'#nobslev#',I3,/,
     $'#obslev1#',E15.7,'#obslev2#',E15.7,'#obslev3#',E15.7,/,
     $'#obslev4#',E15.7,'#obslev5#',E15.7,'#obslev6#',E15.7,/,
     $'#obslev7#',E15.7,'#obslev8#',E15.7,'#obslev9#',E15.7,/,
     $'#obslev10#',E15.7,'#hcut#',E15.7,'#mcut#',E15.7,/,'#ecut#',E15.7,
     $'#gcut#',E15.7,'#thetal#',E15.7,/,'#thetau#',E15.7,'#phil#',E15.7,
     $'#phiu#',E15.7,/,'#fixhei#',E15.7,'#n1sttr#',I3,'#fixchi#',E15.7,
     $/,'#stepfc#',E15.7,'#arrang#',E15.7,'#muaddi#',I2,'#nseq#',I2,/,
     $'#seq1seed1#',I9,'#seq1seed2#',I9,'#seq1seed3#',I9,/,'#seq2seed1#'
     $,I9,'#seq2seed2#',I9,'#seq2seed3#',I9,/,'#seq3seed1#',I9,
     $'#seq3seed2#',I9,'#seq3seed3#',I9,/,'#size#',I10,'#dsn_events#',
     $A59,/,'#dsn_prtout# ',A9,'#tape_name#',A10,'#backup#',A10,/,
     $'#howmanyshowers#',I10,'#host#',A20,'#user#',A20,/
     $'#thinning#',L4,'#thinninglevel#',E15.7)
 
C  RESET DSN
        DSN(IBL:IBL+14) = '               '
        CLOSE(UNIT=MDBASE)
      ENDIF
 
      WRITE(MONIOU,*)'NUMBER OF SHOWERS TO GENERATE =',NSHOW
      WRITE(MONIOU,*)
      RETURN
      END
