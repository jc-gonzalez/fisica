      SUBROUTINE UTQSEA(X1,X2,X3)
 
C-----------------------------------------------------------------------
C  UT(ILITY ROUTINE) SEA (QUARK STRUCTURE FUNCTION)
C
C  SEA QUARK STRUCTURE FUNCTION INTEGRAL
C  RETURNS INTEGRAL (XSE(1)->XSE(I)) OF FU(Z) DZ
C
C  THIS SUBROUTINE IS CALLED FROM VENLNK
C
C  DESIGN   : D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
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
 
      PARAMETER (NSTRU=2049)
      COMMON /FILES/   IFCH,IFDT,IFHI,IFMT,IFOP
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
      COMMON /STRU2/   DELTA0,DELTA1,QSEH(NSTRU),QSEPI(NSTRU)
     *                ,QVAH(NSTRU),QVAPI(NSTRU),XSE(NSTRU),XVA(NSTRU)
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'UTQSEA:'
 
      X0 = 0.
      N = NSTRU
      IF ( ISH .GE. 90 ) THEN
        IF ( X1.LT.X0 .OR. X2.LT.X1 .OR. X3.LT.X2 ) THEN
          CALL UTMSG('UTQSEA')
          WRITE(IFCH,*)'   XI=',X0,X1,X2,X3
          CALL UTMSGF
        ENDIF
      ENDIF
      I1 = N/3
      I2 = 2*N/3
      FAC1 = (X1-X0)/FLOAT(I1-1)
      DO 11 I=1,I1-1
        XSE(I)=(I-1.)*FAC1+X0
 11   CONTINUE
      FAC2 = (X2-X1)/FLOAT(I2-I1)
      DO 12 I=I1,I2-1
        XSE(I)=FLOAT(I-I1)*FAC2 +X1
 12   CONTINUE
      FAC3 = (X3-X2)/FLOAT(N-I2)
      DO 13 I=I2,N
        XSE(I)=MIN( FLOAT(I-I2)*FAC3 +X2, 0.99999999 )
 13   CONTINUE
 
      XCUT2 = XCUT**2
      XCUT4 = XCUT2**2
      XCUT6 = XCUT2*XCUT4
      CUTLOG = LOG(XCUT)
C  COEFFICIENTS FOR HADRONIC SEA QUARK STRUCTURE FUNCTION
      AH0 = -8. + 37.333333*XCUT2 - 29.866667*XCUT4 + 3.65714286*XCUT6
      AH1 = 14. - 26.25*XCUT2 + 8.75*XCUT4 - 0.2734375*XCUT6
      AH2 = -18.666667 + 14.933333*XCUT2 - 1.82857143*XCUT4
      AH3 = 17.5 - 5.8333333*XCUT2 + 0.182291667*XCUT4
      AH4 = -11.2 + 1.37142857*XCUT2
      AH5 = 4.6666667 - 0.14583333*XCUT2
      AH6 = -1.14285714
      AH7 = 0.125
      QAH = 1. - AH1 * XCUT2
      AHCUT = AH0 * XCUT
C  COEFFICIENTS FOR PIONIC SEA QUARK STRUCTURE FUNCTION
      API0 = -5. + 6.6666667*XCUT2 - 0.53333333*XCUT4
      API1 = 5. - 1.875*XCUT2
      API2 = -3.3333333 + 0.26666667*XCUT2
      API3 = 1.25
      API4 = -0.2
      QAPI = 1. - API1 * XCUT2
      APICUT = API0 * XCUT
 
      QSEH(1)  = 0.
      QSEPI(1) = 0.
      DO 2 I=2,N
        Z = XSE(I)
        ROOT     = SQRT(Z**2 + XCUT2)
        ROOTLG   = LOG( Z + ROOT ) - CUTLOG
        QSEH(I)  = 1.265 * ( QAH * ROOTLG - AHCUT
     *             + ROOT * (AH0 + Z*(AH1 + Z*(AH2 + Z*(AH3
     *             + Z*(AH4 + Z*(AH5 + Z*(AH6 + Z*AH7))))))) )
        QSEPI(I) = 0.9 * ( QAPI * ROOTLG - APICUT
     *             + ROOT * (API0+Z*(API1+Z*(API2+Z*(API3+Z*API4)))) )
 2    CONTINUE
 
      RETURN
      END
