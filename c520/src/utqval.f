      SUBROUTINE UTQVAL(Q,NEND)
 
C-----------------------------------------------------------------------
C  UT(ILITY ROUTINE) VAL(ENCE QUARK STRUCTURE FUNCTION)
C
C  VALENCE QUARK STRUCTURE FUNCTION
C  RETURNS  INTEGRAL (XVA(1)->XVA(I)) FU(Z) DZ
C  THIS INTEGRAL IS ONLY CALCULATED FOR SMALL VALUES OF XVA UP TO 25
C  TIMES THE VALUE OF XCUT. FOR LARGER VALUES THE TABULATED VALUES OF
C  DATASET 'VENUSDAT' ARE TAKEN AND CORRECTED BY THE CONSTANT SHIFT
C  DELTA0 (FOR HADRONS) OR DELTA1 (FOR PIONS).
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
      COMMON /CIPIO/   IPIO
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
      COMMON /STRU2/   DELTA0,DELTA1,QSEH(NSTRU),QSEPI(NSTRU)
     *                ,QVAH(NSTRU),QVAPI(NSTRU),XSE(NSTRU),XVA(NSTRU)
 
      DIMENSION        Y0(9),Y1(9),Q(NEND)
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'UTQVAL: IPIO,NEND=',IPIO,NEND
 
      XCUT2 = XCUT**2
      Q(1)  = 0.
      Z = XVA(1)
      DENOMI = 1. / SQRT(Z**2 + XCUT2)
 
      IF ( IPIO .EQ. 0 ) THEN
C  CALCULATE THE FIRST NEND VALUES OF STRUCTURE FUNCTION FOR HADRONS
        Y0(1) = 0.
        DO 3 I=2,NEND
          FACT = (XVA(I) - Z) * 0.125
          DO 2 J=2,8
            Z = Z + FACT
            DENOMI = 1. / SQRT(Z**2 + XCUT2)
            Y0(J) = (1.-Z)**3.46 * Z**.419 * (2.74793064*Z + 0.62452969)
     *              * DENOMI
 2        CONTINUE
          Z = XVA(I)
          DENOMI = 1. / SQRT(Z**2 + XCUT2)
          Y0(9) = (1.-Z)**3.46 * Z**.419 * (2.74793064*Z + 0.62452969)
     *             * DENOMI
C  INTEGRATION AFTER BODE'S RULE (ABRAMOWITZ + STEGUN, HANDBOOK OF
C  MATHEMATICAL FUNCTIONS, DOVER PUBLICATIONS (1970), FORMULA 25.4.18)
          Q(I) =  2.8218694E-4 * FACT *       ( 989. * (Y0(1) + Y0(9))
     *            +  5888. * (Y0(2) + Y0(8)) -  928. * (Y0(3) + Y0(7))
     *            + 10496. * (Y0(4) + Y0(6)) - 4540. *  Y0(5) )
     *                         + Q(I-1)
          Y0(1) = Y0(9)
 3      CONTINUE
 
      ELSE
C  CALCULATE THE FIRST NEND VALUES OF STRUCTURE FUNCTION FOR PIONS
        Y1(1) = 0.
        DO 5 I=2,NEND
          FACT = (XVA(I) - Z) * 0.125
          DO 4 J=2,8
            Z = Z + FACT
            DENOMI = 1. / SQRT(Z**2 + XCUT2)
            Y1(J) = (1.-Z)**0.7 * Z**.4 * DENOMI
 4        CONTINUE
          Z = XVA(I)
          DENOMI = 1. / SQRT(Z**2 + XCUT2)
          Y1(9) = (1.-Z)**0.7 * Z**.4 * DENOMI
C  INTEGRATION AFTER BODE'S RULE (ABRAMOWITZ + STEGUN, HANDBOOK OF
C  MATHEMATICAL FUNCTIONS, DOVER PUBLICATIONS (1970), FORMULA 25.4.18)
          Q(I) =  2.8218694E-4 * FACT *       ( 989. * (Y1(1) + Y1(9))
     *            +  5888. * (Y1(2) + Y1(8)) -  928. * (Y1(3) + Y1(7))
     *            + 10496. * (Y1(4) + Y1(6)) - 4540. *  Y1(5) )
     *            * 0.1730725  + Q(I-1)
          Y1(1) = Y1(9)
 5      CONTINUE
      ENDIF
 
      RETURN
      END
