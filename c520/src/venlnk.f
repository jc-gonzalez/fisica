      SUBROUTINE VENLNK
 
C-----------------------------------------------------------------------
C  VEN(US) L(I)NK (TO CORSIKA)
C
C  LINKS VENUS PACKAGE TO CORSIKA, NEEDS FIRST CALL OF VENINI
C  THIS SUBROUTINE IS CALLED FROM SDPM
C
C  DESIGN   : D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
*KEEP,INTER.
      COMMON /INTER/   AVCH,AVCH3,DC0,DLOG,DMLOG,ECMDIF,ECMDPM,ELAB,
     *                 FNEUT,FNEUT2,GNU,PLAB,POSC2,POSC3,POSN2,POSN3,
     *                 RC3TO2,S,SEUGF,SEUGP,SLOG,SLOGSQ,SMLOG,
     *                 WIDC2,WIDC3,WIDN2,WIDN3,YCM,YY0,ZN,
     *                 IDIF,ITAR
      DOUBLE PRECISION AVCH,AVCH3,DC0,DLOG,DMLOG,ECMDIF,ECMDPM,ELAB,
     *                 FNEUT,FNEUT2,GNU,PLAB,POSC2,POSC3,POSN2,POSN3,
     *                 RC3TO2,S,SEUGF,SEUGP,SLOG,SLOGSQ,SMLOG,
     *                 WIDC2,WIDC3,WIDN2,WIDN3,YCM,YY0,ZN
      INTEGER          IDIF,ITAR
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
*KEEP,REST.
      COMMON /REST/    CONTNE,TAR,LT
      DOUBLE PRECISION CONTNE(3),TAR
      INTEGER          LT
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
*KEEP,VENUS.
      COMMON /VENUS/   ISH0,IVERVN,MTAR99,FVENUS,FVENSG
      INTEGER          ISH0,IVERVN,MTAR99
      LOGICAL          FVENUS,FVENSG
*KEND.
 
      PARAMETER (KOLLMX=2500)
      PARAMETER (MXEPS=10)
      PARAMETER (NDEP=129)
      PARAMETER (NDET=129)
      PARAMETER (NPRBMS=20)
      PARAMETER (NPTQ=129)
      PARAMETER (NSTRU=2049)
      COMMON /ACCUM/   AMSAC,ILAMAS,IMSG,INOIAC,IPAGE,JERR,NAEVT,NREVT
     *                ,NRPTL,NRSTR,NTEVT
      COMMON /CDEN/    MASSNR,RMX,R0
      COMMON /CIPIO/   IPIO
      COMMON /CNSTA/   AINFIN,PI,PIOM,PROM
      COMMON /COL/     BIMP,BMAX,COORD(4,KOLLMX),DISTCE(KOLLMX)
     *                ,QDEP(NDEP),QDET14(NDET),QDET16(NDET),QDET40(NDET)
     *                ,QDET99(NDET),RMPROJ,RMTARG(4),XDEP(NDEP)
     *                ,XDET14(NDET),XDET16(NDET),XDET40(NDET)
     *                ,XDET99(NDET)
     *                ,KOLL,LTARG,NORD(KOLLMX),NPROJ,NRPROJ(KOLLMX)
     *                ,NRTARG(KOLLMX),NTARG
      COMMON /CPRBMS/  PRBMS(NPRBMS)
      COMMON /CPTQ/    QPTH(NPTQ),QPTQ(NPTQ),XPTQ(NPTQ),QPTQMX,QPTHMX
      DOUBLE PRECISION SEEDC,SEEDI
      COMMON /CSEED/   SEEDC,SEEDI
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
      COMMON /STRU/    QSEP(NSTRU),QSET(NSTRU),QVAP(NSTRU)
     *                ,QVAT(NSTRU),XCUTAR,XSTRU(NSTRU)
     *                ,IDTG
      COMMON /STRU2/   DELTA0,DELTA1,QSEH(NSTRU),QSEPI(NSTRU)
     *                ,QVAH(NSTRU),QVAPI(NSTRU),XSE(NSTRU),XVA(NSTRU)
 
      DOUBLE PRECISION ERRER,VALUE
      INTEGER          IFLAG
 
      COMMON /VENLIN/  PTQ1,PTQ2,PTQ3,QMUST1,QMUST2,QMUST3
     *                ,IDTABL(100)
 
      EXTERNAL         SDENSI,SPTQ,SSE0,SVA0,SVA1
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'VENLNK: TAR',SNGL(TAR)
 
      NSTRUC = NSTRU
      IF ( DEBUG ) THEN
        ISH  = ISH0
      ELSE
        ISH  = 0
      ENDIF
      NEVNT = SHOWNO
C  SET RANDOM NUMBER GENERATOR STATUS
      SEEDC=ISEED(2,1)+1.D9*ISEED(3,1)
C  CALCULATE ENERGY IN LAB SYSTEM FOR ELASTICITY FOR VARIOUS PROJECTILES
      IF     ( ITYPE .EQ. 1 ) THEN
C  TREAT PHOTON PROJECTILES (FROM EGS)
        CALL RMMAR(RD,1,1)
        IF ( RD(1) .LE. 0.5 ) THEN
          ITYPE = 7
        ELSE
          ITYPE = 17
        ENDIF
        ELAB  = CURPAR(2)
        CURPAR(2) = ELAB / PAMA(ITYPE)
      ELSEIF ( ITYPE .LT. 100 ) THEN
C  TREAT ORDINARY PROJECTILES
        ELAB  = CURPAR(2) * PAMA(ITYPE)
      ELSE
C  TREAT NUCLEI PROJECTILES
        NPROT = MOD(ITYPE,100)
        NNEUT = ITYPE/100 - NPROT
        ELAB  = CURPAR(2) * ( PAMA(14)*NPROT + PAMA(13)*NNEUT )
      ENDIF
C  SET TARGET PARAMETERS
      MATARG = NINT(TAR)
      IDTARG = 1120
      AMTARG = PAMA(14)
      IF     ( TAR. EQ. 14.D0 ) THEN
        LTARG  = 1
        LATARG = 7
      ELSEIF ( TAR .EQ. 16.D0 ) THEN
        LTARG  = 2
        LATARG = 8
      ELSEIF ( TAR .EQ. 40.D0 ) THEN
        LTARG  = 3
        LATARG = 18
      ELSE
        WRITE(MONIOU,*)'VENLNK: UNDEFINED TARGET TAR=',SNGL(TAR)
      ENDIF
 
C  FOR THE CASE OF AN ARBITRARY TARGET (NOT AIR)
      IF ( LTARG .GT. 3 ) THEN
        MASSNR = MATARG
        IF ( MASSNR .GT. 1 ) THEN
          IF ( MASSNR .NE. MTAR99 ) THEN
            R0 = 1.19*MASSNR**(.3333333) -1.61*MASSNR**(-.3333333)
            CX = R0+FCTRMX*0.54
            RMTARG(4) = CX
            CALL UTQUAF(SDENSI,NDET,XDET99,QDET99,0.,.33*CX,.66*CX,CX)
            MTAR99 = MATARG
          ENDIF
        ELSE
          RMTARG(4) = 0.
        ENDIF
      ENDIF
 
C  SET PROJECTILE PARAMETERS
      IF ( ITYPE .LT. 100 ) THEN
        IDPROJ = IDTABL(ITYPE)
        IF     ( IDPROJ .EQ. 20  .OR.  IDPROJ .EQ. -20 ) THEN
C  TREAT NEUTRAL KAONS  (K(0)S AND K(0)L)
          CALL RMMAR(RD,1,1)
          IF ( RD(1) .LE. 0.5 ) THEN
            IDPROJ = 230
          ELSE
            IDPROJ = -230
          ENDIF
        ELSEIF ( IDPROJ .EQ. 2130 ) THEN
C  VENUS CANNOT TREAT LAMBDA, TAKE INSTEAD SIGMA(0))
          IDPROJ = 1230
        ELSEIF ( IDPROJ .EQ. -2130 ) THEN
C  VENUS CANNOT TREAT ANTI-LAMBDA, TAKE INSTEAD ANTI-SIGMA(0))
          IDPROJ = -1230
        ENDIF
C  ALL OTHER PARTICLE CODES UNCHANGED
        CALL IDMASS(IDPROJ,AMPROJ)
        LAPROJ = -1
        MAPROJ = 1
        PNLL   = CURPAR(2)*AMPROJ
      ELSE
C  PROJECTILE IS NUCLEUS
        IDPROJ = 1120
        CALL IDMASS(IDPROJ,AMPROJ)
        LAPROJ = MOD(ITYPE,100)
        MAPROJ = ITYPE/100
        PNLL   = CURPAR(2)*(PAMA(14)+PAMA(13))*0.5
      ENDIF
 
      IF ( ABS(IDPROJ) .LT. 1000 ) THEN
        IF ( ABS(IDPROJ) .EQ. 230  .OR.  ABS(IDPROJ) .EQ. 130 ) THEN
C  DIFFRACTIVE PROBABILITY FOR KAON PROJECTILES
          WPROJ = 0.24
        ELSE
C  DIFFRACTIVE PROBABILITY FOR PION PROJECTILES
          WPROJ = 0.20
        ENDIF
      ELSE
C  DIFFRACTIVE PROBABILITY FOR BARYON PROJECTILES
        WPROJ = 0.32
      ENDIF
C  DIFFRACTIVE PROBABILITY FOR TARGET (ALWAYS NUCLEONS)
      WTARG = 0.32
 
      ENGY = SQRT( 2.*SQRT(PNLL**2+AMPROJ**2)*AMTARG+AMTARG**2
     *                     +AMPROJ**2 )
      IF ( DEBUG ) WRITE(MDEBUG,*)'VENLNK: ELAB = ',PNLL,
     *                                   ' ENGY = ',ENGY
CDH   IF ( ENGY .LT. 12. ) THEN
      IF ( ENGY .LT. 9.5 ) THEN
        WRITE (IFMT,*)'VENLNK: ENGY, IDPROJ=',ENGY,IDPROJ
        CALL UTSTOP('VENLNK: INCIDENT ENERGY TOO SMALL       ')
      ENDIF
      ENGYI = ENGY
      PNLLI = PNLL
      IF ( PNLL .LT. 1.E2 * AMPROJ ) THEN
        TRM  = SQRT(PNLL**2+AMPROJ**2)
        ENGY = SQRT((TRM+AMTARG-PNLL)*(TRM+AMTARG+PNLL))
      ELSE
        TRM  = AMPROJ**2*0.5/PNLL+AMTARG
        ENGY = SQRT(TRM*(2.*PNLL+TRM))
      ENDIF
      D1 = ABS(PNLLI-PNLL)/PNLL
      D2 = ABS(ENGYI-ENGY)/ENGY
      IF ( D1 .GT. 1.E-3  .OR.  D2 .GT. 1.E-3 ) THEN
        IF ( ISH .GE. 0 ) THEN
          CALL UTMSG('VENLNK')
          WRITE(IFCH,*)'*****  PNLL,PNLLI:',PNLL,PNLLI
          WRITE(IFCH,*)'*****  ENGY,ENGYI:',ENGY,ENGYI
          CALL UTMSGF
        ENDIF
      ENDIF
      S = ENGY**2
      SROOTI = 1./ENGY
      PNLLX = UTPCM(ENGY,AMPROJ,AMTARG)
      YHAHA = LOG((SQRT(PNLL**2+S)+PNLL)/ENGY)
      YPJTL = LOG((SQRT(PNLL**2+AMPROJ**2)+PNLL)/AMPROJ)
      IF ( ISH .GE. 91 ) WRITE(IFCH,*)'VENLNK: YPJTL=',YPJTL
 
      ENGYLG = LOG(ENGY)
      QMUST = QMUST1+QMUST2*ENGYLG+QMUST3*ENGYLG**2
      PTQ = PTQ1+PTQ2*ENGYLG+PTQ3*ENGYLG**2
CDH   PHARD = 0.030+0.12*(LOG10(S)-LOG10(30.**2))
      PHARD = 0.030+0.12*(LOG10(S)-2.9542425)
      PHARD = MIN(1.,PHARD)
      PHARD = MAX(0.030,PHARD)
 
C  PROJECTILE
      XCUT  = CUTMSQ*SROOTI
      XCUT2 = XCUT**2
      IF ( ABS(IDPROJ) .GE. 1000 ) THEN
C  STRUCTURE FUNCTION INTEGRAL FOR BARYONS OF PROJECTILE
        IPIO = 0
        CALL UINTEG(VALUE,SSE0,0.D0,1.D0,0.D0,1.D-5,1,ERRER,IFLAG)
        IF ( IFLAG .GT. 3  .AND.  ISH .GT. 0 )
     *                      WRITE(IFCH,*)'VENLNK: SSE0:IFLAG=',IFLAG
        QSEPC = VALUE
        CALL UINTEG(VALUE,SVA0,0.D0,1.D0,0.D0,1.D-5,1,ERRER,IFLAG)
        IF ( IFLAG .GT. 3  .AND.  ISH .GT. 0 )
     *                      WRITE(IFCH,*)'VENLNK: SVA0:IFLAG=',IFLAG
        QVAPC = VALUE
      ELSE
C  STRUCTURE FUNCTION INTEGRAL FOR MESONS OF PROJECTILE
        IPIO = 1
        A0 = -5.0 + 6.6666667*XCUT2 - 0.53333333*XCUT2**2
        A1 =  5.0 - 1.875*XCUT2
        A2 = -3.3333333 + 0.26666667*XCUT2
        A3 =  1.25
        A4 = -0.2
        ROOT  = SQRT(XCUT2+1.)
        QSEPC = 0.9*( (1.-XCUT2*A1)*( LOG(1.+ROOT)-LOG(XCUT) )
     *                   - XCUT*A0 + ROOT*(A0+A1+A2+A3+A4) )
        CALL UINTEG(VALUE,SVA1,0.D0,1.D0,0.D0,1.D-5,1,ERRER,IFLAG)
        IF ( IFLAG .GT. 3  .AND.  ISH .GT. 0 )
     *                      WRITE(IFCH,*)'VENLNK: SVA1:IFLAG=',IFLAG
        QVAPC = VALUE
      ENDIF
      IDTG = IPIO
 
C  TARGET
      IF ( IDTG .EQ. 1 ) THEN
        IF ( ABS(IDTARG) .GE. 1000 ) THEN
C  STRUCTURE FUNCTION INTEGRAL FOR BARYONS OF TARGET
          IPIO = 0
          CALL UINTEG(VALUE,SSE0,0.D0,1.D0,0.D0,1.D-5,1,ERRER,IFLAG)
          IF ( IFLAG .GT. 3  .AND.  ISH .GT. 0 )
     *                        WRITE(IFCH,*)'VENLNK: SSE0:IFLAG=',IFLAG
          QSETC = VALUE
          CALL UINTEG(VALUE,SVA0,0.D0,1.D0,0.D0,1.D-5,1,ERRER,IFLAG)
          IF ( IFLAG .GT. 3  .AND.  ISH .GT. 0 )
     *                        WRITE(IFCH,*)'VENLNK: SVA0:IFLAG=',IFLAG
          QVATC = VALUE
        ELSE
          IPIO=1
          QVATC = QVAPC
          QSETC = QSEPC
        ENDIF
      ELSE
        IF ( ABS(IDTARG) .GE. 1000 ) THEN
          IPIO = 0
          QVATC = QVAPC
          QSETC = QSEPC
        ELSE
C  STRUCTURE FUNCTION INTEGRAL FOR BARYONS OF TARGET
          IPIO=1
          A0 = -5.0 + 6.6666667*XCUT2 - 0.53333333*XCUT2**2
          A1 =  5.0 - 1.875*XCUT2
          A2 = -3.3333333 + 0.26666667*XCUT2
          A3 =  1.25
          A4 = -0.2
          ROOT  = SQRT(XCUT2+1.)
          QSETC = 0.9*( (1.-XCUT2*A1)*( LOG(1.+ROOT)-LOG(XCUT) )
     *                     - XCUT*A0 + ROOT*(A0+A1+A2+A3+A4) )
          CALL UINTEG(VALUE,SVA1,0.D0,1.D0,0.D0,1.D-5,1,ERRER,IFLAG)
          IF ( IFLAG .GT. 3  .AND.  ISH .GT. 0 )
     *                        WRITE(IFCH,*)'VENLNK: SVA1:IFLAG=',IFLAG
          QVATC = VALUE
        ENDIF
      ENDIF
      IF ( ISH .EQ. 16 .OR. DEBUG ) THEN
        WRITE(IFCH,301) QVAPC, QSEPC, QVATC, QSETC
 301    FORMAT(' VENLNK: QVAPC, QSEPC, QVATC, QSETC=',4(F10.7,2X))
      ENDIF
 
      IF ( PROSEA .GE. 0. ) THEN
        QVAPC = 1.0
        QVATC = 1.0
        QSEPC = PROSEA
        QSETC = PROSEA
      ENDIF
 
      XCUT = CUTMSS*SROOTI
      XCUTAR = XCUT
      B = MIN( 0.05, XCUT*500. )
      A = MIN( 0.2*B, XCUT*100. )
      PNLLLG = LOG(PNLL)
      DELTA0 = EXP(-2.791922 - 0.2091742 * PNLLLG)
      DELTA1 = EXP(-3.885293 - 0.2029558 * PNLLLG)
      CALL UTQSEA(A,B,1.)
      IF ( XCUT .LT. 0.04  ) THEN
        NEND=1.+REAL(NSTRUC)*2./PI*ACOS(1.-2./PI*ACOS(1.-25.*XCUT))
      ELSE
        NEND = NSTRUC
      ENDIF
 
      IF ( ABS(IDPROJ) .GE. 1000 ) THEN
        IPIO = 0
        DO 203 N = 1,NSTRUC
          QSEP(N) = QSEH(N)
 203    CONTINUE
        DO 2031 N = NEND,NSTRUC
          QVAP(N) = QVAH(N) - DELTA0
 2031   CONTINUE
      ELSE
        IPIO = 1
        DO 204 N = 1,NSTRUC
          QSEP(N) = QSEPI(N)
 204    CONTINUE
        DO 2041 N = NEND,NSTRUC
          QVAP(N) = QVAPI(N) - DELTA1
 2041   CONTINUE
      ENDIF
      CALL UTQVAL(QVAP,NEND)
 
      IF ( IDTG .EQ. 0 ) THEN
        IF ( ABS(IDTARG) .GE. 1000 ) THEN
          IPIO = 0
          DO 205 N=1,NSTRUC
            QSET(N) = QSEP(N)
            QVAT(N) = QVAP(N)
 205      CONTINUE
        ELSE
          IPIO = 1
          DO 209 N = 1,NSTRUC
            QSET(N) = QSEPI(N)
 209      CONTINUE
          DO 2091 N = NEND,NSTRUC
            QVAT(N) = QVAPI(N) - DELTA1
 2091     CONTINUE
          CALL UTQVAL(QVAT,NEND)
        ENDIF
 
      ELSE
        IF ( ABS(IDTARG) .GE. 1000 ) THEN
          IPIO = 0
          DO 210 N = 1,NSTRUC
            QSET(N) = QSEH(N)
 210      CONTINUE
          DO 2101 N = NEND,NSTRUC
            QVAT(N) = QVAH(N) - DELTA0
 2101     CONTINUE
          CALL UTQVAL(QVAT,NEND)
 
        ELSE
          IPIO = 1
          DO 216 N=1,NSTRUC
            QSET(N) = QSEP(N)
            QVAT(N) = QVAP(N)
 216      CONTINUE
        ENDIF
      ENDIF
 
      IF ( ISH .EQ. 21 ) THEN
        CALL UTHSEA
        CALL UTSTOP(' VENLNK:                                ')
      ENDIF
 
      QPTHMX = 0.5/PTH**2-PTH**2/(2.*(PTH**2+PTMX**2)**2)
      IF     ( IOPTQ .EQ. 2 ) THEN
        QPTQMX = 1. - EXP(-PI*PTMX**2/(4.*PTQ**2) )
      ELSEIF ( IOPTQ .EQ. 3 ) THEN
        QPTQMX = 1. - PTQ**2/(PTQ**2+PTMX**2)
      ELSE
        CX = PTMX
        CALL UTQUAF(SPTQ,NPTQ,XPTQ,QPTQ,0.,.33*CX,.66*CX,CX)
      ENDIF
 
      SIGPPI = -1.0
C  CALCULATE ENERGY DEPENDENT CROSS SECTION FOR BARYONS
      CALL RACPRO('GRI',QMUST,NPRBMS,PRBMS)
      IF     ( ABS(IDPROJ) .LE. 120  .OR.  ABS(IDPROJ) .EQ. 220 ) THEN
C  CROSS SECTION FOR PIONS (OR ETA FOR PHOTONS FROM EGS)
        SIGPPI = SIGPPI * 0.6667
      ELSEIF ( ABS(IDPROJ) .EQ. 130  .OR.  ABS(IDPROJ) .EQ. 230 ) THEN
C  CROSS SECTION FOR KAONS
        SIGPPI = SIGPPI * 0.5541
      ENDIF
 
      MASSNR = MAPROJ
      RMPROJ = 0.
      IF ( MASSNR .GT. 1 ) THEN
        R0 = 1.19*MASSNR**(.3333333) -1.61*MASSNR**(-.3333333)
        CX = R0+FCTRMX*0.54
        RMPROJ = CX
        CALL UTQUAF(SDENSI,NDEP,XDEP,QDEP,0.,.33*CX,.66*CX,CX)
      ENDIF
 
      IF ( IDPM .EQ. 1 ) THEN
        QSEPC = 0.
        QSETC = 0.
      ENDIF
      BMAX = RMPROJ+RMTARG(LTARG)
 
      IF ( ISH .GE. 91 ) WRITE(IFCH,*)'VENLNK: AVENUS IS NOW CALLED'
      CALL AVENUS
 
C  NOW BRING PARTICLES TO CORSIKA STACK
      CALL VSTORE
 
      IF ( ISH .GE. 91 ) WRITE(IFCH,*)'VENLNK: (EXIT)'
      RETURN
      END
