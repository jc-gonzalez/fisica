      SUBROUTINE VSTORE
 
C-----------------------------------------------------------------------
C  V(ENUS PARTICLES) STORE (INTO CORSIKA STACK)
C
C  STORES VENUS OUTPUT PARTICLES INTO CORSIKA STACK
C  THIS SUBROUTINE IS CALLED FROM VENLNK
C
C  DESIGN   : D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,DPMFLG.
      COMMON /DPMFLG/  NFLAIN,NFLDIF,NFLPI0,NFLCHE,NFLPIF,NFRAGM
      INTEGER          NFLAIN,NFLDIF,NFLPI0,NFLCHE,NFLPIF,NFRAGM
*KEEP,ELADPM.
      COMMON /ELADPM/  ELMEAN,ELMEAA,IELDPM,IELDPA
      DOUBLE PRECISION ELMEAN(37),ELMEAA(37)
      INTEGER          IELDPM(37,13),IELDPA(37,13)
*KEEP,ELASTY.
      COMMON /ELASTY/  ELAST,IELIS,IELHM,IELNU,IELPI
      DOUBLE PRECISION ELAST
      INTEGER          IELIS(20),IELHM(20),IELNU(20),IELPI(20)
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
*KEEP,ISTA.
      COMMON /ISTA/    IFINET,IFINNU,IFINKA,IFINPI,IFINHY
      INTEGER          IFINET,IFINNU,IFINKA,IFINPI,IFINHY
*KEEP,MULT.
      COMMON /MULT/    EKINL,MSMM,MULTMA,MULTOT
      DOUBLE PRECISION EKINL
      INTEGER          MSMM,MULTMA(37,13),MULTOT(37,13)
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
*KEND.
 
      PARAMETER (KOLLMX=2500)
      PARAMETER (MXPTL=70000)
      PARAMETER (MXSTR=3000)
      PARAMETER (NDEP=129)
      PARAMETER (NDET=129)
      COMMON /ACCUM/   AMSAC,ILAMAS,IMSG,INOIAC,IPAGE,JERR,NAEVT,NREVT
     *                ,NRPTL,NRSTR,NTEVT
      COMMON /CEVT/    BIMEVT,COLEVT,EGYEVT,PHIEVT,PMXEVT
     *                ,KOLEVT,NEVT,NPJEVT,NTGEVT
      COMMON /COL/     BIMP,BMAX,COORD(4,KOLLMX),DISTCE(KOLLMX)
     *                ,QDEP(NDEP),QDET14(NDET),QDET16(NDET),QDET40(NDET)
     *                ,QDET99(NDET),RMPROJ,RMTARG(4),XDEP(NDEP)
     *                ,XDET14(NDET),XDET16(NDET),XDET40(NDET)
     *                ,XDET99(NDET)
     *                ,KOLL,LTARG,NORD(KOLLMX),NPROJ,NRPROJ(KOLLMX)
     *                ,NRTARG(KOLLMX),NTARG
      COMMON /CPTL/    PPTL(5,MXPTL),TIVPTL(2,MXPTL),XORPTL(4,MXPTL)
     *                ,IBPTL(4,MXPTL),ICLPTL(MXPTL),IDPTL(MXPTL)
     *                ,IFRPTL(2,MXPTL),IORPTL(MXPTL),ISTPTL(MXPTL)
     *                ,JORPTL(MXPTL),NPTL,NQJPTL(MXPTL)
      COMMON /CSTR/    PSTR(5,MXSTR),ROTSTR(3,MXSTR),XORSTR(4,MXSTR)
     *                ,ICSTR(4,MXSTR),IORSTR(MXSTR),IRLSTR(MXSTR),NSTR
      COMMON /FILES/   IFCH,IFDT,IFHI,IFMT,IFOP
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
 
      DOUBLE PRECISION EA,ELASTI,EMAX,GAMMAX,COSTET,PHIV,PL2,PT2,PTM
      DOUBLE PRECISION PFRX(60),PFRY(60)
      INTEGER          ITYP(60),NRPTLA(MXPTL)
C-----------------------------------------------------------------------
 
      IF ( DEBUG ) WRITE(MDEBUG,*) 'VSTORE:'
 
C  NUMBER OF SPECTATORS OF REMAINING NUCLEUS IS NREST
      NREST = ITYPE/100 - NPJEVT
      IREST = ITYPE
      NNEW  = 0
      INEW  = 0
      ETOT  = 0.
 
      LEVT  = 1
      LPTL  = 3
      NPTLS = 0
      DO  1 I=1,NPTL
        NRPTLA(I) = -999
        IF ( ISTPTL(I) .GT. ISTMAX ) GOTO 1
        NPTLS = NPTLS+1
        NRPTLA(I) = NPTLS
 1    CONTINUE
 
C  EVENT VARIABLES:
C     LEVT................... RECORD LABEL (LEVT=1)
C     NREVT.................. EVENT NUMBER
C     NPTLS ................. NUMBER OF (STORED!) PARTICLES PER EVENT
C     BIMEVT ................ IMPACT PARAMETER
C     KOLEVT,COLEVT ......... REAL/EFFECTIVE # OF COLLISIONS
C     PMXEVT ................ REFERENCE MOMENTUM
C     EGYEVT ................ PP CM ENERGY (HAD) OR STRING ENERGY (STR)
C     NPJEVT,NTGEVT ......... # OF PROJ/TARG PARTICIPANTS
 
      GNU = KOLEVT
      GNU = COLEVT
C  SET COORDINATES, WHICH ARE IDENTICAL FOR ALL SECONDARY PARTICLES
      DO  6 I=5,8
        SECPAR(I) = CURPAR(I)
 6    CONTINUE
CC    GAMMAX = 0.D0
      EMAX   = 0.D0
 
C  PARTICLE LOOP
      DO  5 I=1,NPTL
        IF ( NRPTLA(I) .LE. 0 ) GOTO 5
 
C  PARTICLE VARIABLES:
C     LPTL ......... RECORD LABEL (LPTL=3)
C     NREVT ........ EVENT NUMBER
C     NRPTL ........ PARTICLE NUMBER
C     I ............ ORIGINAL PTL NUMBER
C     IDPTL ........ PARTICLE ID
C     PPTL ......... 5-MOMENTUM (PX,PY,PZ,EN,MASS) IN LAB
C     IOPTL ........ ORIGIN (-999:PARENT NOT STORED, -1,0:NO PARENT)
C     JOPTL ........ ORIGIN (SECOND PARENT)
C     ISTPTL ....... STABLE (=0) OR NOT (=1)
C     XORPTL ....... SPACE-TIME POINT (X,Y,Z,T) ON PTL TRACK (PP-CM)
C     TIVPTL ....... TIME INTERVAL OF EXISTENCE
C     NQJPTL ....... QUARK NUMBERS OF JETS
 
C  ELIMINATE TARGET SPECTATORS
        IF ( PPTL(3,I) .EQ. 0. ) GOTO 5
 
C  ELIMINATE BACKWARD GOING PARTICLES
        IF ( PPTL(3,I) .LT. 0. ) GOTO 5
C  CONVERT PARTICLE CODE  VEN(US) ---> C(O)RS(IKA)
C  MOST FREQUENT PARTICLES COME FIRST
        KODVEN = IDPTL(I)
C  MESONS
        IF     ( KODVEN .EQ.   110 ) THEN
          KODCRS = 7
        ELSEIF ( KODVEN .EQ.   120 ) THEN
          KODCRS = 8
        ELSEIF ( KODVEN .EQ.  -120 ) THEN
          KODCRS = 9
        ELSEIF ( KODVEN .EQ.   220 ) THEN
          KODCRS = 17
C  NUCLEONS
        ELSEIF ( KODVEN .EQ.  1220 ) THEN
          KODCRS = 13
        ELSEIF ( KODVEN .EQ.  1120 ) THEN
          KODCRS = 14
        ELSEIF ( KODVEN .EQ. -1120 ) THEN
          KODCRS = 15
        ELSEIF ( KODVEN .EQ. -1220 ) THEN
          KODCRS = 25
C  STRANGE MESONS
        ELSEIF ( KODVEN .EQ.   -20 ) THEN
          KODCRS = 10
        ELSEIF ( KODVEN .EQ.   130 ) THEN
          KODCRS = 11
        ELSEIF ( KODVEN .EQ.  -130 ) THEN
          KODCRS = 12
        ELSEIF ( KODVEN .EQ.    20 ) THEN
          KODCRS = 16
C  STRANGE BARYONS
        ELSEIF ( KODVEN .EQ.  2130 ) THEN
          KODCRS = 18
        ELSEIF ( KODVEN .EQ.  1130 ) THEN
          KODCRS = 19
        ELSEIF ( KODVEN .EQ.  1230 ) THEN
          KODCRS = 20
        ELSEIF ( KODVEN .EQ.  2230 ) THEN
          KODCRS = 21
        ELSEIF ( KODVEN .EQ.  1330 ) THEN
          KODCRS = 22
        ELSEIF ( KODVEN .EQ.  2330 ) THEN
          KODCRS = 23
        ELSEIF ( KODVEN .EQ.  3331 ) THEN
          KODCRS = 24
        ELSEIF ( KODVEN .EQ. -2130 ) THEN
          KODCRS = 26
        ELSEIF ( KODVEN .EQ. -1130 ) THEN
          KODCRS = 27
        ELSEIF ( KODVEN .EQ. -1230 ) THEN
          KODCRS = 28
        ELSEIF ( KODVEN .EQ. -2230 ) THEN
          KODCRS = 29
        ELSEIF ( KODVEN .EQ. -1330 ) THEN
          KODCRS = 30
        ELSEIF ( KODVEN .EQ. -2330 ) THEN
          KODCRS = 31
        ELSEIF ( KODVEN .EQ. -3331 ) THEN
          KODCRS = 32
C  LEPTONS
        ELSEIF ( KODVEN .EQ.    10 ) THEN
          KODCRS = 1
        ELSEIF ( KODVEN .EQ.   -12 ) THEN
          KODCRS = 2
        ELSEIF ( KODVEN .EQ.    12 ) THEN
          KODCRS = 3
        ELSEIF ( KODVEN .EQ.   -14 ) THEN
          KODCRS = 5
        ELSEIF ( KODVEN .EQ.    14 ) THEN
          KODCRS = 6
C  NEUTRINOS ARE SKIPPED
        ELSEIF ( KODVEN .EQ.    11 ) THEN
          GOTO 5
        ELSEIF ( KODVEN .EQ.   -11 ) THEN
          GOTO 5
        ELSEIF ( KODVEN .EQ.    13 ) THEN
          GOTO 5
        ELSEIF ( KODVEN .EQ.   -13 ) THEN
          GOTO 5
        ELSE
          WRITE(MONIOU,*)'VSTORE: UNKNOWN PARTICLE CODE IDPTL=',IDPTL(I)
          GOTO 5
        ENDIF
        SECPAR(1) = KODCRS
 
        IF ( KODCRS .NE. 1  .AND. KODCRS .LE. 65 ) THEN
C  ORDINARY SECONDARY PARTICLES
          SECPAR(2) = PPTL(4,I)/PAMA(KODCRS)
C  LOOK FOR SPECTATOR NUCLEONS
          IF ( KODCRS .EQ. 13  .OR.  KODCRS .EQ. 14 ) THEN
C  ELIMINATE TARGET SPECTATORS
            IF ( SECPAR(2) .LE. 1.002D0 ) GOTO 5
C  TREAT PROJECTILE SPECTATORS
            IF ( SECPAR(2) .GT. 0.999D0*GAMMA  .AND.
     *           SECPAR(2) .LT. 1.001D0*GAMMA  .AND.
     *           PPTL(1,I).EQ.0.  .AND.  PPTL(2,I).EQ.0. ) THEN
              IF ( NFRAGM .NE. 0 ) THEN
C  COMPOSE PROJECTILE SPECTATORS TO REMAINING NUCLEUS
                NREST = NREST - 1
                NNEW  = NNEW + 1
                IF     ( KODCRS .EQ. 14 ) THEN
                  INEW  = INEW + 101
                  IREST = IREST - 101
                ELSEIF ( KODCRS .EQ. 13 ) THEN
                  INEW  = INEW + 100
                  IREST = IREST - 100
                ENDIF
                GOTO 5
              ENDIF
C  DISREGARD PROJECTILE SPECTATORS FOR ELASTICITY
              GOTO 7
            ENDIF
          ENDIF
 
CC        IF ( SECPAR(2) .GT. GAMMAX ) THEN
CC          GAMMAX = SECPAR(2)
C  CALCULATE ELASTICITY FROM ENERGY OF FASTEST PARTICLE (LEADER)
CC          ELASTI = GAMMAX * PAMA(KODCRS) / ELAB
CC        ENDIF
          IF ( SECPAR(2)*PAMA(KODCRS) .GT. EMAX ) THEN
            EMAX = SECPAR(2)*PAMA(KODCRS)
C  CALCULATE ELASTICITY FROM MOST ENERGETIC PARTICLE (LEADER)
            ELASTI = EMAX / ELAB
          ENDIF
        ELSE
C  GAMMAS AND NEUTRINOS
          SECPAR(2) = PPTL(4,I)
        ENDIF
 
C  COUNTER FOR ENERGY-MULTIPLICITY MATRIX
        MSMM = MSMM + 1
 
C  DETERMINE ANGLES FROM LONGITUDINAL AND TRANSVERSAL MOMENTA
 7      CONTINUE
        PT2 = DBLE(PPTL(1,I))**2 + DBLE(PPTL(2,I))**2
        PL2 = DBLE(PPTL(3,I))**2
 
        IF ( PL2+PT2 .LE. 0.D0 ) THEN
          COSTET = 0.D0
        ELSE
          COSTET = PPTL(3,I) / SQRT(PL2+PT2)
        ENDIF
        COSTET = MAX( MIN(COSTET, 1.D0), -1.D0 )
        IF ( PPTL(1,I)  .NE. 0.  .OR.  PPTL(2,I) .NE. 0. ) THEN
          PHIV = ATAN2( DBLE(PPTL(1,I)), DBLE(PPTL(2,I)) )
        ELSE
          PHIV = 0.D0
        ENDIF
 
 
        ETOT = ETOT + PPTL(4,I)
        CALL ADDANG( COSTHE,PHI, COSTET,PHIV, SECPAR(3),SECPAR(4) )
        IF ( SECPAR(3) .GE. C(29) ) THEN
          CALL TSTACK
        ENDIF
 
C  COUNTERS FOR FIRST INTERACTION
        IF ( FIRSTI ) THEN
          IF     ( SECPAR(1) .EQ.  7.D0  .OR.  SECPAR(1) .EQ.  8.D0
     *       .OR.  SECPAR(1) .EQ.  9.D0                           ) THEN
            IFINPI = IFINPI + 1
          ELSEIF ( SECPAR(1) .EQ. 13.D0  .OR.  SECPAR(1) .EQ. 14.D0
     *       .OR.  SECPAR(1) .EQ. 15.D0 .OR. SECPAR(1) .EQ. 25.D0 ) THEN
            IFINNU = IFINNU + 1
          ELSEIF ( SECPAR(1) .EQ. 10.D0  .OR.  SECPAR(1) .EQ. 11.D0
     *       .OR.  SECPAR(1) .EQ. 12.D0 .OR. SECPAR(1) .EQ. 16.D0 ) THEN
            IFINKA = IFINKA + 1
          ELSEIF ( SECPAR(1) .EQ. 17.D0 ) THEN
            IFINET = IFINET + 1
          ELSEIF ((SECPAR(1) .GE. 18.D0 .AND. SECPAR(1) .LE. 24.D0)
     *       .OR. (SECPAR(1) .GE. 26.D0 .AND. SECPAR(1) .LE. 32.D0))THEN
            IFINHY = IFINHY + 1
          ENDIF
        ENDIF
 
 5    CONTINUE
 
      IF (DEBUG) WRITE(MDEBUG,*) 'VSTORE: NTGEVT,ETOT =',NTGEVT,ETOT
 
      IF ( NFRAGM .NE. 0  .AND.  INEW .GT. 0 ) THEN
C  TREAT REMAINING NUCLEUS
        IF ( DEBUG ) WRITE(MDEBUG,150) INEW,(CURPAR(I),I=2,8)
 150    FORMAT(' VSTORE: REMNNT=',1P,I10,7E10.3)
        SECPAR(2) = CURPAR(2)
        SECPAR(3) = CURPAR(3)
        SECPAR(4) = CURPAR(4)
 
        IF     ( INEW .EQ. 100 ) THEN
C  REMAINING NUCLEUS IS SINGLE NEUTRON
          SECPAR(1) = 13.D0
          CALL TSTACK
          ETOT = ETOT + SECPAR(2) * PAMA(13)
          GOTO 140
 
        ELSEIF ( INEW .EQ. 101 ) THEN
C  REMAINING NUCLEUS IS SINGLE PROTON
          SECPAR(1) = 14.D0
          CALL TSTACK
          ETOT = ETOT + SECPAR(2) * PAMA(14)
          GOTO 140
 
        ELSEIF ( NFRAGM .GE. 2 ) THEN
C  REMAINING NUCLEUS IS EVAPORATING NUCLEONS AND ALPHA PARTICLES
          NZNEW = MOD(INEW,100)
          NNNEW = INEW/100 - NZNEW
          JFIN  = 0
          CALL VAPOR(MAPROJ,INEW,JFIN,ITYP,PFRX,PFRY)
          IF ( JFIN .EQ. 0 ) GOTO 139
C  LOOP TO TREAT THE REMANENTS OF THE DESINTEGRATED FRAGMENT
          KNEW = 0
          DO  135  J=1,JFIN
            EA = GAMMA * PAMA(ITYP(J))
            IF(DEBUG)WRITE(MDEBUG,*)'VSTORE: J,ITYP,EA=',J,ITYP(J),EA
C  MOMENTA SQUARED
            PTM = EA**2 - PAMA(ITYP(J))**2
            PT2 = PFRX(J)**2 + PFRY(J)**2
            IF ( PT2 .GE. PTM ) THEN
              IF(DEBUG)WRITE(MDEBUG,*)'VSTORE: PT REJECT PARTICLE',J
              GOTO 135
            ENDIF
            IF ( PTM .GT. 0.D0 ) THEN
              COSTET = SQRT( 1.D0 - PT2/PTM )
            ELSE
              COSTET = 1.D0
            ENDIF
            IF ( PFRX(J) .NE. 0.D0  .OR.  PFRY(J) .NE. 0.D0 ) THEN
              PHIV = ATAN2( PFRY(J), PFRX(J) )
            ELSE
              PHIV = 0.D0
            ENDIF
            CALL ADDANG( COSTHE,PHI, COSTET,PHIV, SECPAR(3),SECPAR(4) )
            IF ( SECPAR(3) .GE. C(29) ) THEN
              IF ( J .LT. JFIN ) THEN
                SECPAR(1) = ITYP(J)
                CALL TSTACK
              ELSE
                KNEW  = ITYP(JFIN)
              ENDIF
            ELSE
              IF(DEBUG)WRITE(MDEBUG,*)'VSTORE: ANGLE REJECT PARTICLE',J
            ENDIF
 135      CONTINUE
 
        ELSEIF ( NFRAGM .EQ. 1 ) THEN
C  REMAINING NUCLEUS IS ONE FRAGMENT
          NZNEW = MOD(INEW,100)
          NNNEW = INEW/100 - NZNEW
          KNEW  = INEW
        ENDIF
 
        IF     ( KNEW/100 .EQ. 5 ) THEN
C  REMAINING NUCLEUS: MASS 5 CANNOT BE TREATED IN BOX2
          IF ( MOD(KNEW,100) .GE. 3 ) THEN
C  MASS 5: SPLIT OFF ONE PROTON
            SECPAR(1) = 14.D0
            CALL TSTACK
            KNEW = KNEW - 101
          ELSE
C  MASS 5: SPLIT OFF ONE NEUTRON
            SECPAR(1) = 13.D0
            CALL TSTACK
            KNEW = KNEW - 100
          ENDIF
        ELSEIF ( KNEW/100 .EQ. 8 ) THEN
C  REMAINING NUCLEUS: MASS 8 CANNOT BE TREATED IN BOX2
          IF     ( MOD(KNEW,100) .GE. 5 ) THEN
C  MASS 8: SPLIT OFF ONE PROTON
            SECPAR(1) = 14.D0
            CALL TSTACK
            KNEW = KNEW - 101
          ELSEIF ( MOD(KNEW,100) .LE. 3 ) THEN
C  MASS 8: SPLIT OFF ONE NEUTRON
            SECPAR(1) = 13.D0
            CALL TSTACK
            KNEW = KNEW - 100
          ELSE
C  MASS 8: SPLIT OFF ONE ALPHA PARTICLE
            SECPAR(1) = 402.D0
            CALL TSTACK
            KNEW = KNEW - 402
          ENDIF
        ENDIF
 
        SECPAR(1) = KNEW
        CALL TSTACK
      ENDIF
 
 139  ETOT = ETOT + SECPAR(2)*(PAMA(13)*NNNEW + PAMA(14)*NZNEW)
 140  CONTINUE
      IF ( DEBUG ) WRITE(MDEBUG,*)'VSTORE: ELASTI,ETOT,ELAB=',
     *                               SNGL(ELASTI),ETOT,ELAB
 
C  FILL ELASTICITY IN MATRICES
      MEL = MIN ( 1.D0+10.D0*      MAX( 0.D0, ELASTI ) , 11.D0 )
      MEN = MIN ( 4.D0+ 3.D0*LOG10(MAX( .1D0, EKINL  )), 37.D0 )
      IELDPM(MEN,MEL) = IELDPM(MEN,MEL) + 1
      IELDPA(MEN,MEL) = IELDPA(MEN,MEL) + 1
      IF ( ELASTI .LT. 1.D0 ) THEN
        ELMEAN(MEN) = ELMEAN(MEN) + ELASTI
        ELMEAA(MEN) = ELMEAA(MEN) + ELASTI
      ENDIF
 
      IF ( FIRSTI ) THEN
        ELAST  = ELASTI
        FIRSTI = .FALSE.
      ENDIF
 
      RETURN
      END
