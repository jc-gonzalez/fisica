      SUBROUTINE START
 
C-----------------------------------------------------------------------
C  START
C
C  PERFORMS INITIALISATIONS AND CHECKS AT THE BEGINNING OF RUN.
C  CALLS DATAC TO READ IN DATA CARDS.
C  CHECKS AND INITIALIZES SELECTED HADRONIC INTERACTION MODEL.
C  THIS SUBROUTINE IS CALLED FROM MAIN
C
C  REDESIGN: J. KNAPP   IK1  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,AIR.
      COMMON /AIR/     COMPOS,PROBTA,AVERAW,AVOGAD
      DOUBLE PRECISION COMPOS(3),PROBTA(3),AVERAW,AVOGAD
*KEEP,ANNI.
      COMMON /ANNI/    CAN,CANN
      DOUBLE PRECISION CAN(50),CANN(50)
*KEEP,ATMOS.
      COMMON /ATMOS/   AATM,BATM,CATM,DATM
      DOUBLE PRECISION AATM(5),BATM(5),CATM(5),DATM(5)
*KEEP,ATMOS2.
      COMMON /ATMOS2/  HLAY,THICKL
      DOUBLE PRECISION HLAY(5),THICKL(5)
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
*KEEP,EDECAY.
      COMMON /EDECAY/  CETA
      DOUBLE PRECISION CETA(5)
*KEEP,ELABCT.
      COMMON /ELABCT/  ELCUT
      DOUBLE PRECISION ELCUT(4)
*KEEP,ETHMAP.
      COMMON /ETHMAP/  ECTMAP,ELEFT
      DOUBLE PRECISION ECTMAP,ELEFT
*KEEP,KAONS.
      COMMON /KAONS/   CKA
      DOUBLE PRECISION CKA(80)
*KEEP,MAGNET.
      COMMON /MAGNET/  BX,BZ,BVAL,BNORMC,BNORM,COSB,SINB,BLIMIT
      DOUBLE PRECISION BX,BZ,BVAL,BNORMC
      REAL             BNORM,COSB,SINB,BLIMIT
*KEEP,MUMULT.
      COMMON /MUMULT/  CHC,OMC,FMOLI
      DOUBLE PRECISION CHC,OMC
      LOGICAL          FMOLI
*KEEP,MUPART.
      COMMON /MUPART/  AMUPAR,BCUT,CMUON,FMUBRM,FMUORG
      DOUBLE PRECISION AMUPAR(14),BCUT,CMUON(11)
      LOGICAL          FMUBRM,FMUORG
*KEEP,NCSNCS.
      COMMON /NCSNCS/  SIGN30,SIGN45,SIGN60,SIGO30,SIGO45,SIGO60,
     *                 SIGA30,SIGA45,SIGA60,PNOA30,PNOA45,PNOA60,
     *                 SIG30A,SIG45A,SIG60A
      DOUBLE PRECISION SIGN30(56),SIGN45(56),SIGN60(56),
     *                 SIGO30(56),SIGO45(56),SIGO60(56),
     *                 SIGA30(56),SIGA45(56),SIGA60(56),
     *                 PNOA30(1540,3),PNOA45(1540,3),PNOA60(1540,3),
     *                 SIG30A(56),SIG45A(56),SIG60A(56)
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
*KEEP,RANGE.
      COMMON /RANGE/   CC
      DOUBLE PRECISION CC(20)
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
*KEEP,STACKF.
      COMMON /STACKF/  STACK,STACKP,EXST,NSHIFT,NOUREC,ICOUNT,NTO,NFROM
      INTEGER          MAXSTK
      PARAMETER        (MAXSTK = 12*340*2)
      DOUBLE PRECISION STACK(MAXSTK)
      INTEGER          STACKP,EXST,NSHIFT,NOUREC,ICOUNT,NTO,NFROM
*KEEP,STRBAR.
      COMMON /STRBAR/  CSTRBA
      DOUBLE PRECISION CSTRBA(11)
*KEEP,VERS.
      COMMON /VERS/    VERNUM,MVDATE,VERDAT
      DOUBLE PRECISION VERNUM
      INTEGER          MVDATE
      CHARACTER*18     VERDAT
*KEEP,VENUS.
      COMMON /VENUS/   ISH0,IVERVN,MTAR99,FVENUS,FVENSG
      INTEGER          ISH0,IVERVN,MTAR99
      LOGICAL          FVENUS,FVENSG
*KEEP,CEREN3.
      COMMON /CEREN3/  CERCNT,DATAB2,LHCER
      INTEGER          MAXBF2
      PARAMETER        (MAXBF2 = 39 * 7)
      DOUBLE PRECISION CERCNT
      REAL             DATAB2(MAXBF2)
      INTEGER          LHCER
*KEND.
 
      DOUBLE PRECISION COAN,SE,TEMP1,TEMP2,TEMP3,THICK,TTIME,ZE,ZS,ZX
      INTEGER          I,IA,J,L,N
      EXTERNAL         THICK
      CHARACTER*1      MARK
C-----------------------------------------------------------------------
 
C  SAY HELLO
      WRITE(MONIOU,112)
  112 FORMAT(/' ',120('A')//
     *'   OOO      OOO     OOOO       OOOO    OO   O      O      O   '/
     *'  O   O    O   O    O    O    O    O   OO   O    O       O O  '/
     *' O        O     O   O     O   O        OO   O  O        O   O '/
     *' O        O     O   O    O     OOOO    OO   OO         O     O'/
     *' O        O     O   OOOO           O   OO   O  O       OOOOOOO'/
     *'  O   O    O   O    O   O     O    O   OO   O    O     O     O'/
     *'   OOO      OOO     O     O    OOOO    OO   O      O   O     O'//
     *' COSMIC RAY SIMULATION FOR KASCADE'///
     *' A PROGRAM TO SIMULATE EXTENSIVE AIR SHOWERS IN ATMOSPHERE'//
     *' BASED ON A PROGRAM OF P.K.F. GRIEDER, UNIVERSITY BERN,',
     *' SWITZERLAND'/
     *' HDPM MODEL ACCORDING TO J.N. CAPDEVIELLE, COLLEGE DE FRANCE,',
     *' PARIS, FRANCE'/
     *' VENUS MODEL ACCORDING TO K. WERNER, UNIVERSITY NANTES, FRANCE'/
     *' GHEISHA ROUTINES ACCORDING TO H. FESEFELDT, RWTH. AACHEN,',
     *' GERMANY'/
     *' EGS4 AND NKG FORMULAS FOR SIMULATION OF EL.MAG. PARTICLES'//)
 
      MARK = '1'
 
      WRITE(MONIOU,912) VERNUM,MARK,VERDAT
  912 FORMAT(' INSTITUT FUER KERNPHYSIK '/
     *       ' FORSCHUNGSZENTRUM UND UNIVERSITAET KARLSRUHE'/
     *       ' POSTFACH 3640'/
     *       ' D-76021 KARLSRUHE'/
     *       ' GERMANY'//
     *       ' IN CASE OF PROBLEMS CONTACT:'/
     *       '           DIETER HECK             JOHANNES KNAPP'/
     *       ' E-MAIL:   HECK@IK3.FZK.DE         KNAPP@IK1.FZK.DE'/
     *       ' FAX:      (49) 7247-82-4075       (49) 7247-82-3548'/
     *       ' TEL:      (49) 7247-82-3777       (49) 7247-82-3549'//
     *       ' NUMBER OF VERSION : ',F6.3,A1/
     *       ' DATE   OF VERSION : ',A18 /)
 
      WRITE(MONIOU,141)
  141 FORMAT(//' CERENKOV RADIATION IS GENERATED'/
     *         ' ==============================='//)
 
C  INITIALIZE FIELD WITH PARTICLE MASSES
      CALL PAMAF
 
 
C  READ RUN STEERING DATA CARDS
      CALL DATAC
 
C  CLEARS BUFFERS FOR HEADER AND FILLS IN PERMANENT INFORMATION
      DO 889  L = 1,MAXBUF
        EVTH(L)  = 0.
        EVTE(L)  = 0.
        RUNH(L)  = 0.
        RUNE(L)  = 0.
        DATAB(L) = 0.
        DATAB2(L) = 0.
  889 CONTINUE
 
 
C  PERMANENT INFORMATION
C  CHARACTER STRINGS
      CRUNH = 'RUNH'
      CRUNE = 'RUNE'
      CEVTH = 'EVTH'
      CEVTE = 'EVTE'
 
      RUNH(2)  = NRRUN
      RUNE(2)  = NRRUN
      EVTH(44) = NRRUN
 
C  DATE OF RUN
      WRITE(MONIOU,101)
 101  FORMAT(//' ',10('='),' START OF RUN ',55('='))
      CALL PRTIME(TTIME)
      RUNH(3)  = TTIME
      EVTH(45) = TTIME
 
C  VERSION OF PROGRAM
      RUNH(4)  = VERNUM
      EVTH(46) = VERNUM
 
C-----------------------------------------------------------------------
C  INITIALISATION FOR RANDOM NUMBER GENERATOR
      IF ( FEGS  .AND.  NSEQ .LT. 2 ) NSEQ = 2
C  CERENKOV SELECTION DEMANDS ALWAYS EGS CALCULATION
      FEGS = .TRUE.
C  IN CASE OF CERENKOV CALCULATIONS THE 3. RANDOM SEQUENCE IS NEEDED
      IF ( NSEQ .LT. 3 ) NSEQ = 3
      DO 281  I = 1,NSEQ
        IF ( .NOT. DEBUG   .AND.  .NOT. DEBDEL   .AND.
     *      (ISEED(2,I) .GT. 1000  .OR.  ISEED(3,I) .GT. 0) ) THEN
          WRITE(MONIOU,2811)  I
2811      FORMAT(/' #########################################'/
     *            ' ##  IMPROPER INITIALIZATION OF RANDOM  ##'/
     *            ' ##   NUMBER GENERATOR SEQUENCE ',I6,'  ##'/
     *            ' ##     IS EXTREMELY TIME CONSUMING     ##'/
     *            ' ##       PLEASE READ THE MANUALS       ##'/
     *            ' #########################################'/)
        ENDIF
        CALL RMMAQ( ISEED(1,I), I, 'S' )
  281 CONTINUE
      KNOR = .TRUE.
 
      WRITE(MONIOU,158) (L,(ISEED(J,L),J=1,3),L=1,NSEQ)
  158 FORMAT (/' RANDOM NUMBER GENERATOR AT BEGIN OF RUN :'/
     *        (' SEQUENCE = ',I2,'  SEED = ',I9,'  CALLS = ',I9,
     *         '  BILLIONS = ',I9))
 
C-----------------------------------------------------------------------
C  READ CROSS SECTIONS AND PROBABILITIES FOR NUCLEUS-NUCLEUS COLLISIONS
      OPEN(UNIT=NUCNUC,FILE='NUCNUCCS',STATUS='OLD')
      READ(NUCNUC,500) SIGN30,SIGN45,SIGN60,SIGO30,SIGO45,SIGO60,
     *                 SIGA30,SIGA45,SIGA60
      READ(NUCNUC,500) (PNOA30(I,1),I=1,1540),(PNOA45(I,1),I=1,1540),
     *                 (PNOA60(I,1),I=1,1540),(PNOA30(I,2),I=1,1540),
     *                 (PNOA45(I,2),I=1,1540),(PNOA60(I,2),I=1,1540),
     *                 (PNOA30(I,3),I=1,1540),(PNOA45(I,3),I=1,1540),
     *                 (PNOA60(I,3),I=1,1540)
 500  FORMAT( 5E16.10 )
      CLOSE(UNIT=NUCNUC)
 
C  INELASTIC CROSS SECTIONS FOR PROJECTICLE WITH MASS NUMBER IA
      DO 501  IA = 1,56
        SIG30A(IA) = COMPOS(1)*SIGN30(IA) + COMPOS(2)*SIGO30(IA)
     *                                    + COMPOS(3)*SIGA30(IA)
        SIG45A(IA) = COMPOS(1)*SIGN45(IA) + COMPOS(2)*SIGO45(IA)
     *                                    + COMPOS(3)*SIGA45(IA)
        SIG60A(IA) = COMPOS(1)*SIGN60(IA) + COMPOS(2)*SIGO60(IA)
     *                                    + COMPOS(3)*SIGA60(IA)
 
        IF (DEBUG) WRITE(MDEBUG,544) IA,SIG30A(IA),SIG45A(IA),SIG60A(IA)
 544    FORMAT(' START : CROSS SECTIONS A-AIR : A=',I2,1P,3E14.6)
 501  CONTINUE
 
      WRITE(MONIOU,503)
 503  FORMAT (//' ',10('='),' INTERACTION MODELS ',49('='))
C  HIGH ENERGY HADRONIC INTERACTION MODEL
      IF ( FVENUS ) THEN
        WRITE(MONIOU,*) 'VENUS TREATS HIGH ENERGY HADRONIC INTERACTIONS'
        CALL VENINI
        IF ( .NOT. GHEISH ) THEN
          GHEISH = .TRUE.
          WRITE(MONIOU,*)'GHEISHA OPTION NOT SELECTED, BUT SWITCHED ON'
        ENDIF
        IF     ( NFRAGM .EQ. 0 ) THEN
          WRITE(MONIOU,*)
     *    ' TOTAL FRAGMENTATION OF PRIMARY NUCLEUS IN FIRST INTERACTION'
        ELSEIF ( NFRAGM .EQ. 1 ) THEN
          WRITE(MONIOU,*)
     *      ' NO FRAGMENTATION, NO EVAPORATION OF REMAINDER'
        ELSEIF ( NFRAGM .EQ. 2 ) THEN
          WRITE(MONIOU,1504)
        ELSEIF ( NFRAGM .EQ. 3 ) THEN
          WRITE(MONIOU,1505)
        ELSE
          NFRAGM = 4
          WRITE(MONIOU,1507)
        ENDIF
        WRITE(MONIOU,*)
      ELSE
        WRITE(MONIOU,1506)
      ENDIF
1506  FORMAT(' HDPM ROUTINES TREAT HIGH ENERGY HADRONIC INTERACTIONS')
 
 
      IF ( .NOT. FVENUS ) THEN
C  INPUT FLAGS FOR HDPM OPTIONS
      WRITE(MONIOU,*)'HDPM GENERATOR SPECIFICATIONS ARE:'
      IF ( NFLAIN .EQ. 0 ) THEN
        WRITE(MONIOU,*) ' RANDOM NUMBER OF INTERACTIONS IN AIR TARGET'
        IF ( NFLDIF .EQ. 0 ) THEN
          WRITE(MONIOU,*) ' NO DIFFRACTIVE SECOND INTERACTIONS'
        ELSE
          WRITE(MONIOU,*) ' DIFFRACTIVE SECOND INTERACTIONS'
        ENDIF
      ELSE
        WRITE(MONIOU,*) ' FIXED NUMBER OF INTERACTIONS IN AIR TARGET'
      ENDIF
      IF ( NFLPI0 .EQ. 0 ) THEN
        WRITE(MONIOU,*) ' RAPIDITY OF PI0 ACCORDING TO COLLIDER DATA'
      ELSE
        WRITE(MONIOU,*) ' RAPIDITY OF PI0 SAME AS THAT OF CHARGED'
      ENDIF
      IF ( NFLPIF .EQ. 0 ) THEN
        WRITE(MONIOU,*) ' NO FLUCTUATIONS OF NUMBER OF PI0'
      ELSE
        WRITE(MONIOU,*)' FLUCTUATIONS OF NUMBER OF PI0 AS MEASURED ',
     *         'AT THE COLLIDER'
      ENDIF
      IF ( NFLCHE .EQ. 0 ) THEN
        WRITE(MONIOU,*) ' CHARGE EXCHANGE INTERACTION POSSIBLE '
      ELSE
        WRITE(MONIOU,*) ' NO CHARGE EXCHANGE INTERACTION POSSIBLE '
      ENDIF
      IF     ( NFRAGM .EQ. 0 ) THEN
        WRITE(MONIOU,*)' TOTAL FRAGMENTION OF PRIMARY NUCLEUS IN ',
     *          'FIRST INTERACTION'
      ELSEIF ( NFRAGM .EQ. 1 ) THEN
        WRITE(MONIOU,*) ' NO FRAGMENTATION, NO EVAPORATION OF REMAINDER'
      ELSEIF ( NFRAGM .EQ. 2 ) THEN
        WRITE(MONIOU,1504)
1504    FORMAT('  NO FRAGMENTATION, EVAPORATION OF REMAINDER ',
     *           ' (PT AFTER JACEE)')
      ELSEIF ( NFRAGM .EQ. 3 ) THEN
        WRITE(MONIOU,1505)
1505    FORMAT('  NO FRAGMENTATION, EVAPORATION OF REMAINDER ',
     *           ' (PT AFTER GOLDHABER)')
      ELSE
        NFRAGM = 4
        WRITE(MONIOU,1507)
1507    FORMAT('  NO FRAGMENTATION, EVAPORATION OF REMAINDER ',
     *           ' (WITH PT = 0.)')
      ENDIF
      ENDIF
      WRITE(MONIOU,*)
 
C  LOW ENERGY HADRONIC INTERACTION MODEL
      IF ( GHEISH ) THEN
        WRITE(MONIOU,*) 'GHEISHA TREATS LOW ENERGY HADRONIC ',
     *                  'INTERACTIONS'
        CALL CGHINI
      ELSE
        WRITE(MONIOU,*) 'ISOBAR ROUTINES TREAT LOW ENERGY HADRONIC ',
     *                  'INTERACTIONS'
        HILOELB = 53.D0
      ENDIF
 
C  WRITE HADRONIC STEERING FLAGS TO RUNHEADER
      RUNH(270) = NFLAIN
      RUNH(271) = NFLDIF
      RUNH(272) = NFLPI0 + 100. * NFLPIF
      RUNH(273) = NFLCHE + 100. * NFRAGM
 
      EVTH(65)  = NFLAIN
      EVTH(66)  = NFLDIF
      EVTH(67)  = NFLPI0
      EVTH(68)  = NFLPIF
      EVTH(69)  = NFLCHE
      EVTH(70)  = NFRAGM
 
      HILOECM = SQRT(2.D0*PAMA(14)*(PAMA(14) + HILOELB))
      WRITE(MONIOU,*) 'START: HIGH ENERGY INTERACTION MODEL USED ABOVE'
      WRITE(MONIOU,*)  '     ',HILOELB,' GEV LAB ENERGY   OR'
      WRITE(MONIOU,*)  '     ',HILOECM,' GEV CM ENERGY'
 
C  INPUT STEERING FLAGS FOR ELECTROMAGNETIC PART
      WRITE(MONIOU,*)
      IF ( FNKG ) THEN
        WRITE(MONIOU,*)'ELECTROMAGNETIC COMPONENT SIMULATED WITH NKG'
        IF ( ULIMIT .GT. 2.D7 ) THEN
          WRITE(MONIOU,*)'#############################################'
          WRITE(MONIOU,*)'#  W A R N I N G  NKG IS WITHOUT LPM EFFECT #'
          WRITE(MONIOU,*)'#############################################'
        ENDIF
        WRITE(MONIOU,*)
      ENDIF
      IF ( FEGS ) THEN
        WRITE(MONIOU,*)'ELECTROMAGNETIC COMPONENT SIMULATED WITH EGS4'
        WRITE(MONIOU,*)
      ENDIF
      IF ( .NOT. (FNKG .OR. FEGS) ) WRITE(MONIOU,*)
     *              'ELECTROMAGNETIC COMPONENT IS NOT SIMULATED'
      IF ( FEGS ) THEN
        IF ( STEPFC .GT. 10.  .OR.  STEPFC .LE. 0. ) THEN
          WRITE(MONIOU,*)'STEP LENGTH FACTOR FOR ELECTRON MULTIPLE ',
     *       'SCATTERING =',STEPFC,' NOT CORRECT'
          WRITE(MONIOU,*)'PLEASE READ THE MANUALS'
          STOP
        ENDIF
        IF ( STEPFC .LT. 10. ) WRITE(MONIOU,*)'STEP LENGTH ',
     *            'FACTOR FOR ELECTRON MULTIPLE SCATTERING =',STEPFC
C  INITIALIZE EGS4 PACKAGE
        CALL EGSINI
        IF ( ULIMIT .GT. 2.D7 ) THEN
          WRITE(MONIOU,*)'#############################################'
          WRITE(MONIOU,*)'#  W A R N I N G  EGS IS WITHOUT LPM EFFECT #'
          WRITE(MONIOU,*)'#############################################'
        ENDIF
      ENDIF
C  WRITE STEERING FLAGS FOR ELECTROMAGNETIC PART AS REAL TO HEADER
      IF ( FNKG ) THEN
        RUNH(20) = 1.
        EVTH(74) = 1.
      ELSE
        RUNH(20) = 0.
        EVTH(74) = 0.
      ENDIF
      IF ( FEGS ) THEN
        RUNH(19) = 1.
        EVTH(73) = 1.
      ELSE
        RUNH(19) = 0.
        EVTH(73) = 0.
      ENDIF
 
      EVTH(95) = STEPFC
 
C  PROGRAM CONFIGURATIONS FOR EVENT HEADER
      IF ( GHEISH ) THEN
        EVTH(75) = 1.
      ELSE
        EVTH(75) = 0.
      ENDIF
      IF ( FVENUS ) THEN
        EVTH(76) = 1.
      ELSE
        EVTH(76) = 0.
      ENDIF
      EVTH(139) = 0.
      EVTH(140) = 0.
      EVTH(141) = 0.
      EVTH(142) = 0.
      EVTH(143) = 0.
      EVTH(144) = 0.
      EVTH(145) = 0.
      EVTH(77) = 1.
      EVTH(78) = 0.
      EVTH(79) = 0.
      EVTH(80) = 3.
 
C-----------------------------------------------------------------------
C  BEGIN OF TAPE FOR IBM,  FOR TRANSPUTER SEE BEGIN OF EVT
 
C-----------------------------------------------------------------------
C  PHYSICAL CONSTANTS
      ENEPER  = EXP(1.D0)
      C(6)    = ( PAMA(5) / PAMA(11) )**2
      C(7)    = ( PAMA(5) / PAMA(8) )**2
      C(8)    = ( PAMA(5)**2 + PAMA(2)**2 ) * 0.5D0 / PAMA(5)
      C(20)   = 10.D0 * C(21)
      C(27)   = COS( C(26) )
      C(29)   = COS( C(28) )
      C(44)   = MAX( PAMA(8)+C(4), PAMA(14)+C(5) )
      C(45)   = PAMA(8) * PAMA(14) * 2.D0
      C(46)   = PAMA(8)**2 + PAMA(14)**2
      C(48)   = (PAMA(8)**2 + PAMA(5)**2) / (2.D0*PAMA(8)*PAMA(5))
      C(49)   = SQRT(C(48)**2 - 1.D0) / C(48)
 
      CKA(13) = 2.D0 * PAMA(11) * PAMA(14)
      CKA(14) = PAMA(11)**2 + PAMA(14)**2
      CKA(17) = SQRT( ( (PAMA(11)**2 + PAMA(5)**2)
     *          / (2.D0*PAMA(11)) )**2 - PAMA(5)**2 )
      CKA(18) = SQRT( ( (PAMA(11)**2 + PAMA(8)**2 - PAMA(7)**2)
     *          / (2.D0*PAMA(11)) )**2 - PAMA(8)**2 )
      CKA(22) = MAX( C(5)+PAMA(14), PAMA(11)+C(4) )
      CKA(28) = SQRT(1.D0 + CKA(17)**2/PAMA(5)**2)
      CKA(29) = SQRT(1.D0 - 1.D0/CKA(28)**2)
      CKA(30) = SQRT(1.D0 + CKA(18)**2/PAMA(8)**2)
      CKA(31) = SQRT(1.D0 - 1.D0/CKA(30)**2)
      CKA(41) = PAMA(16)
      CKA(42) = (PAMA(11)**2 + PAMA(7)**2 - PAMA(8)**2) /
     *          (2.D0*PAMA(11)*PAMA(7))
      CKA(43) = CKA(41) / (2.D0*PAMA(7))
      CKA(44) = SQRT(1.D0 - 1.D0/CKA(43)**2)
      CKA(45) = CKA(41) / (2.D0*PAMA(8))
      CKA(46) = SQRT(1.D0 - 1.D0/CKA(45)**2)
 
C  SET CONSTANTS FOR MUON BREMSSTRAHLUNG
      CMUON(3)  =  7.D0**OB3
      CMUON(6)  =  8.D0**OB3
      CMUON(9)  = 18.D0**OB3
      CMUON(1)  = LOG( 189.D0 * PAMA(5) / (CMUON(3)*PAMA(2)) )
      CMUON(4)  = LOG( 189.D0 * PAMA(5) / (CMUON(6)*PAMA(2)) )
      CMUON(7)  = LOG( 189.D0 * PAMA(5) / (CMUON(9)*PAMA(2)) )
     *                   + LOG( TB3/CMUON(9) )
      SE        = SQRT(EXP(1.D0))
      CMUON(2)  = 189.D0 * SE*PAMA(5)**2/(2.D0*PAMA(2)*CMUON(3))
      CMUON(5)  = 189.D0 * SE*PAMA(5)**2/(2.D0*PAMA(2)*CMUON(6))
      CMUON(8)  = 189.D0 * SE*PAMA(5)**2/(2.D0*PAMA(2)*CMUON(9))
      CMUON(10) = 0.75D0 * PAMA(5) * SE
      CMUON(3)  = CMUON(3) * CMUON(10)
      CMUON(6)  = CMUON(6) * CMUON(10)
      CMUON(9)  = CMUON(9) * CMUON(10)
      CMUON(11) = LOG( BCUT/PAMA(5) )
 
      DO  1  I = 1,50
        CANN(I) = 0.D0
    1 CONTINUE
      COAN = 0.D0
      DO 25  N = 1,12
        COAN    = COAN + CAN(N)
        CANN(N) = COAN
   25 CONTINUE
      COAN = 0.D0
      DO 26  N = 13,26
        COAN    = COAN + CAN(N)
        CANN(N) = COAN
   26 CONTINUE
 
C-----------------------------------------------------------------------
C  INITIALIZE CONSTANTS FOR MUON MULTIPLE SCATTERING (MOLIERE)
C  SEE SUBROUTINE GMOLI OF GEANT321 (CERN)
      IF (FMOLI) THEN
        TEMP1 = COMPOS(1) *  7.D0 *  8.D0 / 14.D0
        TEMP2 = COMPOS(2) *  8.D0 *  9.D0 / 16.D0
        TEMP3 = COMPOS(3) * 18.D0 * 19.D0 / 40.D0
        ZS  = TEMP1 + TEMP2 + TEMP3
        ZE  = -TB3*(TEMP1*LOG(7.D0) +TEMP2*LOG(8.D0) +TEMP3*LOG(18.D0))
        ZX  =  TEMP1*LOG(1.D0 + 3.34D0 * ( 7.D0/C(50))**2)
     *        +TEMP2*LOG(1.D0 + 3.34D0 * ( 8.D0/C(50))**2)
     *        +TEMP3*LOG(1.D0 + 3.34D0 * (18.D0/C(50))**2)
C  NOTE: CHC IS DEFINED DIFFERENT FROM GEANT WITHOUT DENSITY
        CHC = 0.39612D-3 * SQRT(ZS)
C  NOTE: OMC IS DEFINED DIFFERENT FROM GEANT WITHOUT DENSITY
        OMC = 6702.33D0 * ZS * EXP( (ZE-ZX)/ZS )
        EVTH(146) = 1.
      ELSE
        EVTH(146) = 0.
      ENDIF
 
C-----------------------------------------------------------------------
C  TEST ON INPUT VALUES
 
C  PRINT CONTROL OUTPUT
      IF ( CC(1)         .GE. CC(2)     .OR.
     *     CC(2)         .GE. CC(3)     .OR.
     *     CC(3)         .GE. CC(4)     .OR.
     *     CC(5)         .GE. CC(6)     .OR.
     *     CC(6)         .GE. CC(7)     .OR.
     *     CC(7)         .GE. CC(8)     .OR.
     *     CC(9)         .GE. CC(10)    .OR.
     *     CC(10)        .GE. CC(11)    .OR.
     *     CC(11)        .GE. CC(12)    .OR.
     *     PAMA(14)+C(3) .GT. CC(1)     .OR.
     *     PAMA(14)+C(4) .GT. CC(2)     .OR.
     *     C(4)*2.       .GT. CC(3)     .OR.
     *     C(3)+PAMA(8)  .GT. CC(5)     .OR.
     *     C(44)         .GT. CC(6)     .OR.
     *     C(4)+C(5)     .GT. CC(7)     .OR.
     *     PAMA(14)+C(4) .GE. C(4)*2.   .OR.
     *     C(44)         .GE. C(4)+C(5)        ) THEN
        WRITE(MONIOU,106)
  106   FORMAT (' ERROR OR INCOMPATIBILITY IN CONSTANTS')
C  PRINT CONTROL OUTPUT
        WRITE(MONIOU,103) (C(I),I=1,50)
  103   FORMAT (//' ',10('='),' CONSTANTS AND PARAMETERS ',43('=')
     *          //' PHYSICAL CONSTANTS (C)' // (1P,4(E15.8,1X),E15.8) )
        WRITE(MONIOU,110) (CKA(I),I=1,80)
  110   FORMAT (//' CONSTANTS FOR KAONS CKA(1) TO CKA(40)'
     *          // (1P,4(E15.8,1X),E15.8) )
        WRITE(MONIOU,114) (CETA(I),I=1,5)
  114   FORMAT (//' CONSTANTS FOR ETAS CETA(1) TO CETA(5)'
     *          // (1P,4(E15.8,1X),E15.8) )
        WRITE(MONIOU,115) (CSTRBA(I),I=1,11)
  115   FORMAT (//' CONSTANTS FOR STRANGE BARYONS CSTRBA(1) TO ',
     *            'CSTRBA(11)'// (1P,4(E15.8,1X),E15.8) )
        WRITE(MONIOU,206) (CAN(I),I=1,30)
  206   FORMAT (//' ANNIHILATION PARAMETERS, SET 1 (CAN)'
     *          // (1P,4(E15.8,1X),E15.8) )
        WRITE(MONIOU,209) (CANN(I),I=1,30)
  209   FORMAT (//' ANNIHILATION PARAMETERS, SET 2 (CANN)'
     *          // (1P,4(E15.8,1X),E15.8) )
        WRITE(MONIOU,60) (CC(I),I=1,12)
   60   FORMAT (//' THRESHOLD ENERGIES OF INTERACTION INTERVALS IN GEV',
     *          ' (CC)'// (1P,4(E15.8,1X),E15.8) )
        WRITE(MONIOU,106)
        STOP
      ENDIF
 
C  FILL CONSTANTS IN RUN HEADER
      DO 3001  L = 1,50
        RUNH(24+L)  = C(L)
        RUNH(154+L) = CAN(L)
        RUNH(204+L) = CANN(L)
 3001 CONTINUE
      DO 3002  L = 1,20
        RUNH(74+L)  = CC(L)
 3002 CONTINUE
      DO 3003  L = 1,40
        RUNH(94+L)  = CKA(L)
 3003 CONTINUE
      DO 3004  L = 1,5
        RUNH(134+L)  = CETA(L)
 3004 CONTINUE
      DO 3005  L = 1,11
        RUNH(139+L)  = CSTRBA(L)
 3005 CONTINUE
      DO 3007  L = 1,5
        RUNH(254+L) = AATM(L)
        RUNH(259+L) = BATM(L)
        RUNH(264+L) = CATM(L)
        DATM(L)     = 1.D0 / CATM(L)
 3007 CONTINUE
 
C  SET LOWER BOUNDARIES OF THE AIR LAYERS
      HLAY(1)   = 0.D0
      HLAY(2)   = 4.D5
      HLAY(3)   = 1.D6
      HLAY(4)   = 4.D6
      HLAY(5)   = 1.D7
C  CALCULATE THICKNESS AT LOWER BOUNDARIES OF AIR LAYERS
      DO 100 L= 1,5
        THICKL(L) = THICK(HLAY(L))
 100  CONTINUE
 
      CALL STAEND
 
      RETURN
      END
