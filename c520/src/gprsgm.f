      REAL FUNCTION GPRSGM(Z,E)
 
C-----------------------------------------------------------------------
C  G(EANT) P(AI)R S(I)GM(A)
C  CALCULATES MUON PAIR PRODUCTION CROSS SECTIONS
C
C  THIS SUBROUTINE IS TAKEN FROM GEANT321 PACKAGE (WITH MODIFICATIONS)
C  CALCULATES CROSS-SECTION IN CURRENT MATERIAL FOR DISCRETE(HARD) MUON
C  PAIR PRODUCTION.  (SIG IN BARN/ATOM)
C  FOR A DESCRIPTION SEE: CERN PROGRAM LIBRARY LONG WRITEUP W5013 (1993)
C  THIS SUBROUTINE IS CALLED FROM BOX2
C  ARGUMENTS:
C   Z (R4) = ATOMIC NUMBER OF PENETRATET MATERIAL
C   E (R4) = TOTAL ENERGY OF MUON
C
C  AUTHOR  : L.URBAN
C  MODIFIED: D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,MUPART.
      COMMON /MUPART/  AMUPAR,BCUT,CMUON,FMUBRM,FMUORG
      DOUBLE PRECISION AMUPAR(14),BCUT,CMUON(11)
      LOGICAL          FMUBRM,FMUORG
*KEEP,PAM.
      COMMON /PAM/     PAMA,SIGNUM
      DOUBLE PRECISION PAMA(6000),SIGNUM(6000)
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
 
      REAL        C(100),C1(60),C2(40),AKSI,ALFA,E,ECMAX,ECMIN,FAC,GAM,
     *            S,SS,VS,X,XX,Y,YY,Z
      INTEGER     I,J,K
      EQUIVALENCE (C(1),C1(1)),(C(61),C2(1))
      SAVE        C
      DATA        AKSI/1.16/,ALFA/3.46/,GAM/0.06/,VS/0.019/
      DATA        ECMIN/2.044E-3/
      DATA C1/0.230181E-08,-0.280842E-08, 0.137525E-08,-0.156503E-09
     +      , 0.728088E-11,-0.122631E-12, 0.133014E-08,-0.160591E-09
     +      ,-0.390814E-09, 0.314492E-10, 0.251296E-12,-0.574223E-13
     +      , 0.604923E-09,-0.560766E-09, 0.660253E-09,-0.103474E-09
     +      , 0.621338E-11,-0.135273E-12, 0.103739E-09, 0.710290E-09
     +      ,-0.544755E-10,-0.211241E-11, 0.286443E-12,-0.644602E-14
     +      , 0.332492E-09,-0.484785E-10, 0.126921E-10,-0.165217E-11
     +      , 0.845273E-13,-0.143180E-14,-0.112267E-13, 0.113308E-11
     +      , 0.292577E-12,-0.733441E-13, 0.475747E-14,-0.976279E-16
     +      ,-0.112856E-07, 0.936398E-08,-0.291882E-08, 0.422266E-09
     +      ,-0.279042E-10, 0.678485E-12, 0.112383E-07,-0.964400E-08
     +      , 0.313121E-08,-0.440224E-09, 0.278668E-10,-0.643012E-12
     +      ,-0.414131E-08, 0.355112E-08,-0.115035E-08, 0.158539E-09
     +      ,-0.976788E-11, 0.216911E-12, 0.521380E-09,-0.442265E-09
     +      , 0.141753E-09,-0.190826E-10, 0.114038E-11,-0.242085E-13/
      DATA C2/0.572943E-10,-0.296824E-10, 0.630217E-11,-0.623179E-12
     +      , 0.211467E-13,-0.143579E-10,-0.137247E-11, 0.118670E-11
     +      ,-0.793091E-13, 0.124745E-14,-0.269884E-10, 0.125314E-10
     +      ,-0.239259E-11, 0.181151E-12,-0.470277E-14,-0.342454E-11
     +      , 0.976666E-12,-0.236792E-12, 0.213290E-13,-0.607799E-15
     +      ,-0.748844E-12, 0.178214E-12,-0.226827E-13, 0.148441E-14
     +      ,-0.367972E-16, 0.840330E-12, 0.820025E-11,-0.294797E-11
     +      , 0.294669E-12,-0.970294E-14,-0.830636E-12,-0.309273E-11
     +      , 0.124169E-11,-0.135879E-12, 0.481683E-14, 0.438223E-12
     +      , 0.259162E-12,-0.149284E-12, 0.180170E-13,-0.677948E-15/
C-----------------------------------------------------------------------
      GPRSGM=0.
C     IF ( ECMIN .GT. BCUT ) CUT=ECMIN
 
      ECMAX  = E - CMUON(10) * Z**OB3
      IF ( ECMAX .LE. BCUT ) RETURN
      X = LOG(E/PAMA(5))
      Y = LOG( BCUT/(VS*E) )
 
      S = 0.
      YY = 1.
      DO 30 I = 1,2
        XX = 1.
        DO 20 J = 1,6
          K  = 6*I + J - 6
          S  = S + C(K) * XX * YY
          XX = XX * X
 20     CONTINUE
        YY = YY * Y
 30   CONTINUE
      DO 50 I = 3,6
        XX = 1.
        DO 40 J = 1,6
          K = 6*I + J - 6
          IF ( Y .LE. 0. ) THEN
            S = S + C(K) * XX * YY
          ELSE
            S = S + C(K+24) * XX * YY
          ENDIF
          XX = XX * X
 40     CONTINUE
        YY = YY * Y
 50   CONTINUE
      SS = 0.
      YY = 1.
      DO 70 I = 1,2
        XX = 1.
        DO 60 J = 1,5
          K = 5*I + J + 55
          SS = SS + C(K) * XX * YY
          XX = XX * X
 60     CONTINUE
        YY = YY * Y
 70   CONTINUE
      DO 90 I = 3,5
        XX = 1.
        DO 80 J = 1,5
          K = 5*I + J + 55
          IF ( Y .LE. 0. ) THEN
            SS = SS + C(K) * XX * YY
          ELSE
            SS = SS + C(K+15) * XX * YY
          ENDIF
          XX = XX * X
 80     CONTINUE
        YY = YY * Y
 90   CONTINUE
 
      S = S + Z * SS
      IF ( S .LE. 0. ) RETURN
C  DE/DX SHOULD BE MONOTON INCREASING AS A
C  FUNCTION OF THE CUT
C  SOLUTION: LIN. INTERPOLATION FOR 0.2*ECMAX<CUT<ECMAX
      FAC = 2. * ( LOG(ECMAX/BCUT) )**ALFA
      FAC = Z * ( Z + AKSI*( 1.+GAM*LOG(Z) ) ) * FAC
      GPRSGM = FAC * S
*     IF ( DEBUG ) WRITE(MDEBUG,444) Z,E,GPRSGM
* 444 FORMAT(' GPRSGM: Z=',F3.0,' E=',1P,E10.4,' GPRSGM=',E10.4)
 
 99   RETURN
      END
