      REAL FUNCTION GBRSGM(Z,E)
 
C-----------------------------------------------------------------------
C  CALCULATES MUON BREMSSTRAHLUNG CROSS SECTIONS
C
C  THIS SUBROUTINE IS TAKEN FROM GEANT321 PACKAGE (WITH MODIFICATIONS)
C  CALCULATES CROSS-SECTION IN CURRENT MATERIAL FOR DISCRETE(HARD) MUON
C  BREMSSTRAHLUNG.   (SIG IN BARN/ATOM)
C  FOR A DESCRIPTION SEE: CERN PROGRAM LIBRARY LONG WRITEUP W5013 (1993)
C  THIS FUNCTION IS CALLED FROM BOX2
C  ARGUMENTS:
C   Z (R4) = ATOMIC NUMBER OF PENETRATET MATERIAL
C   E (R4) = TOTAL ENERGY OF MUON
C
C  AUTHOR  : L.URBAN
C  MODIFIED: D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
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
 
      REAL    C(52),AKSI,ALFA,E,ECMAX,FAC,GAM,
     *        S,SS,X,XX,Y,YY,Z
      INTEGER I,J,K
      SAVE    C
      DATA    AKSI/2.30/, ALFA/1.06/, GAM/0.63/
      DATA    C/ 0.949313E-07,-0.819600E-07, 0.529075E-07,-0.832023E-08
     +         , 0.539299E-09,-0.127042E-10,-0.165784E-08,-0.307788E-07
     +         , 0.977905E-08,-0.113658E-08, 0.574481E-10,-0.106221E-11
     +         , 0.968339E-09,-0.108640E-08,-0.177634E-09, 0.889497E-10
     +         ,-0.876878E-11, 0.264303E-12, 0.216263E-08,-0.152680E-08
     +         , 0.380989E-09,-0.455274E-10, 0.264172E-11,-0.596016E-13
     +         , 0.444927E-09,-0.272978E-09, 0.645634E-10,-0.748783E-11
     +         , 0.424890E-12,-0.940837E-14, 0.162289E-10,-0.362486E-11
     +         ,-0.576652E-12, 0.211269E-12,-0.185482E-13, 0.522065E-15
     +         ,-0.215590E-09, 0.112204E-09,-0.819133E-11, 0.145128E-12
     +         ,-0.206029E-09, 0.559940E-10,-0.483350E-11, 0.134252E-12
     +         ,-0.368469E-10, 0.999457E-11,-0.904967E-12, 0.272717E-13
     +         ,-0.303446E-11, 0.853429E-12,-0.785466E-13, 0.236435E-14/
C-----------------------------------------------------------------------
 
      GBRSGM = 0.
      IF ( E-PAMA(5) .LE. BCUT ) RETURN
      ECMAX = E - CMUON(10) * Z**0.333333
      IF ( ECMAX .LE. BCUT ) RETURN
      X = LOG(E/PAMA(5))
 
      S = 0.
      YY = 1.
      DO 30 I = 1,6
        XX = 1.
        DO 20 J = 1,6
          K  = 6*I + J - 6
          S  = S + C(K) * XX * YY
          XX = XX * X
  20    CONTINUE
        YY = YY * CMUON(11)
  30  CONTINUE
      SS = 0.
      YY = 1.
      DO 50 I = 1,4
        XX = 1.
        DO 40 J = 1,4
          K  = 4*I + J + 32
          SS = SS + C(K) * XX * YY
          XX = XX * X
  40    CONTINUE
        YY = YY * CMUON(11)
  50  CONTINUE
      S = S + Z * SS
      IF ( S .LE. 0. ) RETURN
 
      FAC = LOG(ECMAX/BCUT)
      IF ( FAC .LE. 0. ) RETURN
      FAC = Z * ( Z + AKSI * (1.+GAM*LOG(Z)) ) * FAC**ALFA
      GBRSGM = FAC * S
 
*     IF ( DEBUG ) WRITE(MDEBUG,444) Z,E,GBRSGM
* 444 FORMAT(' GBRSGM: Z=',F3.0,' E=',1P,E10.4,' GBRSGM=',E10.4)
 
  99  RETURN
      END
