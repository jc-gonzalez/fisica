      BLOCK DATA BLOCK1
 
C-----------------------------------------------------------------------
C
C  INITIALIZES DATA
C-----------------------------------------------------------------------
 
*KEEP,AIR.
      COMMON /AIR/     COMPOS,PROBTA,AVERAW,AVOGAD
      DOUBLE PRECISION COMPOS(3),PROBTA(3),AVERAW,AVOGAD
*KEEP,ANNI.
      COMMON /ANNI/    CAN,CANN
      DOUBLE PRECISION CAN(50),CANN(50)
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
*KEEP,EDECAY.
      COMMON /EDECAY/  CETA
      DOUBLE PRECISION CETA(5)
*KEEP,GNUPR.
      COMMON /GNUPR/   SE14,SE16,SE40
      DOUBLE PRECISION SE14(3,14),SE16(3,16),SE40(3,40)
*KEEP,KAONS.
      COMMON /KAONS/   CKA
      DOUBLE PRECISION CKA(80)
*KEEP,MUPART.
      COMMON /MUPART/  AMUPAR,BCUT,CMUON,FMUBRM,FMUORG
      DOUBLE PRECISION AMUPAR(14),BCUT,CMUON(11)
      LOGICAL          FMUBRM,FMUORG
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
*KEEP,RANGE.
      COMMON /RANGE/   CC
      DOUBLE PRECISION CC(20)
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
*KEEP,CEREN3.
      COMMON /CEREN3/  CERCNT,DATAB2,LHCER
      INTEGER          MAXBF2
      PARAMETER        (MAXBF2 = 39 * 7)
      DOUBLE PRECISION CERCNT
      REAL             DATAB2(MAXBF2)
      INTEGER          LHCER
*KEND.
 
C-----------------------------------------------------------------------
C  AIR
      DATA COMPOS / 0.78479D0, 0.21052D0, 0.00469D0 /
      DATA PROBTA / 0.78479D0, 0.99531D0, 1.00000D0 /
      DATA AVERAW / 14.543D0 /
      DATA AVOGAD / 6.0225D-4 /
C  ANNI
      DATA CAN / 0.0042D0, 0.0728D0, 0.0981D0, 0.2458D0, 0.0295D0,
     *           0.0654D0, 0.1888D0, 0.1751D0, 0.0443D0, 0.0211D0,
     *           0.0179D0, 0.0032D0, 0.0020D0, 0.0000D0, 0.0800D0,
     *           0.0900D0, 0.0070D0, 0.0230D0, 0.1700D0, 0.1000D0,
     *           0.2600D0, 0.0400D0, 0.0420D0, 0.1200D0, 0.0660D0,
     *           0.0000D0,   24* 0.0D0 /
C  ATMOS
      DATA AATM / -186.5562D0,  -94.919D0,  0.61289D0,0.D0,.01128292D0 /
      DATA BATM / 1222.6562D0,1144.9069D0,1305.5948D0,540.1778D0,0.D0  /
      DATA CATM / 994186.38D0,878153.55D0,636143.04D0,772170.16D0,1.D-9/
C  CEREN3
      DATA CERCNT / 0.D0 /
C  CONST
      DATA PI  / 3.141592653589793D0 /
      DATA PI2 / 6.283185307179586D0 /
      DATA OB3 / 0.333333333333333D0 /
      DATA TB3 / 0.666666666666666D0 /
C  ENEPER IS CALCULATED IN START: ENEPER = EXP(1.D0)
C  DATA FOR MUPART: CUTOFF FOR BREMSSTRAHLUNG AT 3 MEV
      DATA BCUT /0.003D0/
C  DATA FOR REST: AVERAGE ATOMIC WEIGHT, NEUTRON CONTENTS OF N,O,AR
      DATA TAR / 14.6D0 /, CONTNE / 0.5D0, 0.5D0, 0.55D0 /, LT / 1 /
C  KAON CONSTANTS
      DATA CKA /     0.0D0,  0.1D0, 1.2371D-8, 1.2371D-8, 0.8922D-10,
     *             5.16D-8, 0.25D0,     0.5D0,    0.75D0,      1.0D0,
     *               0.5D0,  0.2D0,     0.0D0,     0.0D0,    149.6D0,
     *             149.6D0,0.236D0,   0.206D0,   0.135D0,    0.222D0,
     *               0.5D0,  0.0D0,   0.635D0,   0.686D0,    0.876D0,
     *             0.658D0,0.387D0,     0.0D0,     0.0D0,      0.0D0,
     *               0.0D0,  0.0D0,     0.0D0,     0.0D0,      1.0D0,
     *               1.0D5,  0.0D0,     0.0D0,     0.0D0,      0.0D0,
     *               0.0D0,  0.0D0,     0.0D0,     0.0D0,      0.0D0,
     *               0.0D0,0.8468D0, 0.9027D0,  0.9509D0,   0.9827D0,
     *           -0.2154D0, 0.012D0,-0.0101D0,    1.27D0,    0.594D0,
     *             0.035D0,   0.0D0,   1.84D0,     0.0D0,      1.0D0,
     *              0.67D0, 0.079D0, 0.0098D0,    2.22D0,    0.028D0,
     *               0.0D0,1.288D-2,  0.033D0,   0.004D0,   1.194D-2,
     *              0.03D0,   0.0D0, 1.310D-2,   0.034D0,    0.025D0,
     *            1.241D-2,   0.0D0,    0.0D0,     0.0D0,      0.0D0  /
C  DATA FOR ETA DECAY
      DATA CETA /  0.3913D0,  0.7122D0,  0.9506D0,  -1.07D0,  2.07D0 /
C  DATA FOR STRANGE BARYON DECAY
      DATA CSTRBA / 2.632D-10, 0.799D-10, 7.4D-20, 1.479D-10, 0.642D0,
     *              0.5164D0, 2.90D-10, 1.639D-10, 0.822D-10, 0.678D0,
     *              0.914D0 /
C  PARPAR
      DATA C /   0.0D0,    0.0D0,      1.4D0,      2.5D0,       1.35D0,
     *           0.0D0,    0.0D0,      0.0D0,      0.0D0,       2.07D0,
     *           8.2D0,    0.1D0,      0.0D0,      0.0D0,        0.0D0,
     *          88.0D0,   110.D0,      2.603D-8,   2.19703D-6,   0.0D0,
     *          37.7D0, 1.532873D-4, 9.386417D0,   2.D-3, 29.9792458D9,
     *           1.0D0,    0.0D0,     1.57D0,      0.0D0,      0.021D0,
     *          88.0D0,  110.0D0,      0.0D0,      2.0D1,        0.1D0,
     *          0.12D0,    0.0D0,      0.0D0,      0.0D0,       0.09D0,
     *           0.0D0,    0.1D0,      0.0D0,      0.0D0,        0.0D0,
     *           0.0D0,    0.0D0,      0.0D0,      0.0D0,137.0359895D0 /
C  DATA FOR RANGE: ENERGY REGIONS FOR SELECTION OF BOXES IN ISOBAR MODEL
      DATA CC / 2.5D0,  3.5D0, 5.5D0, 10.1D0,
     *          1.6D0,  2.7D0, 3.9D0, 10.1D0,
     *          1.6D0, 3.05D0, 3.9D0, 10.1D0, 8*0.D0 /
C  RUNPAR,STACKF
      DATA MONIIN /  5 /, MONIOU /  6 /, PATAPE / 90 /, EXST / 96 /,
     *     MDEBUG /  6 /, NUCNUC / 11 /, MDBASE / 45 /
     *    ,CETAPE / 91 /
C  GNUPR   (NEW VERSION OCT 1991)
C  NITROGEN TARGET  14
      DATA ((SE14(I,J),I=1,3),J=1,14)
     *                       / 0.472000D+00,-0.426710D-02, 0.726439D-04,
     *                         0.230324D+00,-0.989733D-03,-0.807077D-05,
     *                         0.138623D+00, 0.609624D-03,-0.401675D-04,
     *                         0.827139D-01, 0.135103D-02,-0.360236D-04,
     *                         0.445693D-01, 0.137582D-02,-0.137674D-04,
     *                         0.206106D-01, 0.998620D-03, 0.422867D-05,
     *                         0.792756D-02, 0.559858D-03, 0.957875D-05,
     *                         0.247793D-02, 0.247480D-03, 0.701650D-05,
     *                         0.615535D-03, 0.860096D-04, 0.324410D-05,
     *                         0.118279D-03, 0.230732D-04, 0.104282D-05,
     *                         0.169210D-04, 0.461424D-05, 0.235175D-06,
     *                         0.169481D-05, 0.647634D-06, 0.358189D-07,
     *                         0.105988D-06, 0.568994D-07, 0.332920D-08,
     *                         0.311374D-08, 0.235385D-08, 0.143213D-09/
C  OXYGEN TARGET  16
      DATA ((SE16(I,J), I=1,3),J=1,16)
     *                        /0.475002D+00,-0.434401D-02, 0.734217D-04,
     *                         0.230261D+00,-0.966152D-03,-0.982228D-05,
     *                         0.137372D+00, 0.642454D-03,-0.408490D-04,
     *                         0.813380D-01, 0.135241D-02,-0.354835D-04,
     *                         0.437870D-01, 0.135776D-02,-0.134429D-04,
     *                         0.204919D-01, 0.988538D-03, 0.398723D-05,
     *                         0.812995D-02, 0.567070D-03, 0.942943D-05,
     *                         0.269031D-02, 0.263160D-03, 0.728079D-05,
     *                         0.732711D-03, 0.993722D-04, 0.366933D-05,
     *                         0.161940D-03, 0.303662D-04, 0.134776D-05,
     *                         0.285325D-04, 0.740356D-05, 0.371648D-06,
     *                         0.390910D-05, 0.140655D-05, 0.768260D-07,
     *                         0.401145D-06, 0.200620D-06, 0.116200D-07,
     *                         0.290010D-07, 0.202033D-07, 0.121929D-08,
     *                         0.131709D-08, 0.128046D-08, 0.795482D-10,
     *                         0.282645D-10, 0.384068D-10, 0.243535D-11/
C  ARGON TARGET  40
      DATA ((SE40(I,J),I=1,3),J=1,18)
     *                       / 0.318084D+00,-0.352566D-02, 0.829469D-04,
     *                         0.193581D+00,-0.238538D-02, 0.404919D-04,
     *                         0.148699D+00,-0.118791D-02,-0.130378D-04,
     *                         0.117201D+00, 0.966097D-04,-0.536044D-04,
     *                         0.876737D-01, 0.106482D-02,-0.612882D-04,
     *                         0.600279D-01, 0.150343D-02,-0.412273D-04,
     *                         0.370180D-01, 0.147347D-02,-0.130096D-04,
     *                         0.204422D-01, 0.117625D-02, 0.743960D-05,
     *                         0.101003D-01, 0.807913D-03, 0.155153D-04,
     *                         0.447163D-02, 0.489622D-03, 0.146804D-04,
     *                         0.177806D-02, 0.265260D-03, 0.102802D-04,
     *                         0.636671D-03, 0.129412D-03, 0.591434D-05,
     *                         0.205809D-03, 0.571042D-04, 0.291674D-05,
     *                         0.601981D-04, 0.228546D-04, 0.126074D-05,
     *                         0.159631D-04, 0.831226D-05, 0.484001D-06,
     *                         0.384379D-05, 0.275100D-05, 0.166440D-06,
     *                         0.841490D-06, 0.829259D-06, 0.515615D-07,
     *                         0.167633D-06, 0.227810D-06, 0.144446D-07/
      DATA((SE40(I,J),I=1,3),J=19,36)
     *                        /0.304029D-07, 0.570494D-07, 0.366843D-08,
     *                         0.502077D-08, 0.130224D-07, 0.845876D-09,
     *                         0.754786D-09, 0.270844D-08, 0.177211D-09,
     *                         0.103229D-09, 0.512862D-09, 0.337323D-10,
     *                         0.128308D-10, 0.883149D-10, 0.583066D-11,
     *                         0.144721D-11, 0.138082D-10, 0.914113D-12,
     *                         0.147837D-12, 0.195621D-11, 0.129757D-12,
     *                         0.136429D-13, 0.250465D-12, 0.166371D-13,
     *                         0.113379D-14, 0.288894D-13, 0.192092D-14,
     *                         0.845213D-16, 0.299003D-14, 0.198959D-15,
     *                         0.562496D-17, 0.276346D-15, 0.183981D-16,
     *                         0.332222D-18, 0.226723D-16, 0.151001D-17,
     *                         0.172872D-19, 0.163915D-17, 0.109200D-18,
     *                         0.785321D-21, 0.103480D-18, 0.689517D-20,
     *                         0.307886D-22, 0.563885D-20, 0.375787D-21,
     *                         0.102630D-23, 0.261299D-21, 0.174154D-22,
     *                         0.285163D-25, 0.100944D-22, 0.672832D-24,
     *                         0.642589D-27, 0.316302D-24, 0.210839D-25/
      DATA((SE40(I,J),I=1,3),J=37,40)
     *                        /0.112817D-28, 0.772286D-26, 0.514807D-27,
     *                         0.144773D-30, 0.137838D-27, 0.918858D-29,
     *                         0.120779D-32, 0.159956D-29, 0.106632D-30,
     *                         0.491605D-35, 0.905709D-32, 0.603784D-33/
 
C  VERSION NUMBER AND DATE OF RELEASE
      DATA VERNUM / 5.201 /
      DATA MVDATE / 19970416 /
C                  -YYYYMMDD-
      DATA VERDAT / 'APRIL     16, 1997' /
C                    ----+----+----+---
      END
