      SUBROUTINE STAEND
 
C-----------------------------------------------------------------------
C  STA(RT) END
C
C  SUBROUTINE FOR GETTING THE CONTROL PRINTOUT OF THE CONSTANT ARRAYS
C  PRINT CONTROL OUTPUT
C  THIS SUBROUTINE IS CALLED FROM MAIN AND START
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,ANNI.
      COMMON /ANNI/    CAN,CANN
      DOUBLE PRECISION CAN(50),CANN(50)
*KEEP,ATMOS.
      COMMON /ATMOS/   AATM,BATM,CATM,DATM
      DOUBLE PRECISION AATM(5),BATM(5),CATM(5),DATM(5)
*KEEP,EDECAY.
      COMMON /EDECAY/  CETA
      DOUBLE PRECISION CETA(5)
*KEEP,KAONS.
      COMMON /KAONS/   CKA
      DOUBLE PRECISION CKA(80)
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
*KEEP,STRBAR.
      COMMON /STRBAR/  CSTRBA
      DOUBLE PRECISION CSTRBA(11)
*KEND.
 
      INTEGER I
C-----------------------------------------------------------------------
 
C  PRINT CONTROL OUTPUT
      WRITE(MONIOU,103) (C(I),I=1,50)
  103 FORMAT (//' ',10('='),' CONSTANTS AND PARAMETERS ',43('=')
     *        //' PHYSICAL CONSTANTS (C)' // (1P,4(E15.8,1X),E15.8) )
      WRITE(MONIOU,110) (CKA(I),I=1,80)
  110 FORMAT (//' CONSTANTS FOR KAONS CKA(1) TO CKA(40)'
     *        // (1P,4(E15.8,1X),E15.8) )
      WRITE(MONIOU,114) (CETA(I),I=1,5)
  114 FORMAT (//' CONSTANTS FOR ETAS CETA(1) TO CETA(5)'
     *        // (1P,4(E15.8,1X),E15.8) )
      WRITE(MONIOU,115) (CSTRBA(I),I=1,11)
  115 FORMAT (//' CONSTANTS FOR STRANGE BARYONS CSTRBA(1) TO ',
     *            'CSTRBA(11)'// (1P,4(E15.8,1X),E15.8) )
      IF ( .NOT. GHEISH ) THEN
        WRITE(MONIOU,206) (CAN(I),I=1,30)
  206   FORMAT (//' ANNIHILATION PARAMETERS, SET 1 (CAN)'
     *          // (1P,4(E15.8,1X),E15.8) )
        WRITE(MONIOU,209) (CANN(I),I=1,30)
  209   FORMAT (//' ANNIHILATION PARAMETERS, SET 2 (CANN)'
     *          // (1P,4(E15.8,1X),E15.8) )
        WRITE(MONIOU,60) (CC(I),I=1,12)
   60   FORMAT (//' THRESHOLD ENERGIES OF INTERACTION INTERVALS IN '
     *             ,'GEV (CC)'// (1P,4(E15.8,1X),E15.8) )
      ENDIF
 
      WRITE(MONIOU,303) (AATM(I),BATM(I),CATM(I)*1.E-5,I=1,4),
     *                    AATM(5),CATM(5)*1.E5
  303 FORMAT (//' ',10('='),' ATMOSPHERE ', 57('=') /
     * ' ( US STANDARD ATMOSPHERE PARAMETRIZED BY LINSLEY )'//
     * ' HEIGHT H IN KM GIVES THICKNESS OF ATMOSPHERE T IN G/CM**2'/1P,
     * ' H =   0 ...  4 KM ---> T = ',
     * E12.5,' +',E11.4,' * EXP ( - H /',E11.4,' )'/
     * ' H =   4 ... 10 KM ---> T = ',
     * E12.5,' +',E11.4,' * EXP ( - H /',E11.4,' )'/
     * ' H =  10 ... 40 KM ---> T = ',
     * E12.5,' +',E11.4,' * EXP ( - H /',E11.4,' )'/
     * ' H =  40 .. 100 KM ---> T = ',
     * E12.5,' +',E11.4,' * EXP ( - H /',E11.4,' )'/
     * ' H = 100 ...    KM ---> T = ',
     * E12.5,' -',E11.4,' * H ' )
 
      RETURN
      END
