C=======================================================================

      DOUBLE PRECISION FUNCTION THICK( ARG )

C-----------------------------------------------------------------------
C  THICK(NESS OF ATMOSPHERE)
C
C  CALCULATES THICKNESS (G/CM**2) OF ATMOSPHERE DEPENDING ON HEIGHT (CM)
C  (US STANDARD ATMOSPHERE)
C  THIS FUNCTION IS CALLED FROM MAIN, BOX2, BOX3, CERENE, CERENH, EGS4,
C  ELECTR, ININKG, INPRM, NKG, PHOTON, AND START
C  ARGUMENT:
C   ARG    = HEIGHT IN CM
C-----------------------------------------------------------------------

      IMPLICIT NONE
*KEEP,ATMOS.
      COMMON /ATMOS/   AATM,BATM,CATM,DATM
      DOUBLE PRECISION AATM(5),BATM(5),CATM(5),DATM(5)
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
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c Try
c------------------------------------------------------------
*KEEP,PARPAR.
      COMMON /PARPAR/  CURPAR,SECPAR,PRMPAR,OUTPAR,C,
     *                 E00,E00PN,PTOT0,PTOT0N,THICKH,ITYPE,LEVL
      DOUBLE PRECISION CURPAR(14),SECPAR(14),PRMPAR(14),OUTPAR(14),
     *                 C(50),E00,E00PN,PTOT0,PTOT0N,THICKH
      INTEGER          ITYPE,LEVL
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*KEND.
C*******************************************************************
C     Modificado por Aitor (5-febrero-98)
      
      common /aitor/   aitoth
      double precision aitoth
C*******************************************************************

      DOUBLE PRECISION ARG,H,RT
      PARAMETER (RT=6348.0D5)
C-----------------------------------------------------------------------

CC    IF ( DEBUG ) WRITE(MDEBUG,*) 'THICK : ARG=',SNGL(ARG)

c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> Modification (HZA trick) cancelled >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> JCG Wed Sep 21 10:49:14 MET DST 1998 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      IF     ( ARG .LT. 4.D5 ) THEN
        THICK = AATM(1) + BATM(1) * EXP ( -ARG * DATM(1) )
      ELSEIF ( ARG .LT. 1.D6 ) THEN
        THICK = AATM(2) + BATM(2) * EXP ( -ARG * DATM(2) )
      ELSEIF ( ARG .LT. 4.D6 ) THEN
        THICK = AATM(3) + BATM(3) * EXP ( -ARG * DATM(3) )
      ELSEIF ( ARG .LT. 1.D7 ) THEN
        THICK = AATM(4) + BATM(4) * EXP ( -ARG * DATM(4) )
      ELSE
        THICK = AATM(5) - ARG * CATM(5)
      ENDIF
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cC*******************************************************************
cC     Modificado por Aitor (5-febrero-98)
c
c      H = -RT + SQRT(RT**2 + (ARG/COS(aitoth))**2 +(2.0D0*RT*ARG))
cC*******************************************************************
c
cC      R = SQRT(CURPAR(7)**2+CURPAR(8)**2)
cC      H = SQRT((RT+ARG)**2+R**2)-RT
cc      print *,'THICK>>',arg,r,h,curpar(7),curpar(8)
c
c      IF     ( H .LT. 4.D5 ) THEN
c        THICK = AATM(1) + BATM(1) * EXP ( -H * DATM(1) )
c      ELSEIF ( H .LT. 1.D6 ) THEN
c        THICK = AATM(2) + BATM(2) * EXP ( -H * DATM(2) )
c      ELSEIF ( H .LT. 4.D6 ) THEN
c        THICK = AATM(3) + BATM(3) * EXP ( -H * DATM(3) )
c      ELSEIF ( H .LT. 1.D7 ) THEN
c        THICK = AATM(4) + BATM(4) * EXP ( -H * DATM(4) )
c      ELSE
c        THICK = AATM(5) - H * CATM(5)
c      ENDIF
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      RETURN
      END