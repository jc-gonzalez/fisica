C=======================================================================

      DOUBLE PRECISION FUNCTION HEIGH( ARG )

C-----------------------------------------------------------------------
C  HEIGH(T AS FUNCTION OF THICKNESS)
C
C  CALCULATES HEIGHT DEPENDING ON THICKNESS OF ATMOSPHERE
C  (US STANDARD ATMOSPHERE)
C  THIS FUNCTION IS CALLED FROM MAIN, BOX2, BOX3, ININKG, INPRM,
C  MUTRAC, AND UPDATE
C  ARGUMENT:
C   ARG    = MASS OVERLAY IN G/CM**2
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


      DOUBLE PRECISION ARG,H,R,RT
      PARAMETER (RT=6348.0D5)

C-----------------------------------------------------------------------

CC    IF ( DEBUG ) WRITE(MDEBUG,*) 'HEIGH : ARG=',SNGL(ARG)


c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> Modification (HZA trick) cancelled >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>> JCG Wed Sep 21 10:49:14 MET DST 1998 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      IF     ( ARG .GT. 631.1D0 ) THEN
        HEIGH = CATM(1) * LOG ( BATM(1) / (ARG - AATM(1)) )
      ELSEIF ( ARG .GT. 271.7D0 ) THEN
        HEIGH = CATM(2) * LOG ( BATM(2) / (ARG - AATM(2)) )
      ELSEIF ( ARG .GT. 3.0395D0 ) THEN
        HEIGH = CATM(3) * LOG ( BATM(3) / (ARG - AATM(3)) )
      ELSEIF ( ARG .GT. 0.00128292D0 ) THEN
        HEIGH = CATM(4) * LOG ( BATM(4) / (ARG - AATM(4)) )
      ELSE
        HEIGH = (AATM(5) - ARG) * DATM(5)
      ENDIF
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c      IF     ( ARG .GT. 631.1D0 ) THEN
c        H = CATM(1) * LOG ( BATM(1) / (ARG - AATM(1)) )
c      ELSEIF ( ARG .GT. 271.7D0 ) THEN
c        H = CATM(2) * LOG ( BATM(2) / (ARG - AATM(2)) )
c      ELSEIF ( ARG .GT. 3.0395D0 ) THEN
c        H = CATM(3) * LOG ( BATM(3) / (ARG - AATM(3)) )
c      ELSEIF ( ARG .GT. 0.00128292D0 ) THEN
c        H = CATM(4) * LOG ( BATM(4) / (ARG - AATM(4)) )
c      ELSE
c        H = (AATM(5) - ARG) * DATM(5)
c      ENDIF
c
cC************************************************************************
cC     Modificacion hecha por Aitor (5-febrero-98)
c
c      HEIGH = (COS(aitoth))**2 * (-RT + SQRT(RT**2 + 
c     * ((H**2 + (2.0D0*RT*H))/(COS(aitoth))**2)))
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

C************************************************************************

C      R = SQRT(CURPAR(7)**2+CURPAR(8)**2)
C      HEIGH = SQRT((RT+H)**2-R**2)-RT
c      print *,'HEIGH>>',ARG,r,heigh,curpar(7),curpar(8)

      RETURN
      END
