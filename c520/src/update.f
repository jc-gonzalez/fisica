      SUBROUTINE UPDATE( HNEW,THCKHN,IPAS )
 
C-----------------------------------------------------------------------
C  UPDATE(S PARTICLE PARAMETERS)
C
C  UPDATES PARTICLE PARAMETERS TO OBSERVATION LEVEL WITH NUMBER IPAS
C                           OR TO POINT OF INTERACTION OR DECAY (IPAS=0)
C  FOR CHARGED PARTICLES THE ENERGY LOSS IS COMPUTED FOR THE WHOLE STEP,
C  SUBDIVIDED BY THE BOUNDARIES OF THE ATMOSPHERIC LAYERS.
C  THE PARTICLE IS FLYING THE 1ST HALF (DH/2) WITH INITIAL ENERGY
C  AND ANGLE AND THE 2ND HALF WITH FINAL ENERGY AND ANGLE.
C  THE TIME CALCULATION FOLLOWS THIS SIMPLIFICATION.
C  CHARGED PARTICLES ARE DEFLECTED IN THE EARTH MAGNETIC FIELD.
C  THE ANGLE OF DEFLECTION BY MULTIPLE SCATTERING IS COMPUTED ONLY
C  FOR MUONS AND ONLY ONCE FOR THE WHOLE STEP.
C  IF PARTICLES COME TO REST BY STOPPING, THEIR PATH TO THE STOPPING
C  POINT IS CALCULATED.
C  CERENKOV RADIATION IS CALCULATED ONLY FOR LOWEST OBSERVATION LEVEL
C  THIS SUBROUTINE IS CALLED FROM MAIN, BOX3, AND MUTRAC
C  ARGUMENTS:
C   HNEW   = ALTITUDE OF PARTICLE AFTER UPDATE
C   THCKHN = THICKNESS OF HNEW
C   IPAS   = 0  TRANSPORT TO END OF RANGE OF PARTICLE
C       .NE. 0  TRANSPORT TO PASSAGE OF OBSERVATION LEVEL IPAS
C
C  REDESIGN: D. HECK    IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,ATMOS2.
      COMMON /ATMOS2/  HLAY,THICKL
      DOUBLE PRECISION HLAY(5),THICKL(5)
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,ELABCT.
      COMMON /ELABCT/  ELCUT
      DOUBLE PRECISION ELCUT(4)
*KEEP,GENER.
      COMMON /GENER/   GEN,ALEVEL
      DOUBLE PRECISION GEN,ALEVEL
*KEEP,IRET.
      COMMON /IRET/    IRET1,IRET2
      INTEGER          IRET1,IRET2
*KEEP,MAGNET.
      COMMON /MAGNET/  BX,BZ,BVAL,BNORMC,BNORM,COSB,SINB,BLIMIT
      DOUBLE PRECISION BX,BZ,BVAL,BNORMC
      REAL             BNORM,COSB,SINB,BLIMIT
*KEEP,MUMULT.
      COMMON /MUMULT/  CHC,OMC,FMOLI
      DOUBLE PRECISION CHC,OMC
      LOGICAL          FMOLI
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
*KEEP,RANDPA.
      COMMON /RANDPA/  FAC,U1,U2,RD,NSEQ,ISEED,KNOR
      DOUBLE PRECISION FAC,U1,U2
      REAL             RD(3000)
      INTEGER          ISEED(103,10),NSEQ
      LOGICAL          KNOR
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
*KEEP,CERHDR.
      COMMON/CERHDR/   TPART,UPART,VPART,WPART,XPART,YPART,ZPART
      DOUBLE PRECISION TPART,UPART,VPART,WPART,XPART,YPART,ZPART
*KEND.
 
      DOUBLE PRECISION ALPHA1,ALPHA2,BETAN,DENS,DH,DR,DTHICK,ELOSS,
     *                 FNORM1,FNORM2,F1COS1,F1COS2,F1SIN1,F1SIN2,
     *                 GAMMAN,GAMSQ,GLCUT,GMSQM1,GAM0,HMIDDL,HNEW,OMEGA,
     *                 PHISCT,PHI1,RADINV,RANNOR,RHOF,
     *                 SINTH1,SINTH2,SN,SN1,SN2,SN3,SN4,
     *                 THCKHN,TH0,U10,U12,U20,U22,V,VSCAT,VVV,
     *                 V10,V12,V20,V22,W10,W12,W20,W22
      INTEGER          I,IL,ILAY,IPAS
      LOGICAL          MUS
      SAVE             VSCAT,PHISCT
      EXTERNAL         RANNOR,RHOF
      DOUBLE PRECISION CHIT,DT,GAMK,HEIGH,HNEWC,RATIO,THCKHC
      INTEGER          ICRNKV
      LOGICAL          TFLAG
      EXTERNAL         HEIGH
C-----------------------------------------------------------------------
 
      IF (DEBUG) WRITE(MDEBUG,457) (CURPAR(I),I=1,9),HNEW
  457 FORMAT(' UPDATE: CURPAR=',1P,9E10.3/
     *       '         TO HEIGHT ',0P,F11.1)
 
      IRET2  = 1
C  TOTAL HEIGHT DIFFERENCE
      DH     = MAX( H - HNEW, 1.D-10 )
C  ATMOSPHERE THICKNESS TRAVERSED
      DTHICK = (THCKHN - THICKH) / COSTHE
C  TOTAL PATH FOR UNDEFLECTED PARTICLE
      SN     = DH / COSTHE
      SN1    = 0.25D0 * SN
 
 
C  CALCULATE KINETIC ENERGY CUT
      IF ( ITYPE .EQ. 5 .OR. ITYPE .EQ. 6 ) THEN
        MUS   = .TRUE.
        GLCUT = ELCUT(2) / PAMA(ITYPE) + 1.D0
      ELSE
        MUS   = .FALSE.
        GLCUT = ELCUT(1) / PAMA(ITYPE) + 1.D0
      ENDIF
 
C  CALCULATE THE ENERGY LOSS FOR CHARGED PARTICLES
      IF ( SIGNUM(ITYPE) .NE. 0.D0 ) THEN
C  LOOK WITHIN WHICH LAYER THE PARTICLE STARTS
        IF     ( H .LE. HLAY(2) ) THEN
          ILAY = 1
          TH0  = THICKH
        ELSEIF ( H .LE. HLAY(3) ) THEN
          ILAY = 2
          TH0  = THICKH
        ELSEIF ( H .LE. HLAY(4) ) THEN
          ILAY = 3
          TH0  = THICKH
        ELSE
          ILAY = 4
          TH0    = MAX( THICKH, 2.D-4 )
        ENDIF
C  SET START VALUES FOR ITERATION
        GAM0   = GAMMA
        IL     = ILAY
 1      CONTINUE
        GAM0   = MAX( GAM0, 1.0001D0 )
        GAMSQ  = GAM0**2
        GMSQM1 = GAMSQ - 1.D0
C  ENERGY LOSS BY IONIZATION
        ELOSS  = SIGNUM(ITYPE)**2 * C(22) *
     *              ( GAMSQ * (LOG(GMSQM1) + C(23)) / GMSQM1 - 1.D0 )
C  LOOK WETHER PARTICLE PENETRATES LAYER BOUNDARY
        IF ( THICKL(IL) .LT. THCKHN  .AND.  IL .GT. 1 ) THEN
C  CALCULATE NEW START VALUES AT LAYER BOUNDARY
          GAM0 = GAM0 - ELOSS * (THICKL(IL) - TH0)
     *                              / (PAMA(ITYPE)*COSTHE)
          IF ( GAM0 .LE. 1.D0 ) THEN
            GAMMAN = 1.0001D0
            GOTO 3
          ENDIF
          TH0  = THICKL(IL)
          IL   = IL - 1
          GOTO 1
        ENDIF
C  GAMMA VALUE FOR CHARGED PARTICLES AT END OF STEP
        GAMMAN = GAM0 - ELOSS * (THCKHN-TH0) / (PAMA(ITYPE)*COSTHE)
 3      CONTINUE
 
      ELSE
C  NO LOSS FOR NEUTRAL PARTICLES
        GAMMAN = GAMMA
      ENDIF
 
C  PARTICLE HAS TO BE TRACKED TO THE CUTOFF ENERGY FOR CERENKOV PHOTONS
C  (AS NEUTRAL DO NOT LOOSE ENERGY IN UPDATE, THIS CONDITION IS
C  FULFILLED BY CHARGED PARTICLES ONLY)
C  (AS CERENKOV RUNS NOT WITH HORIZONT, NO PROGRAMMING FOR HORIZONT)
      IF ( GAMMAN .LT. GLCUT ) THEN
        GAMMAN = 0.9D0 + GLCUT * 0.1D0
 
C  SET START VALUES FOR ITERATION
        IL     = ILAY
        CHIT   = 0.D0
        GAM0   = GAMMA
        TH0    = MAX( THICKH, 2.D-4 )
 2      CONTINUE
        GAM0   = MAX( GAM0, 1.0001D0 )
        GAMSQ  = GAM0**2
        GMSQM1 = GAMSQ - 1.D0
C  ENERGY LOSS BY IONIZATION
        ELOSS  = SIGNUM(ITYPE)**2 * C(22) *
     *             ( GAMSQ * (LOG(GMSQM1) + C(23)) / GMSQM1 -1.D0 )
        ELOSS  =  ELOSS / (PAMA(ITYPE) * COSTHE)
        GAMK   = GAM0 - ELOSS * (THICKL(ILAY) - TH0)
C  LOOK WETHER PARTICLE PENETRATES LAYER BOUNDARY
        IF (GAMMAN .LT. GAMK .AND. IL. GT. 1 ) THEN
C  CALCULATE PORTION OF RANGE AND NEW START VALUES AT LAYER BOUNDARY
          CHIT = CHIT + (THICKL(IL) - TH0) / COSTHE
          GAM0 = GAMK
          TH0  = THICKL(IL)
          IL   = IL - 1
          GOTO 2
        ENDIF
C  PENETRATED MATTER THICKNESS
        CHI    = CHIT + (GAM0 - GAMMAN) / (ELOSS*COSTHE)
        IF ( DEBUG ) WRITE(MDEBUG,*)'UPDATE: GAMMAN,CHI=',
     *                                  SNGL(GAMMAN),SNGL(CHI)
C  CALCULATE CORRECTED PATH PARAMETERS
        THCKHC = THICKH + COSTHE * CHI
        HNEWC  = HEIGH(THCKHC)
        DT     = SN / (C(25) * BETA * GAMMA)
        RATIO  = .5D0 * (H-HNEWC) / DH
        DH     = H - HNEWC
        SN     = DH / COSTHE
        SN1    = 0.25D0 * SN
        TFLAG  = .TRUE.
      ELSE
        TFLAG  = .FALSE.
      ENDIF
 
C-----------------------------------------------------------------------
      IF ( IPAS .EQ. 0 ) THEN
C  UPDATE TO THE END POINT OF THE TRACK
 
        IF ( MUS ) THEN
C  COULOMB SCATTERING ANGLE (FOR MUONS ONLY)
          IF ( FMOLI) THEN
C  TREAT MUON MULTIPLE SCATTERING BY MOLIERE THEORY (SEE GEANT)
C  CALCULATE AVERAGE DENSITY AND NUMBER OF SCATTERING (OMEGA)
            DENS   = CHI/DH * COSTHE
            OMEGA  = OMC * CHI /  BETA**2
            IF ( OMEGA .LE. 20.D0 ) THEN
C  FEW SCATTERING EVENTS, APPLY PLURAL COULOMB SCATTERING
              CALL MUCOUL(OMEGA,DENS,VSCAT)
            ELSE
C  ENOUGH SCATTERING EVENTS, APPLY MOLIERE'S THEORY
              CALL MMOLIE(OMEGA,DENS,VSCAT)
            ENDIF
          ELSE
C  TREAT MUON MULTIPLE SCATTERING BY GAUSS DISTRIBUTION
            VSCAT = RANNOR( 0.D0, C(30) * SQRT( CHI/C(21) )
     *                          / (PAMA(5) * GAMMA * BETA**2) )
          ENDIF
          V = VSCAT
          CALL RMMAR( RD,1,1 )
          PHISCT = RD(1) * PI2
          IF(DEBUG)WRITE(MDEBUG,*)'UPDATE: VSCAT=',SNGL(VSCAT),
     *                                  ' PHISCT=',SNGL(PHISCT)
        ENDIF
 
C  CERENKOV RADIATION: LOOK, WHETHER PATH ENDS ABOVE LOWEST OBSERV.LEVEL
        IF ( TFLAG ) THEN
          HNEW   = HNEWC
          THCKHN = THCKHC
          IF (DEBUG) WRITE(MDEBUG,*)'UPDATE: CHANGED HNEW =',SNGL(HNEW)
        ENDIF
        IF ( HNEW .GT. OBSLEV(NOBSLV) ) THEN
          ICRNKV = 1
        ELSE
          ICRNKV = 0
        ENDIF
 
C  UPDATE TO THE OBSERVATION LEVELS
      ELSE
        IF ( MUS ) THEN
C  COULOMB SCATTERING ANGLE (FOR MUONS ONLY)
          V = VSCAT * SQRT( DTHICK / CHI )
        ENDIF
 
C  CERENKOV RADIATION: LOOK, WHETHER LOWEST OBSERVATION LEVEL
        IF ( IPAS .EQ. NOBSLV ) THEN
          ICRNKV = 1
        ELSE
          ICRNKV = 0
        ENDIF
      ENDIF
 
C  REJECT ALL PARTICLES IF BELOW KINETIC ENERGY CUT
      IF ( GAMMAN .LT. GLCUT .AND. ICRNKV .EQ. 0 ) THEN
        IF (DEBUG)
     *    WRITE(MDEBUG,*) 'UPDATE: PARTICLE ',ITYPE,' BELOW ENERGY CUT'
     *                   ,' CERENKOV LIGHT NOT CALCULATED'
        RETURN
      ENDIF
 
C-----------------------------------------------------------------------
C  CHARGED PARTICLES SUFFER IONIZATION LOSS, DEFLECTION IN MAGNETIC
C  FIELD AND MUONS IN ADDITION DO MULTIPLE COULOMB SCATTERING
 
      IF ( SIGNUM(ITYPE) .NE. 0.D0 ) THEN
C  DEFLECTION IN EARTH MAGNETIC FIELD ON FIRST HALF OF STEP
        ALPHA1 = SIGNUM(ITYPE) *
     *         MIN( 1.D0, 2.D0*SN1*BNORMC /(PAMA(ITYPE)*BETA*GAMMA) )
        SINTH1 = SQRT( 1.D0 - COSTHE**2 )
        U10    = SINTH1 * COS(-PHI)
        V10    = SINTH1 * SIN(-PHI)
        W10    = COSTHE
        FNORM1 = 1.D0 - 0.5D0*ALPHA1**2 * (1.D0 - 0.75D0*ALPHA1**2)
        F1COS1 = ( 1.D0 - FNORM1 ) * COSB
        F1SIN1 = ( 1.D0 - FNORM1 ) * SINB
        VVV = V10 * ALPHA1 * FNORM1
        U12 = U10 * (1.D0 - F1SIN1*SINB) + W10*F1SIN1*COSB + VVV*SINB
        V12 = FNORM1 * ( V10 - ALPHA1 * (U10 * SINB - W10 * COSB) )
        W12 = W10 * (1.D0 - F1COS1*COSB) + U10*F1COS1*SINB - VVV*COSB
        RADINV = 1.5D0 - 0.5D0 * ( U12**2 + V12**2 + W12**2 )
        W12 = MIN( 1.D0, RADINV * W12 )
        IF ( W12 .LE. C(29) ) THEN
          IF (DEBUG)
     *    WRITE(MDEBUG,*) 'UPDATE: PARTICLE ',ITYPE,' BELOW ANGLE CUT 1'
          RETURN
        ENDIF
        SN2 = 0.25D0 * DH / W12
        U12 = RADINV * U12
        V12 = RADINV * V12
        IF ( U12**2 + V12**2 .GT. 3.D-38 ) THEN
          PHI1 = -ATAN2( V12, U12 )
        ELSE
          PHI1 = 0.D0
        ENDIF
C  CERENKOV RADIATION: FILL PARTICLE COORDINATES INTO COMMON CERHDR
        IF ( ICRNKV .EQ. 1 ) THEN
          XPART = X + SN1 * U10 + SN2 * U12
          YPART = Y - SN1 * V10 - SN2 * V12
          TPART = T + ( SN1 + SN2 ) / ( C(25) * BETA )
          ZPART = H - DH * 0.5D0
          WPART = W12
          UPART = U12
          VPART = -V12
          CALL CERENH( SN1+SN2, BETA )
        ENDIF
 
C  CHANGE DIRECTION BY COULOMB SCATTERING (FOR MUONS ONLY)
C  BEFORE SCATTERING : DIRECTION COSINES ARE U12,V12,W12
C  AFTER  SCATTERING : DIRECTION COSINES ARE U20,V20,W20
        IF ( MUS ) THEN
          CALL ADDANG( W12,PHI1, COS(V),PHISCT, W20,PHI1 )
          IF ( W20 .LT. C(29) ) THEN
            IF (DEBUG) WRITE(MDEBUG,*) 'UPDATE: MUON BELOW ANGLE CUT'
            RETURN
          ENDIF
          SINTH2 = SQRT( 1.D0 - W20**2 )
          U20    = SINTH2 * COS( -PHI1 )
          V20    = SINTH2 * SIN( -PHI1 )
        ELSE
          U20    = U12
          V20    = V12
          W20    = W12
        ENDIF
 
C  NEW PATH LENGTH, NEW BETA VALUE BECAUSE OF IONIZATION ENERGY LOSS
        SN3    = 0.25D0 * DH / W20
        BETAN  = SQRT( GAMMAN**2 - 1.D0 ) / GAMMAN
C  DEFLECTION IN EARTH MAGNETIC FIELD ON SECOND HALF OF STEP
        ALPHA2 = SIGNUM(ITYPE) *
     *           MIN(1.D0,2.D0*SN3*BNORMC / (PAMA(ITYPE)*BETAN*GAMMAN))
        FNORM2 = 1.D0 - 0.5D0*ALPHA2**2 * (1.D0 - 0.75D0*ALPHA2**2)
        F1SIN2 = ( 1.D0 - FNORM2 ) * SINB
        F1COS2 = ( 1.D0 - FNORM2 ) * COSB
        VVV = V20 * ALPHA2 * FNORM2
        U22 = U20*(1.D0 - F1SIN2*SINB) + W20*F1SIN2*COSB + VVV*SINB
        V22 = FNORM2 * ( V20 - ALPHA2 * (U20 * SINB - W20 * COSB) )
        W22 = W20*(1.D0 - F1COS2*COSB) + U20*F1COS2*SINB - VVV*COSB
        RADINV = 1.5D0 - 0.5D0 * ( U22**2 + V22**2 + W22**2 )
        W22 = MIN( 1.D0, RADINV * W22 )
        IF ( W22 .LT. C(29) ) THEN
          IF (DEBUG)
     *    WRITE(MDEBUG,*) 'UPDATE: PARTICLE ',ITYPE,' BELOW ANGLE CUT 2'
          RETURN
        ENDIF
        SN4 = 0.25D0 * DH / W22
        U22 = RADINV * U22
        V22 = RADINV * V22
        OUTPAR(3) = W22
        IF ( U22**2 + V22**2 .GT. 3.D-38 ) THEN
          OUTPAR(4) = -ATAN2( V22, U22 )
        ELSE
          OUTPAR(4) = 0.D0
        ENDIF
C  UPDATE COORDINATES AND TIME TO THE END OF DISTANCE
        IF ( TFLAG ) THEN
          OUTPAR(6) = T + DT* ( RATIO*GAMMA + (1.D0-RATIO)*GAMMAN)
        ELSE
          OUTPAR(6) = T + (SN1 + SN2)/(BETA *C(25)) +
     *                    (SN3 + SN4)/(BETAN*C(25))
        ENDIF
        OUTPAR(7) = X + SN1*U10 + SN2*U12 + SN3*U20 + SN4*U22
        OUTPAR(8) = Y - SN1*V10 - SN2*V12 - SN3*V20 - SN4*V22
C  CERENKOV RADIATION: FILL PARTICLE COORDINATES INTO COMMON CERHDR
        IF ( ICRNKV .EQ. 1 ) THEN
          XPART = OUTPAR(7)
          YPART = OUTPAR(8)
          ZPART = HNEW
          TPART = OUTPAR(6)
          WPART = W22
          UPART = U22
          VPART = -V22
          CALL CERENH( SN3+SN4, BETAN )
C  REJECT PARTICLES AFTER PRODUCTION OF CERENKOV LIGHT
          IF ( GAMMAN .LT. GLCUT ) THEN
            IF (DEBUG) WRITE(MDEBUG,*) 'UPDATE: PARTICLE ',ITYPE,
     *           ' BELOW ENERGY CUT AFTER CREATION OF CERENKOV LIGHT'
            RETURN
          ENDIF
        ENDIF
      ELSE
 
C-----------------------------------------------------------------------
C  NEUTRAL PARTICLES
C  NO COULOMB SCATTERING, NO DEFLECTION IN MAGNETIC FIELD
 
C  HORIZONTAL PATH LENGTH
        DR        = SN * SQRT( 1.D0 - COSTHE**2 )
C  UPDATE COORDINATES AND TIME
        OUTPAR(3) = COSTHE
        OUTPAR(4) = PHI
        OUTPAR(6) = T + SN / ( C(25) * BETA )
        OUTPAR(7) = X + DR * COS(PHI)
        OUTPAR(8) = Y + DR * SIN(PHI)
      ENDIF
 
C-----------------------------------------------------------------------
      OUTPAR( 1) = CURPAR(1)
      OUTPAR( 2) = GAMMAN
      OUTPAR( 5) = HNEW
      OUTPAR( 9) = GEN
      OUTPAR(10) = ALEVEL
 
C  REGULAR END OF UPDATE
      IRET2 = 0
 
 
      IF (DEBUG) WRITE(MDEBUG,458) (OUTPAR(I),I=1,9)
  458 FORMAT(' UPDATE: OUTPAR=',1P,9E10.3)
 
      RETURN
      END
