c----------------------------------------------------------------------*
c
      SUBROUTINE Read_Cfile
c
      COMMON/Cfile/ cerbuf(273,30) , irecor , n_C_blks , isb , ieof
      COMMON /check/ iCheck, iCheck_Energy, iCheck_Theta, iCheck_Phi
      COMMON /LUNs/ Lun_Inp , Lun_Out , Lun_CORS_Cfile , Lun_airPM
     *         , Lun_stat , Lun_airQT
      COMMON / C_header / C_header(20)
      READ ( Lun_CORS_Cfile  , rec = irecor+1 , err = 10)
     *      ((cerbuf(i,isb),i=1,273),isb=1,n_C_blks)
c>>>
c Convert from ie3 to OS
c      call conv_ie3tos( cerbuf, 273, n_C_blks )
c>>>
      IF ( (irecor .EQ. 0) .AND. (iCheck .GE. 1) ) THEN
        CALL Check_Inputs
c
c Get header for output data file
c
        DO i=1, 14
          C_header(i) = cerbuf( i , 2 )
        ENDDO
      ENDIF
c
      irecor = irecor + 1
      isb = 1
      GOTO 20
   10 ieof = 1
      WRITE( Lun_Out ,*) 'end of Cfile at record ', irecor
   20 RETURN
      END
c
c----------------------------------------------------------------------*
c
      SUBROUTINE Check_Inputs
C
C GET THE ENERGY, THETA AND TYPE OF INCIDENT PARTICLE
c Also, get and check array parameters 
C
      COMMON/Cfile/ cerbuf(273,30) , irecor , n_C_blks , isb , ieof
      COMMON/IncPart_prop/ IncPartType , IncEnergy , IncTheta , IncPhi
      REAL IncPartType , IncEnergy(2) , IncTheta(2) , IncPhi(2)
      COMMON /LUNs/ Lun_Inp , Lun_Out , Lun_CORS_Cfile , Lun_airPM
     *         , Lun_stat , Lun_airQT
      COMMON /check/ iCheck, iCheck_Energy, iCheck_Theta, iCheck_Phi
      COMMON /check_2/ iCheck_Carray, iCheck_Chut
      common /C_Array_2/ C_ArraySide , C_HutSide

      DATA pi / 3.141593 /
C
c       IncPartType -> cerbuf(1,2)
c       IncEnergy -> cerbuf(2,2)
c       IncTheta -> cerbuf(3,2)
c       IncPhi -> cerbuf(4,2)
c       C_ArraySide -> cerbuf(10,2)
c       C_HutSide -> cerbuf(12,2)
C
      PartType = cerbuf(1,2)
      Energy = cerbuf(2,2)
      Theta = cerbuf(3,2) / pi * 180.
      Phi = cerbuf(4,2) / pi * 180.

      CALL compare( PartType, IncPartType, IncPartType, JFLAG )
      IF ( JFLAG . ne . 0 ) THEN
        WRITE( Lun_Out ,*)' the incident particle type is not
     *  what was expected'
        write( Lun_Out ,*)' Instead , it is ', PartType
        STOP
      endif
C
c----------------------------------------------------------------------*
      if ( iCheck_Energy .ne. 1 ) goto 10
      call compare( Energy, IncEnergy(1), IncEnergy(2), JFLAG )
      if (JFLAG. ne. 0) then
        write( Lun_Out ,*)'the incident energy is not in the
     *  expected range'
        write( Lun_Out ,*)' Instead , it is ', Energy, ' GeV'
        STOP
      endif
C
   10 continue

      if ( iCheck_Theta .ne. 1 ) goto 20
      call compare( Theta, IncTheta(1), IncTheta(2), JFLAG )
      if (JFLAG. ne. 0) then
        write( Lun_Out ,*)'the incident angle theta is not in the
     *  expected range'
        write( Lun_Out ,*)' Instead , it is ', Theta, ' deg'
        STOP
      endif
C
   20 continue
      if ( iCheck_Phi .ne. 1 ) goto 30
      call compare( Phi, IncPhi(1), IncPhi(2), JFLAG )
      if (JFLAG. ne. 0) then
        write( Lun_Out ,*)'the incident angle phi is not in the
     *  expected range'
        write( Lun_Out ,*)' Instead , it is ', Phi, ' deg'
        STOP
      endif
C
   30 continue
      if ( iCheck_Carray .ne. 1 ) goto 40
      call compare( C_ArraySide, cerbuf(10,2), cerbuf(10,2), JFLAG )
      if (JFLAG. ne. 0) then
        write( Lun_Out , '(a,a)')
     &    ' The side length of the C-array used in CORSIKA',
     &    ' is not the expected one'
        write( Lun_Out ,*)' Instead , it is ', cerbuf(10,2), ' m'
        STOP
      endif
C
   40 continue
      if ( iCheck_Chut .ne. 1 ) goto 50
      call compare( C_HutSide, cerbuf(12,2), cerbuf(12,2), JFLAG )
      if (JFLAG. ne. 0) then
        write( Lun_Out , '(a,a)')
     &    ' The side length of the Chut used in CORSIKA',
     &    ' is not the expected one'
        write( Lun_Out ,*)' Instead , it is ', cerbuf(12,2), ' m'
        STOP
      endif
c
   50 continue

      return
      end
c
c ---------------------------------------------------------------------*
c
      SUBROUTINE compare(a,b1,b2,JFLAG)
C
C COMPARES THE VALUES a AND b AND DECIDES IF THEY ARE EQUAL IF THEY
C DIFFER IN LESS THAN SOME PER CENT ( JFLAG = 0 )
      data RelativeError / 1.e-4 /
C
      JFLAG = 0
      c1 = b1 * (1.0 - RelativeError)
      c2 = b2 * (1.0 + RelativeError)
      if ( (a .lt. c1) .or. (a .gt. c2) ) JFLAG = 1
      return
      end
c
c ---------------------------------------------------------------------*
c
      SUBROUTINE Get_NSLNoise
c
      parameter ( nbins = 2000 )
      common /NSL/ Av_NSLNoise , Av_NSLCurrent , current_factor ,
     *         Gain_PM
      common /histog3/ nbins_PM , TimeWindow_PM , time_shift_PM
      common /TrTimePM/ nTrTime , TrTime_PM , Dist_TrTime_PM
      dimension TrTime_PM(100) , Dist_TrTime_PM(100)
      dimension NSLNoise( nbins )
      real NSLNoise
c
      call vzero( NSLNoise , nbins )

      do 20 iNoise = 1 , nbins_PM + nTrTime - 1
        jchan1 = iNoise - nTrTime + 1
        jchan1 = max0( 1 , jchan1 )
        jchan2 = iNoise
        jchan2 = min0( nbins_PM , jchan2 )
corti--
c actualizacion de poissn -> rnpssn
c        call poissn( Av_NSLNoise , n , ierror )
        call rnpssn( Av_NSLNoise , n , ierror )
corti
        do 30 jchan = jchan1 , jchan2
          jTr = nTrTime - iNoise + jchan
          if ( (jTr .lt. 1) .or. (jTr .gt. nTrTime) ) goto 30
          NSLNoise(jchan) = NSLNoise(jchan) +
     *                            n * Gain_PM * current_factor *
     *                            Dist_TrTime_PM(jTr)
   30   continue
   20 continue

      call hpak( 1500 , NSLNoise )
      return

      end
c
c ---------------------------------------------------------------------*
c
      SUBROUTINE Atmos_absorption( height , cos_Theta )
c
      parameter ( nlambdas = 4 )
      common /TRAtmos/ TR_Atmos(nlambdas)
      common /cerenkov/ C_spectrum(nlambdas) , lambda(nlambdas)
      real lambda , L_Mie
      data H_Rayleigh / 7.1e5 /
      data H_LaPalma / 2.2e5 /
      data Rho_0 / 1.29e-3 /
      data XR_400nm / 2970. /
c
      data L_Mie / 14e5 /
      data H_Mie / 1.2e5 /
c     ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc*
c
c calculate absorption due to scattering Rayleigh
c
      do i=1,nlambdas
        f1 = Rho_0 / XR_400nm * ( 400. / lambda(i) ) ** 4
     *               * H_Rayleigh / cos_Theta
        TR_Atmos(i) = exp( f1 * ( exp( -height / H_Rayleigh ) -
     *                        exp( -H_LaPalma / H_Rayleigh ) ) )
      enddo
c
c calculate absorption due to aerosol scattering
c
      f2 = H_Mie / L_Mie / cos_Theta
      TRAtmos_Mie = exp( f2 * ( exp( -height / H_Mie ) -
     *                exp( -H_LaPalma / H_Mie ) ) )
c
      do i=1,nlambdas
        TR_Atmos(i) = TR_Atmos(i) * TRAtmos_Mie
      enddo
c
      return
      end
c
c ---------------------------------------------------------------------*
c
      SUBROUTINE Init_AIROBICC
c
      parameter ( nlambdas = 4 )
      parameter ( mempaw =   30 000 000 )
      parameter ( nhuts = 1200 )
      parameter ( nbins = 2000 )
      common /pawc/ hpaw(mempaw)
      common /Chutgeom/ r_cone(0:3),zu(0:3),ta(3),ca(3),nz(3),zd(3)
      real nz
      common /sphere_PM/ r_sph,zu_sph,sph1,sph2
      common /NSL/ Av_NSLNoise , Av_NSLCurrent , current_factor ,
     *         Gain_PM
      common /C_Array/ C_ArrayHuts , C_ArrayGrid , C_Array_xmax ,
     *         C_Array_ymax , imiddle , jmiddle
      integer C_ArrayHuts
      common /C_Array_2/ C_ArraySide , C_HutSide
      common /TrTimePM/ nTrTime , TrTime_PM , Dist_TrTime_PM
      dimension TrTime_PM(100) , Dist_TrTime_PM(100)
      common /cerenkov/ C_spectrum(nlambdas) , lambda(nlambdas)
      real lambda
      COMMON /cerenkov_2/ extended_spectrum_factor
c ---------------------------------------------------------------------*
      common /histog/ nC_bins , C_TimeWindow , C_time_shift
      common /histog2/ nbins_Amp1_2 , TimeWindow_Amp1_2
      common /histog3/ nbins_PM , TimeWindow_PM , time_shift_PM
      common /histog4/ Window_shift
      common /poiscalls/ npois_calls
c
c ATS stands for Amplifier Time Spread
c
      common / Amplifiers / Gain_Amp1 , Gain_Amp1_2 , FWHM_ATS
     *         , ATS_Dist( 200 ) , nbins_ATS
      common / CFD / CFD_delay1 , CFD_delay2 , U_Threshold
      common /ADC_TDC / q_ADC1( nhuts ), q_ADC1_2( nhuts )
     *         , TDC1_2( nhuts ) , Gate_qADC1_2
      common / anodePM / Resist_befAMP , currentPM

      DIMENSION currentPM(nbins)
c
      data C_ArrayHuts / 27 / C_ArrayGrid / 15. /
      data r_cone / 10. , 12.35 , 15. , 20. /
      data zu / 0. , 2.6 , 10.1 , 46.7 /
      data r_sph / 11. / , zu_sph / 6.418 /
      data lambda / 325. , 375. , 425. , 475. /
      data nTrTime / 30 /
      data electron_charge / 1.6e-19 /
c
c change Gate_qADC1_2 25 -> 30 ns (20-6-95)
c
      data Gate_qADC1_2 / 30. /
      data C_TimeWindow / 100. /
      data TimeWindow_PM / 800. /
      data TimeWindow_Amp1_2 / 600. /
      data FWHM_ATS / 7. /
      data Gain_Amp1 / 25. / , Gain_Amp1_2 / 500. /
      data Transm_cable / 0.6 /
      data Resist_befAMP / 50 /
c
c----------------------------------------------------------------------*
      data ( Dist_TrTime_PM(i) , i = 1 , 5 ) / 0.0 , 0.0 , 0.0 , 0.0
     *        , 0.0 /
      data ( Dist_TrTime_PM(i) , i = 6 , 10 ) / 0.4 , 1.0 , 2.1 , 4.9
     *        , 7.9  /
      data ( Dist_TrTime_PM(i) , i = 11 , 15 ) /  9.0 , 9.7 , 10.0
     *        , 9.9 , 9.6 /
      data ( Dist_TrTime_PM(i) , i = 16 , 20 ) / 9.0 , 7.6 , 5.5
     *        , 4.0 , 2.9 /
      data ( Dist_TrTime_PM(i) , i = 21 , 25 ) / 2.1 , 1.5 , 1.0
     *        , 0.7 , 0.5 /
      data ( Dist_TrTime_PM(i) , i = 26 , 30 ) / 0.3 , 0.2 , 0.1
     *        , 0.0 , 0.0 /
c----------------------------------------------------------------------*
      Gain_Amp1 = Gain_Amp1 * Transm_cable * 0.3
c
      Gain_Amp1_2 = Gain_Amp1_2 * Transm_cable * 0.6
c
c Read input variables specified by users from LUN = Lun_Inp
c
      call Read_DataCards
c
c Warm up poisson generator
c
      do i=1, npois_calls
corti
c        call poissn( 1000. , n , ierror )
        call rnpssn( 1000. , n , ierror )
corti
      enddo
c
c Get the Amplifier Time Spread (ATS) distribution
c
      nbins_ATS = FWHM_ATS * 2 * 3
      sigma_ATS = FWHM_ATS / 2 / sqrt( alog(4.) )
      sum_ATS_Dist = 0.
      do i=1, nbins_ATS
        xx = (i - nbins_ATS/2)/2 /sigma_ATS
        xx = xx ** 2 / 2
        ATS_Dist(i) = exp( -xx )
        sum_ATS_Dist =  sum_ATS_Dist + ATS_Dist(i)
      enddo
c
      do i=1, nbins_ATS
        ATS_Dist(i) = ATS_Dist(i) / sum_ATS_Dist
      enddo
c
c Normalize the TrTime distribution
c
      sum_DistTrTime = 0.
      do i=1,nTrTime
        TrTime_PM(i) = i * 0.5 - 0.25
        sum_DistTrTime = sum_DistTrTime + Dist_TrTime_PM(i)
      enddo
c
c----------------------------------------------------------------------*
c
      do i=1,nTrTime
        Dist_TrTime_PM(i) = Dist_TrTime_PM(i) / sum_DistTrTime
      enddo
      sum_lambda_2 = 0.
      do i=1,nlambdas
        sum_lambda_2 = sum_lambda_2 + 1. / lambda(i) ** 2
      enddo
      do i=1,nlambdas
        C_spectrum(i) = 1. / sum_lambda_2 / lambda(i) ** 2
      enddo
c
c Correct for change in C-spectral range: 300-450 nm -> 300-500 nm
c
      extended_spectrum_factor = 1. / (1. - C_spectrum(4))
c
      C_ArrayHuts = 2 * int( (C_ArraySide - C_HutSide)/ 2 / C_ArrayGrid)
     &            + 1

      imiddle = ( C_ArrayHuts + 1 ) / 2
      jmiddle = ( C_ArrayHuts + 1 ) / 2
c
      C_Array_xmax = (jmiddle-1) * C_ArrayGrid
      C_Array_ymax = (imiddle-1) * C_ArrayGrid
c
c conversion factor to get anode current in microAmpere
c
      current_factor = electron_charge * 2 * 1e9 * 1e6
      Av_NSLNoise = Av_NSLCurrent / current_factor / Gain_PM
c
c----------------------------------------------------------------------*
c
c calculate geometrical parameters of the C-hut mirrors
c
      do i = 1,3
        a = r_cone(i-1) * zu(i) - r_cone(i) * zu(i-1)
        b = r_cone(i) - r_cone(i-1)
        zd(i) = a / b
        ta(i) = r_cone(i) / ( zu(i) + zd(i) )
        ca(i) = 1 / sqrt ( 1 + ta(i) ** 2 )
        nz(i) = ta(i) * ca(i)
      enddo
c
c calculate geometrical parameters for the Photocathode
c
      zd_sph = r_sph - zu_sph
      sph1 = 2 * zd_sph
      sph2 = zd_sph ** 2 - r_sph ** 2
c
c Book histograms for C_pulse timing
c
      call hlimit(mempaw)
c
      nC_bins = C_TimeWindow * 2
      C_time_shift = 10.

      nbins_PM = TimeWindow_PM * 2.
      time_shift_PM = TimeWindow_PM / 2.

      nbins_Amp1_2 = TimeWindow_Amp1_2 * 2
      Window_shift = (TimeWindow_PM - TimeWindow_Amp1_2) / 2
c
      CALL hbook1(3001,'eas_xcore(m)',200,-40.,40.,0.)
      CALL hbook1(3002,'eas_ycore(m)',200,-40.,40.,0.)
      CALL hbook1(3003,'caos_xcore(m)',200,-40.,40.,0.)
      CALL hbook1(3004,'caos_ycore(m)',200,-40.,40.,0.)
      DO id = 3001,3004
        CALL hidopt(id,'STAT')
      ENDDO
c
      RETURN
      END
c
c ---------------------------------------------------------------------*
c
      SUBROUTINE Init_Histos
c
      COMMON/Cfile/ cerbuf(273,30) , irecor , n_C_blks , isb , ieof
      common /C_Array/ C_ArrayHuts , C_ArrayGrid , C_Array_xmax ,
     *         C_Array_ymax , imiddle , jmiddle
      integer C_ArrayHuts
      COMMON / C_header / C_header(20)
      equivalence (C_header(8), x_core) , (C_header(9), y_core)
      common /EASfront/ X_delay1 , Y_delay1, X_delay2, Y_delay2
c ---------------------------------------------------------------------*
      common /histog/ nC_bins , C_TimeWindow , C_time_shift

      logical hexist
      real LightSpeed
      character*40 HistTitle
c
      data pi / 3.141593 /
      data LightSpeed / 30. /
c
c Book histograms for C_pulse timing
c
      Theta_Inc = cerbuf(3,2)
      Phi_Inc = cerbuf(4,2) + pi
c
      u_Inc = sin( Theta_Inc ) * cos( Phi_Inc )
      X_delay1 = - C_ArrayGrid * u_Inc / ( LightSpeed / 100. )
      X_delay2 = x_core * u_Inc / LightSpeed

      v_Inc = sin( Theta_Inc ) * sin( Phi_Inc )
      Y_delay1 = - C_ArrayGrid * v_Inc / ( LightSpeed / 100. )
      Y_delay2 = y_core * v_Inc / LightSpeed
c
      HistTitle = ' PM-Current x = 111111 m , y = 111111 m$'
c
      DO 10 j=1,C_ArrayHuts
        jhut = j
        X_tmin = X_delay1 * ( jhut - jmiddle ) + X_delay2
        xhut = C_ArrayGrid * ( jhut - jmiddle )
        WRITE(HistTitle(17:22),'(f6.1)') xhut
        DO 10 i=1,C_ArrayHuts
          ihut = i
          Y_tmin = Y_delay1 * ( ihut - imiddle ) + Y_delay2
          tmin = X_tmin + Y_tmin - C_time_shift
          tmax = tmin + C_TimeWindow
          id_hut = C_ArrayHuts * ( ihut - 1 ) + jhut
          yhut = C_ArrayGrid * ( ihut - imiddle )
          WRITE(HistTitle(32:37),'(f6.1)') yhut
          if ( hexist(id_hut) ) call hdelet( id_hut)
          CALL hbook1( id_hut , HistTitle , nC_bins ,
     *                       tmin , tmax , 0. )
c----------------------------------------------------------------------*
   10 CONTINUE

c
      RETURN
      END
c
c----------------------------------------------------------------------*
c
      SUBROUTINE Read_DataCards
c
      COMMON /datac1/ InpDirectory , OutDirectory , dsn0
      CHARACTER*60 InpDirectory , OutDirectory
      CHARACTER*9 dsn0
      COMMON /datac2/ i_C_file1 , n_C_files , iAtmos_absor
      COMMON/Cfile/ cerbuf(273,30) , irecor , n_C_blks , isb , ieof
      COMMON /check/ iCheck, iCheck_Energy, iCheck_Theta, iCheck_Phi
      COMMON /check_2/ iCheck_Carray, iCheck_Chut
      COMMON/IncPart_prop/ IncPartType , IncEnergy , IncTheta , IncPhi
      REAL IncPartType , IncEnergy(2) , IncTheta(2) , IncPhi(2)
      COMMON /NSL/ Av_NSLNoise , Av_NSLCurrent , current_factor ,
     *         Gain_PM
      COMMON /poiscalls/ npois_calls
      COMMON /LUNs/ Lun_Inp , Lun_Out , Lun_CORS_Cfile , Lun_airPM
     *         , Lun_stat , Lun_airQT
      COMMON / CFD / CFD_delay1 , CFD_delay2 , U_Threshold
      COMMON /C_Array/ C_ArrayHuts , C_ArrayGrid , C_Array_xmax ,
     *         C_Array_ymax , imiddle , jmiddle
      INTEGER C_ArrayHuts
      common /C_Array_2/ C_ArraySide , C_HutSide
c
      CHARACTER*10  KEYW
      CHARACTER*80 LINE
c
C----------------------------------------------------------------------*
c
      DO i=1,60
        InpDirectory(i:i) = ' '
        OutDirectory(i:i) = ' '
      ENDDO
      dsn0 = 'CER      '
      IncPartType = 1.
      IncEnergy(1) = 1e5
      IncEnergy(2) = 1e5
      IncTheta(1) = 0.
      IncTheta(2) = 0.
      IncPhi(1) = 0.
      IncPhi(2) = 0.
      i_C_file1 = 1
      n_C_files = 1
      n_C_blks = 30
      iAtmos_absor = 1
      iCheck = 0
      iCheck_Energy = 0
      iCheck_Theta = 0
      iCheck_Phi = 0
      iCheck_Carray = 0
      iCheck_Chut = 0
      Av_NSLCurrent = 32.
      Gain_PM = 8e3
      CFD_delay1 = 5.
      CFD_delay2 = 10.
      U_Threshold = 1.
      C_HutSide = 1.
      C_ArraySide = 390.
c
      IF ( Lun_Inp .NE. 5 ) THEN

        OPEN ( unit=Lun_Inp , file='airobic.in', form='FORMATTED',
     *        access='SEQUENTIAL',status='OLD')

      ENDIF
c
C  GET A NEW INPUT LINE AND PRINT IT
c
    1 CONTINUE
      READ ( Lun_Inp , 500 , END=1000) LINE
  500 FORMAT(A80)
c
C  GET TYPE OF DATACARD
c
      READ ( LINE(1:10) , 100) KEYW
  100 FORMAT(A10)

C----------------------------------------------------------------------*
C  INTERPRET DATA CARD AND READ PARAMETERS

C  END OF DATA CARD INPUT
      IF ( KEYW .EQ. 'EXIT      ' ) THEN
        CLOSE( Lun_Inp )
        ind_O = index( OutDirectory , ' ' )
        IF (ind_O . eq . 1) OutDirectory = InpDirectory
        RETURN

C  GET Input Directory
      ELSEIF ( KEYW .EQ. 'INPUT_DIRE' ) THEN
        READ(LINE(1:71),101) KEYW , InpDirectory

C  GET Output Directory
      ELSEIF ( KEYW .EQ. 'OUTPUT_DIR' ) THEN
        READ(LINE(1:71),101) KEYW , OutDirectory

C  GET leading characters of Cerenkov files
      ELSEIF ( KEYW .EQ. 'DSN0      ' ) THEN
        READ(LINE(1:19),104) KEYW , dsn0

C  GET TYPE OF PRIMARY PARTICLE
      ELSEIF ( KEYW .EQ. 'INC_PARTYP' ) THEN
        READ(LINE(1:20),102) KEYW , IncPartType

C  GET ENERGY OF PRIMARY PARTICLE
      ELSEIF ( KEYW .EQ. 'INC_ENERGY' ) THEN
        READ(LINE(1:30),107) KEYW , IncEnergy

C  GET THETA OF PRIMARY PARTICLE
      ELSEIF ( KEYW .EQ. 'INC_THETA ' ) THEN
        READ(LINE(1:30),108) KEYW , IncTheta

C  GET PHI OF PRIMARY PARTICLE
      ELSEIF ( KEYW .EQ. 'INC_PHI   ' ) THEN
        READ(LINE(1:30),108) KEYW , IncPhi

C  GET first event number
      ELSEIF ( KEYW .EQ. 'FIRSTEVENT' ) THEN
        READ(LINE(1:20),103) KEYW , i_C_file1

C  GET number of events being processed
      ELSEIF ( KEYW .EQ. 'NUM_EVENTS' ) THEN
        READ(LINE(1:20),103) KEYW , n_C_files

C  GET number of blocks of the Cerenkov_file from CORSIKA
      ELSEIF ( KEYW .EQ. 'NUM_C_BLKS' ) THEN
        READ(LINE(1:20),103) KEYW , n_C_blks

C  GET length of side for extended C_Array
      ELSEIF ( KEYW .EQ. 'CARRAYSIDE' ) THEN
        READ(LINE(1:20),102) KEYW , C_ArraySide

C  GET the grid spacing of the extended C_Array
      ELSEIF ( KEYW .EQ. 'CARRAYGRID' ) THEN
        READ(LINE(1:20),102) KEYW , C_ArrayGrid

C  GET the side length of Chut assumed in CORSIKA simul.
      ELSEIF ( KEYW .EQ. 'C_HUT_SIDE' ) THEN
        READ(LINE(1:20),102) KEYW , C_HutSide

C  GET flag for use of atmospheric absorption
      ELSEIF ( KEYW .EQ. 'ATMOSABSOR' ) THEN
        READ(LINE(1:20),103) KEYW , iAtmos_absor

C  GET flag for checking header & incident particle properties 
c  of Cerenkov file
      ELSEIF ( KEYW .EQ. 'CHECK_HEAD' ) THEN
        READ(LINE(1:50),106) KEYW , iCheck, iCheck_Energy, 
     *          iCheck_Theta, iCheck_Phi

C  GET flag for checking C-array parameters
      ELSEIF ( KEYW .EQ. 'CHECKARRAY' ) THEN
        READ(LINE(1:50),109) KEYW , iCheck_Carray, iCheck_Chut

C  GET number of calls to warm up poisson generator
      ELSEIF ( KEYW .EQ. 'POIS_CALLS' ) THEN
        READ(LINE(1:20),103) KEYW , npois_calls

C  GET average current at anode of PM from NSL
      ELSEIF ( KEYW .EQ. 'NSLCURRENT' ) THEN
        READ(LINE(1:20),102) KEYW , Av_NSLCurrent

C  GET gain of PM
      ELSEIF ( KEYW .EQ. 'GAIN_PM   ' ) THEN
        READ(LINE(1:20),105) KEYW , Gain_PM

C  GET 1st delay factor of CFD
      ELSEIF ( KEYW .EQ. 'CFD_DELAY1' ) THEN
        READ(LINE(1:20),102) KEYW , CFD_delay1

C  GET 2nd delay factor of CFD
      ELSEIF ( KEYW .EQ. 'CFD_DELAY2' ) THEN
        READ(LINE(1:20),102) KEYW , CFD_delay2

C  GET U_threshold of CFD (in miliVolts)
      ELSEIF ( KEYW .EQ. 'UTHRESHOLD' ) THEN
        READ(LINE(1:20),102) KEYW , U_Threshold

C  DUMMY LINE
      ELSEIF ( KEYW .EQ. '          ' ) THEN

C  ILLEGAL KEYWORD
      ELSE
        WRITE( Lun_Out , * ) 'Read_DataCards : UNKNOWN KEYWORD :
     *  ', KEYW

      ENDIF

      GOTO 1

C----------------------------------------------------------------------*
 1000 CONTINUE

  101 FORMAT(A10,1X,A60)
  102 FORMAT(a10,f10.1)
  103 FORMAT(a10,i10)
  104 FORMAT(A10,A9)
  105 FORMAT(a10,e10.2)
  106 FORMAT(a10,4i10)
  107 FORMAT(a10,2e10.2)
  108 FORMAT(a10,2f10.2)
  109 FORMAT(a10,2i10)

      RETURN
      END
c
c----------------------------------------------------------------------*
c
      SUBROUTINE Get_current_Amp1_2
c
c Gets the output current after the 2nd amplifier
c
      PARAMETER ( nbins = 2000 )
      COMMON /histog2/ nbins_Amp1_2 , TimeWindow_Amp1_2
      COMMON /histog3/ nbins_PM , TimeWindow_PM , time_shift_PM
      COMMON / Amplifiers / Gain_Amp1 , Gain_Amp1_2 , FWHM_ATS
     *         , ATS_Dist( 200 ) , nbins_ATS
      COMMON /NSL/ Av_NSLNoise , Av_NSLCurrent , current_factor ,
     *         Gain_PM
      COMMON / anodePM / Resist_befAMP , currentPM
      DIMENSION currentPM( nbins )
      DIMENSION currentAmp1_2( nbins )
c
      ibin1_PM = (nbins_PM - nbins_Amp1_2)/2 - nbins_ATS/2
      ibin2_PM = nbins_PM - ibin1_PM
c
      CALL vzero( currentAmp1_2 , nbins )
c
      DO 20 ibin_PM = ibin1_PM + 1, ibin2_PM + 1

        jbin1_Amp = (ibin_PM - ibin1_PM) - nbins_ATS + 1
        jbin1_Amp = max0( 1 , jbin1_Amp )
        jbin2_Amp = (ibin_PM - ibin1_PM)
        jbin2_Amp = min0( nbins_Amp1_2 , jbin2_Amp )
        DO 30 jbin_Amp = jbin1_Amp , jbin2_Amp
          jATS = nbins_ATS - (ibin_PM - ibin1_PM) + jbin_Amp
          IF ( (jATS .LT. 1) .OR. (jATS .GT. nbins_ATS) )
     *                GOTO 30
          currentAmp1_2(jbin_Amp) = currentAmp1_2(jbin_Amp) +
     *                       (currentPM(ibin_PM) - Av_NSLCurrent) *
     *                       Gain_Amp1_2 * ATS_Dist(jATS)
   30   CONTINUE
   20 CONTINUE

      CALL hpak( 2000 , currentAmp1_2 )

      RETURN
      END
c
c ---------------------------------------------------------------------*
c
      SUBROUTINE Get_qADC1(id_hut)
c
      PARAMETER ( nhuts = 1200 )
      PARAMETER ( nbins = 2000 )
      COMMON /ADC_TDC / q_ADC1( nhuts ), q_ADC1_2( nhuts )
     *         , TDC1_2( nhuts ) , Gate_qADC1_2
      COMMON / anodePM / Resist_befAMP , currentPM
      DIMENSION currentPM(nbins)
      COMMON /histog2/ nbins_Amp1_2 , TimeWindow_Amp1_2
      COMMON /histog3/ nbins_PM , TimeWindow_PM , time_shift_PM
      COMMON / Amplifiers / Gain_Amp1 , Gain_Amp1_2 , FWHM_ATS
     *         , ATS_Dist( 200 ) , nbins_ATS
      COMMON /NSL/ Av_NSLNoise , Av_NSLCurrent , current_factor ,
     *         Gain_PM
c
      ibin1_PM = (nbins_PM - nbins_Amp1_2)/2 + 1
      ibin2_PM = ibin1_PM + nbins_Amp1_2 - 1
c
c ---------------------------------------------------------------------*
c
c Get the integrated charge in pC
c
      q_ADC1(id_hut) =  0.0
      DO 20 ibin_PM = ibin1_PM , ibin2_PM
        q_ADC1(id_hut) =  q_ADC1(id_hut) +
     *                (currentPM(ibin_PM) - Av_NSLCurrent)
     *                / 2 / 1e15 * 1e12
   20 CONTINUE

      q_ADC1(id_hut) = q_ADC1(id_hut) * Gain_Amp1
c
      RETURN
      END
c
c ---------------------------------------------------------------------*
c
      SUBROUTINE Get_TDC_qADC1_2_DATA(id_hut)
c
c Gets the output current after the 2nd amplifier
c
      PARAMETER ( nbins = 2000 )
      PARAMETER ( nhuts = 1200 )
      COMMON /histog2/ nbins_Amp1_2 , TimeWindow_Amp1_2
      COMMON / CFD / CFD_delay1 , CFD_delay2 , U_Threshold
      COMMON /ADC_TDC / q_ADC1( nhuts ), q_ADC1_2( nhuts )
     *         , TDC1_2( nhuts ) , Gate_qADC1_2
      COMMON / anodePM / Resist_befAMP , currentPM
      DIMENSION currentPM(nbins)
      DIMENSION currentAmp1_2( nbins )
c
c t0: 2 -> 5 ns (20-6-95)
c
      data t0 / 5. /
c
      idel1 = 2 * CFD_delay1
      idel2 = 2 * CFD_delay2
      Current_THRES = U_Threshold / Resist_befAMP * 1e3
c
      CALL hunpak( 2000 , currentAmp1_2 , ' '  , 0 )
      TDC1_2( id_hut ) = 1 000 000.
      q_ADC1_2( id_hut ) = 0.

      ibin1_Amp = max0( 0 , idel2 )
      ibin_Amp_max = min0(nbins_Amp1_2 , nbins_Amp1_2 + idel1)
c ---------------------------------------------------------------------*
      DO 20 ibin_Amp = ibin1_Amp + 1 , ibin_Amp_max
        IF ( (-.5*currentAmp1_2( ibin_Amp ) ) .GT.
     *             Current_THRES )  GOTO 20
c
        S_CFD = 3. * currentAmp1_2( ibin_Amp - idel2 ) -
     *                  currentAmp1_2( ibin_Amp - idel1 )
        IF (S_CFD . lt . 0.) THEN
          GOTO 20
        ELSE
          ibin0 = (ibin_Amp - idel2) - 2 * t0
          CALL hix( 2000 , ibin0
     *                            , TDC1_2_temp )
          TDC1_2( id_hut ) = TDC1_2_temp
          GOTO 30
        ENDIF
   20 CONTINUE
   30 CONTINUE
c
c Get the integrated charge in pC
c
      IF ( TDC1_2( id_hut ) . ne . 1 000 000. ) THEN
        ibin1 = ibin0
        ibin2 = ibin1 + 2 * Gate_qADC1_2
        IF(ibin2 .GT. nbins_Amp1_2) GOTO 10
        DO ibin = ibin1, ibin2
          q_ADC1_2( id_hut ) = q_ADC1_2( id_hut )
     *                          + currentAmp1_2(ibin) * 0.45
     *                          /2 / 1e15 * 1e12
        ENDDO
      ENDIF

   10 CONTINUE

      RETURN
      END
c
c ---------------------------------------------------------------------*
