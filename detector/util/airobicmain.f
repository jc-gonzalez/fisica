c       ****************************************************************
      PROGRAM MAIN
c
c       AUTHOR: Sergio Martinez (Madrid-HEGRA group)
c       LAST UPDATE:  
c            corti: actualiz rndm, poissn   22 / 6 / 95            
c
c       Program to simulate the AIROBICC detector.
c       Input files: CER00000# from CORSIKA
c       Output files: airqt000# with TDC and qADC signals in each C_hut
c       ****************************************************************
      PARAMETER ( nlambdas = 4 )
      PARAMETER ( nbins = 2000 )
      PARAMETER ( nhuts = 1200 )
      COMMON / C_header / C_header(20)
      equivalence (C_header(8), x_core) , (C_header(9), y_core)
      COMMON /datac1/ InpDirectory  , OutDirectory , dsn0
      CHARACTER*60 InpDirectory , OutDirectory
      CHARACTER*9 dsn0 , dsn
      COMMON /datac2/ i_C_file1 , n_C_files , iAtmos_absor
      COMMON /LUNs/ Lun_Inp , Lun_Out , Lun_CORS_Cfile , Lun_airPM
     *         , Lun_stat , Lun_airQT
      COMMON /Cfile/ cerbuf(273,30) , irecor , n_C_blks , isb , ieof
      COMMON /NSL/ Av_NSLNoise , Av_NSLCurrent , current_factor ,
     *         Gain_PM
      COMMON /histog/ nC_bins , C_TimeWindow , C_time_shift
      COMMON /histog2/ nbins_Amp1_2 , TimeWindow_Amp1_2
      COMMON /histog3/ nbins_PM , TimeWindow_PM , time_shift_PM
      COMMON /histog4/ Window_shift
      COMMON /C_Array/ C_ArrayHuts , C_ArrayGrid , C_Array_xmax ,
     *         C_Array_ymax , imiddle , jmiddle
      INTEGER C_ArrayHuts
      common /EASfront/ X_delay1 , Y_delay1, X_delay2, Y_delay2
      COMMON /TrTimePM/ nTrTime , TrTime_PM , Dist_TrTime_PM
      DIMENSION TrTime_PM(100) , Dist_TrTime_PM(100)
      COMMON /TRAtmos/ TR_Atmos(nlambdas)
      COMMON /cerenkov/ C_spectrum(nlambdas) , lambda(nlambdas)
      REAL lambda
      COMMON /cerenkov_2/ extended_spectrum_factor
      COMMON /Chutgeom/ r_cone(0:3),zu(0:3),ta(3),ca(3),nz(3),zd(3)
      REAL nz
      COMMON / anodePM / Resist_befAMP , currentPM
      DIMENSION currentPM(nbins) , C_current(nbins)
      COMMON / CFD / CFD_delay1 , CFD_delay2 , U_Threshold
      COMMON /ADC_TDC / q_ADC1( nhuts ), q_ADC1_2( nhuts )
     *         , TDC1_2( nhuts ) , Gate_ADC1_2
      common /C_Array_2/ C_ArraySide , C_HutSide
      dimension TR_Plexiglas(nlambdas)
      DIMENSION Reflex_mirror(nlambdas) , TR_Filter(nlambdas)
      DIMENSION QE_Photocath(nlambdas) , TR_total(nlambdas)
      DIMENSION curphot(7)
      DIMENSION C_hut_count(50,50) , C_hut_arrTime(50,50)
      DIMENSION C_hut_count_At(50,50) , C_hut_arrTime_At(50,50)
c----------------------------------------------------------------------*
      DIMENSION TDC_qADC( nhuts , 3 )
      dimension Conversion_qPhot_HG( 50, 50 )
      dimension Conversion_qPhot_LG( 50, 50 )
corti-alpha
      character*4 ccelo
      real rcelo
      equivalence (ccelo,rcelo)
corti-alpha
c
corti actualizacion de rndm
      parameter( lon_rn_vec = 3 )
      dimension rn_vec(lon_rn_vec)
corti
      REAL LightSpeed
      double precision C_phot_total
      INTEGER QT_RECL
      CHARACTER*70 C_corsika_file , Hist_file , C_stat_file
      CHARACTER*70 C_QT_file
      CHARACTER*40 HTitleAmp1_2
      CHARACTER*40 HTitlePM
c
c Assign values to variables
corti-alpha
      ccelo='CELO'
corti-alpha
c
      DATA LightSpeed / 30. /
      DATA pi / 3.141593 /
      DATA TR_Atmos / nlambdas * 1. /
c
c Values taken from figure (3.6) in Albrecht's Thesis (p. 44)
c
      data TR_Plexiglas / 0.8 , 0.88 , 0.9 , 0.9 /
      DATA Reflex_mirror / 0.86 , 0.9 , 0.9 , 0.92 /
corti 5.8.98 Filters out at LP
c      DATA TR_Filter / 0.89 , 0.91 , 0.85 , 0.5 /
      DATA TR_Filter / 1. , 1. , 1. , 1. /
corti
      DATA QE_Photocath / 0.19 , 0.26 , 0.22 , 0.15 /
c
      DATA Lun_Inp / 5 / , Lun_Out / 6 / , Lun_CORS_Cfile / 3 / ,
     *       Lun_airPM / 4 / , Lun_stat / 7 / , Lun_airQT / 8 / ,
     *       Lun_Cores / 9 /
c
      DATA nData_C_hut / 3 /
      DATA nBytes_QT_Data / 4 /
c
c Initialize variables , arrays and histograms
c
      CALL Init_AIROBICC
c
c RedFac: reduction factor to get correct intensity at the huts
c
      RedFac =  pi * (r_cone(3)/100.) ** 2 / C_HutSide ** 2
c
corti-alpha
      lrec = 4 * 273 * n_C_blks
      QT_RECL = C_ArrayHuts * nData_C_hut * nBytes_QT_Data
corti-alpha: en bytes
c
      IF ( Lun_Out .NE. 6 ) THEN

        OPEN ( unit=Lun_Out , file='airobic.out',form='formatted',
     *          access='sequential',status='unknown')

      ENDIF
c
      ind_I = index( InpDirectory , ' ' )
      ind_O = index( OutDirectory , ' ' )
c
      N = nC_bins
      HTitlePM = 'AnodeCurrent x = 111111 m, y = 111111 m$'
      HTitleAmp1_2 = 'Amp-Current x = 111111 m , y = 111111 m$'

      DO i=1,70
        Hist_file(i:i) = ' '
      ENDDO
      Hist_file = OutDirectory
      WRITE(Hist_file(ind_O:ind_O+10) , '(a11)') 'airobic.his'
corti
      write(6,*) Hist_file, 'voy a abrirlo...'
      OPEN ( unit=Lun_Cores ,file=Hist_file,form='unformatted',
     *          access='direct',status='new',recl=4096)
      write(6,*) Hist_file, '...lo he abierto'
corti
c
      CALL hrfile( Lun_Cores ,'cores','N')
c
c  *************************   SHOWER LOOP     *************************
c
      DO 2000 iEvent = i_C_file1 , i_C_file1 + n_C_files - 1
c
c open the I/O units
c
        DO i=1,70
          C_corsika_file(i:i) = ' '
          C_stat_file(i:i) = ' '
          C_QT_file(i:i) = ' '
        ENDDO
c ####################################################################
c OPEN CERENKOV FILE FROM CORSIKA
C ####################################################################
        dsn = dsn0
        WRITE(dsn(4:9),'(i6)') iEvent
        DO i=4,9
          IF ( dsn(i:i) . eq . ' ' ) dsn(i:i) = '0'
        ENDDO
c
        C_corsika_file = InpDirectory
        WRITE(C_corsika_file(ind_I:ind_I+8) , '(a9)') dsn
corti     WRITE(C_corsika_file(ind_I+9:ind_I+12) , '(a4)') '.dat'
c----------------------------------------------------------------------*
c
corti
        write(6,*) C_corsika_file, 'going to open CER## file...'
        OPEN ( unit=Lun_CORS_Cfile , file=C_corsika_file , form=
     *        'unformatted', access='direct',status='old',recl=lrec
     *        , err=2000)
calpha: no se traga el 'readonly'
        write(6,*) C_corsika_file, '...CER## successfully opened'
corti
c
c ####################################################################
c OPEN OUTPUT FILE FOR THE FINAL DATA ( TIME & CHARGE )
C ####################################################################
        dsn = 'airqt0000'
        WRITE(dsn(7:9),'(i3)') iEvent
        DO i=7,9
          IF ( dsn(i:i) . eq . ' ' ) dsn(i:i) = '0'
        ENDDO
        C_QT_file = OutDirectory
        WRITE(C_QT_file(ind_O:ind_O+8) , '(a9)') dsn
c
corti   
        write(6,*) C_QT_file, 'going to open airqt file...'
        OPEN ( unit=Lun_airQT ,file=C_QT_file,form='unformatted',
     *          access='direct',status='new',recl = QT_RECL)
        write(6,*) C_QT_file, '...airqt successfully opened'
corti
c
c ####################################################################
c OPEN OUTPUT FILE FOR STATISTICS OF C-PHOTONS BEFORE C-DETECTOR
C ####################################################################
        dsn = 'airsta000'
        WRITE(dsn(7:9),'(i3)') iEvent
        DO i=7,9
          IF ( dsn(i:i) . eq . ' ' ) dsn(i:i) = '0'
        ENDDO
        C_stat_file = OutDirectory
        WRITE(C_stat_file(ind_O:ind_O+8) , '(a9)') dsn
c
corti
        write(6,*) C_stat_file, 'going to open sta file...'
        OPEN ( unit=Lun_stat ,file=C_stat_file,form='formatted',
     *          access='sequential',status='new')
        write(6,*) C_stat_file, '...sta file successfully opened'
corti
c----------------------------------------------------------------------*
        WRITE( Lun_stat , * ) ' SHOWER #', iEvent
c
        WRITE( Lun_Out , '(//)' )
        WRITE( Lun_Out ,*) ' EVENT NUMBER = ' , iEvent
c
c Initialize some variables for information of single events
c
        CALL vzero( C_hut_count , 2500 )
        CALL vzero( C_hut_arrTime , 2500 )
c
        CALL vzero( C_hut_count_At , 2500 )
        CALL vzero( C_hut_arrTime_At , 2500 )
c
        C_phot_total = 0.d0
c
        n_Bad_uv = 0
c
c Initialize variables to read C-files
c
        irecor = 0
        ieof = 0
        isb = n_C_blks
c
c LOOP FOR READING IN THE DATA OF THE C-PHOTONS
c
   20   isb = isb + 1
        IF ( isb . gt . n_C_blks ) THEN
          CALL Read_Cfile
          IF ( irecor . eq . 1 ) THEN

            PartType = cerbuf(1,2)
            Energy = cerbuf(2,2) / 1e3
c cerbuf(3,2): thetap in radians
            Theta = cerbuf(3,2) / pi * 180.
c cerbuf(4,2): phi in rad.
            Phi = cerbuf(4,2) / pi * 180. + 180.
            if (Phi .gt. 360.) Phi = Phi - 360.
            C_header(4) = Phi / 180. * pi

            call Init_Histos

            WRITE( Lun_Out , '(/)' )
            WRITE( Lun_Out , '(a,f5.0)') ' PARTICLE = ', PartType
            WRITE( Lun_Out , '(a,f6.1,a)') ' ENERGY = ',
     *  Energy, ' TeV'
            WRITE( Lun_Out , '(a,f5.1,a)') ' THETA = ', Theta, ' deg'
            WRITE( Lun_Out , '(a,f5.1,a)') ' PHI = ', Phi, ' deg'
            WRITE( Lun_Out , '(/)' )
c
            WRITE( Lun_stat , '(a,i5)' ) ' PARTICLE = ', int(PartType)
            WRITE( Lun_stat , 7002 ) ' ENERGY = ',
     *  Energy, ' TeV ; ' , ' THETA = ', Theta, ' deg',
     *                      ' PHI = ', Phi, ' deg'
 7002       FORMAT( a , f6.1 , a , a , f5.1 , a , a , f5.1 , a)

c
c Skip 1st block (Corsika EVTH)
c
            isb = 2
c Skip Cerenkov header
            lh = 2
            GOTO 30
          ENDIF
        ENDIF
c
c  End of C_corsika_file ?
c
        IF ( ieof . eq . 1 ) THEN
c----------------------------------------------------------------------*
          WRITE( Lun_Out ,*) 'Total number of Cerenkov photons in
     *  the extended C-Array = ', sngl( C_phot_total )
          GOTO 1000
        ENDIF
corti-alpha: no se traga real=character        
        if ( cerbuf(1, isb) .eq. RCELO ) then
          WRITE( Lun_Out ,*) 'Total number of Cerenkov photons in
     *  the extended C-Array = ', sngl( C_phot_total )
          GOTO 1000
        ENDIF

        lh = 0
   30   DO 40 i = 1,7
          curphot(i) = cerbuf( 7 * lh + i , isb )
   40   CONTINUE
c
c  End of C_corsika_file ?
c----------------------------------------------------------------------*
c
        IF ( (curphot(1) . eq . 0.) . and . (curphot(7) . eq . 0.) )
     &    THEN
          WRITE( Lun_Out ,*) 'Total number of Cerenkov photons in
     *  the extended C-Array = ', sngl( C_phot_total )
          WRITE( Lun_Out ,*) 'Number of Cerenkov bunches with
     *  illegal values of direction cosines u, v = ', n_Bad_uv
          WRITE( Lun_Out ,*) 'irecor = ', irecor
          WRITE( Lun_Out ,*) 'isb = ', isb
          WRITE( Lun_Out ,*) 'lh = ', lh
          GOTO 1000
        ELSEIF ( curphot(1) . eq . 0. ) THEN
          GOTO 50
        ENDIF
c
        C_photons_0 = curphot(1)
c
c Correct for change in C-spectral range: 300-450 nm -> 300-500 nm
c
        C_photons_0 = C_photons_0 * extended_spectrum_factor
c
c C_x, C_y in meters
c
        C_x = curphot(2) / 100.
        C_y = curphot(3) / 100.
c
        C_u_emis = curphot(4)
        C_v_emis = curphot(5)
        C_ArrTime = curphot(6)
        C_z_emis = curphot(7)
        C_phot_total = C_phot_total + C_photons_0
        C_w_emis = 1. - C_u_emis ** 2 - C_v_emis ** 2
        IF ( C_w_emis .LT. 0. ) THEN
          n_Bad_uv = n_Bad_uv + 1
          GOTO 50
        ENDIF

        C_w_emis = sqrt( C_w_emis )
        x = C_x + C_Array_xmax
        y = C_y + C_Array_ymax
corticheck
c        write(6,*) 'Corrected for change in C-spectral range'
c
c----------------------------------------------------------------------*
c check if particle is inside the extended C_array
c
        IF ( (x .lt. -C_HutSide/2) .or. (x .gt. (2 * C_Array_xmax +
     *        C_HutSide/2) ) ) then
c>>>
          write( 6,*) 'C-bunch fuera del array'
          GOTO 50
        endif
        IF ( (y .lt. -C_HutSide/2) .or. (y .gt. (2 * C_Array_ymax +
     *        C_HutSide/2) ) ) then
c>>>
          write( 6,*) 'C-bunch fuera del array'
          GOTO 50
        endif
corticheck
c        write(6,*) 'particle inside the C_array checked' 
c
c calculate the index (ihut,jhut) of the C-hut: >=0
c
        ihut = ( y + C_HutSide/2 ) / C_ArrayGrid + 1
        jhut = ( x + C_HutSide/2 ) / C_ArrayGrid + 1
c
c calculate relative coordinates to the center of the C-hut
c
        x0 = x - C_ArrayGrid * (jhut-1)
        y0 = y - C_ArrayGrid * (ihut-1)
        if ( (abs(x0) .gt. C_HutSide/2) .or. 
     &       (abs(y0) .gt. C_HutSide/2) ) then
c>>>
          write( 6,*) 'C-bunch fuera de la caseta'
          goto 50
        endif
        x0 = x0 * 100.
        y0 = y0 * 100.
corticheck
c        write(6,*) 'relative coordinates calculated' 
c
c get statistics for the shower in some C-huts
c

        C_hut_count( ihut , jhut ) =
     *      C_hut_count( ihut , jhut ) + C_photons_0

        C_hut_arrTime( ihut , jhut ) =
     *      C_hut_arrTime( ihut , jhut ) + C_photons_0 * C_ArrTime

        IF ( iAtmos_absor . eq . 1 ) THEN
          CALL Atmos_absorption( C_z_emis , C_w_emis )
c----------------------------------------------------------------------*
          C_phot_Atmos = 0.
          DO i =1 , nlambdas
            C_phot_Atmos = C_phot_Atmos + C_spectrum(i) *
     *                         TR_Atmos(i) * C_photons_0
          ENDDO
          C_hut_count_At( ihut , jhut ) =
     *      C_hut_count_At( ihut , jhut ) + C_phot_Atmos

          C_hut_arrTime_At( ihut , jhut ) =
     *      C_hut_arrTime_At( ihut , jhut ) + C_phot_Atmos * C_ArrTime
        ENDIF
c
c get a random position at the entrance of the C-hut
c
corticheck
cc        write(6,*) 'going for ranmar'
corti actualizacion de rndm(x) -> ranmar(vec,len)
   10   CALL RANMAR(rn_vec,lon_rn_vec)
*        rndm01 =  rndm( dummy )
        rndm01 = rn_vec(1)
cc        write(6,*) 'ranmar ok'
c
c the number of C-bunches between r , r+dr is
c proportional to r
c
*        rndm02 = rndm( dummy )
        rndm02 = rn_vec(2)
        IF ( rndm02 . gt . rndm01 ) GOTO 10
*        rndm03 = rndm( dummy )
        rndm03 = rn_vec(3)
corti
        r_rndm = r_cone(3) * rndm01
        phi_rndm = 2 * pi * rndm03
        x_rndm = r_rndm * cos( phi_rndm )
        y_rndm = r_rndm * sin( phi_rndm )
c
c tracking of the C-photons to the photocathode
c
        CALL Ctracking( x_rndm , y_rndm , C_u_emis , C_v_emis ,
     *                  tracklength , nreflex )
c
ccorticheck 
c        write(6,*) 'tracking ok'
        IF ( tracklength . le . 0.0 ) GOTO 50
c----------------------------------------------------------------------*
c
c Get the number of photoelectrons at the photocathode : Photoelect_PM
c
        C_photons_0 = C_photons_0 * RedFac
        Photoelect_PM = 0.
        DO i=1,nlambdas
          TR_total(i) = TR_Atmos(i) * TR_Plexiglas(i) *
     *                  Reflex_mirror(i) ** nreflex *
     *                  TR_Filter(i) * QE_Photocath(i)
          Photoelect_PM = Photoelect_PM + C_photons_0 *
     *                          C_spectrum(i) * TR_total(i)
        ENDDO
cc        write(6,*) 'number of phe calculated'
c
c Correct arrival time due to shower front delay
c
        delta_x = x0 - x_rndm
        delta_y = y0 - y_rndm
        time_corr = ( delta_x * C_u_emis + delta_y * C_v_emis )
     *              / LightSpeed
        C_ArrTime = C_ArrTime - time_corr
c
        id_hut = C_ArrayHuts * (ihut-1)  + jhut 
        delay_hut = tracklength / LightSpeed
c
c Fill histograms: get the current at the anode of the PM
c
        DO i=1,nTrTime
          time = C_ArrTime + delay_hut + TrTime_PM(i)
          current_PM = Photoelect_PM * Gain_PM *
     *                       current_factor * Dist_TrTime_PM(i)
          CALL hfill(id_hut,time,0.,current_PM)
        ENDDO
c
c ########################################################
   50   lh = lh + 1
        IF ( lh . eq . 39 ) GOTO 20
        GOTO 30
c
c END OF LOOP OVER CERENKOV PHOTONS
c
 1000   CONTINUE
        CLOSE( Lun_CORS_Cfile )

c
c Fill header of output data file
c
        C_header(15) = C_phot_total
        C_header(16) = U_Threshold
        WRITE( Lun_airQT , REC= 1 ) ( C_header(i) , i=1, 16 )

c
C **********************************************************************
c *******                                                   ************
c *******      Get the ADC and TDC SIGNALS for each HUT     ************
c *******                                                   ************
C **********************************************************************
c
c
        CALL vzero( q_ADC1, nhuts )
c
        DO 60 j=1,C_ArrayHuts
          jhut = j
          X_tmin = X_delay1 * ( jhut - jmiddle ) + X_delay2
          xhut = C_ArrayGrid * ( jhut - jmiddle )
          WRITE(HTitlePM(18:23),'(f6.1)') xhut
          WRITE(HTitleAmp1_2(17:22),'(f6.1)') xhut
          DO 70 i=1,C_ArrayHuts
            ihut = i
            Y_tmin = Y_delay1 * ( ihut - imiddle ) + Y_delay2
            tmin = X_tmin + Y_tmin - time_shift_PM
            tmax = tmin + TimeWindow_PM
            tmin_Amp1_2 = tmin + Window_shift
            tmax_Amp1_2 = tmin_Amp1_2 + TimeWindow_Amp1_2
            yhut = C_ArrayGrid * ( ihut - imiddle )
            WRITE(HTitlePM(32:37),'(f6.1)') yhut
            WRITE(HTitleAmp1_2(32:37),'(f6.1)') yhut
c----------------------------------------------------------------------*
            CALL hbook1( 1500 , HTitlePM , nbins_PM ,
     *                       tmin , tmax , 0. )
c----------------------------------------------------------------------*
            CALL hbook1( 2000 , HTitleAmp1_2 ,
     *               nbins_Amp1_2 , tmin_Amp1_2 , tmax_Amp1_2 , 0. )
c
c Get photoelectron noise from NSL and fill
c histograms with such noise
c
            CALL Get_NSLNoise
            CALL hunpak( 1500 , currentPM , ' ' , 0 )

            id_hut = C_ArrayHuts * ( ihut - 1 ) + jhut
            CALL hunpak( id_hut , C_current , ' ' , 0 )

            ibin1 = 2 * (time_shift_PM - C_time_shift) + 1
            ibin2 = ibin1 + nC_bins - 1
            DO ibin = ibin1, ibin2
              currentPM(ibin) = currentPM(ibin) +
     *                            C_current(ibin - ibin1 + 1)
            ENDDO

            CALL hpak( 1500 , currentPM )
c
c 1st get the integrated current after the 1st amplifier (LOW GAIN)
c
            CALL Get_qADC1(id_hut)
c
c 2nd get the histograms of the current after
c the 2nd amplifier (HIGH GAIN)
C
            CALL Get_current_Amp1_2
c
c----------------------------------------------------------------------*
c Get the arrival time of the C-pulse and integrate the current of
c the pulse using a fixed timewindow
c
            CALL Get_TDC_qADC1_2_DATA(id_hut)

            CALL hdelet(1500)
            CALL hdelet(2000)

   70     CONTINUE
   60   CONTINUE

c
c write to disk information about TDC, q_ADC1_2 , and q_ADC1
c
        DO id_hut = 1 , C_ArrayHuts ** 2
          TDC_qADC( id_hut , 1 ) = TDC1_2( id_hut )
          TDC_qADC( id_hut , 2 ) = q_ADC1_2( id_hut )
          TDC_qADC( id_hut , 3 ) = q_ADC1( id_hut )
        ENDDO
c
        DO ihut= 1 , C_ArrayHuts
          id_hut1 = (ihut-1) * C_ArrayHuts + 1
          id_hut2 = ihut * C_ArrayHuts
          WRITE( Lun_airQT , REC= ihut + 1 )
     *               ( ( TDC_qADC( id_hut , j ) , j=1,3 )
     *                 , id_hut = id_hut1 , id_hut2  )
        ENDDO

        caos_xcore = 0.0
        caos_ycore = 0.0
        caos_photons = 0.0
        DO i=1, C_ArrayHuts
          yhut = (i - imiddle) * C_arrayGrid
          DO j=1, C_ArrayHuts
            xhut = (j - jmiddle) * C_arrayGrid
            id_hut = C_ArrayHuts * (i-1)  + j
            caos_xcore = caos_xcore + xhut * TDC_qADC( id_hut , 2)
            caos_ycore = caos_ycore + yhut * TDC_qADC( id_hut , 2)
            caos_photons = caos_photons + TDC_qADC( id_hut , 2)
          ENDDO
        ENDDO
c
c Remove the central hut (since it has a large weight)
c
        caos_photons = caos_photons -
     *                TDC_qADC( C_ArrayHuts**2 / 2 + 1 , 2)
        if (caos_photons .gt. 0.0) then
          caos_xcore = caos_xcore / caos_photons
          caos_ycore = caos_ycore / caos_photons
        else
          caos_xcore = -10000.
          caos_ycore = -10000.
        endif
C
c
c **********************************************************************
c
        CLOSE( Lun_airQT )
c
        DO 1010 i = 1 , C_ArrayHuts
          DO 1010 j = 1 , C_ArrayHuts
            IF ( C_hut_count(i,j) . gt . 0. ) then
              C_hut_arrTime(i,j) = C_hut_arrTime(i,j) /
     *                  C_hut_count(i,j)
              C_hut_count(i,j) = C_hut_count(i,j) / C_HutSide**2
              IF ( iAtmos_absor . eq . 1 ) C_hut_arrTime_At(i,j) =
     *                  C_hut_arrTime_At(i,j) / C_hut_count_At(i,j)
              IF ( iAtmos_absor . eq . 1 ) C_hut_count_At(i,j) =
     &          C_hut_count_At(i,j) / C_HutSide**2

            ENDIF
            id_hut = (i-1) * C_ArrayHuts + j
            if ( TDC_qADC(id_hut,2) * iAtmos_absor .gt. 0.) then
              Conversion_qPhot_HG(i,j) = C_hut_count_At(i,j) /
     *                                TDC_qADC(id_hut,2)
              Conversion_qPhot_LG(i,j) = C_hut_count_At(i,j) /
     *                                TDC_qADC(id_hut,3)
            else
              Conversion_qPhot_HG(i,j) = 0.
              Conversion_qPhot_LG(i,j) = 0.
            endif
 1010   CONTINUE
c
c write statistics for Cerenkov photons to disk
c
        WRITE( Lun_stat ,'(/)')
        WRITE( Lun_stat ,*) '   NUMBER OF CERENKOV PHOTONS IN C-HUTS '
        WRITE( Lun_stat ,*) '   NOT INCLUDING ATMOSPHERIC ABSORPTION '
        WRITE( Lun_stat ,'(/)')
        DO 1020 i = C_ArrayHuts-1 , 2 , -3
          WRITE( Lun_stat , 7010) ( int( C_hut_count(i,3*j-1) ),
     *                                    j = 1 , C_ArrayHuts/3 )
 1020   CONTINUE
C
        WRITE( Lun_stat ,'(//)')
        WRITE( Lun_stat ,*) '   MEAN ARRIVAL TIME OF CERENKOV PHOTONS IN
     *  C-HUTS (NSECS)'
        WRITE( Lun_stat ,*) '   NOT INCLUDING ATMOSPHERIC ABSORPTION '
        WRITE( Lun_stat ,'(/)')
c----------------------------------------------------------------------*
        DO 1030 i = C_ArrayHuts-1 , 2 , -3
          WRITE( Lun_stat , 7020) ( C_hut_arrTime(i,3*j-1) ,
     *                                    j = 1 , C_ArrayHuts/3 )
 1030   CONTINUE
c
        IF ( iAtmos_absor . eq . 1 ) THEN
c
          WRITE( Lun_stat ,'(////)')
c----------------------------------------------------------------------*
          WRITE( Lun_stat ,*) '   NUMBER OF CERENKOV PHOTONS IN C-
     *  HUTS '
          write( Lun_stat ,*) '   INCLUDING ATMOSPHERIC ABSORPTION
     *  '
          write( Lun_stat ,'(/)')
          DO 1040 i = C_ArrayHuts-1 , 2 , -3
            WRITE( Lun_stat , 7010)
     *      ( int( C_hut_count_At(i,3*j-1) ), j = 1 , C_ArrayHuts/3 )
 1040     CONTINUE
C
          WRITE( Lun_stat ,'(//)')
          WRITE( Lun_stat ,*) '   MEAN ARRIVAL TIME OF  CERENKOV
     *  PHOTONS  IN C-HUTS (NSECS)'
c----------------------------------------------------------------------*
          WRITE( Lun_stat ,*) '   INCLUDING ATMOSPHERIC ABSORPTION
     *  '
          write( Lun_stat ,'(/)')
          DO 1050 i = C_ArrayHuts-1 , 2 , -3
            WRITE( Lun_stat , 7020)
     *           ( C_hut_arrTime_At(i,3*j-1), j = 1, C_ArrayHuts/3 )
 1050     CONTINUE

          WRITE( Lun_stat ,'(//)')
          WRITE( Lun_stat ,*) '   CONVERSION FACTOR FOR',
     *                        ' CHARGE (pC) --> PHOTONS/M2'
c----------------------------------------------------------------------*
          WRITE( Lun_stat ,*) '   INCLUDING ATMOSPHERIC ABSORPTION'
          write( Lun_stat ,'(/)')
          WRITE( Lun_stat ,*) '   1) HIGH GAIN BRANCH'
          write( Lun_stat ,'(/)')
          DO 1060 i = C_ArrayHuts-1 , 2 , -3
            WRITE( Lun_stat , 7020)
     *           ( Conversion_qPhot_HG(i,3*j-1), j = 1, C_ArrayHuts/3 )
 1060     CONTINUE

          write( Lun_stat ,'(//)')
          WRITE( Lun_stat ,*) '   2) LOW GAIN BRANCH'
          write( Lun_stat ,'(/)')
          DO 1070 i = C_ArrayHuts-1 , 2 , -3
            WRITE( Lun_stat , 7020)
     *           ( Conversion_qPhot_LG(i,3*j-1), j = 1, C_ArrayHuts/3 )
 1070     CONTINUE

          eas_xcore = 0.0
          eas_ycore = 0.0
          eas_photons = 0.0
          DO i=1, C_ArrayHuts
            yhut = (i - imiddle) * C_arrayGrid
            DO j=1, C_ArrayHuts
              xhut = (j - jmiddle) * C_arrayGrid
              eas_xcore = eas_xcore + xhut * C_hut_count_At(i,j)
              eas_ycore = eas_ycore + yhut * C_hut_count_At(i,j)
              eas_photons = eas_photons + C_hut_count_At(i,j)
            ENDDO
          ENDDO
c
c Remove the central hut (since it has a large weight)
c
          eas_photons = eas_photons -
     *                        C_hut_count_At( imiddle, jmiddle )
c
          eas_xcore = eas_xcore / eas_photons
          eas_ycore = eas_ycore / eas_photons
          WRITE(Lun_stat, 7025) x_core/100., y_core/100.
          WRITE(Lun_stat, 7030) eas_xcore, eas_ycore
          WRITE(Lun_stat, 7040) caos_xcore, caos_ycore

          CALL hfill(3001,eas_xcore,0.,1.)
          CALL hfill(3002,eas_ycore,0.,1.)
          CALL hfill(3003,caos_xcore,0.,1.)
          CALL hfill(3004,caos_ycore,0.,1.)
C
        ENDIF
C
 7010   FORMAT(40i8)
 7020   FORMAT(40f8.1,/)
 7025   FORMAT(/,'true xcore (m)= ',f10.1,5x,'true ycore (m)= ',f10.1/)
 7030   FORMAT('eas_xcore (m)= ',f10.1,5x,'eas_ycore (m)= ',f10.1/)
 7040   FORMAT('caos_xcore (m)= ',f10.1,5x,'caos_ycore (m)= ',f10.1/)

        CLOSE ( Lun_stat )
c
 2000 CONTINUE
c
c ******************     END OF SHOWER LOOP     *********************
c
      CLOSE( Lun_Out )

      CALL hcdir( '//cores', ' ' )
      DO id=3001, 3004
        CALL hrout( id, icycle, ' ' )
      ENDDO

      CALL hrend( 'cores' )
      CLOSE( Lun_Cores )

      END
c
c----------------------------------------------------------------------*




