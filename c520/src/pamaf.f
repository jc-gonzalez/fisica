      SUBROUTINE PAMAF
 
C-----------------------------------------------------------------------
C  PA(RTICLE) MA(SS) F(ILLING)
C
C  FILLS PARTICLE MASS FOR PARTICLE IP IN ARRAY PAMA
C  RESONANCES AND STRANGE BARYONS INCLUDED
C  PARTICLE MASSES ACCORDING TO GEANT TABLE,
C  TAKEN FROM THE PERIODIC TABLE
C  OR CALCULATED WITH THE MASS FORMULA OF WEIZSAECKER
C  THIS SUBROUTINE IS CALLED FROM START
C-----------------------------------------------------------------------
 
      IMPLICIT NONE
*KEEP,CONST.
      COMMON /CONST/   PI,PI2,OB3,TB3,ENEPER
      DOUBLE PRECISION PI,PI2,OB3,TB3,ENEPER
*KEEP,PAM.
      COMMON /PAM/     PAMA,SIGNUM
      DOUBLE PRECISION PAMA(6000),SIGNUM(6000)
*KEND.
 
      DOUBLE PRECISION AMUS(59,14),BIND,B1,B2,B3,B4,B5,CHARGE(75),
     *                 MASSES(75),SS
      INTEGER          I,IA,IC,IN,IP,L
C-----------------------------------------------------------------------
      DATA MASSES /
     * 0.0D0      ,.51099906D-3,.51099906D-3, 0.0D0      ,.105658389D0,
     *.105658389D0, .1349743D0 , .1395679D0 , .1395679D0 , 0.497671D0 ,
     * 0.493646D0 , 0.493646D0 ,.93956563D0 ,.93827231D0 ,.93827231D0 ,
     * 0.497671D0 , 0.54745D0  , 1.11563D0  , 1.18937D0  , 1.19255D0  ,
     * 1.197465D0 , 1.31485D0  , 1.32133D0  , 1.67243D0  ,.93956563D0 ,
     * 1.11563D0  , 1.18937D0  , 1.19255D0  , 1.19743D0  , 1.31485D0  ,
     * 1.32133D0  , 1.67243D0  , 1.7841D0   , 1.7841D0   , 1.8693D0   ,
     * 1.8693D0   , 1.8645D0   , 1.8645D0   , 1.9693D0   , 1.9693D0   ,
     * 2.2852D0   , 80.6D0     , 80.6D0     , 91.161D0   , 1.877D0    ,
     * 2.817D0    , 3.755D0    , 0.0D0      , 0.0D0      , 0.0D0      ,
     * 0.7669D0   , 0.7681D0   , 0.7681D0   , 1.2309D0   , 1.2323D0   ,
     * 1.2336D0   , 1.2349D0   , 1.2309D0   , 1.2323D0   , 1.2336D0   ,
     * 1.2349D0   , 0.89624D0  , 0.89209D0  , 0.89209D0  , 0.89624D0  ,
     * 0.0D0      , 0.0D0      , 0.0D0      , 0.0D0      , 0.0D0      ,
     * 0.54745D0  , 0.54745D0  , 0.54745D0  , 0.54745D0  , 0.0D0      /
 
      DATA CHARGE /
     *  0.D0,+1.D0,-1.D0, 0.D0,+1.D0,-1.D0, 0.D0,+1.D0,-1.D0, 0.D0,
     * +1.D0,-1.D0, 0.D0,+1.D0,-1.D0, 0.D0, 0.D0, 0.D0,+1.D0, 0.D0,
     * -1.D0, 0.D0,-1.D0,-1.D0, 0.D0, 0.D0,-1.D0, 0.D0,+1.D0, 0.D0,
     * +1.D0,+1.D0,+1.D0,-1.D0,+1.D0,-1.D0, 0.D0, 0.D0,+1.D0,-1.D0,
     * +1.D0,+1.D0,-1.D0, 0.D0,+1.D0,+1.D0,+2.D0, 0.D0, 0.D0, 0.D0,
     *  0.D0,+1.D0,-1.D0,+2.D0,+1.D0, 0.D0,-1.D0,-2.D0,-1.D0, 0.D0,
     * +1.D0, 0.D0,+1.D0,-1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0,
     *  0.D0, 0.D0, 0.D0, 0.D0, 0.D0 /
 
C  ISOTOPE MASSES CALCULATED FROM: ATOMIC DATA AND NUCL.DATA TABLES 39
C  (1988) 289, (WAPSTRA'S VALUES, CORRECTED FOR ELECTRON MASSES)
      DATA ((AMUS(I,L),I=1,59),L=1,7) /
     * 1.8756D0,  2.8089D0,                                    57*0.D0,
     * 2.8083D0,  3.7273D0,  4.6678D0,  5.6054D0,  6.5454D0,   54*0.D0,
     * 2*0.D0  ,  5.6014D0,  6.5337D0,  7.4712D0,  8.4067D0,
     *                       9.3471D0, 10.2856D0,              51*0.D0,
     * 2*0.D0  ,  6.5341D0,  7.4547D0,  8.3926D0,  9.3253D0,
     *                      10.2644D0, 11.2008D0,              51*0.D0,
     * 2*0.D0  ,  7.4722D0,  8.3932D0,  9.3243D0, 10.2524D0,
     *           11.1886D0, 12.1232D0, 13.0618D0, 13.9986D0,   49*0.D0,
     * 2*0.D0  ,  8.4091D0,  9.3274D0, 10.2538D0, 11.1747D0, 12.1093D0,
     *           13.0406D0, 13.9790D0, 14.9143D0, 15.8531D0,   48*0.D0,
     * 4*0.D0  , 11.1915D0, 12.1110D0, 13.0400D0, 13.9687D0, 14.9057D0,
     *           15.8394D0, 16.7761D0, 17.7104D0,              47*0.D0/
      DATA ((AMUS(I,L),I=1,59),L=8,14) /
     * 4*0.D0, 12.1282D0, 13.0446D0, 13.9709D0, 14.8948D0, 15.8302D0,
     *             16.7617D0, 17.6973D0, 18.6293D0, 19.5650D0, 46*0.D0,
     * 7*0.D0, 15.8325D0, 16.7629D0, 17.6920D0, 18.6429D0, 19.5564D0,
     *             20.4907D0, 21.4227D0, 22.3587D0,            44*0.D0,
     * 6*0.D0, 15.8464D0, 16.7668D0, 17.6947D0, 18.6174D0, 19.5502D0,
     *  20.4794D0, 21.4137D0, 22.3444D0, 23.2839D0, 24.2138D0, 43*0.D0,
     * 8*0.D0, 18.6308D0, 19.5532D0, 20.4817D0, 21.4088D0, 22.3414D0,
     *  23.2720D0, 24.2059D0, 25.1387D0, 26.0746D0, 27.0099D0,
     *  27.9469D0, 28.8820D0, 29.8173D0, 30.7546D0, 31.6913D0, 36*0.D0,
     * 7*0.D0, 18.6410D0, 19.5658D0, 20.4860D0, 21.4124D0, 22.3354D0,
     *  23.2676D0, 24.1961D0, 25.1292D0, 26.0602D0, 26.9961D0,
     *  27.9291D0, 28.8660D0, 29.7994D0, 30.7376D0,            38*0.D0,
     * 9*0.D0, 21.4241D0, 22.3488D0, 23.2714D0, 24.1996D0, 25.1261D0,
     *  26.0579D0, 26.9880D0, 27.9218D0, 28.8541D0, 29.7894D0,
     *  30.7233D0, 31.6599D0, 32.5944D0, 33.5316D0,            36*0.D0,
     * 9*0.D0, 22.3591D0, 23.2836D0, 24.2041D0, 25.1304D0, 26.0527D0,
     *  26.9838D0, 27.9128D0, 28.8457D0, 29.7761D0, 30.7111D0,
     *  31.6431D0, 32.5803D0, 33.5128D0, 34.4505D0, 35.3837D0, 35*0.D0/
C-----------------------------------------------------------------------
 
C  GEANT PARTICLES  INCLUDING RHO, K*, AND DELTA
      DO  1  IP = 1,75
        PAMA  (IP) = MASSES(IP)
        SIGNUM(IP) = CHARGE(IP)
  1   CONTINUE
 
C  RESET REST OF THE ARRAY
      DO  2  IP = 76,6000
        PAMA  (IP) = 0.D0
        SIGNUM(IP) = 0.D0
  2   CONTINUE
 
      DO  3  IA = 1,59
      DO  3  IC = 1,IA
        IN = IA - IC
        IP = IA * 100 + IC
cc      IF ( IC .LE. 14 ) THEN
C  MASSES FROM MASS TABLE FOR ISOTOPES
cc        IF ( IN .EQ. 0 ) THEN
cc          PAMA(IP) = IC * PAMA(14)
cc        ELSE
cc          PAMA(IP) = AMUS(IN,IC)
cc        ENDIF
C  SIMPLE SUM OF PROTON AND NEUTRON MASSES
cc        IF ( PAMA(IP) .EQ. 0.D0 )
cc   *               PAMA(IP) = IC * PAMA(14) + IN * PAMA(13)
cc      ELSE
C  WEIZSAECKERS MASS FORMULA GIVES BINDING ENERGY IN MEV
cc        B1 =  14.1D0 * IA
cc        B2 = -13.D0 * IA**TB3
cc        B3 = -0.595D0 * IC**2 / IA**OB3
cc        B4 = -19.D0 * (IC-IN)**2 / IA
cc        B5 =  33.5D0 / IA**0.75D0
cc        IF     ( MOD(IC,2) .EQ. 0  .AND.  MOD(IN,2) .EQ. 0 ) THEN
cc          SS =  1.D0
cc        ELSEIF ( MOD(IC,2) .EQ. 1  .AND.  MOD(IN,2) .EQ. 1 ) THEN
cc          SS = -1.D0
cc        ELSE
cc          SS =  0.D0
cc        ENDIF
cc        BIND = (B1 + B2 + B3 + B4 + SS*B5)* 1.D-3
cc        BIND = MAX( 0.D0, BIND )
cc        PAMA(IP) = IN * MASSES(13) + IC * MASSES(14) - BIND
cc      ENDIF
 
C  DO NOT USE BINDING ENERGY EFFECTS
        PAMA(IP) = IN * MASSES(13) + IC * MASSES(14)
 
C  NUCLEI ARE ASSUMED TO BE FULLY IONIZED
        SIGNUM(IP) = +IC
  3   CONTINUE
 
C  MASSES OF MULTINEUTRON CLUSTERS
      DO  4  IN = 1,59
        IP = 100 * IN
        PAMA  (IP) = IN * PAMA(13)
        SIGNUM(IP) = 0.D0
  4   CONTINUE
 
      RETURN
      END
