      BLOCK DATA EGS4BD
C                                VERSION 4.00  --  26 JAN 1986/1900
C******************************************************************
      COMMON/BOUNDS/ECUT(6),PCUT(6),VACDST
      COMMON/ELECIN/EKELIM,ICOMP,EKE0,EKE1,CMFP0,CMFP1,RANGE0,RANGE1, XR
     *0,TEFF0,BLCC,XCC,PICMP0(1),PICMP1(1),EICMP0(1),EICMP1(1),MPEEM(1),
     * ESIG0(500),ESIG1(500),PSIG0(500),PSIG1(500),EDEDX0(500),EDEDX1(50
     *0),PDEDX0(500),PDEDX1(500),EBR10(500),EBR11(500),PBR10(500),PBR11(
     *500),PBR20(500),PBR21(500),TMXS0(500),TMXS1(500),CMFPE0(1),CMFPE1(
     *1),CMFPP0(1),CMFPP1(1),ERANG0(1),ERANG1(1),PRANG0(1),PRANG1(1),CXC
     *2E0(1),CXC2E1(1),CXC2P0(1),CXC2P1(1),CLXAE0(1),CLXAE1(1),CLXAP0(1)
     *,CLXAP1(1), THR0(1,1),THR1(1,1),THR2(1,1),THRI0(1,1),THRI1(1,1),TH
     *RI2(1,1),FSTEP(16),FSQR(16),MSMAP(200), VERT1(1000),VERT2(100,16),
     *MSTEPS,JRMAX,MXV1, MXV2,NBLC,NRNTH,NRNTHI,BLC0,BLC1,RTHR0,RTHR1,RT
     *HRI0,RTHRI1
*KEEP,EPCONT.
      COMMON/EPCONT/   EDEP,RATIO,TSTEP,TUSTEP,USTEP,TVSTEP,VSTEP,IDISC,
     *                 IROLD,IRNEW,RHOFAC, EOLD,ENEW,EKE,ELKE,BETA2,GLE,
     *                 TSCAT,IAUSFL
      DOUBLE PRECISION EDEP,RATIO
      REAL             TSTEP,TUSTEP,USTEP,TVSTEP,VSTEP,RHOFAC,EOLD,ENEW,
     *                 EKE,ELKE,BETA2,GLE,TSCAT
      INTEGER          IDISC,IROLD,IRNEW,IAUSFL(29)
*KEND.
      COMMON /MEDIA/ NMED, RLC,RLDU,RLDUI,RHO,MSGE,MGE,MSEKE,MEKE,MLEKE,
     *MCMFP,MRANGE,IRAYLM,HBARO(6),HBAROI(6)
      CHARACTER MEDIA*24
      COMMON/MEDIAC/MEDIA
      COMMON/MISC/KMPI,KMPO,DUNIT,NOSCAT,MED(6),RHOR(6),IRAYLR(6)
      COMMON/MULTS/NG21,B0G21,B1G21,G210(7),G211(7),G212(7), NG22,B0G22,
     *B1G22,G220(8),G221(8),G222(8), NG31,B0G31,B1G31,G310(11),G311(11),
     *G312(11), NG32,B0G32,B1G32,G320(25),G321(25),G322(25), NBGB,B0BGB,
     *B1BGB,BGB0(8),BGB1(8),BGB2(8)
      COMMON/PATHCM/NPTH,B0PTH,B1PTH,PTH0(6),PTH1(6),PTH2(6)
      COMMON/THRESH/RMT2,RMSQ,ESCD2,AP,API,AE,UP,UE,TE,THMOLL
      COMMON/UPHIOT/THETA,SINTHE,COSTHE,SINPHI, COSPHI,PI,TWOPI,PI5D2
      DOUBLE PRECISION PZERO,PRM,PRMT2,RMI,VC
      COMMON/USEFUL/PZERO,PRM,PRMT2,RMI,VC,RM,MEDIUM,MEDOLD,IBLOBE,ICALL
      COMMON/ACLOCK/NCLOCK,JCLOCK
      CHARACTER MEDIA1*24
      EQUIVALENCE (MEDIA1,MEDIA)
      DATA NCLOCK/0/,JCLOCK/ 1/
      DATA ECUT/6*0./,PCUT/6*0./,VACDST/1.E8/
      DATA EKELIM/0./,ICOMP/1/
      DATA IAUSFL/5*1,24*0/,RHOFAC/1.0/
      DATA NMED/1/,MEDIA1/'NAI                     '/
      DATA IRAYLM/1*0/
      DATA KMPI/12/,KMPO/8/,DUNIT/1./,NOSCAT/0/
      DATA MED/6*1/,RHOR/6*0./,IRAYLR/6*0/
      DATA NG21/ 7/,B0G21/ 2.0000E+00/,B1G21/ 5.0000E+00/
      DATA G210(1),G211(1),G212(1)/-9.9140E-04, 2.7672E+00,-1.1544E+00/
      DATA G210(2),G211(2),G212(2)/-9.9140E-04, 2.7672E+00,-1.1544E+00/
      DATA G210(3),G211(3),G212(3)/-7.1017E-02, 3.4941E+00,-3.0773E+00/
      DATA G210(4),G211(4),G212(4)/-7.3556E-02, 3.5487E+00,-3.1989E+00/
      DATA G210(5),G211(5),G212(5)/ 3.6658E-01, 2.1162E+00,-2.0311E+00/
      DATA G210(6),G211(6),G212(6)/ 1.4498E+00,-5.9717E-01,-3.2951E-01/
      DATA G210(7),G211(7),G212(7)/ 1.4498E+00,-5.9717E-01,-3.2951E-01/
      DATA NG22/ 8/,B0G22/ 2.0000E+00/,B1G22/ 6.0000E+00/
      DATA G220(1),G221(1),G222(1)/-5.2593E-04, 1.4285E+00,-1.2670E+00/
      DATA G220(2),G221(2),G222(2)/-5.2593E-04, 1.4285E+00,-1.2670E+00/
      DATA G220(3),G221(3),G222(3)/-6.4819E-02, 2.2033E+00,-3.6399E+00/
      DATA G220(4),G221(4),G222(4)/ 3.7427E-02, 1.6630E+00,-2.9362E+00/
      DATA G220(5),G221(5),G222(5)/ 6.1955E-01,-6.2713E-01,-6.7859E-01/
      DATA G220(6),G221(6),G222(6)/ 1.7584E+00,-4.0390E+00, 1.8810E+00/
      DATA G220(7),G221(7),G222(7)/ 2.5694E+00,-6.0484E+00, 3.1256E+00/
      DATA G220(8),G221(8),G222(8)/ 2.5694E+00,-6.0484E+00, 3.1256E+00/
      DATA NG31/ 11/,B0G31/ 2.0000E+00/,B1G31/ 9.0000E+00/
      DATA G310(1),G311(1),G312(1)/ 4.9437E-01, 1.9124E-02, 1.8375E+00/
      DATA G310(2),G311(2),G312(2)/ 4.9437E-01, 1.9124E-02, 1.8375E+00/
      DATA G310(3),G311(3),G312(3)/ 5.3251E-01,-6.1555E-01, 4.5595E+00/
      DATA G310(4),G311(4),G312(4)/ 6.6810E-01,-2.2056E+00, 8.9293E+00/
      DATA G310(5),G311(5),G312(5)/-3.8262E+00, 2.5528E+01,-3.3862E+01/
      DATA G310(6),G311(6),G312(6)/ 4.2335E+00,-1.0604E+01, 6.6702E+00/
      DATA G310(7),G311(7),G312(7)/ 5.0694E+00,-1.4208E+01, 1.0456E+01/
      DATA G310(8),G311(8),G312(8)/ 1.4563E+00,-3.3275E+00, 2.2601E+00/
      DATA G310(9),G311(9),G312(9)/-3.2852E-01, 1.2938E+00,-7.3254E-01/
      DATA G310(10),G311(10),G312(10)/-2.2489E-1, 1.0713E+0,-6.1358E-1/
      DATA G310(11),G311(11),G312(11)/-2.2489E-1, 1.0713E+0,-6.1358E-1/
      DATA NG32/ 25/,B0G32/ 2.0000E+00/,B1G32/ 2.3000E+01/
      DATA G320(1),G321(1),G322(1)/ 2.9907E-05, 4.7318E-01, 6.5921E-01/
      DATA G320(2),G321(2),G322(2)/ 2.9907E-05, 4.7318E-01, 6.5921E-01/
      DATA G320(3),G321(3),G322(3)/ 2.5820E-03, 3.5853E-01, 1.9776E+00/
      DATA G320(4),G321(4),G322(4)/-5.3270E-03, 4.9418E-01, 1.4528E+00/
      DATA G320(5),G321(5),G322(5)/-6.6341E-02, 1.4422E+00,-2.2407E+00/
      DATA G320(6),G321(6),G322(6)/-3.6027E-01, 4.7190E+00,-1.1380E+01/
      DATA G320(7),G321(7),G322(7)/-2.7953E+00, 2.6694E+01,-6.0986E+01/
      DATA G320(8),G321(8),G322(8)/-3.6091E+00, 3.4125E+01,-7.7512E+01/
      DATA G320(9),G321(9),G322(9)/ 1.2491E+01,-7.1103E+01, 9.4496E+01/
      DATA G320(10),G321(10),G322(10)/ 1.9637E+1,-1.1371E+2, 1.5794E+2/
      DATA G320(11),G321(11),G322(11)/ 2.1692E+0,-2.5019E+1, 4.5340E+1/
      DATA G320(12),G321(12),G322(12)/-1.6682E+1, 6.2067E+1,-5.5257E+1/
      DATA G320(13),G321(13),G322(13)/-2.1539E+1, 8.2651E+1,-7.7065E+1/
      DATA G320(14),G321(14),G322(14)/-1.4344E+1, 5.5193E+1,-5.0867E+1/
      DATA G320(15),G321(15),G322(15)/-5.4990E+0, 2.3874E+1,-2.3140E+1/
      DATA G320(16),G321(16),G322(16)/ 3.1029E+0,-4.4708E+0, 2.1318E-1/
      DATA G320(17),G321(17),G322(17)/ 6.0961E+0,-1.3670E+1, 7.2823E+0/
      DATA G320(18),G321(18),G322(18)/ 8.6179E+0,-2.0950E+1, 1.2536E+1/
      DATA G320(19),G321(19),G322(19)/ 7.5064E+0,-1.7956E+1, 1.0520E+1/
      DATA G320(20),G321(20),G322(20)/ 5.9838E+0,-1.4065E+1, 8.0342E+0/
      DATA G320(21),G321(21),G322(21)/ 4.4959E+0,-1.0456E+1, 5.8462E+0/
      DATA G320(22),G321(22),G322(22)/ 3.2847E+0,-7.6709E+0, 4.2445E+0/
      DATA G320(23),G321(23),G322(23)/ 1.9514E+0,-4.7505E+0, 2.6452E+0/
      DATA G320(24),G321(24),G322(24)/ 4.8808E-1,-1.6910E+0, 1.0459E+0/
      DATA G320(25),G321(25),G322(25)/ 4.8808E-1,-1.6910E+0, 1.0459E+0/
      DATA NBGB/ 8/,B0BGB/ 1.5714E+00/,B1BGB/ 2.1429E-01/
      DATA BGB0(1),BGB1(1),BGB2(1)/-1.0724E+00, 2.8203E+00,-3.5669E-01/
      DATA BGB0(2),BGB1(2),BGB2(2)/ 3.7136E-01, 1.4560E+00,-2.8072E-02/
      DATA BGB0(3),BGB1(3),BGB2(3)/ 1.1396E+00, 1.1910E+00,-5.2070E-03/
      DATA BGB0(4),BGB1(4),BGB2(4)/ 1.4908E+00, 1.1267E+00,-2.2565E-03/
      DATA BGB0(5),BGB1(5),BGB2(5)/ 1.7342E+00, 1.0958E+00,-1.2705E-03/
      DATA BGB0(6),BGB1(6),BGB2(6)/ 1.9233E+00, 1.0773E+00,-8.1806E-04/
      DATA BGB0(7),BGB1(7),BGB2(7)/ 2.0791E+00, 1.0649E+00,-5.7197E-04/
      DATA BGB0(8),BGB1(8),BGB2(8)/ 2.0791E+00, 1.0649E+00,-5.7197E-04/
      DATA NPTH/ 6/,B0PTH/ 2.0000E+00/,B1PTH/ 1.8182E+01/
      DATA PTH0(1),PTH1(1),PTH2(1)/ 1.0000E+00, 9.8875E-01, 2.5026E+00/
      DATA PTH0(2),PTH1(2),PTH2(2)/ 1.0000E+00, 9.8875E-01, 2.5026E+00/
      DATA PTH0(3),PTH1(3),PTH2(3)/ 1.0060E+00, 7.8657E-01, 4.2387E+00/
      DATA PTH0(4),PTH1(4),PTH2(4)/ 1.0657E+00,-2.5051E-01, 8.7681E+00/
      DATA PTH0(5),PTH1(5),PTH2(5)/ 1.6971E+00,-7.5600E+00, 2.9946E+01/
      DATA PTH0(6),PTH1(6),PTH2(6)/ 1.6971E+00,-7.5600E+00, 2.9946E+01/
      DATA RMT2/1.022007/,RMSQ/.2611245/
      DATA PI/3.141593/,TWOPI/6.283185/,PI5D2/7.853982/
      DATA ICALL/0/,RM/.5110034/
      END
