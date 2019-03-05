C============================================================
C    SSSSSS   IIIIIII  BBBBB   YY      YY   L        L
C   S            I     B    B    YY  YY     L        L
C    SSSSS       I     BBBBB       YY       L        L
C         S      I     B    B      YY       L        L
C   SSSSSS    IIIIIII  BBBBB       YY       LLLLLLL  LLLLLLL
C=============================================================
C  Code for SIBYLL:  hadronic interaction Montecarlo
C=============================================================
C
C   Version 1.6   
C
C       By   R.S. Fletcher
C            T.K. Gaisser
C            Paolo Lipari
C            Todor Stanev
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C*****  Please  have people who want this code contact one of the authors.
C*****  Please report any problems.       *******
C
C      For a correct copy contact:
C      Decnet   6360::Gaisser
C               6360::Stanev
C               JHUP::Fletcher
C               40808::Lipari
C
C      Internet  Gaisser@brivs2.bartol.udel.edu
C                Stanev@udbri.bartol.udel.edu
C                Fletcher@JHUP.pha.jhu.edu
C                Lipari@roma1.infn.it
C
C       16-4-97  Bug in IFLAV eliminated by D.H.
C 
C       15-4-97  Bugs in PART_INT eliminated by D.H.
C 
C       17-3-97  Bounds_check fixed in FPNI, NJETR, SIB_SIGMA_HAIR,
C                SIB_SIGMA_PIP, SIB_SIGMA_PP, SIGMA_PIP,SIGMA_PP by D.H.
C
C       18-6-96  Bugs in ZSAMPLE and NUC_CONF eliminated by D.H.
C
C       10-5-96  Bug in treatment of antinucleons eliminated
C
C       15-9-95  random  number generator  as RNDM(0)
C                minor corrections
C
C       6-15-94: minor changes suggested by  O. Palamara 
C                for compatiblility with 
C                MACRO codes. Parameter statements made standard. 
C                Bug in Parton found by S. Kashahara fixed.
C                NUCLIB code moved to the end of the file for 
C                easy removal.
C       
C      2-4-94: Bug fix to avoid infinite loop  at low energies. BEAM_SPLIT
C      
C      4-93 This version is the first to include the NUCLIB like
C      treatment of the wounded nucleon distributions.
C
C=============================================================
C==========SIBYLL=============================================
C=============================================================
C+++++ Code for SIBYLL:  hadronic interaction Montecarlo
C=============================================================
C
C      SIBYLL is an hadronic interaction Monte Carlo simulation
C      based on  the dual parton Model, String Fragmentation
c      and the minijet model. It reproduces data 
C      resonably well from around 200. GeV up.
C      The program runs from Sqrt(s)=Sqrt(2.*Mp*E_beam)= 10 GeV
C      up to sqrt(s)=10**6 GeV (around 10**21 eV in the laboratory system).
C
C      The program gets inefficient at low energies 
C      (below sqrt[s] = 20 GeV or so)
C      Results at very high energy are subject to all the  usual
C      problems associated with extrapolating over several 
C      orders of magnitude in energy.
C===========================================================================
      function A_pip (b)
C...Convolution of parton distribution for pip interaction
      IMPLICIT REAL*4 (A-Z)
      COMMON /S_CH0CNV/ NU2, MU2, NUPI2, NU, MU, NUPI
      data pi / 3.1415926/

      eta = nu2/nupi2
      c = nu2/(2.*pi) * 1./(1.-eta)

      if (b .gt. 0.)  then
         b1 = b*nu
         b2 = b*nupi
         f1 = 0.5*b1 * bessk1(b1)
         f2 = eta/(1.-eta)*(bessk0(b2)- bessk0(b1))
         A_pip = c*(f1+f2)
      else
         A_pip = c*(0.5 + eta/(1.-eta)*log(nu/nupi))
      endif
      return
      end
      function A_pp (b)
C...Convolution of parton distribution for pp interaction
      IMPLICIT REAL*4 (A-Z)
      COMMON /S_CH0CNV/ NU2, MU2, NUPI2, NU, MU, NUPI
      data pi / 3.1415926/
      c = nu**5/(96.*pi)
      if (b .gt. 0.)  then
         A_pp = c*b**3 * bessk (3, b*nu)
      else
         A_pp = nu**2/(12.*pi)
      endif
      return
      end
      REAL FUNCTION BDIFFRACT(SQS,ipart)
C
C    INPUT    SQS (GeV)
C             ipart is the code for the scattering particle(p,pi)
C    OUTPUT:  DIFFRACT is the single diffractive cross
C            section parameterized with a log.
c            in mB
C                                    RSF
C......................................................
      real a(2),b(2)
      data a/0.0,1.2/
      data b/1.56,0.66/
      BDIFFRACT = (a(ipart)+b(ipart)*log(sqs))/2.0
      RETURN
      END
      SUBROUTINE BEAM_SPLIT (L, NW, XX, IFL, XJET, LXBAD,stringmas0)
C...This subroutine split a hadron of code L
C.  into 2*NW partons, each of energy XX(j) and
C.  flavor IFL.  The minimum fractional energy of
C.  each parton is X_min = 2*stringmas0/sqrt(s)
c.
c.  Variable qmas changed to stringmas0 to agree with name in SIBYLL
c.      and added to calling sequenceto insure symetry.
c      Also a factor of (1-xjet) is added to the def. of xmin for nw=1
c.                               RSF  Apr-2-92
C---------------------------------------------------------------------
      COMMON /S_RUN/ SQS, S, Q2MIN, XMIN, ZMIN , kb ,kt
      DIMENSION XX(30), IFL(30)
      DATA AC /-0.2761856692/             ! log(2) - gamma(Eulero)
      DATA GAMMA /2./
      DATA NBAD / 0 /
c-------
c  New code to handle low energy p nuc problem.
c------
      LXBAD = 0
      XMIN = 2.*stringmas0/SQS
      IF (1.-XJET .LT. FLOAT(2*NW)*XMIN)  THEN
         NBAD = NBAD + 1
         LXBAD = 1
         IF (NBAD .LE. 100) THEN
           WRITE (6, *) 'BEAM_SPLIT: kinematically forbidden situation'
           WRITE (6, 5)  NBAD, SQS, XJET, NW
         ENDIF
 5       FORMAT(1X,'NBAD = ',I3,3X,'sqs = ',E10.3,
     &            3X, 'x_jet = ', F9.3, 3X, ' NW = ',I2)
         IF (NBAD .eq. 100) THEN
           WRITE (6, *)
     &     ' BEAM_SPLIT : Last warning about bad splittings '
           WRITE (6, *) ' The energy threshold is probably too low.'
         ENDIF
         RETURN
      ENDIF

      IF (NW .EQ. 1)  THEN
         XVAL = 1.-XJET
         GOTO 200
      ENDIF

C...Choose total energy of sea partons
      N = 2*(NW-1)
      Z1 = LOG(FLOAT(N))
      Z2 = LOG(0.5*SQS*(1.-XJET)/stringmas0-2.)
100   R=RAN(0)
      Z=(Z1+AC)*(1.+R*(((Z2+AC)/(Z1+AC))**N-1.))**(1./FLOAT(N))-AC
      XSEA = XMIN*EXP(Z)
      IF ( (1.-XSEA)**GAMMA .LT. RAN(0)) GOTO 100
C...Split the energy  of sea partons among the different partons
      XREM = XSEA - FLOAT(N)*XMIN
      DO J=3,N+1
         XA = XREM*RAN(0)
         XREM = XREM - XA
         XX(J) = XMIN + XA
      ENDDO
      XX(N+2) = XMIN + XREM
      XVAL = 1.-XSEA-XJET
C...Flavor of sea partons
      DO J=1,N/2
         J1 =  3 + (J-1)*2
         IFL(J1) = INT(1.+1.99*RAN(0))
         IFL(J1+1) = -IFL(J1)
      ENDDO
C...Prepare the valence partons
200   CALL HSPLI (L,IFL(1),IFL(2))
      CHI = CHIDIS(L,IFL(1),IFL(2))
      XX(1) = MAX(CHI*XVAL,XMIN)
      XX(1) = MIN(XX(1),XVAL-XMIN)
C      FOR MESONS, SPLIT ENERGY SYMETRICALLY.
C????? SPLIT K'S WITH ENERGY TO S QUARK?
C
      if (abs(l).le.12.and.RAN(0).le.0.5) xx(1)=XVAL-XX(1)
      XX(2) = XVAL-XX(1)
      RETURN
      END

      FUNCTION BESSI0(X)
C----------------------------------------------------------------------------
C  Bessel functions
C----------------------------------------------------------------------------
      REAL*8 Y,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9
      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,3.5156229D0,3.0899424D0,
     *    1.2067492D0,
     *    0.2659732D0,0.360768D-1,0.45813D-2/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,0.1328592D-1,
     *    0.225319D-2,-0.157565D-2,0.916281D-2,-0.2057706D-1,
     *    0.2635537D-1,-0.1647633D-1,0.392377D-2/
      IF (ABS(X).LT.3.75) THEN
        Y=(X/3.75)**2
        BESSI0=P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))))
      ELSE
        AX=ABS(X)
        Y=3.75/AX
        BESSI0=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4
     *      +Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
      ENDIF
      RETURN
      END
      FUNCTION BESSI1(X)
C----------------------------------------------------------------------------
C  Bessel functions
C----------------------------------------------------------------------------
      REAL*8 Y,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9
      DATA P1,P2,P3,P4,P5,P6,P7/0.5D0,0.87890594D0,0.51498869D0,
     *    0.15084934D0,0.2658733D-1,0.301532D-2,0.32411D-3/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,-0.3988024D-1,
     *    -0.362018D-2,0.163801D-2,-0.1031555D-1,0.2282967D-1,
     *    -0.2895312D-1,0.1787654D-1,-0.420059D-2/
      IF (ABS(X).LT.3.75) THEN
        Y=(X/3.75)**2
        BESSI1=X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
        AX=ABS(X)
        Y=3.75/AX
        BESSI1=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+
     *      Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
      ENDIF
      RETURN
      END
      FUNCTION BESSK(N,X)
C----------------------------------------------------------------------------
C  Bessel functions
C----------------------------------------------------------------------------
      IF (N.LT.2) PAUSE 'bad argument N in BESSK'
      TOX=2.0/X
      BKM=BESSK0(X)
      BK=BESSK1(X)
      DO 11 J=1,N-1
        BKP=BKM+J*TOX*BK
        BKM=BK
        BK=BKP
11    CONTINUE
      BESSK=BK
      RETURN
      END
      FUNCTION BESSK0(X)
C----------------------------------------------------------------------------
C  Bessel functions
C----------------------------------------------------------------------------
      REAL*8 Y,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7
      DATA P1,P2,P3,P4,P5,P6,P7/-0.57721566D0,0.42278420D0,
     *    0.23069756D0,0.3488590D-1,0.262698D-2,0.10750D-3,0.74D-5/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,-0.7832358D-1,
     * 0.2189568D-1,-0.1062446D-1,0.587872D-2,-0.251540D-2,0.53208D-3/
      IF (X.LE.2.0) THEN
        Y=X*X/4.0
        BESSK0=(-LOG(X/2.0)*BESSI0(X))+(P1+Y*(P2+Y*(P3+
     *        Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
        Y=(2.0/X)
        BESSK0=(EXP(-X)/SQRT(X))*(Q1+Y*(Q2+Y*(Q3+
     *        Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
      ENDIF
      RETURN
      END
      FUNCTION BESSK1(X)
C----------------------------------------------------------------------------
C  Bessel functions
C----------------------------------------------------------------------------
      REAL*8 Y,P1,P2,P3,P4,P5,P6,P7,
     *    Q1,Q2,Q3,Q4,Q5,Q6,Q7
      DATA P1,P2,P3,P4,P5,P6,P7/1.0D0,0.15443144D0,-0.67278579D0,
     *    -0.18156897D0,-0.1919402D-1,-0.110404D-2,-0.4686D-4/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7/1.25331414D0,0.23498619D0,
     *    -0.3655620D-1,0.1504268D-1,-0.780353D-2,0.325614D-2,
     *    -0.68245D-3/
      IF (X.LE.2.0) THEN
        Y=X*X/4.0
        BESSK1=(LOG(X/2.0)*BESSI1(X))+(1.0/X)*(P1+Y*(P2+
     *      Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
        Y=2.0/X
        BESSK1=(EXP(-X)/SQRT(X))*(Q1+Y*(Q2+Y*(Q3+
     *      Y*(Q4+Y*(Q5+Y*(Q6+Y*Q7))))))
      ENDIF
      RETURN
      END
      SUBROUTINE BLOCK(SQS,SIG1,SIG2,SLOP1,SLOP2,
     +                 RHO1,RHO2,SIGEL1,SIGEL2)
C------------------------------------------------------------------------
C.  Fit of Block and Cahn to pp and pbar-p cross sections
C------------------------------------------------------------------------
C...p-p and pbar-p cross sections
C.  Parametrization of  Block and Cahn
C
C.  INPUT  : SQS   (GeV)  = c.m. energy
C.
C.  OUPUT : SIG1 (mbarn)    = pp  total  cross section
C.          SLOP1 (GeV**2)  = slope of elastic scattering
C.          RHO1            = Real/Imaginary part of the amplitude
C.                            for forward elastic  scattering (pp)
C.          SIGEL1 (mbarn)  = pp  elastic scattering  cross section
C.          [1 -> 2   : pp -> pbar p]
C-----------------------------------------------------------------------
      DATA PI /3.1415926/
      DATA CMBARN /0.389385/
      S = SQS*SQS
      CALL FPLUS  (S, FR, FI)
      CALL FMINUS (S, GR, GI)
      SIG1 = FI-GI
      SIG2 = FI+GI
      RHO1 = (FR-GR)/(FI-GI)
      RHO2 = (FR+GR)/(FI+GI)
      CALL SSLOPE (S, BP, BM)
      SLOP1 = BP - GI/FI*(BM-BP)
      SLOP2 = BP + GI/FI*(BM-BP)
      SIGEL1 = SIG1**2*(1.+RHO1**2)/(16.*PI*SLOP1)/CMBARN
      SIGEL2 = SIG2**2*(1.+RHO2**2)/(16.*PI*SLOP2)/CMBARN
      RETURN
      END

      SUBROUTINE BLOCK_INI
C...Parameters of fit IFIT=1 of Block and Cahn
      COMMON /BLOCKC/ AA, BETA, S0, CC, AMU, DD, ALPHA, A0
      COMMON /BLOCKD/ CP, DP, EP, CM, DM
      AA = 41.74
      BETA = 0.66
      S0 = 338.5
      CC = 0.
      AMU = 0.
      DD = -39.37
      ALPHA = 0.48
      A0 = 0.
      CP = 10.90
      DP = -0.08
      EP = 0.043
      CM = 23.27
      DM = 0.93
      RETURN
      END

      FUNCTION CHIDIS (KPARTin, IFL1, IFL2)
C...Generate CHI (fraction of energy of a hadron carried by
C.                the valence quark, or diquark, as specified by IFL1)
C.  INPUT KPART = code of particle
C.        IFL1, IFL2 = codes of partons (3, 3bar of color)
C.........................................................
      COMMON /S_RUN/ SQS, S, Q2MIN, XMIN, ZMIN , kb ,kt
      COMMON /S_CPSPL/ CCHIK(3,7:14)
      COMMON/S_cutof/stringmas0
C O. Palamara 27/8/1993
C      parameter QMAS=0.35
      parameter (QMAS=0.35)
      kpart=IABS(kpartin)
      IFQ=IABS(IFL1)
      IF (IFQ.GT.10) IFQ=IABS(IFL2)
c      CUT=2.*QMAS/SQS
      CUT=2.*stringmas0/SQS
100      CHIDIS=RAN(0)**2
      if (chidis.lt.cut) goto 100
      if (chidis.gt.(1.-cut)) goto 100
c      IF((CHIDIS**2/(CHIDIS**2+CUT**2))**0.25
      IF((CHIDIS**2/(CHIDIS**2+CUT**2))**0.5
     +   *(1.-CHIDIS)**CCHIK(IFQ,KPART).LT.RAN(0)) GOTO 100
      CHIDIS = MAX(0.5*CUT,CHIDIS)
      CHIDIS = MIN(1.-CUT,CHIDIS)
      IF (IABS(IFL1).GT.10)  CHIDIS=1.-CHIDIS
      RETURN
      END
      REAL FUNCTION DDIFFRACT(SQS,ipart)
C... This routine only includes pp scattering.
C    INPUT    SQS (GeV)
C             ipart is the code for the scattering particle(p,pi)
C    OUTPUT:  dDIFFRACT is the Double Diffractive cross
C            section parameterized with a log.
C            in mb.
C                                    RSF
C......................................................
      real a(2),b(2)
      data a/-1.23,.41/
      data b/.7,0.12/
      DDIFFRACT =( a(ipart)+b(ipart)*log(sqs))
      RETURN
      END
      FUNCTION DENSA (Z)
C....Woods Saxon nuclear density (normalised to 1)
C.   for a nucleus of mass number A.
C.   INPUT z = z coordinate (fm)
C.         JA = integer mass number
C.         B (in common /CC01/)  impact parameter  (fm)
C.  OUTPUT (fm**-3)
C--------------------------------------------------------
      COMMON /CC01/  B
      COMMON /CCDA/ JA
      COMMON /CWOOD/ RR0(19:56), AA0(19:56), CC0(19:56)
      R = SQRT (Z*Z + B*B)
      DENSA = CC0(JA)/(1.+EXP((R-RR0(JA))/AA0(JA)))
      RETURN
      END

      FUNCTION DENS_NUC (R, JA)
C===========================================================================
C.   Code about nuclear densities
C===========================================================================
C....Nuclear density (normalised to 1)
C.   for a nucleus of mass number JA
C.   INPUT R = radial coordinate  (fm)
C.         JA = integer mass number
C.  OUTPUT (fm**-3)
C--------------------------------------------------------
      COMMON /CWOOD/ RR0(19:56), AA0(19:56), CC0(19:56)
      IF (JA .GT. 18)  THEN
         DENS_NUC = WOOD_SAXON(R,JA)
      ELSE IF (JA .NE. 4)  THEN
         DENS_NUC = HELIUM(R)
      ELSE
         DENS_NUC = SHELL(R,JA)
      ENDIF
      RETURN
      END

      SUBROUTINE DIFDEC (L0, P0)
C..."decay" of an excited state with the quantum numbers
C.   of particle L0 and the 5-momentum P0
C........................................................
      COMMON /S_PLIST/ NP, P(5000,5), LLIST(5000)
      COMMON /S_MASS1/ AM(49), AM2(49)
      COMMON /S_CHP/ ICHP(49), ISTR(49), IBAR(49)
      DIMENSION P0(5), LL(10), PD(10,5), BE(3), LCON(7:14)
      DATA EMIN /0.7/
      DATA LCON /6,6,11,11,9,9,14,13/
      DATA PCHEX /0.33/            ! probability of charge exchange

      LA = IABS(L0)
      DELTAE = P0(5) - AM(LA)

C..."string-like" decay
      IF (DELTAE .GT. EMIN)  THEN
           N1 = NP+1
         CALL HSPLI(L0,IFL1,IFL2)
         IF (P0(3) .GT. 0.)  THEN
            IFLA = IFL2
            IFL2 = IFL1
            IFL1 = IFLA
         ENDIF
10         CALL STRING_FRAG (P0(5), IFL1, IFL2, 0.,0.,0.,0.,IFBAD)
         IF (IFBAD .EQ. 1)  GOTO 10
         DO J=1,3
            BE(J)=P0(J)/P0(4)
         ENDDO
         GA=P0(4)/P0(5)
         DO I=N1,NP
            BEP=BE(1)*P(I,1)+BE(2)*P(I,2)+BE(3)*P(I,3)
            DO J=1,3
               P(I,J)=P(I,J)+GA*(GA*BEP/(1.+GA)+P(I,4))*BE(J)
            ENDDO
            P(I,4)=GA*(P(I,4)+BEP)
         ENDDO

C...Phase space decay of the excited state
      ELSE
        AV = 2.*SQRT(DELTAE)
100        NPI = AV*(1.+0.5*GASDEV(0))
        IF(NPI.LE.0.OR.NPI.GT.9.OR.AM(LA)+NPI*AM(7)+0.02
     .            .GT.P0(5))  GOTO 100
        IF (RAN(0).LT.PCHEX)  THEN
         LL(NPI+1) = LCON(LA)*ISIGN(1,L0)
         IF(L0 .EQ. 11)  LL(NPI+1) = LL(NPI+1)+INT(2.*RAN(0))
        ELSE
            LL(NPI+1) = L0
        ENDIF
        JQQ = ICHP(LA)*ISIGN(1,L0)-
     .            ICHP(IABS(LL(NPI+1)))*ISIGN(1,LL(NPI+1))
120        JQTOT = 0.
        DO K=1,NPI-1
           LL(K) = 6+INT(RAN(0)*2.99999)
           JQTOT = JQTOT + ICHP(LL(K))
        ENDDO
        JQR = JQQ-JQTOT
        IF (JQR.LT.-1.OR.JQR.GT.1)  GOTO 120
        LL(NPI) = 6+JQR
        IF (LL(NPI) .EQ. 5)  LL(NPI)=8
        CALL DECPAR (0,P0,NPI+1,LL, PD)
        DO J=1,NPI+1
           NP = NP+1
           LLIST(NP) = LL(J)
           DO K=1,5
              P(NP,K) = PD(J,K)
           ENDDO
        ENDDO
      ENDIF
      RETURN
      END


      SUBROUTINE DIFF_GEN (L0, JDIF)
C----------------------------------------------------------------------------
C  Code for diffraction
C----------------------------------------------------------------------------
C...Single diffractive interaction
C.  INPUT L0 = index of "beam particle"
C.             the target is assumed to be a proton.
C.        JDIF = 1  "beam diffraction"
C.             = 2  "target diffraction"
C.             = 3  "double diffraction"
C......................................................
      COMMON /S_PLIST/ NP, P(5000,5), LLIST(5000)
      COMMON /S_RUN/ SQS, S, Q2MIN, XMIN, ZMIN , kb ,kt
      COMMON /S_MASS1/ AM(49), AM2(49)
      COMMON /S_CHIST/ NW,NJET,NNJET(15),XX1JET(100)
     +   ,XX2JET(100),PPTJET(100),PHIJET(100),NNPJET(100),NNPSTR(30)
     +   , JJDIF, XMB, XMT
      DIMENSION XM2MIN(3), ALXMIN(3)
      DIMENSION P0(5)
      DIMENSION KK(7:14)

      DATA PI /3.1415926/
      DATA KK /2*2,4*3,2*1/
      DATA XM2MIN /1.5, 0.2, 0.6/                  ! M_x**2(min) GeV**2
      DATA ALXMIN /0.405465,-1.6094379,-0.5108256/      ! log[M_x**2(min)]
      DATA SLOP0 /6.5/            ! b (slope_ for Mx**2 > 5 GeV**2
      DATA ASLOP /31.10362/            ! fit to the slope parameter.
      DATA BSLOP /-15.29012/

      LA = IABS(L0)
      XM2MAX = 0.10*S

C...Double diffraction
      IF (JDIF .EQ. 3)   THEN
         K = KK(LA)
         AL = LOG(XM2MAX/XM2MIN(K))
         ALX = ALXMIN(K) + AL*RAN(0)
         XMB2 = EXP(ALX)
         XMB = SQRT (XMB2)
         AL = LOG(XM2MAX/XM2MIN(1))
         ALX = ALXMIN(1) + AL*RAN(0)
         XMT2 = EXP(ALX)
         XMT = SQRT (XMT2)
         X1 = 1.+(XMB2-XMT2)/S
         X2 = 2.-X1
         SLOPE = MAX(SLOP0, ASLOP+BSLOP*ALX)
50       T = -LOG(RAN(0))/SLOPE
         PT = SQRT(T)
         PZ1 = 0.25*S*X1*X1-XMB2-PT*PT
         PZ2 = 0.25*S*X2*X2-XMT2-PT*PT
         IF (PZ1.LT.0. .OR. PZ2.LT.0.)   GOTO 50
         PHI = PI*RAN(0)
         P0(5) = XMB
         P0(4) = 0.5*SQS*X1
         P0(1) = PT*COS(PHI)
         P0(2) = PT*SIN(PHI)
         P0(3) = SQRT(PZ1)
         CALL DIFDEC (L0, P0)
         P0(5) = XMT
         P0(4) = 0.5*SQS*X2
         P0(1) = -P0(1)
         P0(2) = -P0(2)
         P0(3) = -SQRT(PZ2)
         CALL DIFDEC (13, P0)
         RETURN
      ENDIF

C...Single diffraction
      IF (JDIF.EQ. 1)  THEN
         K = KK(LA)
         EM  = AM(13)
         EM2 = AM2(13)
         L = 13
         ZD = -1.
      ELSE
         K = 1
         EM  = AM(LA)
         EM2 = AM2(LA)
         L = L0
         ZD = +1.
      ENDIF
C      Generate the mass of the diffracted system Mx (1/Mx**2 distribution)
      AL = LOG(XM2MAX/XM2MIN(K))
      ALX = ALXMIN(K) + AL*RAN(0)
      XM2 = EXP(ALX)
      XM = SQRT (XM2)
      XMB = XM
      XMT = XM
C      Generate the Kinematics of the pseudoelastic hadron
      X = 1.-(XM2-EM2)/S
      NP = NP+1
      P(NP,4) = 0.5*SQS*X
      SLOPE = MAX(SLOP0, ASLOP+BSLOP*ALX)
60      T = -LOG(MAX(1.E-10,RAN(0)))/SLOPE
      PT = SQRT(T*X)
      PZ2 = P(NP,4)**2-EM2 - PT*PT
      IF (PZ2 .LT.0.)   GOTO 60
      PHI = PI*RAN(0)
      P(NP,3) = SQRT(PZ2)*ZD
      P(NP,1) = PT*COS(PHI)
      P(NP,2) = PT*SIN(PHI)
      P(NP,5) = EM
      LLIST(NP) = L
C      Generating the hadronic system recoling against the produced particle
      P0(5) = SQRT(XM2)
      P0(4) = 0.5*SQS*(2.-X)
      DO J=1,3
         P0(J) = -P(NP,J)
      ENDDO
      CALL DIFDEC (L0, P0)
      RETURN
      END
      function estar(ap,at,b)
      implicit real*8(a-h,o-z)
      real*4 ap,at,b,estar
      sigma=4.5  !total n-n cross section in fm**2
      rt=.82*at**.3333 !target radius
      rp=.82*ap**.3333 !projectile radius
      alpha=rt**2/rp**2
      beta=b**2/rt**2
      f=at*sigma/(3.14159*rt**2)
      alf = log(f)
      alalf = log(alpha)
      gfac=0
      gfac1=0
      s1=0.
      s2=0.
      s3=0.
      ii=1
      do n=0,10 ! This limit may not need to be so high.
         if(n.ge.2) then
            gfac1=gfac
            gfac=gfac+log(float(n))
         endif
         g0=n*alf -n*beta*alpha/(n+alpha)+alalf
         g1=g0-log(alpha+n)-gfac
         g2=(n+2)*log(f)-(n+2)*beta*alpha/(n+2+alpha)
     >      +log(n+2+alpha+beta*alpha**2)-3*log(n+2+alpha)-gfac
         g3=g0-2*log(n+alpha)-gfac1
         ii=-ii
         s1=s1+ii*exp(g1)
         s2=s2+ii*exp(g2)
         if(n.ge.1) s3=s3+ii*exp(g3)
      enddo

      pb=s1
      e1b=197.**2/(2*938.*rp**2*pb) *s2
c      a=b*(s3/pb-1)
c      a=-b*s3/pb
c      e2b=-.5* 938. * (41./(ap**.333))**2 * a**2 /(197.**2)
c      estar=e1b+e2b
      estar = e1b
      return
      end

      FUNCTION ESTARP (NPF, NW)
C CONTRIBUTION TO E* FROM ENERGY DEPOSITED BY SECONDARIES
C VERY NAIVE VERSION INCORPORATING HUEFFNER'S IDEAS
      APF = NPF
      F1 = 15.3/APF**0.666666666
C AVERAGE KINETIC ENERGY/NUCLEON IN PREFRAGMENT (MeV)
C PER PATHLENGTH EQUAL TO THE PREFRAGMENT RADIUS
      ESTARP = 0.
      DO I=1,NW
      IF (RAN(0) .GT. 0.5) THEN
      F2 = F1*RDIS(0)
      ESTARP = ESTARP + F2
      ENDIF
      ENDDO
C SAMPLE RANDOMLY PER WOUNDED NUCLEON, x NW
      RETURN
      END
      SUBROUTINE ESUM(N1,N2,ETOT,PXT,PYT,PZT,NF)
C...Return the energy,px,py,pz and the number of stable
C.  particles in the list between N1 and N2
      COMMON /S_PLIST/ NP, P(5000,5), LLIST(5000)
      NF=0
      ETOT=0.
      PXT=0.
      PYT=0.
      PZT=0.
      DO J=N1,N2
         L = LLIST(J)
         IF (IABS(L) .LT. 10000)  THEN
           NF = NF+1
           ETOT = ETOT + P(J,4)
           PXT = PXT + P(J,1)
           PYT = PYT + P(J,2)
           PZT = PZT + P(J,3)
         ENDIF
      ENDDO
      RETURN
      END
      subroutine evap(npf,eb,eps,nnuc,nalp)
      eps=7.5+sqrt(8*eb)
      n=min(npf*int(eb/eps),npf)
      nalp=n/5
      nnuc=n-4*nalp
      return
      end
      SUBROUTINE FACT_INI
      COMMON /S_CFACT/ FACT (0:20), CO_BIN(0:20,0:20)
      FACT(0) = 1.
      DO J=1,20
         FACT(J) = FACT(J-1)*FLOAT(J)
      ENDDO
      DO J=0,20
         DO K=0,J
            CO_BIN(J,K) = FACT(J)/(FACT(K)*FACT(J-K))
         ENDDO
      ENDDO
      RETURN
      END
      REAL FUNCTION FDIFFRACT(SQS,ipart)
C==================================================================
C..Diffractive cross sections
C==================================================================
C... This routine only includes pp scattering.
C    INPUT    SQS (GeV)
C             ipart is the code for the scattering particle(p,pi)
C    OUTPUT:  DIFFRACT is the single diffractive cross
C            section parameterized with a log.
c            in mb.
C                                    RSF
C......................................................
      real a(2),b(2)
      data a/0.0,1.42/
      data b/1.56,0.72/
      FDIFFRACT =( a(ipart)+b(ipart)*log(sqs))/2.0
      RETURN
      END
      FUNCTION FERMK(A)
      DIMENSION AA(6), FK(6)
      DATA AA/4., 6., 12., 24., 40., 57./
      DATA FK/130.,169.,221.,235.,251.,260./
      DO I=2,4
      IF (A .LT. AA(I)) GO TO 25
      ENDDO
      I = 5
   25      F11 = AA(I-1)
      F12 = AA(I)
      F13 = AA(I+1)
      F21 = FK(I-1)
      F22 = FK(I)
      F23 = FK(I+1)
      FERMK = QUAD_INT(A,F11,F12,F13, F21,F22,F23)
      RETURN
      END

      SUBROUTINE FMINUS (S, FR, FI)
      COMMON /BLOCKC/ AA, BETA, S0, CC, AMU, DD, ALPHA, A0
      DATA PI /3.1415926/
      F1 = S**(ALPHA-1.)
      F2 = 0.5*PI*(1.-ALPHA)
      FR = -DD*F1*COS(F2)
      FI = -DD*F1*SIN(F2)
      RETURN
      END

      SUBROUTINE FPLUS (S, FR, FI)
      COMMON /BLOCKC/ AA, BETA, S0, CC, AMU, DD, ALPHA, A0
      COMPLEX Z1, Z2, Z3
      DATA PI /3.1415926/
      F1 = LOG(S/S0)
      Z1 = CMPLX(F1,-PI/2.)
      Z1 = Z1*Z1
      Z2 = 1. + A0*Z1
      Z3 = Z1/Z2
      F2 = CC*S**(AMU-1.)
      F3 = 0.5*PI*(1.-AMU)
      FI = AA + F2*COS(F3) + BETA*REAL(Z3)
      FR = -BETA*AIMAG(Z3)+F2*SIN(F3)
      RETURN
      END

      FUNCTION FPNI (E,L)
C...This function  returns the interaction length
C.  of an hadronic particle travelling in air
C.  INPUT:   E (TeV)   particle energy
C.           L         particle code
C.  OUTPUT:  FPNI      (g cm-2)
C...................................................
        COMMON /CSAIR/ NSQS, ASQSMIN, ASQSMAX, DASQS,
     +           SSIG0(41,2),SSIGA(41,2),ALINT(41,2)
      DIMENSION KK(7:14)
      DATA KK /6*2, 2*1/
      SQS = SQRT(2000.*E*0.937)                        ! GeV
      AL = LOG10 (SQS)
      T = (AL-ASQSMIN)/DASQS
      J = INT(T)
C D.H.
      J = MIN(J,39)
      J = MAX(J,0)

      T = T-FLOAT(J)
      FPNI = (1.-T)*ALINT(J+1,KK(L)) + T*ALINT(J+2,KK(L))      ! g cm-2
      RETURN
      END

      SUBROUTINE FRAGM (IAT,IAP, NW,B, NF, IAF)
C...Nuclear Fragmentation, Abrasion-ablation model,
C...Based on Jon Engel's routines ABRABL
C...This most recent version adds for all prefragment
C...masses > 10 the model calculation for the fragment
C...mass distribution and the energy carried by the fragment
C...of W. Friedmann
C...The average values are used to implement the model
C...in the montecarlo fashion / TSS, Dec '91
C...Needs INITFRAG to fill in the model data from INITFRAG.TAB
C.
C.  INPUT: IAP = mass of incident nucleus
C.         IAT = mass of target   nucleus
C.         NW = number of wounded nucleons in the beam nucleus
C.         B  = impact parameter in the interaction
C.
C.  OUTPUT : NF = number of fragments  of the spectator nucleus
C.           IAF(1:NF) = mass number of each fragment
C.           PF(3,60) in common block /FRAGMENTS/ contains
C.           the three momentum components (MeV/c) of each
C.           fragment in the projectile frame
C..............................................................
      COMMON /FRAGMENTS/ PPP(3,60)
      COMMON /FRAGMOD/A(10,10,20),AE(10,10,20),ERES(10,10),NFLAGG(10,10)
      DIMENSION IAF(60)
      DIMENSION AA(10), EAA(10)
      DATA AA/10.,15.,20.,25.,30.,35.,40.,45.,50.,56./
      DATA EAA/1.,2.,4.,6.,8.,10.,12.,16.,20.,30/
      AP=IAP
      AT=IAT
      NPF = IAP - NW
      IF (NPF .EQ. 0) THEN
         NF = 0
         RETURN
      ENDIF

      EB = ESTAR(AP,AT, B)
      EBP = ESTARP (NPF, NW)
C CONTRIBUTION TO E* FROM ENERGY DEPOSITED BY SECONDARIES
      EB = EB + EBP
C TOTAL E* IS THE SUM OF THE TWO COMPONENTS

C.....Prefragment transverse momentum (MeV/nucleon)...
            FK = FERMK(AP)
C FERMI MOMENTUM OF THE PROJECTILE NUCLEUS
            IF (NW .LT. IAP) THEN
            SIG = FK*SQRT(NW*NPF/(AP-1.))/3.162
C GAUSSIAN SIGMA IN ALL THREE DIRECTION
            ELSE
            SIG = FK/3.162
C THIS IS NOT CORRECT, TOO LARGE !!!!!!!!!!!!!!
            ENDIF
             PPFX = SIG*GASDEV(0)/NPF
             PPFY = SIG*GASDEV(0)/NPF
C THREE MOMENTUM COMPONENTS PER NUCLEON FOR THE PREFRAGMENT

C.............Crude model for small prefragment mass .......
            IF (NPF .LT. 10) THEN
                 CALL EVAP(NPF, EB, EPS, NNUC, NALP)
C                  EPS IS THE KINETIC ENERGY CARRIED BY THE EVAPORATED NUCLEONS
               ETOT = 938. + EPS
                 PP = SQRT((ETOT*ETOT - 8.79844E5)/3.)
C                  AVERAGE MOMENTUM OF EVAPORATED NUCLEONS IN EACH DIRECTION
                 NUC = NPF - NNUC - 4*NALP
                 NF = 0
                 IF (NUC .GT. 0) THEN
                    NF = NF + 1
                    IAF(NF) = NUC
                    PPP(1,NF) = NUC*PPFX
                    PPP(2,NF) = NUC*PPFY
                 ENDIF
                 IF (NALP .NE. 0) THEN
                 DO I=1,NALP
                   NF = NF + 1
                    IAF(NF) = 4
                   CALL SINCO(S1,C1)
                   CALL SINCO(S2,C2)
                   PXE = 4.*PP*S1*S2
                   PYE = 4.*PP*S1*C2
                   PPP(1,NF) = 4.*PPFX + PXE
                   PPP(2,NF) = 4.*PPFY + PYE
                   PPP(1,1) = PPP(1,1) - PXE
                   PPP(2,1) = PPP(2,1) - PYE
                 ENDDO
                 ENDIF
                 IF (NNUC .NE. 0) THEN
                 DO I=1,NNUC
                    NF = NF + 1
                    IAF(NF) = 1
                    CALL SINCO(S1,C1)
                    CALL SINCO(S2,C2)
                    PXE = PP*S1*S2
                    PYE = PP*S1*C2
                    PPP(1,NF) = 4.*PPFX + PXE
                    PPP(2,NF) = 4.*PPFY + PYE
                    PPP(1,1) = PPP(1,1) - PXE
                    PPP(2,1) = PPP(2,1) - PYE
                 ENDDO
                 ENDIF
                 RETURN
            ENDIF

C.........More refined model calculation .............
      JA = NPF/5 -1
      IF (JA .LT. 10) THEN
      IF ((NPF - AA(JA)) .GT. (AA(JA+1)-NPF)) JA = JA + 1
      ENDIF
      ARAT = FLOAT(NPF)/AA(JA)
      DO J=1,10
      IF (EB .LT. EAA(J)) GO TO 29
      ENDDO
      JE = 10
      GO TO 39
   29      JE = J
   39      IF (JE .GT. 1 .AND. JE .NE. 10) THEN
      IF ((EB - EAA(J-1)) .LT. (EAA(J)-EB)) JE = J - 1
      ENDIF
      ERAT = EB/EAA(JE)
        IF (EB .LT. 1.) THEN
        ERAT = EB
        ENDIF
C INTERPOLATE BETWEEN EB=0. (NOTHING HAPPENS) AND EB = 1. MeV

         IF (JA .EQ. 10 .AND. JE .GT. 6) THEN
         WRITE(*,*)' JA=',JA,',   JE=',JE
         ENDIF
   43      ESUM = 0.
      NSUM = 0
      JF = 0
      DO J=20,1,-1
      FR =  A(JA, JE, J)*ARAT*ERAT
      N1 = 1 + FR
      FR1 = FR/FLOAT(N1)
      DO K=1, N1
      IF (RAN(0) .LT. FR1) THEN
      JF = JF + 1
      IAF(JF) = J
      NSUM = NSUM + J
      EKIN = ERAT*AE(JA,JE, J)
         IF (EKIN .GT. 0.) THEN
         ESUM = ESUM + EKIN
         ETOT = 938.*IAF(JF) + EKIN
           PP = SQRT(2.*(ETOT*ETOT - IAF(JF)**2*8.79844E5)/3.)
         CALL SINCO(S1,C1)
         CALL SINCO(S2,C2)
         PPP(1,JF) = PP*S1*S2 + IAF(JF)*PPFX
         PPP(2,JF) = PP*S1*C2 + IAF(JF)*PPFY
         ENDIF
        IF (NSUM .GT. NPF) THEN
C        WRITE(*,*)' WARNING, NSUM=', NSUM,',  NPF=',NPF
C        WRITE(*,*)'  ARAT =', ARAT
        GO TO 43
        ELSE
        IF (NSUM .EQ. NPF) THEN
        GO TO 44
        ENDIF
        ENDIF
      ENDIF
      ENDDO
      ENDDO
      IF (NFLAGG(JA,JE) .EQ. 0) THEN
C 'THE RESIDUE' IS A NUCLEAR FRAGMENT
      JF = JF + 1
      IAF(JF) = NPF - NSUM
      F1 = NPF*EB - ESUM
      IF (F1 .LT. 0.) F1 = 0.
C GIVE THE REST OF EB TO THE FRAGMENT
      EKIN = F1
         IF (EKIN .GT. 0.) THEN
         ETOT = 938.*IAF(JF) + EKIN
           PP = SQRT(2.*(ETOT*ETOT - IAF(JF)**2*8.79844E5)/3.)
         CALL SINCO(S1,C1)
         CALL SINCO(S2,C2)
         PPP(1,JF) = PP*S1*S2 + IAF(JF)*PPFX
         PPP(2,JF) = PP*S1*C2 + IAF(JF)*PPFY
         ENDIF
      ELSE
C 'THE RESIDUE' CONSISTS OF SPECTATOR NUCLEONS
      N1 = NPF - NSUM
      DO K=1,N1
      JF = JF + 1
      IAF(JF) = 1
      EKIN = ERAT*ERES(JA,JE)
         IF (EKIN .GT. 0.) THEN
         ETOT = 938.*IAF(JF) + EKIN
           PP = SQRT(2.*(ETOT*ETOT - IAF(JF)**2*8.79844E5)/3.)
         CALL SINCO(S1,C1)
         CALL SINCO(S2,C2)
         PPP(1,JF) = PP*S1*S2 + PPFX
         PPP(2,JF) = PP*S1*C2 + PPFY
         ENDIF
      ENDDO
      ENDIF
   44      NF = JF
      RETURN
      END
      SUBROUTINE FRAGM1 (IA,NW, NF, IAF)
C...Nuclear Fragmentation
C.  total dissolution of nucleus
C..........................................
      DIMENSION IAF(60)
      NF = IA-NW
      DO J=1,NF
         IAF(J) = 1
      ENDDO
      RETURN
      END
      SUBROUTINE FRAGM2 (IA,NW, NF, IAF)
C...Nuclear Fragmentation
C.  Spectator in one single fragment
C..........................................
      DIMENSION IAF(60)
      IF (IA-NW .GT. 0)  THEN
         NF = 1
         IAF(1) = IA-NW
      ELSE
         NF = 0
      ENDIF
      RETURN
      END
      BLOCK DATA FRAG_DATA
C====================================================================
C...Code of fragmentation  of spectator nucleons
C.  based on Jon Engel  abrasion-ablation algorithms
C...Data for the fragmentation of  nucleus  projectiles
      COMMON /FRAGMOD/A(10,10,20),AE(10,10,20),ERES(10,10),NFLAGG(10,10)
      DATA (NFLAGG(I, 1),I=1,10)  /
     +    0,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA (NFLAGG(I, 2),I=1,10)  /
     +    0,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA (NFLAGG(I, 3),I=1,10)  /
     +    0,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA (NFLAGG(I, 4),I=1,10)  /
     +    0,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA (NFLAGG(I, 5),I=1,10)  /
     +    0,  0,  0,  0,  0,  0,  0,  0,  0,  0 /
      DATA (NFLAGG(I, 6),I=1,10)  /
     +    0,  0,  0,  0,  0,  0,  0,  1,  1,  1 /
      DATA (NFLAGG(I, 7),I=1,10)  /
     +    1,  1,  1,  1,  1,  1,  1,  1,  1,  1 /
      DATA (NFLAGG(I, 8),I=1,10)  /
     +    1,  1,  1,  1,  1,  1,  1,  1,  1,  1 /
      DATA (NFLAGG(I, 9),I=1,10)  /
     +    1,  1,  1,  1,  1,  1,  1,  1,  1,  1 /
      DATA (NFLAGG(I,10),I=1,10)  /
     +    1,  1,  1,  1,  1,  1,  1,  1,  1,  1 /
      DATA (A(I, 1, 1),I=1,10)  /
     +  .438E-01,.172    ,.283    ,.511    ,.715    ,.920    ,1.19    ,
     +  1.37    ,1.65    ,2.14     /
      DATA (A(I, 1, 2),I=1,10)  /
     +  .147E-01,.249E-01,.439E-01,.592E-01,.776E-01,.886E-01,.108    ,
     +  .117    ,.126    ,.128     /
      DATA (A(I, 1, 3),I=1,10)  /
     +  .216E-02,.627E-02,.834E-02,.108E-01,.144E-01,.152E-01,.196E-01,
     +  .200E-01,.210E-01,.224E-01 /
      DATA (A(I, 1, 4),I=1,10)  /
     +  .593E-01,.653E-01,.116    ,.145    ,.184    ,.204    ,.234    ,
     +  .257    ,.271    ,.248     /
      DATA (A(I, 1, 5),I=1,10)  /
     +  .000E+00,.918E-02,.362E-02,.805E-02,.436E-02,.728E-02,.466E-02,
     +  .707E-02,.932E-02,.130E-01 /
      DATA (A(I, 1, 6),I=1,10)  /
     +  .000E+00,.180E-02,.247E-02,.208E-02,.224E-02,.214E-02,.226E-02,
     +  .233E-02,.230E-02,.194E-02 /
      DATA (A(I, 1, 7),I=1,10)  /
     +  .000E+00,.106E-02,.703E-03,.687E-03,.739E-03,.674E-03,.819E-03,
     +  .768E-03,.756E-03,.720E-03 /
      DATA (A(I, 1, 8),I=1,10)  /
     +  .000E+00,.000E+00,.188E-02,.130E-02,.138E-02,.117E-02,.124E-02,
     +  .119E-02,.111E-02,.829E-03 /
      DATA (A(I, 1, 9),I=1,10)  /
     +  .000E+00,.000E+00,.302E-03,.258E-03,.249E-03,.208E-03,.248E-03,
     +  .222E-03,.210E-03,.187E-03 /
      DATA (A(I, 1,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.235E-03,.222E-03,.172E-03,.181E-03,
     +  .166E-03,.152E-03,.124E-03 /
      DATA (A(I, 1,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.238E-03,.179E-03,.145E-03,.156E-03,
     +  .138E-03,.129E-03,.111E-03 /
      DATA (A(I, 1,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.368E-03,.400E-03,.255E-03,.262E-03,
     +  .221E-03,.182E-03,.112E-03 /
      DATA (A(I, 1,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.753E-04,.712E-04,.527E-04,
     +  .537E-04,.538E-04,.487E-04 /
      DATA (A(I, 1,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.103E-03,.589E-04,.578E-04,
     +  .468E-04,.385E-04,.269E-04 /
      DATA (A(I, 1,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.444E-04,.372E-04,
     +  .318E-04,.284E-04,.218E-04 /
      DATA (A(I, 1,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.487E-04,.473E-04,
     +  .338E-04,.243E-04,.122E-04 /
      DATA (A(I, 1,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.121E-04,.117E-04,
     +  .932E-05,.792E-05,.583E-05 /
      DATA (A(I, 1,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.147E-04,
     +  .101E-04,.756E-05,.496E-05 /
      DATA (A(I, 1,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.755E-05,
     +  .612E-05,.505E-05,.341E-05 /
      DATA (A(I, 1,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  .630E-05,.444E-05,.282E-05 /
      DATA (A(I, 2, 1),I=1,10)  /
     +  .269    ,.510    ,.738    ,1.12    ,1.46    ,1.83    ,2.22    ,
     +  2.57    ,3.00    ,3.67     /
      DATA (A(I, 2, 2),I=1,10)  /
     +  .121    ,.133    ,.190    ,.234    ,.293    ,.332    ,.395    ,
     +  .431    ,.468    ,.502     /
      DATA (A(I, 2, 3),I=1,10)  /
     +  .227E-01,.374E-01,.474E-01,.578E-01,.722E-01,.794E-01,.960E-01,
     +  .102    ,.110    ,.120     /
      DATA (A(I, 2, 4),I=1,10)  /
     +  .287    ,.196    ,.270    ,.314    ,.373    ,.408    ,.462    ,
     +  .498    ,.529    ,.523     /
      DATA (A(I, 2, 5),I=1,10)  /
     +  .000E+00,.433E-01,.218E-01,.384E-01,.263E-01,.385E-01,.298E-01,
     +  .405E-01,.504E-01,.671E-01 /
      DATA (A(I, 2, 6),I=1,10)  /
     +  .000E+00,.151E-01,.177E-01,.159E-01,.173E-01,.173E-01,.187E-01,
     +  .196E-01,.201E-01,.191E-01 /
      DATA (A(I, 2, 7),I=1,10)  /
     +  .000E+00,.457E-02,.607E-02,.610E-02,.677E-02,.670E-02,.784E-02,
     +  .787E-02,.806E-02,.803E-02 /
      DATA (A(I, 2, 8),I=1,10)  /
     +  .000E+00,.000E+00,.702E-02,.536E-02,.558E-02,.510E-02,.554E-02,
     +  .546E-02,.538E-02,.489E-02 /
      DATA (A(I, 2, 9),I=1,10)  /
     +  .000E+00,.000E+00,.190E-02,.199E-02,.205E-02,.191E-02,.221E-02,
     +  .214E-02,.213E-02,.204E-02 /
      DATA (A(I, 2,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.226E-02,.219E-02,.195E-02,.208E-02,
     +  .204E-02,.203E-02,.194E-02 /
      DATA (A(I, 2,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.213E-02,.195E-02,.175E-02,.191E-02,
     +  .183E-02,.179E-02,.166E-02 /
      DATA (A(I, 2,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.588E-03,.186E-02,.137E-02,.141E-02,
     +  .128E-02,.117E-02,.947E-03 /
      DATA (A(I, 2,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.554E-03,.562E-03,.454E-03,
     +  .485E-03,.505E-03,.509E-03 /
      DATA (A(I, 2,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.490E-03,.533E-03,.531E-03,
     +  .476E-03,.437E-03,.369E-03 /
      DATA (A(I, 2,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.427E-03,.382E-03,
     +  .358E-03,.340E-03,.294E-03 /
      DATA (A(I, 2,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.239E-03,.298E-03,
     +  .238E-03,.196E-03,.134E-03 /
      DATA (A(I, 2,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.299E-04,.893E-04,
     +  .796E-04,.744E-04,.683E-04 /
      DATA (A(I, 2,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.127E-03,
     +  .107E-03,.916E-04,.720E-04 /
      DATA (A(I, 2,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.397E-04,
     +  .630E-04,.565E-04,.461E-04 /
      DATA (A(I, 2,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  .511E-04,.459E-04,.402E-04 /
      DATA (A(I, 3, 1),I=1,10)  /
     +  .708    ,1.02    ,1.41    ,1.91    ,2.42    ,3.00    ,3.53    ,
     +  4.09    ,4.71    ,5.57     /
      DATA (A(I, 3, 2),I=1,10)  /
     +  .397    ,.410    ,.539    ,.648    ,.795    ,.910    ,1.06    ,
     +  1.17    ,1.29    ,1.42     /
      DATA (A(I, 3, 3),I=1,10)  /
     +  .845E-01,.122    ,.157    ,.190    ,.232    ,.262    ,.307    ,
     +  .335    ,.366    ,.402     /
      DATA (A(I, 3, 4),I=1,10)  /
     +  .210    ,.379    ,.450    ,.490    ,.574    ,.636    ,.709    ,
     +  .769    ,.820    ,.849     /
      DATA (A(I, 3, 5),I=1,10)  /
     +  .000E+00,.102    ,.675E-01,.104    ,.858E-01,.115    ,.102    ,
     +  .129    ,.154    ,.194     /
      DATA (A(I, 3, 6),I=1,10)  /
     +  .000E+00,.392E-01,.615E-01,.593E-01,.649E-01,.674E-01,.735E-01,
     +  .779E-01,.817E-01,.828E-01 /
      DATA (A(I, 3, 7),I=1,10)  /
     +  .000E+00,.539E-02,.222E-01,.238E-01,.269E-01,.280E-01,.320E-01,
     +  .334E-01,.350E-01,.361E-01 /
      DATA (A(I, 3, 8),I=1,10)  /
     +  .000E+00,.000E+00,.838E-02,.130E-01,.133E-01,.131E-01,.141E-01,
     +  .144E-01,.149E-01,.152E-01 /
      DATA (A(I, 3, 9),I=1,10)  /
     +  .000E+00,.000E+00,.228E-02,.647E-02,.688E-02,.687E-02,.772E-02,
     +  .786E-02,.811E-02,.824E-02 /
      DATA (A(I, 3,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.664E-02,.828E-02,.802E-02,.845E-02,
     +  .869E-02,.902E-02,.930E-02 /
      DATA (A(I, 3,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.338E-02,.735E-02,.710E-02,.767E-02,
     +  .767E-02,.776E-02,.756E-02 /
      DATA (A(I, 3,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.280E-03,.262E-02,.349E-02,.342E-02,
     +  .322E-02,.312E-02,.291E-02 /
      DATA (A(I, 3,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.618E-03,.161E-02,.138E-02,
     +  .148E-02,.155E-02,.166E-02 /
      DATA (A(I, 3,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.313E-03,.128E-02,.161E-02,
     +  .150E-02,.144E-02,.134E-02 /
      DATA (A(I, 3,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.645E-03,.118E-02,
     +  .115E-02,.111E-02,.103E-02 /
      DATA (A(I, 3,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.117E-03,.497E-03,
     +  .581E-03,.501E-03,.401E-03 /
      DATA (A(I, 3,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.115E-04,.997E-04,
     +  .202E-03,.203E-03,.206E-03 /
      DATA (A(I, 3,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.877E-04,
     +  .242E-03,.263E-03,.226E-03 /
      DATA (A(I, 3,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.158E-04,
     +  .881E-04,.152E-03,.136E-03 /
      DATA (A(I, 3,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  .358E-04,.997E-04,.117E-03 /
      DATA (A(I, 4, 1),I=1,10)  /
     +  .945    ,1.29    ,1.40    ,1.98    ,2.73    ,3.17    ,3.77    ,
     +  4.29    ,4.78    ,5.54     /
      DATA (A(I, 4, 2),I=1,10)  /
     +  .581    ,.599    ,.645    ,.839    ,1.10    ,1.25    ,1.47    ,
     +  1.64    ,1.78    ,1.99     /
      DATA (A(I, 4, 3),I=1,10)  /
     +  .127    ,.182    ,.202    ,.264    ,.344    ,.387    ,.455    ,
     +  .504    ,.549    ,.611     /
      DATA (A(I, 4, 4),I=1,10)  /
     +  .183    ,.464    ,.351    ,.444    ,.642    ,.659    ,.772    ,
     +  .830    ,.882    ,.930     /
      DATA (A(I, 4, 5),I=1,10)  /
     +  .000E+00,.122    ,.803E-01,.136    ,.134    ,.173    ,.164    ,
     +  .203    ,.239    ,.300     /
      DATA (A(I, 4, 6),I=1,10)  /
     +  .000E+00,.393E-01,.766E-01,.872E-01,.108    ,.111    ,.123    ,
     +  .132    ,.139    ,.145     /
      DATA (A(I, 4, 7),I=1,10)  /
     +  .000E+00,.416E-02,.289E-01,.360E-01,.454E-01,.477E-01,.549E-01,
     +  .583E-01,.618E-01,.654E-01 /
      DATA (A(I, 4, 8),I=1,10)  /
     +  .000E+00,.000E+00,.761E-02,.157E-01,.214E-01,.205E-01,.233E-01,
     +  .241E-01,.255E-01,.271E-01 /
      DATA (A(I, 4, 9),I=1,10)  /
     +  .000E+00,.000E+00,.238E-02,.803E-02,.123E-01,.123E-01,.140E-01,
     +  .145E-01,.153E-01,.160E-01 /
      DATA (A(I, 4,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.695E-02,.150E-01,.154E-01,.166E-01,
     +  .172E-01,.181E-01,.192E-01 /
      DATA (A(I, 4,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.355E-02,.104E-01,.143E-01,.156E-01,
     +  .158E-01,.164E-01,.165E-01 /
      DATA (A(I, 4,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.112E-03,.276E-02,.568E-02,.736E-02,
     +  .684E-02,.691E-02,.661E-02 /
      DATA (A(I, 4,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.740E-03,.222E-02,.339E-02,
     +  .352E-02,.382E-02,.409E-02 /
      DATA (A(I, 4,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.369E-03,.160E-02,.322E-02,
     +  .375E-02,.375E-02,.355E-02 /
      DATA (A(I, 4,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.750E-03,.190E-02,
     +  .298E-02,.319E-02,.299E-02 /
      DATA (A(I, 4,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.260E-03,.673E-03,
     +  .117E-02,.156E-02,.126E-02 /
      DATA (A(I, 4,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.283E-05,.131E-03,
     +  .363E-03,.618E-03,.690E-03 /
      DATA (A(I, 4,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.205E-03,
     +  .378E-03,.709E-03,.844E-03 /
      DATA (A(I, 4,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.654E-05,
     +  .150E-03,.341E-03,.527E-03 /
      DATA (A(I, 4,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  .957E-04,.197E-03,.406E-03 /
      DATA (A(I, 5, 1),I=1,10)  /
     +  1.16    ,1.70    ,2.19    ,2.79    ,3.33    ,3.90    ,4.49    ,
     +  5.07    ,5.66    ,6.38     /
      DATA (A(I, 5, 2),I=1,10)  /
     +  .779    ,.899    ,1.09    ,1.28    ,1.51    ,1.71    ,1.96    ,
     +  2.18    ,2.39    ,2.62     /
      DATA (A(I, 5, 3),I=1,10)  /
     +  .167    ,.263    ,.334    ,.408    ,.482    ,.548    ,.632    ,
     +  .700    ,.767    ,.840     /
      DATA (A(I, 5, 4),I=1,10)  /
     +  .203    ,.565    ,.845    ,.867    ,.906    ,.961    ,1.08    ,
     +  1.13    ,1.21    ,1.25     /
      DATA (A(I, 5, 5),I=1,10)  /
     +  .000E+00,.129    ,.152    ,.237    ,.208    ,.268    ,.258    ,
     +  .312    ,.368    ,.450     /
      DATA (A(I, 5, 6),I=1,10)  /
     +  .000E+00,.460E-01,.126    ,.174    ,.182    ,.188    ,.208    ,
     +  .219    ,.233    ,.239     /
      DATA (A(I, 5, 7),I=1,10)  /
     +  .000E+00,.289E-02,.380E-01,.611E-01,.788E-01,.845E-01,.974E-01,
     +  .103    ,.111    ,.117     /
      DATA (A(I, 5, 8),I=1,10)  /
     +  .000E+00,.000E+00,.137E-01,.223E-01,.374E-01,.436E-01,.488E-01,
     +  .488E-01,.524E-01,.547E-01 /
      DATA (A(I, 5, 9),I=1,10)  /
     +  .000E+00,.000E+00,.162E-02,.114E-01,.198E-01,.263E-01,.315E-01,
     +  .323E-01,.348E-01,.364E-01 /
      DATA (A(I, 5,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.149E-01,.240E-01,.320E-01,.428E-01,
     +  .436E-01,.469E-01,.493E-01 /
      DATA (A(I, 5,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.562E-02,.194E-01,.290E-01,.408E-01,
     +  .460E-01,.492E-01,.500E-01 /
      DATA (A(I, 5,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.476E-04,.106E-01,.134E-01,.191E-01,
     +  .227E-01,.264E-01,.253E-01 /
      DATA (A(I, 5,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.281E-02,.679E-02,.879E-02,
     +  .123E-01,.165E-01,.190E-01 /
      DATA (A(I, 5,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.542E-04,.847E-02,.125E-01,
     +  .144E-01,.173E-01,.192E-01 /
      DATA (A(I, 5,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.652E-02,.982E-02,
     +  .129E-01,.159E-01,.192E-01 /
      DATA (A(I, 5,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.109E-03,.688E-02,
     +  .751E-02,.845E-02,.905E-02 /
      DATA (A(I, 5,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.823E-06,.237E-02,
     +  .318E-02,.446E-02,.569E-02 /
      DATA (A(I, 5,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.604E-03,
     +  .610E-02,.673E-02,.827E-02 /
      DATA (A(I, 5,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.716E-06,
     +  .412E-02,.519E-02,.617E-02 /
      DATA (A(I, 5,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  .710E-03,.543E-02,.674E-02 /
      DATA (A(I, 6, 1),I=1,10)  /
     +  1.36    ,2.08    ,2.67    ,3.30    ,3.94    ,4.62    ,5.18    ,
     +  3.60    ,3.64    ,3.95     /
      DATA (A(I, 6, 2),I=1,10)  /
     +  1.07    ,1.33    ,1.58    ,1.82    ,2.10    ,2.44    ,2.74    ,
     +  1.78    ,1.73    ,1.80     /
      DATA (A(I, 6, 3),I=1,10)  /
     +  .158    ,.276    ,.402    ,.506    ,.609    ,.700    ,.802    ,
     +  .638    ,.629    ,.658     /
      DATA (A(I, 6, 4),I=1,10)  /
     +  .308    ,.739    ,1.02    ,1.12    ,1.26    ,1.35    ,1.57    ,
     +  1.94    ,1.71    ,1.55     /
      DATA (A(I, 6, 5),I=1,10)  /
     +  .000E+00,.217    ,.183    ,.324    ,.276    ,.395    ,.393    ,
     +  .558    ,.602    ,.681     /
      DATA (A(I, 6, 6),I=1,10)  /
     +  .000E+00,.658E-01,.251    ,.267    ,.299    ,.326    ,.386    ,
     +  .452    ,.475    ,.409     /
      DATA (A(I, 6, 7),I=1,10)  /
     +  .000E+00,.198E-02,.774E-01,.136    ,.149    ,.164    ,.187    ,
     +  .210    ,.238    ,.256     /
      DATA (A(I, 6, 8),I=1,10)  /
     +  .000E+00,.000E+00,.290E-01,.122    ,.139    ,.128    ,.129    ,
     +  .137    ,.147    ,.167     /
      DATA (A(I, 6, 9),I=1,10)  /
     +  .000E+00,.000E+00,.699E-03,.617E-01,.750E-01,.801E-01,.905E-01,
     +  .974E-01,.105    ,.122     /
      DATA (A(I, 6,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.310E-01,.112    ,.127    ,.140    ,
     +  .143    ,.155    ,.176     /
      DATA (A(I, 6,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.277E-02,.889E-01,.143    ,.150    ,
     +  .175    ,.184    ,.208     /
      DATA (A(I, 6,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.202E-04,.343E-01,.959E-01,.109    ,
     +  .115    ,.112    ,.116     /
      DATA (A(I, 6,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.186E-02,.435E-01,.512E-01,
     +  .744E-01,.856E-01,.103     /
      DATA (A(I, 6,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.144E-04,.427E-01,.786E-01,
     +  .911E-01,.993E-01,.108     /
      DATA (A(I, 6,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.466E-02,.518E-01,
     +  .848E-01,.109    ,.119     /
      DATA (A(I, 6,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.655E-05,.330E-01,
     +  .586E-01,.617E-01,.594E-01 /
      DATA (A(I, 6,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.228E-06,.328E-02,
     +  .190E-01,.301E-01,.454E-01 /
      DATA (A(I, 6,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.218E-04,
     +  .272E-01,.501E-01,.707E-01 /
      DATA (A(I, 6,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.146E-06,
     +  .441E-02,.378E-01,.556E-01 /
      DATA (A(I, 6,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  .160E-03,.204E-01,.679E-01 /
      DATA (A(I, 7, 1),I=1,10)  /
     +  .522    ,.862    ,1.14    ,1.40    ,1.70    ,1.94    ,2.26    ,
     +  2.48    ,2.72    ,3.95     /
      DATA (A(I, 7, 2),I=1,10)  /
     +  .314    ,.450    ,.588    ,.692    ,.834    ,.936    ,1.09    ,
     +  1.18    ,1.28    ,1.80     /
      DATA (A(I, 7, 3),I=1,10)  /
     +  .814E-01,.147    ,.189    ,.226    ,.272    ,.302    ,.351    ,
     +  .378    ,.406    ,.658     /
      DATA (A(I, 7, 4),I=1,10)  /
     +  .252    ,.864    ,1.01    ,.851    ,.837    ,.774    ,.763    ,
     +  .757    ,.748    ,1.55     /
      DATA (A(I, 7, 5),I=1,10)  /
     +  .000E+00,.225    ,.180    ,.276    ,.193    ,.240    ,.190    ,
     +  .228    ,.259    ,.681     /
      DATA (A(I, 7, 6),I=1,10)  /
     +  .000E+00,.485E-01,.272    ,.273    ,.253    ,.216    ,.206    ,
     +  .197    ,.191    ,.409     /
      DATA (A(I, 7, 7),I=1,10)  /
     +  .000E+00,.137E-02,.752E-01,.137    ,.152    ,.134    ,.125    ,
     +  .119    ,.116    ,.256     /
      DATA (A(I, 7, 8),I=1,10)  /
     +  .000E+00,.000E+00,.220E-01,.155    ,.175    ,.155    ,.116    ,
     +  .977E-01,.858E-01,.167     /
      DATA (A(I, 7, 9),I=1,10)  /
     +  .000E+00,.000E+00,.326E-03,.695E-01,.881E-01,.106    ,.897E-01,
     +  .782E-01,.706E-01,.122     /
      DATA (A(I, 7,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.261E-01,.124    ,.131    ,.156    ,
     +  .141    ,.121    ,.176     /
      DATA (A(I, 7,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.785E-03,.864E-01,.130    ,.170    ,
     +  .182    ,.172    ,.208     /
      DATA (A(I, 7,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.896E-05,.225E-01,.105    ,.126    ,
     +  .126    ,.135    ,.116     /
      DATA (A(I, 7,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.542E-03,.427E-01,.553E-01,
     +  .744E-01,.980E-01,.103     /
      DATA (A(I, 7,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.515E-05,.377E-01,.831E-01,
     +  .985E-01,.104    ,.108     /
      DATA (A(I, 7,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.285E-02,.495E-01,
     +  .871E-01,.106    ,.119     /
      DATA (A(I, 7,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.110E-05,.284E-01,
     +  .588E-01,.657E-01,.594E-01 /
      DATA (A(I, 7,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.722E-07,.176E-02,
     +  .170E-01,.305E-01,.454E-01 /
      DATA (A(I, 7,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.148E-05,
     +  .213E-01,.492E-01,.707E-01 /
      DATA (A(I, 7,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.323E-07,
     +  .722E-02,.359E-01,.556E-01 /
      DATA (A(I, 7,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  .461E-05,.155E-01,.679E-01 /
      DATA (A(I, 8, 1),I=1,10)  /
     +  .630    ,.974    ,1.29    ,1.58    ,1.89    ,2.16    ,2.49    ,
     +  2.75    ,3.02    ,3.95     /
      DATA (A(I, 8, 2),I=1,10)  /
     +  .328    ,.459    ,.613    ,.735    ,.879    ,.994    ,1.15    ,
     +  1.27    ,1.38    ,1.80     /
      DATA (A(I, 8, 3),I=1,10)  /
     +  .748E-01,.121    ,.164    ,.197    ,.235    ,.265    ,.310    ,
     +  .339    ,.370    ,.658     /
      DATA (A(I, 8, 4),I=1,10)  /
     +  .194    ,.211    ,.337    ,.344    ,.339    ,.351    ,.390    ,
     +  .419    ,.442    ,1.55     /
      DATA (A(I, 8, 5),I=1,10)  /
     +  .000E+00,.869E-01,.725E-01,.113    ,.810E-01,.106    ,.951E-01,
     +  .120    ,.143    ,.681     /
      DATA (A(I, 8, 6),I=1,10)  /
     +  .000E+00,.288E-01,.102    ,.922E-01,.857E-01,.845E-01,.932E-01,
     +  .983E-01,.102    ,.409     /
      DATA (A(I, 8, 7),I=1,10)  /
     +  .000E+00,.668E-03,.533E-01,.575E-01,.493E-01,.482E-01,.539E-01,
     +  .558E-01,.582E-01,.256     /
      DATA (A(I, 8, 8),I=1,10)  /
     +  .000E+00,.000E+00,.205E-01,.808E-01,.510E-01,.409E-01,.406E-01,
     +  .394E-01,.389E-01,.167     /
      DATA (A(I, 8, 9),I=1,10)  /
     +  .000E+00,.000E+00,.999E-04,.647E-01,.385E-01,.325E-01,.325E-01,
     +  .316E-01,.314E-01,.122     /
      DATA (A(I, 8,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.169E-01,.834E-01,.611E-01,.565E-01,
     +  .533E-01,.519E-01,.176     /
      DATA (A(I, 8,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.107E-03,.769E-01,.922E-01,.805E-01,
     +  .745E-01,.711E-01,.208     /
      DATA (A(I, 8,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.180E-05,.143E-01,.983E-01,.775E-01,
     +  .627E-01,.541E-01,.116     /
      DATA (A(I, 8,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.157E-04,.346E-01,.507E-01,
     +  .479E-01,.455E-01,.103     /
      DATA (A(I, 8,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.752E-06,.248E-01,.721E-01,
     +  .728E-01,.611E-01,.108     /
      DATA (A(I, 8,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.686E-04,.356E-01,
     +  .731E-01,.791E-01,.119     /
      DATA (A(I, 8,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.838E-07,.151E-01,
     +  .470E-01,.567E-01,.594E-01 /
      DATA (A(I, 8,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.759E-08,.400E-04,
     +  .193E-01,.313E-01,.454E-01 /
      DATA (A(I, 8,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.385E-07,
     +  .921E-02,.353E-01,.707E-01 /
      DATA (A(I, 8,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.219E-08,
     +  .348E-03,.226E-01,.556E-01 /
      DATA (A(I, 8,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  .212E-07,.149E-01,.679E-01 /
      DATA (A(I, 9, 1),I=1,10)  /
     +  .736    ,1.13    ,1.49    ,1.82    ,2.20    ,2.49    ,2.86    ,
     +  3.17    ,3.49    ,3.95     /
      DATA (A(I, 9, 2),I=1,10)  /
     +  .339    ,.492    ,.658    ,.789    ,.958    ,1.08    ,1.25    ,
     +  1.37    ,1.50    ,1.80     /
      DATA (A(I, 9, 3),I=1,10)  /
     +  .680E-01,.110    ,.150    ,.180    ,.222    ,.247    ,.289    ,
     +  .318    ,.349    ,.658     /
      DATA (A(I, 9, 4),I=1,10)  /
     +  .110    ,.104    ,.157    ,.156    ,.210    ,.205    ,.246    ,
     +  .274    ,.300    ,1.55     /
      DATA (A(I, 9, 5),I=1,10)  /
     +  .000E+00,.379E-01,.347E-01,.477E-01,.486E-01,.576E-01,.569E-01,
     +  .732E-01,.893E-01,.681     /
      DATA (A(I, 9, 6),I=1,10)  /
     +  .000E+00,.223E-01,.354E-01,.312E-01,.436E-01,.400E-01,.489E-01,
     +  .548E-01,.600E-01,.409     /
      DATA (A(I, 9, 7),I=1,10)  /
     +  .000E+00,.338E-03,.149E-01,.142E-01,.215E-01,.188E-01,.248E-01,
     +  .278E-01,.307E-01,.256     /
      DATA (A(I, 9, 8),I=1,10)  /
     +  .000E+00,.000E+00,.553E-02,.862E-02,.150E-01,.106E-01,.145E-01,
     +  .165E-01,.181E-01,.167     /
      DATA (A(I, 9, 9),I=1,10)  /
     +  .000E+00,.000E+00,.375E-04,.641E-02,.111E-01,.792E-02,.112E-01,
     +  .127E-01,.140E-01,.122     /
      DATA (A(I, 9,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.112E-01,.200E-01,.127E-01,.176E-01,
     +  .200E-01,.220E-01,.176     /
      DATA (A(I, 9,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.244E-04,.261E-01,.162E-01,.232E-01,
     +  .263E-01,.287E-01,.208     /
      DATA (A(I, 9,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.455E-06,.635E-02,.121E-01,.186E-01,
     +  .201E-01,.207E-01,.116     /
      DATA (A(I, 9,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.146E-05,.922E-02,.116E-01,
     +  .145E-01,.165E-01,.103     /
      DATA (A(I, 9,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.135E-06,.128E-01,.202E-01,
     +  .215E-01,.220E-01,.108     /
      DATA (A(I, 9,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.237E-05,.229E-01,
     +  .259E-01,.271E-01,.119     /
      DATA (A(I, 9,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.100E-07,.534E-02,
     +  .210E-01,.193E-01,.594E-01 /
      DATA (A(I, 9,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.915E-09,.847E-06,
     +  .119E-01,.125E-01,.454E-01 /
      DATA (A(I, 9,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.298E-08,
     +  .101E-01,.242E-01,.707E-01 /
      DATA (A(I, 9,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.196E-09,
     +  .243E-05,.234E-01,.556E-01 /
      DATA (A(I, 9,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  .575E-09,.364E-02,.679E-01 /
      DATA (A(I,10, 1),I=1,10)  /
     +  .959    ,1.46    ,1.92    ,2.34    ,2.80    ,3.24    ,3.64    ,
     +  4.05    ,4.48    ,3.95     /
      DATA (A(I,10, 2),I=1,10)  /
     +  .343    ,.516    ,.692    ,.836    ,1.01    ,1.16    ,1.31    ,
     +  1.46    ,1.61    ,1.80     /
      DATA (A(I,10, 3),I=1,10)  /
     +  .512E-01,.837E-01,.115    ,.138    ,.169    ,.195    ,.220    ,
     +  .245    ,.270    ,.658     /
      DATA (A(I,10, 4),I=1,10)  /
     +  .274E-01,.361E-01,.510E-01,.562E-01,.703E-01,.828E-01,.877E-01,
     +  .996E-01,.111    ,1.55     /
      DATA (A(I,10, 5),I=1,10)  /
     +  .000E+00,.850E-02,.875E-02,.118E-01,.124E-01,.170E-01,.154E-01,
     +  .194E-01,.237E-01,.681     /
      DATA (A(I,10, 6),I=1,10)  /
     +  .000E+00,.345E-02,.519E-02,.533E-02,.691E-02,.842E-02,.844E-02,
     +  .987E-02,.113E-01,.409     /
      DATA (A(I,10, 7),I=1,10)  /
     +  .000E+00,.722E-04,.130E-02,.135E-02,.189E-02,.240E-02,.235E-02,
     +  .281E-02,.331E-02,.256     /
      DATA (A(I,10, 8),I=1,10)  /
     +  .000E+00,.000E+00,.283E-03,.272E-03,.394E-03,.557E-03,.480E-03,
     +  .616E-03,.775E-03,.167     /
      DATA (A(I,10, 9),I=1,10)  /
     +  .000E+00,.000E+00,.457E-05,.122E-03,.192E-03,.275E-03,.225E-03,
     +  .292E-03,.373E-03,.122     /
      DATA (A(I,10,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.119E-03,.185E-03,.278E-03,.201E-03,
     +  .274E-03,.364E-03,.176     /
      DATA (A(I,10,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.140E-05,.129E-03,.200E-03,.137E-03,
     +  .188E-03,.252E-03,.208     /
      DATA (A(I,10,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.207E-07,.307E-04,.518E-04,.278E-04,
     +  .421E-04,.608E-04,.116     /
      DATA (A(I,10,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.306E-07,.252E-04,.111E-04,
     +  .188E-04,.295E-04,.103     /
      DATA (A(I,10,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.321E-08,.220E-04,.104E-04,
     +  .162E-04,.243E-04,.108     /
      DATA (A(I,10,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.770E-08,.632E-05,
     +  .105E-04,.162E-04,.119     /
      DATA (A(I,10,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.117E-09,.199E-05,
     +  .321E-05,.492E-05,.594E-01 /
      DATA (A(I,10,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.888E-11,.323E-09,
     +  .106E-05,.192E-05,.454E-01 /
      DATA (A(I,10,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.174E-10,
     +  .131E-05,.218E-05,.707E-01 /
      DATA (A(I,10,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.994E-12,
     +  .233E-09,.104E-05,.556E-01 /
      DATA (A(I,10,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  .144E-11,.724E-06,.679E-01 /
      DATA (AE(I, 1, 1),I=1,10)  /
     +  7.27    ,6.29    ,7.76    ,6.70    ,8.17    ,7.34    ,8.70    ,
     +  8.02    ,7.37    ,6.18     /
      DATA (AE(I, 1, 2),I=1,10)  /
     +  7.41    ,7.52    ,8.14    ,8.20    ,8.96    ,9.05    ,9.96    ,
     +  10.0    ,10.1    ,9.86     /
      DATA (AE(I, 1, 3),I=1,10)  /
     +  7.72    ,7.69    ,9.17    ,8.99    ,10.6    ,10.5    ,12.1    ,
     +  12.1    ,12.0    ,11.5     /
      DATA (AE(I, 1, 4),I=1,10)  /
     +  7.90    ,8.48    ,9.50    ,9.94    ,10.8    ,11.4    ,12.2    ,
     +  12.8    ,13.3    ,13.8     /
      DATA (AE(I, 1, 5),I=1,10)  /
     +  .000E+00,8.52    ,9.59    ,10.1    ,11.1    ,11.8    ,12.7    ,
     +  13.3    ,13.8    ,14.4     /
      DATA (AE(I, 1, 6),I=1,10)  /
     +  .000E+00,9.00    ,10.7    ,11.7    ,13.2    ,14.2    ,15.6    ,
     +  16.5    ,17.3    ,18.0     /
      DATA (AE(I, 1, 7),I=1,10)  /
     +  .000E+00,9.01    ,11.1    ,11.9    ,14.3    ,15.0    ,17.4    ,
     +  18.0    ,18.6    ,18.8     /
      DATA (AE(I, 1, 8),I=1,10)  /
     +  .000E+00,.000E+00,11.2    ,12.4    ,14.5    ,15.7    ,17.6    ,
     +  18.8    ,19.9    ,20.9     /
      DATA (AE(I, 1, 9),I=1,10)  /
     +  .000E+00,.000E+00,11.4    ,12.7    ,15.5    ,16.6    ,19.3    ,
     +  20.2    ,21.1    ,21.7     /
      DATA (AE(I, 1,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,13.2    ,15.8    ,17.3    ,19.9    ,
     +  21.2    ,22.4    ,23.2     /
      DATA (AE(I, 1,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,13.2    ,16.3    ,17.8    ,20.8    ,
     +  22.1    ,23.3    ,24.2     /
      DATA (AE(I, 1,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,13.4    ,16.2    ,18.2    ,21.0    ,
     +  22.8    ,24.4    ,25.9     /
      DATA (AE(I, 1,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,16.5    ,18.4    ,21.6    ,
     +  23.2    ,24.8    ,26.2     /
      DATA (AE(I, 1,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,16.7    ,19.0    ,22.3    ,
     +  24.3    ,26.1    ,27.4     /
      DATA (AE(I, 1,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,19.1    ,22.8    ,
     +  24.7    ,26.6    ,28.2     /
      DATA (AE(I, 1,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,19.2    ,23.0    ,
     +  25.3    ,27.5    ,29.5     /
      DATA (AE(I, 1,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,19.6    ,23.3    ,
     +  25.6    ,27.8    ,29.6     /
      DATA (AE(I, 1,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,23.6    ,
     +  26.2    ,28.5    ,30.4     /
      DATA (AE(I, 1,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,23.7    ,
     +  26.3    ,28.8    ,31.0     /
      DATA (AE(I, 1,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  26.5    ,29.2    ,31.5     /
      DATA (AE(I, 2, 1),I=1,10)  /
     +  8.74    ,8.16    ,9.25    ,8.45    ,9.46    ,8.90    ,9.83    ,
     +  9.38    ,8.96    ,8.15     /
      DATA (AE(I, 2, 2),I=1,10)  /
     +  8.96    ,9.30    ,9.95    ,10.0    ,10.8    ,10.9    ,11.7    ,
     +  11.8    ,11.9    ,11.8     /
      DATA (AE(I, 2, 3),I=1,10)  /
     +  9.44    ,9.66    ,11.0    ,11.0    ,12.3    ,12.5    ,13.7    ,
     +  13.9    ,14.0    ,13.8     /
      DATA (AE(I, 2, 4),I=1,10)  /
     +  8.86    ,9.81    ,10.8    ,11.2    ,12.0    ,12.6    ,13.4    ,
     +  14.0    ,14.5    ,15.1     /
      DATA (AE(I, 2, 5),I=1,10)  /
     +  .000E+00,10.2    ,11.4    ,12.0    ,12.9    ,13.6    ,14.5    ,
     +  15.1    ,15.7    ,16.3     /
      DATA (AE(I, 2, 6),I=1,10)  /
     +  .000E+00,10.7    ,12.5    ,13.5    ,15.1    ,16.0    ,17.5    ,
     +  18.3    ,19.2    ,19.9     /
      DATA (AE(I, 2, 7),I=1,10)  /
     +  .000E+00,11.5    ,12.9    ,13.9    ,16.1    ,17.0    ,19.1    ,
     +  19.8    ,20.6    ,21.0     /
      DATA (AE(I, 2, 8),I=1,10)  /
     +  .000E+00,.000E+00,12.4    ,13.8    ,15.9    ,17.2    ,19.1    ,
     +  20.3    ,21.4    ,22.3     /
      DATA (AE(I, 2, 9),I=1,10)  /
     +  .000E+00,.000E+00,13.4    ,14.5    ,17.1    ,18.3    ,20.9    ,
     +  21.9    ,23.0    ,23.7     /
      DATA (AE(I, 2,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,14.9    ,17.5    ,19.1    ,21.6    ,
     +  22.9    ,24.1    ,25.0     /
      DATA (AE(I, 2,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,15.0    ,18.0    ,19.6    ,22.4    ,
     +  23.8    ,25.2    ,26.2     /
      DATA (AE(I, 2,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,16.2    ,17.3    ,19.4    ,22.2    ,
     +  24.0    ,25.7    ,27.2     /
      DATA (AE(I, 2,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,17.8    ,19.8    ,22.9    ,
     +  24.6    ,26.2    ,27.7     /
      DATA (AE(I, 2,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,19.1    ,20.4    ,23.7    ,
     +  25.7    ,27.6    ,29.1     /
      DATA (AE(I, 2,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,20.5    ,24.1    ,
     +  26.1    ,28.1    ,29.9     /
      DATA (AE(I, 2,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,20.9    ,23.9    ,
     +  26.4    ,28.7    ,30.7     /
      DATA (AE(I, 2,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,22.4    ,24.2    ,
     +  26.7    ,29.0    ,30.9     /
      DATA (AE(I, 2,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,24.8    ,
     +  27.3    ,29.7    ,31.8     /
      DATA (AE(I, 2,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,26.1    ,
     +  27.3    ,29.9    ,32.3     /
      DATA (AE(I, 2,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  27.4    ,30.1    ,32.6     /
      DATA (AE(I, 3, 1),I=1,10)  /
     +  11.0    ,11.0    ,11.7    ,11.3    ,11.9    ,11.4    ,12.1    ,
     +  11.7    ,11.5    ,11.0     /
      DATA (AE(I, 3, 2),I=1,10)  /
     +  11.2    ,12.0    ,12.7    ,12.9    ,13.6    ,13.7    ,14.4    ,
     +  14.6    ,14.7    ,14.6     /
      DATA (AE(I, 3, 3),I=1,10)  /
     +  12.1    ,12.6    ,13.7    ,13.9    ,15.0    ,15.2    ,16.3    ,
     +  16.5    ,16.7    ,16.7     /
      DATA (AE(I, 3, 4),I=1,10)  /
     +  12.6    ,11.3    ,12.4    ,13.0    ,13.8    ,14.2    ,15.0    ,
     +  15.6    ,16.1    ,16.6     /
      DATA (AE(I, 3, 5),I=1,10)  /
     +  .000E+00,12.6    ,13.7    ,14.4    ,15.3    ,16.0    ,16.8    ,
     +  17.5    ,18.1    ,18.6     /
      DATA (AE(I, 3, 6),I=1,10)  /
     +  .000E+00,14.0    ,14.6    ,15.8    ,17.4    ,18.4    ,19.8    ,
     +  20.6    ,21.5    ,22.2     /
      DATA (AE(I, 3, 7),I=1,10)  /
     +  .000E+00,16.0    ,15.2    ,16.3    ,18.3    ,19.3    ,21.1    ,
     +  22.0    ,22.8    ,23.5     /
      DATA (AE(I, 3, 8),I=1,10)  /
     +  .000E+00,.000E+00,15.6    ,15.1    ,17.2    ,18.6    ,20.6    ,
     +  21.8    ,22.9    ,23.8     /
      DATA (AE(I, 3, 9),I=1,10)  /
     +  .000E+00,.000E+00,17.8    ,16.3    ,18.8    ,20.1    ,22.5    ,
     +  23.6    ,24.7    ,25.6     /
      DATA (AE(I, 3,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,17.5    ,19.0    ,20.7    ,23.1    ,
     +  24.5    ,25.8    ,26.8     /
      DATA (AE(I, 3,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,19.2    ,19.4    ,21.1    ,23.8    ,
     +  25.4    ,26.8    ,28.0     /
      DATA (AE(I, 3,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,20.7    ,19.6    ,19.7    ,22.4    ,
     +  24.4    ,26.2    ,27.9     /
      DATA (AE(I, 3,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,21.6    ,20.4    ,23.2    ,
     +  25.1    ,26.9    ,28.5     /
      DATA (AE(I, 3,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,23.5    ,22.0    ,23.8    ,
     +  26.1    ,28.1    ,29.9     /
      DATA (AE(I, 3,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,23.7    ,24.2    ,
     +  26.3    ,28.5    ,30.4     /
      DATA (AE(I, 3,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,25.4    ,24.8    ,
     +  25.6    ,28.1    ,30.5     /
      DATA (AE(I, 3,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,26.9    ,26.8    ,
     +  26.1    ,28.4    ,30.8     /
      DATA (AE(I, 3,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,28.8    ,
     +  27.6    ,29.0    ,31.5     /
      DATA (AE(I, 3,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,30.5    ,
     +  29.2    ,28.9    ,31.5     /
      DATA (AE(I, 3,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  31.0    ,30.0    ,31.7     /
      DATA (AE(I, 4, 1),I=1,10)  /
     +  13.0    ,13.2    ,14.8    ,14.2    ,14.2    ,14.1    ,14.5    ,
     +  14.4    ,14.3    ,14.0     /
      DATA (AE(I, 4, 2),I=1,10)  /
     +  13.5    ,14.5    ,16.1    ,15.9    ,16.0    ,16.3    ,16.8    ,
     +  17.0    ,17.1    ,17.2     /
      DATA (AE(I, 4, 3),I=1,10)  /
     +  14.9    ,15.3    ,17.2    ,17.1    ,17.5    ,17.8    ,18.6    ,
     +  18.9    ,19.1    ,19.3     /
      DATA (AE(I, 4, 4),I=1,10)  /
     +  15.1    ,13.5    ,16.4    ,16.7    ,16.4    ,17.3    ,17.8    ,
     +  18.5    ,19.0    ,19.6     /
      DATA (AE(I, 4, 5),I=1,10)  /
     +  .000E+00,15.6    ,17.5    ,17.7    ,17.8    ,18.6    ,19.2    ,
     +  19.9    ,20.3    ,21.1     /
      DATA (AE(I, 4, 6),I=1,10)  /
     +  .000E+00,18.0    ,18.4    ,19.2    ,19.8    ,20.9    ,22.0    ,
     +  23.1    ,23.6    ,24.7     /
      DATA (AE(I, 4, 7),I=1,10)  /
     +  .000E+00,27.4    ,19.1    ,19.8    ,20.7    ,21.8    ,23.2    ,
     +  24.4    ,24.9    ,25.9     /
      DATA (AE(I, 4, 8),I=1,10)  /
     +  .000E+00,.000E+00,18.9    ,18.9    ,19.3    ,21.1    ,22.5    ,
     +  24.0    ,24.7    ,26.0     /
      DATA (AE(I, 4, 9),I=1,10)  /
     +  .000E+00,.000E+00,21.1    ,19.7    ,20.7    ,22.3    ,24.0    ,
     +  25.6    ,26.3    ,27.7     /
      DATA (AE(I, 4,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,21.0    ,21.1    ,22.9    ,24.6    ,
     +  26.5    ,27.3    ,29.0     /
      DATA (AE(I, 4,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,21.3    ,22.4    ,23.1    ,25.0    ,
     +  27.1    ,27.9    ,29.8     /
      DATA (AE(I, 4,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,36.6    ,21.5    ,22.2    ,23.1    ,
     +  25.6    ,26.8    ,29.1     /
      DATA (AE(I, 4,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,22.9    ,23.1    ,23.7    ,
     +  26.2    ,27.3    ,29.6     /
      DATA (AE(I, 4,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,30.5    ,23.6    ,25.0    ,
     +  26.9    ,28.2    ,30.7     /
      DATA (AE(I, 4,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,25.4    ,26.2    ,
     +  27.2    ,28.3    ,31.0     /
      DATA (AE(I, 4,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,24.5    ,25.9    ,
     +  27.4    ,27.6    ,30.7     /
      DATA (AE(I, 4,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,43.3    ,28.4    ,
     +  27.5    ,27.9    ,30.9     /
      DATA (AE(I, 4,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,27.2    ,
     +  29.1    ,29.0    ,31.4     /
      DATA (AE(I, 4,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,51.3    ,
     +  30.6    ,29.5    ,31.4     /
      DATA (AE(I, 4,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  28.8    ,30.6    ,32.4     /
      DATA (AE(I, 5, 1),I=1,10)  /
     +  15.0    ,14.9    ,15.5    ,15.4    ,15.9    ,15.8    ,16.2    ,
     +  16.2    ,16.1    ,15.9     /
      DATA (AE(I, 5, 2),I=1,10)  /
     +  15.4    ,16.1    ,17.0    ,17.4    ,18.0    ,18.2    ,18.7    ,
     +  18.9    ,19.0    ,19.1     /
      DATA (AE(I, 5, 3),I=1,10)  /
     +  17.1    ,17.2    ,18.3    ,18.7    ,19.3    ,19.6    ,20.3    ,
     +  20.6    ,20.8    ,20.9     /
      DATA (AE(I, 5, 4),I=1,10)  /
     +  14.7    ,14.8    ,15.0    ,16.0    ,17.0    ,17.7    ,18.1    ,
     +  19.0    ,19.4    ,20.0     /
      DATA (AE(I, 5, 5),I=1,10)  /
     +  .000E+00,16.7    ,17.6    ,18.1    ,18.6    ,19.2    ,19.7    ,
     +  20.4    ,20.8    ,21.2     /
      DATA (AE(I, 5, 6),I=1,10)  /
     +  .000E+00,17.8    ,18.2    ,19.2    ,20.0    ,21.0    ,21.9    ,
     +  23.0    ,23.6    ,24.3     /
      DATA (AE(I, 5, 7),I=1,10)  /
     +  .000E+00,35.2    ,18.9    ,20.3    ,20.6    ,21.5    ,22.6    ,
     +  23.7    ,24.2    ,24.7     /
      DATA (AE(I, 5, 8),I=1,10)  /
     +  .000E+00,.000E+00,16.4    ,18.9    ,18.8    ,19.6    ,20.7    ,
     +  22.3    ,23.1    ,23.9     /
      DATA (AE(I, 5, 9),I=1,10)  /
     +  .000E+00,.000E+00,33.9    ,19.8    ,20.3    ,20.7    ,21.9    ,
     +  23.4    ,24.1    ,24.8     /
      DATA (AE(I, 5,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,18.0    ,20.0    ,21.4    ,22.0    ,
     +  23.8    ,24.6    ,25.4     /
      DATA (AE(I, 5,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,26.4    ,20.4    ,21.2    ,22.3    ,
     +  23.8    ,24.7    ,25.5     /
      DATA (AE(I, 5,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,41.7    ,18.2    ,19.8    ,21.1    ,
     +  22.6    ,23.4    ,24.6     /
      DATA (AE(I, 5,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,22.5    ,20.0    ,21.7    ,
     +  22.8    ,23.7    ,24.7     /
      DATA (AE(I, 5,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,54.1    ,19.9    ,21.9    ,
     +  23.2    ,24.3    ,25.3     /
      DATA (AE(I, 5,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,21.2    ,22.2    ,
     +  23.6    ,24.9    ,25.5     /
      DATA (AE(I, 5,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,44.9    ,21.9    ,
     +  23.8    ,25.2    ,25.6     /
      DATA (AE(I, 5,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,47.8    ,22.7    ,
     +  23.8    ,24.9    ,26.3     /
      DATA (AE(I, 5,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,35.5    ,
     +  23.9    ,25.9    ,26.6     /
      DATA (AE(I, 5,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,64.3    ,
     +  24.1    ,25.7    ,27.1     /
      DATA (AE(I, 5,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  34.0    ,25.7    ,27.7     /
      DATA (AE(I, 6, 1),I=1,10)  /
     +  16.6    ,16.5    ,16.8    ,16.7    ,17.0    ,16.5    ,16.7    ,
     +  18.3    ,18.9    ,19.0     /
      DATA (AE(I, 6, 2),I=1,10)  /
     +  16.2    ,16.6    ,17.2    ,17.4    ,17.9    ,17.4    ,17.7    ,
     +  20.7    ,22.0    ,22.6     /
      DATA (AE(I, 6, 3),I=1,10)  /
     +  18.9    ,18.7    ,18.8    ,18.6    ,18.9    ,18.6    ,18.9    ,
     +  21.0    ,22.3    ,22.9     /
      DATA (AE(I, 6, 4),I=1,10)  /
     +  18.3    ,12.7    ,14.2    ,15.0    ,15.7    ,16.1    ,16.3    ,
     +  16.5    ,17.9    ,19.0     /
      DATA (AE(I, 6, 5),I=1,10)  /
     +  .000E+00,15.7    ,15.1    ,15.3    ,16.5    ,16.4    ,16.4    ,
     +  17.0    ,18.3    ,19.4     /
      DATA (AE(I, 6, 6),I=1,10)  /
     +  .000E+00,22.9    ,14.9    ,15.2    ,16.2    ,16.9    ,17.4    ,
     +  18.2    ,19.5    ,21.1     /
      DATA (AE(I, 6, 7),I=1,10)  /
     +  .000E+00,40.7    ,18.4    ,15.9    ,17.1    ,17.7    ,18.9    ,
     +  19.5    ,20.3    ,21.1     /
      DATA (AE(I, 6, 8),I=1,10)  /
     +  .000E+00,.000E+00,23.3    ,16.2    ,16.3    ,17.3    ,18.7    ,
     +  19.5    ,20.3    ,21.1     /
      DATA (AE(I, 6, 9),I=1,10)  /
     +  .000E+00,.000E+00,49.2    ,19.0    ,19.1    ,19.4    ,20.2    ,
     +  20.8    ,21.6    ,22.0     /
      DATA (AE(I, 6,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,27.2    ,21.2    ,20.8    ,21.4    ,
     +  22.3    ,22.8    ,23.3     /
      DATA (AE(I, 6,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,45.6    ,25.0    ,22.8    ,23.9    ,
     +  23.6    ,24.3    ,24.4     /
      DATA (AE(I, 6,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,45.8    ,29.7    ,25.1    ,25.3    ,
     +  25.3    ,26.0    ,26.3     /
      DATA (AE(I, 6,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,42.7    ,29.0    ,28.0    ,
     +  27.0    ,27.2    ,27.6     /
      DATA (AE(I, 6,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,62.0    ,32.0    ,30.0    ,
     +  29.8    ,29.5    ,29.6     /
      DATA (AE(I, 6,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,44.5    ,34.4    ,
     +  32.7    ,31.5    ,31.8     /
      DATA (AE(I, 6,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,75.6    ,37.1    ,
     +  34.6    ,34.4    ,34.4     /
      DATA (AE(I, 6,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,51.2    ,45.2    ,
     +  39.0    ,37.5    ,36.4     /
      DATA (AE(I, 6,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,74.9    ,
     +  42.3    ,39.9    ,38.3     /
      DATA (AE(I, 6,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,69.5    ,
     +  50.7    ,42.3    ,41.4     /
      DATA (AE(I, 6,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  66.3    ,48.0    ,43.4     /
      DATA (AE(I, 7, 1),I=1,10)  /
     +  27.0    ,25.8    ,26.3    ,26.2    ,26.7    ,26.7    ,27.1    ,
     +  27.1    ,27.2    ,19.0     /
      DATA (AE(I, 7, 2),I=1,10)  /
     +  29.1    ,28.9    ,29.7    ,30.3    ,31.0    ,31.4    ,32.0    ,
     +  32.3    ,32.7    ,22.6     /
      DATA (AE(I, 7, 3),I=1,10)  /
     +  31.6    ,29.7    ,30.9    ,31.4    ,32.5    ,33.1    ,34.0    ,
     +  34.6    ,35.1    ,22.9     /
      DATA (AE(I, 7, 4),I=1,10)  /
     +  27.4    ,19.9    ,20.8    ,22.8    ,24.6    ,26.4    ,28.2    ,
     +  29.6    ,30.8    ,19.0     /
      DATA (AE(I, 7, 5),I=1,10)  /
     +  .000E+00,24.6    ,24.1    ,25.0    ,27.2    ,28.7    ,30.7    ,
     +  31.8    ,32.9    ,19.4     /
      DATA (AE(I, 7, 6),I=1,10)  /
     +  .000E+00,35.6    ,25.2    ,25.6    ,27.9    ,30.4    ,32.7    ,
     +  34.6    ,36.3    ,21.1     /
      DATA (AE(I, 7, 7),I=1,10)  /
     +  .000E+00,45.4    ,30.9    ,28.2    ,29.0    ,31.2    ,34.0    ,
     +  35.8    ,37.4    ,21.1     /
      DATA (AE(I, 7, 8),I=1,10)  /
     +  .000E+00,.000E+00,38.2    ,29.6    ,29.4    ,30.3    ,33.2    ,
     +  35.5    ,37.6    ,21.1     /
      DATA (AE(I, 7, 9),I=1,10)  /
     +  .000E+00,.000E+00,59.3    ,34.5    ,33.7    ,32.9    ,35.4    ,
     +  37.6    ,39.6    ,22.0     /
      DATA (AE(I, 7,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,44.5    ,37.8    ,37.5    ,37.2    ,
     +  39.0    ,41.4    ,23.3     /
      DATA (AE(I, 7,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,67.0    ,43.6    ,42.0    ,40.8    ,
     +  41.4    ,43.0    ,24.4     /
      DATA (AE(I, 7,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,49.9    ,50.9    ,44.6    ,43.9    ,
     +  44.2    ,44.2    ,26.3     /
      DATA (AE(I, 7,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,67.2    ,50.5    ,48.7    ,
     +  48.1    ,47.2    ,27.6     /
      DATA (AE(I, 7,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,68.1    ,55.2    ,52.3    ,
     +  51.5    ,51.6    ,29.6     /
      DATA (AE(I, 7,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,68.7    ,58.6    ,
     +  56.5    ,55.7    ,31.8     /
      DATA (AE(I, 7,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,89.3    ,62.9    ,
     +  60.0    ,59.1    ,34.4     /
      DATA (AE(I, 7,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,56.0    ,72.9    ,
     +  66.3    ,64.2    ,36.4     /
      DATA (AE(I, 7,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,105.    ,
     +  71.3    ,68.3    ,38.3     /
      DATA (AE(I, 7,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,73.4    ,
     +  76.8    ,72.4    ,41.4     /
      DATA (AE(I, 7,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  107.    ,79.9    ,43.4     /
      DATA (AE(I, 8, 1),I=1,10)  /
     +  35.5    ,35.3    ,35.7    ,35.7    ,36.3    ,36.3    ,36.7    ,
     +  36.7    ,36.7    ,19.0     /
      DATA (AE(I, 8, 2),I=1,10)  /
     +  40.6    ,41.4    ,41.9    ,42.3    ,43.2    ,43.5    ,44.0    ,
     +  44.3    ,44.5    ,22.6     /
      DATA (AE(I, 8, 3),I=1,10)  /
     +  45.4    ,45.7    ,46.4    ,47.0    ,48.1    ,48.7    ,49.4    ,
     +  49.8    ,50.2    ,22.9     /
      DATA (AE(I, 8, 4),I=1,10)  /
     +  43.9    ,44.3    ,43.4    ,45.1    ,47.3    ,48.7    ,49.6    ,
     +  50.5    ,51.3    ,19.0     /
      DATA (AE(I, 8, 5),I=1,10)  /
     +  .000E+00,49.3    ,49.6    ,50.5    ,53.2    ,54.2    ,55.4    ,
     +  56.1    ,56.8    ,19.4     /
      DATA (AE(I, 8, 6),I=1,10)  /
     +  .000E+00,59.1    ,53.0    ,55.4    ,58.0    ,60.0    ,61.2    ,
     +  62.5    ,63.6    ,21.1     /
      DATA (AE(I, 8, 7),I=1,10)  /
     +  .000E+00,54.5    ,57.1    ,59.2    ,62.3    ,64.4    ,66.0    ,
     +  67.3    ,68.5    ,21.1     /
      DATA (AE(I, 8, 8),I=1,10)  /
     +  .000E+00,.000E+00,65.9    ,62.1    ,65.1    ,67.6    ,69.4    ,
     +  71.1    ,72.6    ,21.1     /
      DATA (AE(I, 8, 9),I=1,10)  /
     +  .000E+00,.000E+00,72.2    ,67.1    ,70.5    ,73.1    ,75.1    ,
     +  76.8    ,78.4    ,22.0     /
      DATA (AE(I, 8,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,80.1    ,75.0    ,78.0    ,80.0    ,
     +  82.1    ,83.9    ,23.3     /
      DATA (AE(I, 8,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,94.5    ,82.2    ,82.8    ,85.1    ,
     +  87.3    ,89.2    ,24.4     /
      DATA (AE(I, 8,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,56.8    ,92.5    ,87.2    ,89.4    ,
     +  91.9    ,94.1    ,26.3     /
      DATA (AE(I, 8,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,116.    ,96.2    ,94.4    ,
     +  97.0    ,99.2    ,27.6     /
      DATA (AE(I, 8,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,78.1    ,104.    ,102.    ,
     +  102.    ,105.    ,29.6     /
      DATA (AE(I, 8,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,128.    ,111.    ,
     +  109.    ,110.    ,31.8     /
      DATA (AE(I, 8,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,104.    ,118.    ,
     +  117.    ,115.    ,34.4     /
      DATA (AE(I, 8,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,64.4    ,138.    ,
     +  124.    ,122.    ,36.4     /
      DATA (AE(I, 8,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,133.    ,
     +  133.    ,132.    ,38.3     /
      DATA (AE(I, 8,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,83.6    ,
     +  146.    ,139.    ,41.4     /
      DATA (AE(I, 8,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  166.    ,147.    ,43.4     /
      DATA (AE(I, 9, 1),I=1,10)  /
     +  43.3    ,43.2    ,43.6    ,43.8    ,44.1    ,44.3    ,44.7    ,
     +  44.8    ,44.8    ,19.0     /
      DATA (AE(I, 9, 2),I=1,10)  /
     +  50.9    ,51.4    ,52.0    ,52.6    ,53.1    ,53.6    ,54.2    ,
     +  54.5    ,54.7    ,22.6     /
      DATA (AE(I, 9, 3),I=1,10)  /
     +  58.0    ,58.4    ,59.3    ,60.1    ,60.7    ,61.5    ,62.3    ,
     +  62.7    ,63.1    ,22.9     /
      DATA (AE(I, 9, 4),I=1,10)  /
     +  62.0    ,63.9    ,63.7    ,65.7    ,65.5    ,67.5    ,68.2    ,
     +  68.9    ,69.7    ,19.0     /
      DATA (AE(I, 9, 5),I=1,10)  /
     +  .000E+00,72.2    ,72.5    ,74.2    ,74.2    ,76.1    ,77.0    ,
     +  77.8    ,78.6    ,19.4     /
      DATA (AE(I, 9, 6),I=1,10)  /
     +  .000E+00,80.4    ,80.5    ,83.1    ,83.0    ,85.5    ,86.8    ,
     +  88.1    ,89.2    ,21.1     /
      DATA (AE(I, 9, 7),I=1,10)  /
     +  .000E+00,63.4    ,88.5    ,91.3    ,91.1    ,94.0    ,95.8    ,
     +  97.3    ,98.6    ,21.1     /
      DATA (AE(I, 9, 8),I=1,10)  /
     +  .000E+00,.000E+00,98.8    ,98.6    ,97.8    ,102.    ,104.    ,
     +  106.    ,108.    ,21.1     /
      DATA (AE(I, 9, 9),I=1,10)  /
     +  .000E+00,.000E+00,84.1    ,107.    ,107.    ,111.    ,113.    ,
     +  116.    ,117.    ,22.0     /
      DATA (AE(I, 9,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,116.    ,115.    ,119.    ,122.    ,
     +  125.    ,127.    ,23.3     /
      DATA (AE(I, 9,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,111.    ,123.    ,127.    ,131.    ,
     +  134.    ,137.    ,24.4     /
      DATA (AE(I, 9,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,65.6    ,136.    ,135.    ,140.    ,
     +  143.    ,146.    ,26.3     /
      DATA (AE(I, 9,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,146.    ,144.    ,149.    ,
     +  152.    ,155.    ,27.6     /
      DATA (AE(I, 9,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,88.7    ,152.    ,158.    ,
     +  162.    ,165.    ,29.6     /
      DATA (AE(I, 9,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,181.    ,167.    ,
     +  171.    ,174.    ,31.8     /
      DATA (AE(I, 9,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,117.    ,174.    ,
     +  180.    ,183.    ,34.4     /
      DATA (AE(I, 9,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,72.0    ,201.    ,
     +  189.    ,192.    ,36.4     /
      DATA (AE(I, 9,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,151.    ,
     +  198.    ,201.    ,38.3     /
      DATA (AE(I, 9,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,95.2    ,
     +  220.    ,210.    ,41.4     /
      DATA (AE(I, 9,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  192.    ,217.    ,43.4     /
      DATA (AE(I,10, 1),I=1,10)  /
     +  62.1    ,62.1    ,62.6    ,62.9    ,63.3    ,63.3    ,64.0    ,
     +  64.0    ,64.0    ,19.0     /
      DATA (AE(I,10, 2),I=1,10)  /
     +  75.1    ,75.4    ,76.3    ,76.8    ,77.6    ,77.9    ,78.8    ,
     +  79.0    ,79.3    ,22.6     /
      DATA (AE(I,10, 3),I=1,10)  /
     +  87.5    ,88.3    ,89.4    ,90.2    ,91.3    ,91.9    ,93.0    ,
     +  93.5    ,93.9    ,22.9     /
      DATA (AE(I,10, 4),I=1,10)  /
     +  104.    ,104.    ,105.    ,106.    ,107.    ,108.    ,109.    ,
     +  110.    ,110.    ,19.0     /
      DATA (AE(I,10, 5),I=1,10)  /
     +  .000E+00,122.    ,122.    ,123.    ,124.    ,125.    ,126.    ,
     +  127.    ,128.    ,19.4     /
      DATA (AE(I,10, 6),I=1,10)  /
     +  .000E+00,138.    ,139.    ,140.    ,142.    ,143.    ,144.    ,
     +  146.    ,147.    ,21.1     /
      DATA (AE(I,10, 7),I=1,10)  /
     +  .000E+00,85.3    ,158.    ,159.    ,161.    ,162.    ,164.    ,
     +  166.    ,167.    ,21.1     /
      DATA (AE(I,10, 8),I=1,10)  /
     +  .000E+00,.000E+00,176.    ,177.    ,179.    ,181.    ,183.    ,
     +  184.    ,186.    ,21.1     /
      DATA (AE(I,10, 9),I=1,10)  /
     +  .000E+00,.000E+00,114.    ,199.    ,201.    ,202.    ,205.    ,
     +  206.    ,207.    ,22.0     /
      DATA (AE(I,10,10),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,218.    ,219.    ,220.    ,224.    ,
     +  225.    ,226.    ,23.3     /
      DATA (AE(I,10,11),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,150.    ,238.    ,238.    ,243.    ,
     +  244.    ,245.    ,24.4     /
      DATA (AE(I,10,12),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,85.8    ,255.    ,255.    ,261.    ,
     +  262.    ,263.    ,26.3     /
      DATA (AE(I,10,13),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,195.    ,272.    ,279.    ,
     +  279.    ,280.    ,27.6     /
      DATA (AE(I,10,14),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,115.    ,290.    ,296.    ,
     +  297.    ,298.    ,29.6     /
      DATA (AE(I,10,15),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,263.    ,313.    ,
     +  314.    ,315.    ,31.8     /
      DATA (AE(I,10,16),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,150.    ,330.    ,
     +  331.    ,332.    ,34.4     /
      DATA (AE(I,10,17),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,90.0    ,319.    ,
     +  349.    ,349.    ,36.4     /
      DATA (AE(I,10,18),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,196.    ,
     +  366.    ,367.    ,38.3     /
      DATA (AE(I,10,19),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,122.    ,
     +  387.    ,384.    ,41.4     /
      DATA (AE(I,10,20),I=1,10)  /
     +  .000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,.000E+00,
     +  247.    ,401.    ,43.4     /
      DATA (ERES(I, 1),I=1,10)  / 10*0./
      DATA (ERES(I, 2),I=1,10)  / 10*0./
      DATA (ERES(I, 3),I=1,10)  / 10*0./
      DATA (ERES(I, 4),I=1,10)  / 10*0./
      DATA (ERES(I, 5),I=1,10)  / 10*0./
      DATA (ERES(I, 6),I=1,10)  /
     +     0.000,   0.000,   0.000,   0.000,   0.000,   0.000,   0.000,
     +     2.780,   2.880,   2.890 /
      DATA (ERES(I, 7),I=1,10)  /
     +     1.500,   2.460,   2.510,   2.610,   2.700,   2.920,   3.070,
     +     3.200,   3.330,   2.890 /
      DATA (ERES(I, 8),I=1,10)  /
     +     4.470,   4.350,   4.390,   4.550,   4.660,   4.890,   4.980,
     +     5.100,   5.220,   2.890 /
      DATA (ERES(I, 9),I=1,10)  /
     +     7.480,   7.380,   7.370,   7.480,   7.510,   7.630,   7.660,
     +     7.750,   7.820,   2.890 /
      DATA (ERES(I,10),I=1,10)  /
     +    15.270,  15.190,  15.200,  15.370,  15.380,  15.430,  15.540,
     +    15.590,  15.630,   2.890 /
      END
      FUNCTION GASDEV(IDUMMY)
C...Gaussian deviation
      SAVE GSET
      DATA ISET/0/
      IF (ISET.EQ.0) THEN
1       V1=2.*RAN(0)-1.
        V2=2.*RAN(0)-1.
        R=V1**2+V2**2
        IF(R.GE.1.)GO TO 1
        FAC=SQRT(-2.*LOG(R)/R)
        GSET=V1*FAC
        GASDEV=V2*FAC
        ISET=1
      ELSE
        GASDEV=GSET
        ISET=0
      ENDIF
      RETURN
      END

      FUNCTION GAUSS (FUN, A,B)
C...Returns the  8 points Gauss-Legendre integral
C.  of function FUN from A to B
C...........................................................
      DIMENSION X(8), W(8)
      DATA X / .0950125098, .2816035507, .4580167776, .6178762444
     1          ,.7554044083, .8656312023, .9445750230, .9894009349/
      DATA W / .1894506104, .1826034150, .1691565193, .1495959888
     1          ,.1246289712, .0951585116, .0622535239, .0271524594/
      XM = 0.5*(B+A)
      XR = 0.5*(B-A)
      SS = 0.
      DO J=1,8
        DX = XR*X(J)
        SS = SS + W(J) * (FUN(XM+DX) + FUN(XM-DX))
      ENDDO
      GAUSS = XR*SS
      RETURN
      END

      SUBROUTINE GG_FRAG (E0)
C...This routine fragments a  gluon-gluon system
C.  of mass E0 (GeV)
C.  the particles produced are in the  jet-jet frame
C.  oriented along the z axis
C...........................................................
      COMMON /S_PLIST/ NP, P(5000,5), LLIST(5000)
      COMMON /S_MASS1/ AM(49), AM2(49)
      DIMENSION WW(2,2),PTOT(4),PX(3),PY(3),IFL(3),PMQ(3)

C...Generate the 'forward' leading particle.
100   I = NP+1
      I0 = -1 + 2.*INT(1.9999*RAN(0))
      CALL IFLAV(I0,0,IFL1, LDUM)
      CALL IFLAV(IFL1,0,IFL2, LLIST(I))
      CALL PTDIS(IFL1,PX1,PY1)
      CALL PTDIS(IFL2,PX2,PY2)
      P(I,1) = PX1+PX2
      P(I,2) = PY1+PY2
      P(I,5) = AM(IABS(LLIST(I)))
      XM1 = P(I,5)**2+P(I,1)**2+P(I,2)**2
      Z1 = ZDIS (IFL1,1,0.25*XM1)
      Z2 = ZDIS (IFL2,1,0.25*XM1)
      T1  = 4.*XM1/(E0*E0*(Z1+Z2))
      P(I,4) = 0.25*E0*(Z1+Z2 + T1)
      P(I,3) = 0.25*E0*(Z1+Z2 - T1)

C...Generate the 'backward' leading particle.
      I = I+1
      CALL IFLAV(-I0,0,IFL3, LDUM)
      CALL IFLAV(IFL3,0,IFL4, LLIST(I))
      CALL PTDIS(IFL3,PX3,PY3)
      CALL PTDIS(IFL4,PX4,PY4)
      P(I,1) = PX3+PX4
      P(I,2) = PY3+PY4
      P(I,5) = AM(IABS(LLIST(I)))
      XM2 = P(I,5)**2+P(I,1)**2+P(I,2)**2
      Z3 = ZDIS (IFL3,1,0.25*XM2)
      Z4 = ZDIS (IFL4,1,0.25*XM2)
      T2  = 4.*XM2/(E0*E0*(Z3+Z4))
      P(I,4) = 0.25*E0*( Z3+Z4 + T2)
      P(I,3) = 0.25*E0*(-Z3-Z4 + T2)

C...Fragment the two remaning strings
      N0 = 0
      DO KS=1,2

      NTRY = 0
200      NTRY = NTRY+1
      I = NP+2+N0
      IF (NTRY .GT. 30)  GOTO 100

      IF (KS .EQ. 1)  THEN
         WW(1,1) = 0.5 * (1 - Z1 - 0.5*T2)
         WW(2,1) = 0.5 * (1 - Z3 - 0.5*T1)
         PX(1) = -PX1
         PY(1) = -PY1
         PX(2) = -PX3
         PY(2) = -PY3
         IFL(1) = -IFL1
         IFL(2) = -IFL3
      ELSE
         WW(1,1) = 0.5 * (1 - Z2 - 0.5*T2)
         WW(2,1) = 0.5 * (1 - Z4 - 0.5*T1)
         PX(1) = -PX2
         PY(1) = -PY2
         PX(2) = -PX4
         PY(2) = -PY4
         IFL(1) = -IFL2
         IFL(2) = -IFL4
      ENDIF
      PX(3) = 0.
      PY(3) = 0.
      PTOT (1) = PX(1)+PX(2)
      PTOT (2) = PY(1)+PY(2)
      PTOT (3) = 0.5*E0*(WW(1,1)-WW(2,1))
      PTOT (4) = 0.5*E0*(WW(1,1)+WW(2,1))

      PMQ(1) = QMASS(IFL(1))
      PMQ(2) = QMASS(IFL(2))

C...produce new particle: side, pT
300      I=I+1
      JT=1.5+RAN(0)
      JR=3-JT
c      CALL PTDIS (IFL(JT), PX(3),PY(3))

C...particle ID
      CALL IFLAV (IFL(JT), 0, IFL(3), LLIST(I))
      PMQ(3) = QMASS(IFL(3))
      P(I,5) = AM(IABS(LLIST(I)))

      CALL PTDIS (IFL(3), PX(3),PY(3))

C...test end of fragmentation
      WREM2 = PTOT(4)**2-PTOT(1)**2-PTOT(2)**2-PTOT(3)**2
      IF (WREM2 .LT. 0.1)  GOTO 200
      WMIN = PMQ(1)+PMQ(2)+2.*PMQ(3)+1.1 + (2.*RAN(0)-1.)*0.2
      IF (WREM2 .LT. WMIN**2)  GOTO 400

C...fill transverse momentum
      P(I,1) = PX(JT) + PX(3)
      P(I,2) = PY(JT) + PY(3)

C...Choose z
      XMT2 = P(I,5)**2+P(I,1)**2+P(I,2)**2
      Z = ZDIS (ifl(3),IFL(JT), XMT2)

      WW(JT,2) = Z*WW(JT,1)
      WW(JR,2) = XMT2/(WW(JT,2)*E0**2)

      P(I,3) = WW(1,2)*0.5*E0 - WW(2,2)*0.5*E0
      P(I,4) = WW(1,2)*0.5*E0 + WW(2,2)*0.5*E0

      DO J=1,4
         PTOT (J) = PTOT(J) - P(I,J)
      ENDDO
      DO K=1,2
         WW(K,1) = WW(K,1) - WW(K,2)
      ENDDO

C...Reset pT and flavor at ends of the string
      PX(JT) = -PX(3)
      PY(JT) = -PY(3)
      IFL(JT) =-IFL(3)
      PMQ(JT) = PMQ(3)
      GOTO 300

C...Final two hadrons
400   IF (IFL(JR)*IFL(3) .GT. 100)  GOTO 200
      CALL IFLAV (IFL(JR), -IFL(3), IFLA, LLIST(I+1))
      P(I+1,5) = AM(IABS(LLIST(I+1)))
      P(I,1)   = PX(JT)+PX(3)
      P(I,2)   = PY(JT)+PY(3)
      I1 = I+1
      P(I1,1) = PX(JR)-PX(3)
      P(I1,2) = PY(JR)-PY(3)
      XM1 = P(I,5)**2+P(I,1)**2+P(I,2)**2
      XM2 = P(I1,5)**2+P(I1,1)**2+P(I1,2)**2
      IF (SQRT(XM1)+SQRT(XM2) .GT. SQRT(WREM2)) GOTO 200
      if (ptot(4).le.0) goto 200
      WREM = SQRT(WREM2)
      EA1 = (WREM2+XM1-XM2)/(2.*WREM)
      PA2 = (EA1**2-XM1)
      if (pa2.ge.0.0) then
        PA = SQRT(pa2)
      else
       goto 200
      endif
      BA = PTOT(3)/PTOT(4)
      GA = PTOT(4)/WREM
      S = FLOAT(3-2*JT)
      P(I,3) = GA*(BA*EA1+S*PA)
      P(I,4) = GA*(EA1+BA*S*PA)
      P(I+1,3) = PTOT(3)-P(I,3)
      P(I+1,4) = PTOT(4)-P(I,4)
      N0 = I-NP-1
      ENDDO                  ! loop on two `remaining strings'
      NP = I+1
      RETURN
      END
      SUBROUTINE GLAUBER(JA,SSIG,SLOPE,ALPHA,SIGT,SIGEL,SIGQEL)
C...Subroutine to compute hadron-Nucleus cross sections
C.  according to:
C.  R.J. Glauber and G.Matthiae  Nucl.Phys. B21, 135, (1970)
C.
C.  This formulas assume that the target nucleus  density is
C.  modeled by a shell-model form.  A reasonable range of models
C   is  4 < JA < 18
C.
C.  INPUT :  A = mass number of the nucleus
C.           SSIG  (mbarn) total pp cross section
C.           SLOPE (GeV**-2)  elastic scattering slope for pp
C.           ALPHA    real/imaginary part of the forward pp elastic
C.                                               scattering amplitude
C.  OUTPUT : SIGT  = Total cross section
C.           SIGEL = Elastic cross section
C.           SIGQEL  = Elastic + Quasi elastic cross section
C.
C. Internally  everything is computed in GeV (length = GeV**-1)
C......................................................................
      COMMON /CA0SH/ R0, R02
      COMPLEX  ZZ, ZS, ZP, ZC
      DIMENSION RR(18)
      DATA CMBARN /0.389385/
      DATA PI /3.1415926/
      DATA BMAX /50./            ! GeV**-1
      DATA NB /100/
C...data on Sqrt[<r**2>] (fm). (A=5,8 are not correct). From Barett and Jackson
      DATA RR /0.81,2.095,1.88,1.674, 2.56,2.56,2.41,2.5,2.519,2.45
     +          ,2.37, 2.460, 2.440, 2.54, 2.58, 2.718, 2.662,2.789 /
      A = FLOAT(JA)
C...Parameter of shell model density
      R0 = RR(JA)/0.197/SQRT(5./2. - 4./A)         ! GeV**-1
      R02 = R0*R0
      SIG = SSIG/CMBARN                           ! GeV**-2
      DB = BMAX/FLOAT(NB)
      SUM = 0.
      SUM1 = 0.
      SUM2 = 0.
      DO JB=1,NB
         B = DB*(FLOAT(JB)-0.5)
         GS = GLAUBGS (B,SLOPE, SIG)
         GP = GLAUBGP (B,SLOPE, SIG)
         XS = (1.- GS)
         YS =  GS*ALPHA
         ZS = CMPLX(XS,YS)
         XP = (1.- GP)
         YP =  GP*ALPHA
         ZP = CMPLX(XP,YP)
         ZZ = ZS**4. * ZP**(A-4.)
         X = REAL (ZZ)
         Y = AIMAG(ZZ)
         ZC = CMPLX(X,-Y)
         SUM = SUM + (1.-X)*B
         SUM1 = SUM1 + ((1.-X)**2 + Y**2)*B
         OMS = OMEGAS(B,SIG,SLOPE,ALPHA)
         OMP = OMEGAP(B,SIG,SLOPE,ALPHA)
         OM = (1.- 2.*GS + OMS)**4. * (1. -2.*GP + OMP)**(A-4.)
         SUM2 = SUM2 + (1.-2.*X + OM)*B
      ENDDO
      SIGT =   SUM  * DB * 4.*PI * CMBARN
      SIGEL =  SUM1 * DB * 2.*PI * CMBARN
      SIGQEL = SUM2 * DB * 2.*PI * CMBARN
      RETURN
      END
      FUNCTION GLAUBGP (B,SLOPE, SIG)
      COMMON /CA0SH/ A0, A02
      DATA PI /3.1415926/
      GAMMA2 = A02/4. + 0.5*SLOPE
      ARG = B**2/(4.*GAMMA2)
      C1 = 1.- A02/(6.*GAMMA2)*(1.-ARG)
      GLAUBGP = SIG/(8.*PI*GAMMA2) *  C1 * EXP(-ARG)
      RETURN
      END
      FUNCTION GLAUBGS (B,SLOPE, SIG)
      COMMON /CA0SH/ A0, A02
      DATA PI /3.1415926/
      GAMMA2 = A02/4. + 0.5*SLOPE
      ARG = B**2/(4.*GAMMA2)
      GLAUBGS = SIG/(8.*PI*GAMMA2) * EXP(-ARG)
      RETURN
      END
      SUBROUTINE HAD_CONV
C----------------------------------------------------------------------------
C  Code for the convolution of hadrons
C----------------------------------------------------------------------------
C...Convolution of hadrons profile
C.  [function A(b) of Durand and Pi]
C.  precalculate and put  in COMMON block
C.........................................
      COMMON /S_CHDCNV/NB,DB,ABPP(200),ABPIP(200),ABPPH(200),
     +        ABPIPH(200)
      REAL*4 NU2, MU2, NUPI2, NU, MU, NUPI

      COMMON /S_CH0CNV/ NU2, MU2, NUPI2, NU, MU, NUPI

      NU2 = 0.71
      MU2 = 0.88
      NUPI2 = 0.54

      NU = SQRT(NU2)
      MU = SQRT(MU2)
      NUPI = SQRT(NUPI2)

C...integration constants
      BMAX = 15.
      NB  = 200
      DB = BMAX/FLOAT(NB)

      DO JB=1,NB
         B = DB*FLOAT(JB-1)
         ABPP(JB)    = A_PP(B)
         ABPIP(JB)   = A_PIP(B)
      ENDDO
      NU2 = 0.71
      MU2 = 0.88
      NUPI2 = 0.54

      NU = SQRT(NU2)
      MU = SQRT(MU2)
      NUPI = SQRT(NUPI2)

      DB = BMAX/FLOAT(NB)
      DO JB=1,NB
         B = DB*FLOAT(JB-1)
         ABPPh(JB)    = A_PP(B)
         ABPIPh(JB)   = A_PIP(B)
      ENDDO
      RETURN
      END
      FUNCTION HELIUM (R)
C... Helium density from Barrett and Jackson
C.   INPUT R = r coordinate (fm)
C.   OUTPUT (fm**-3)
C........................................................
      DATA R0 /0.964/, CA /0.322/   ! fm
      DATA W /0.517/, CC /5.993224E-02/
      HELIUM = CC*(1.+W*(R/R0)**2)/(1. + EXP((R-R0)/CA))
      RETURN
      END

      SUBROUTINE HSPLI (KF, KP1,KP2)
C...This subroutine splits one hadron of code KF
C.  into 2 partons of code KP1 and KP2
C.  KP1 refers to a color triplet [q or (qq)bar]
C.  KP2 to a a color anti-triplet [qbar or (qq)]
C.  allowed inputs:
C.  KF = 7:14 pi+-,k+-,k0L,k0s, p,n
C.     = -13,-14  pbar,nbar
C.................................................

      L = IABS(KF)-6
      GOTO (100,200,300,400,500,500,600,700), L

100      KP1 = 1                  ! pi+
      KP2 = -2
      RETURN
200      KP1 = 2                  ! pi-
      KP2 = -1
      RETURN
300      KP1 = 1                  ! k+
      KP2 = -3
      RETURN
400      KP1 = 3                  ! k-
      KP2 = -1
      RETURN
500      KP1 = 2                  ! k0l, k0s
      KP2 = -3
      IF (RAN(0).GT. 0.5)  THEN
        KP1 = 3
        KP2 = -2
      ENDIF
      return       ! bug fix 5-91
600      R = 6.*RAN(0)            ! p/pbar
      IF (R .LT.3.)       THEN
        KP1 = 1
        KP2 = 12
      ELSEIF (R .LT. 4.)  THEN
        KP1 = 1
        KP2 = 21
      ELSE
        KP1 = 2
        KP2 = 11
      ENDIF
      IF (KF .LT. 0)      THEN
        KPP = KP1
        KP1 = -KP2
        KP2 = -KPP
      ENDIF
      RETURN

700      R = 6.*RAN(0)                  ! n/nbar
      IF (R .LT.3.)       THEN
         KP1 = 2
         KP2 = 12
      ELSEIF (R .LT. 4.)  THEN
        KP1 = 2
        KP2 = 21
      ELSE
        KP1 = 1
        KP2 = 22
      ENDIF
      IF (KF .LT. 0)      THEN
        KPP = KP1
        KP1 = -KP2
        KP2 = -KPP
      ENDIF
      RETURN
      END
      SUBROUTINE IFLAV (IFL1,IFL2A, IFL2, KF)
C...This subroutine receives as input IFL1 the flavor code
C.  of a quark (antiquark) and  generates the antiquark (quark)
C.  of flavor code IFL2 that combine with the original parton
C.  to compose an hadron of code KF. ONLY 3 FLAVORS
C.  If (IFL2A.NE.0) returns an hadron KF composed of IFL1 and IFL2A
C...................................................................
      COMMON /S_CFLAFR/ PAR(8)
      DIMENSION KFLA(3,3,2), CDIAG(12), KDIAG(6)
      DIMENSION KBAR(30), CFR(12), KFR(80)
      DATA KFLA /0,8,10,7,0,22,9,21,0,0,26,29,25,0,31,28,30,0/
      DATA CDIAG /0.5,0.25,0.5,0.25,1.,0.5,0.5,0.,0.5,0.,1.,1./
      DATA KDIAG /6,23,24,27,32,33/
      DATA KBAR /13,14,34,35,36,37,38,9*0,39,3*0,40,41,42,43,44,
     +             45,46,47,48,49/
      DATA CFR /0.75,0.,0.5,0.,0.,1.,0.1667,0.3333,0.0833,0.6667,
     +            0.1667,0.3333/
      DATA KFR/0,16,17,19,100,104,109,115,0,26,27,29,122,126,131,137
     +  ,0,40,42,47,144,158,178,205,0,1,3,6,10,15,21,28,0,0,56,57,240,
     +  246,256,271,0,0,1,3,6,10,15,21,60,61,64,70,292,307,328,356,
     +  0,1,3,6,10,15,21,28,16*0/


      IFLA = IABS(IFL1)
      IF (IFL2A .NE. 0)  THEN
         IFL2A = MOD(IFL2A,100)
         IFL2 = IFL2A
         IFLB = IABS(IFL2A)
         MB = 0
         IF (IFLB .GT. 10)   MB=1
         IF (IFLA .GT. 10)   MB=2
      ELSE
          MB = 2
         IF (IFLA .LT. 10)   THEN
             MB = 1
             IF ((1.+PAR(1))*RAN(0).LT. 1.)  MB=0
         ENDIF
      ENDIF

      IF (MB .EQ. 0)  THEN
         IF (IFL2A.EQ.0)
     +        IFL2=ISIGN(1+INT((2.+PAR(2))*RAN(0)),-IFL1)
         IFLD = MAX(IFL1,IFL2)
         IFLE = MIN(IFL1,IFL2)
         GOTO 100
      ENDIF

C...Decide if the diquark must be split
      IF (MB .EQ. 2 .AND. IFLA .GT. 100)   THEN
         IFLA = MOD(IFLA,100)
           GOTO 200
      ENDIF
      IF (MB .EQ. 2 .AND. IFLA .EQ. 0)   THEN
          IF (RAN(0) .LT. PAR(8))  THEN
             MB = 0
             IFLG = MOD(IFL1,10)
             IFLH =(IFL1-IFLG)/10
             IF (RAN(0) .GT. 0.5)  THEN
                IFLDUM = IFLG
                IFLG = IFLH
                IFLH = IFLDUM
             ENDIF
             IFL11=IFLG
             IFL22=ISIGN(1+INT((2.+PAR(2))*RAN(0)),-IFL1)
             IFLD = MAX(IFL11,IFL22)
             IFLE = MIN(IFL11,IFL22)
             IFL2 = -IFLH*10+IFL22
             IF (RAN(0) .GT. 0.5)  IFL2 = IFL22*10-IFLH
             IFL2 = IFL2+ISIGN(100,IFL2)
          ENDIF
      ENDIF

C...Form a meson: consider spin and flavor mixing for the diagonal states
100      IF (MB .EQ. 0)  THEN
         IF1 = IABS(IFLD)
         IF2 = IABS(IFLE)
         IFLC = MAX(IF1,IF2)
         KSP = INT(PAR(5)+RAN(0))
         IF (IFLC.EQ.3)  KSP = INT(PAR(6)+RAN(0))
C D.H.
         KSP = MIN(KSP,1)

         IF (IF1 .NE. IF2)   THEN
            KF = KFLA(IF1,IF2,KSP+1)
         ELSE
            R = RAN(0)
            JF=1+INT(R+CDIAG(6*KSP+2*IF1-1))+
     +             INT(R+CDIAG(6*KSP+2*IF1))
C D.H.
            JF = MIN(JF,3)

            KF=KDIAG(JF+3*KSP)
         ENDIF
         RETURN
      ENDIF

C...Form a baryon
200      IF (IFL2A .NE. 0)   THEN
          IF (MB .EQ. 1)  THEN
             IFLD = IFLA
             IFLE = IFLB/10
             IFLF = MOD(IFLB,10)
          ELSE
             IFLD = IFLB
             IFLE = IFLA/10
             IFLF = MOD(IFLA,10)
          ENDIF
          LFR = 3+2*((2*(IFLE-IFLF))/(1+IABS(IFLE-IFLF)))
          IF(IFLD.NE.IFLE.AND.IFLD.NE.IFLF)  LFR=LFR+1
      ELSE
110          CONTINUE
          IF(MB.EQ.1)   THEN            ! generate diquark
             IFLD = IFLA
120             IFLE = 1+INT((2.+PAR(2)*PAR(3))*RAN(0))
             IFLF = 1+INT((2.+PAR(2)*PAR(3))*RAN(0))
             IF(IFLE.GE.IFLF.AND.PAR(4).LT.RAN(0))    GOTO 120
             IF(IFLE.LT.IFLF.AND.PAR(4)*RAN(0).GT.1.) GOTO 120
             IFL2=ISIGN(10*IFLE+IFLF,IFL1)
          ELSE                  ! generate quark
             IFL2=ISIGN(1+INT((2.+PAR(2))*RAN(0)),IFL1)
             IFLD=IABS(IFL2)
             IFLE=IFLA/10
             IFLF=MOD(IFLA,10)
          ENDIF
C...SU(6) factors for baryon formation
             LFR=3+2*((2*(IFLE-IFLF))/(1+IABS(IFLE-IFLF)))
          IF(IFLD.NE.IFLE.AND.IFLD.NE.IFLF)  LFR=LFR+1
          WT = CFR(2*LFR-1)+PAR(7)*CFR(2*LFR)
          IF(IFLE.LT.IFLF)   WT=WT/3.
          IF (WT.LT.RAN(0)) GOTO 110
      ENDIF

C...Form Baryon
      IFLG=MAX(IFLD,IFLE,IFLF)
      IFLI=MIN(IFLD,IFLE,IFLF)
      IFLH=IFLD+IFLE+IFLF-IFLG-IFLI
      KSP=2+2*INT(1.-CFR(2*LFR-1)+(CFR(2*LFR-1)+PAR(7)*
     1       CFR(2*LFR))*RAN(0))

C...Distinguish Lambda- and Sigma- like particles
      IF (KSP.EQ.2.AND.IFLG.GT.IFLH.AND.IFLH.GT.IFLI)  THEN
      IF(IFLE.GT.IFLF.AND.IFLD.NE.IFLG) KSP=2+INT(0.75+RAN(0))
       IF(IFLE.LT.IFLF.AND.IFLD.EQ.IFLG) KSP=3
       IF(IFLE.LT.IFLF.AND.IFLD.NE.IFLG) KSP=2+INT(0.25+RAN(0))
      ENDIF
      KF=KFR(16*KSP-16+IFLG)+KFR(16*KSP-8+IFLH)+IFLI
      KF=ISIGN(KBAR(KF-40),IFL1)

      RETURN
      END
      SUBROUTINE INI_WRITE (LUN)
C...This subroutine prints on unit LUN
C.  a table of the cross sections  used in the program
C.  and of the average number of jets, and the average
C.  number of wounded nucleons in a hadron-air interaction
C---------------------------------------------------------
      COMMON /S_CCSIG/ NSQS, ASQSMIN, ASQSMAX, DASQS,
     +    SSIG(51,2), PJETC(0:20,51,2),SSIGN(51,2), ALINT(51,2)
      DIMENSION PJ(2), PW(2)
      DATA ATARG /14.514/
C      CALL PARAM_PRINT(LUN)
      WRITE (LUN, 10)
      WRITE (LUN, 15)
      WRITE (LUN, 16)
      WRITE (LUN, 18)
10    FORMAT(//,' Table of cross sections, and average number',
     +         ' of minijets and wounded nucleons ')
15    FORMAT('        [sqrt(s) in GeV, cross sections in mbarn]. ')
16    FORMAT(' sqrt(s)  sig(pp) sig(pAir)  <n_j>  <n_w>   ',
     +    ' sig(pip) sig(piAir) <n_j> <n_w> ')
18    FORMAT(1X,77('-') )
      DO J=1,51,1
         SQS = 10.**(ASQSMIN + DASQS*FLOAT(J-1))
         DO K=1,2
            PW(K) = ATARG*SSIG(J,K)/SSIGN(J,K)
            PJ(K) = 0.
            DO JJ=1,20
              PJ(K)=PJ(K)+FLOAT(JJ)*(PJETC(JJ,J,K)-PJETC(JJ-1,J,K))
            ENDDO
         ENDDO
         WRITE(LUN,20) SQS,SSIG(J,1),SSIGN(J,1),PJ(1),PW(1)
     +                      ,SSIG(J,2),SSIGN(J,2),PJ(2),PW(2)
      ENDDO
      WRITE (LUN, 18)
20    FORMAT (1X,E8.2, 2(2F8.1,2X,2F7.2,3X))
      RETURN
      END
      SUBROUTINE INT_H_NUC (IA, SIGT, SLOPE, RHO)
C...Compute with a montecarlo method the "multiple interaction structure"
C.  of an hadron-nucleus collision.
C.
C.
C.  INPUT : IA               = mass of target nucleus
C.          SIGT (mbarn)     = total hp cross section
C.          SLOPE (GeV**-2)  = slope of hp elastic scattering
C.          RHO              = real/imaginary part of forward elastic
C.                             scattering amplitude
C.
C.  OUTPUT : in COMMON block /CNCMS0/
C.           B = impact parameter (fm)
C.           BMAX = maximum impact parameter for generation
C.           NTRY = number of "trials" before one interaction
C.           NA = number of wounded nucleons in A
C. Author : P.Lipari  (may 1993)
C---------------------------------------------------------------------------
C D.H.PARAMETER (IAMAX=110)
      PARAMETER (IAMAX=56)
      COMMON /S_CNCM0/ B, BMAX, NTRY, NA
      DIMENSION XA(IAMAX), YA(IAMAX)
      DATA PI /3.1415926/
      DATA CMBARN /0.389385/
      CC = SIGT/(4.*PI*SLOPE*CMBARN)
      DEN = 2.*SLOPE*CMBARN*0.1
      BMAX = 10.                             ! fm
      NTRY = 0
      CALL NUC_CONF (IA, XA, YA)
1000  B = BMAX*SQRT(RAN(0))
      PHI = 2.*PI*RAN(0)
      BX = B*COS(PHI)
      BY = B*SIN(PHI)
      NTRY = NTRY+1
      NA = 0
      DO JA=1,IA
         S = (XA(JA)-BX)**2 + (YA(JA)-BY)**2
         F = EXP(-S/DEN)
         PEL = CC*CC*(1.+RHO*RHO)*F*F
         PINEL  = 2.*CC*F-PEL
         R = RAN(0)
         IF (R .LT. PINEL)  THEN
            NA = NA + 1
         ENDIF
      ENDDO
      IF (NA .EQ. 0)  GOTO 1000
      RETURN
      END

      SUBROUTINE INT_NUC (IA, IB, SIG0, SIGEL)
C========================================================================
C. Multiple interaction structure
C========================================================================
C...Compute with a montecarlo code  the  "multiple interaction structure"
C.  of a nucleus-nucleus interaction
C.
C.  INPUT : IA            = mass of target nucleus
C.          IB            = mass of projectile nucleus
C.          SIG0 (mbarn)  = inelastic pp cross section
C.          SIGEL(mbarn)  = elastic pp cross section
C.
C.  OUTPUT : in common block /CNUCMS/
C.           B = impact parameter (fm)
C.           BMAX = maximum impact parameter for generation
C.           NTRY = number of "trials" before one interaction
C.           NA = number of wounded nucleons in A
C.           NB =    "        "        "     in B
C.           NI = number of nucleon-nucleon inelastic interactions
C.           NAEL = number of elastically scattered nucleons in  A
C.           NBEL =    "         "           "          "    in  B
C.           JJA(J)  [J=1:IA]   = number of inelastic interactions
C.                                of J-th nucleon of nucleus A
C.           JJB(J)  [J=1:IB]   = number of inelastic interactions
C.                                of J-th nucleon of nucleus B
C.           JJAEL(J)  [J=1:IA]   = number of elastic interactions
C.                                of J-th nucleon of nucleus A
C.           JJBEL(J)  [J=1:IB]   = number of elastic interactions
C.                                of J-th nucleon of nucleus B
C.           JJINT(J,K)  [J=1:NB, K=1:NA]  (0 = no interaction)
C.                                         (1 = interaction )
C.                                         between nucleon J of A and K of B
C-----------------------------------------------------------------------------
      PARAMETER (IAMAX=56)
      COMMON /CNUCMS/ B, BMAX, NTRY, NA, NB, NI, NAEL, NBEL
     +         ,JJA(IAMAX), JJB(IAMAX), JJINT(IAMAX,IAMAX)
     +         ,JJAEL(IAMAX), JJBEL(IAMAX)
      DIMENSION XA(IAMAX), YA(IAMAX), XB(IAMAX), YB(IAMAX)
      DATA PI /3.1415926/
      SIGT = SIG0 + SIGEL
      R2  = 0.1 * SIG0/PI
      R2T = 0.1 * SIGT/PI
      BMAX = 15.                             ! fm
      NTRY = 0
      CALL NUC_CONF (IA, XA, YA)
      CALL NUC_CONF (IB, XB, YB)
      NI = 0
      NIEL = 0
      DO JA=1,IA
         JJA(JA) = 0
         JJAEL(JA) = 0
      ENDDO
      DO JB=1,IB
         JJB(JB) = 0
         JJBEL(JB) = 0
         DO JA=1,IA
            JJINT(JB,JA) = 0
         ENDDO
      ENDDO
1000      B = BMAX*SQRT(RAN(0))
      PHI = 2.*PI*RAN(0)
      BX = B*COS(PHI)
      BY = B*SIN(PHI)
      NTRY = NTRY+1
      DO JA=1,IA
         DO JB=1,IB
            S = (XA(JA)-XB(JB)-BX)**2 + (YA(JA)-YB(JB)-BY)**2
            IF (S .LT. R2)  THEN
               NI = NI + 1
               JJA(JA) = JJA(JA)+1
               JJB(JB) = JJB(JB)+1
               JJINT(JB,JA) = 1
            ELSE IF (S .LT. R2T)  THEN
               NIEL = NIEL + 1
               JJAEL(JA) = JJAEL(JA)+1
               JJBEL(JB) = JJBEL(JB)+1
            ENDIF
         ENDDO
      ENDDO
      IF (NI + NIEL .EQ. 0)  GOTO 1000
      NA = 0
      NB = 0
      NAEL = 0
      NBEL = 0
      DO JA=1,IA
         IF (JJA(JA) .GT. 0)  THEN
            NA = NA + 1
         ELSE
            IF (JJAEL(JA) .GT. 0)  NAEL = NAEL+1
         ENDIF
      ENDDO
      DO JB=1,IB
         IF (JJB(JB) .GT. 0)  THEN
            NB = NB + 1
         ELSE
            IF (JJBEL(JB) .GT. 0)  NBEL = NBEL+1
         ENDIF
      ENDDO
      RETURN
      END

      subroutine invert_array (yy, xmin, dx, n, xnew, ymin, dy)
C..    This subroutine receives one   array
C      of n y values in input yy(1:n)
C      that correspond to  equispaced values of x_j = xmin + dx*(j-1)
C
C      and "reverse" the array returning an array of  x values
C      xnew (1:n) that  corresponds to equispaced values of y
C      The relation is assumed monotonous but can be
C      increasing or decreasing
C..............................................................
      dimension  yy(n), xnew (n)
      ymin = yy(1)
      ymax = yy(n)
      dy = (ymax - ymin)/float(n-1)
      xnew (1) = xmin
      xnew (n) = xmin + dx*float(n-1)
      k0 = 1
      do j=2,n-1
         y = ymin + float(j-1)*dy
         do k=k0,n
            if((yy(k) .gt. y) .eqv. (yy(n) .gt. yy(1))) goto 100
         enddo
100      y2 = yy(k)
         y1 = yy(k-1)
         k0 = k-1
         x1 = xmin + dx*float(k-2)
         x2 = x1+dx
         xnew (j)  = x1 + dx* (y-y1)/(y2-y1)
      enddo
      return
      end
      SUBROUTINE JET_FRAG (X1J,X2J,PTJET)
C....Fragmentation of a jet-jet system
C.   Input : Kinematical variables of a
C.           jet-jet system
C............................................
      REAL*8 DX1J, DX2J, DBETJ
      COMMON /S_PLIST/ NP, P(5000,5), LLIST(5000)
      COMMON /S_RUN/ SQS, S, Q2MIN, XMIN, ZMIN , kb ,kt
      COMMON /S_CHIST/ NW,NJET,NNJET(15),XX1JET(100)
     +   ,XX2JET(100),PPTJET(100),PHIJET(100),NNPJET(100),NNPSTR(30)
     +   ,JDIF, EMXB, EMXT
      DATA PGG /1./
      E0 = SQRT(S*X1J*X2J)
      TH = ASIN(MIN(0.999999,2.*PTJET/E0))
      FI = 6.283185*RAN(0)
      NOLD = NP
      IF ( (E0.LT.8.) .OR. (RAN(0).GT.PGG)) THEN
         IS = -1 + 2.*INT(1.9999*RAN(0))
         IFL1 = IS*(INT((2.+0.3)*RAN(0))+1)
         CALL STRING_FRAG (E0,IFL1,-IFL1,0.,0.,0.,0.,IFBAD)
      ELSE
         CALL GG_FRAG(E0)
      ENDIF
      DX1J = X1J
      DX2J = X2J
      DBETJ = (DX1J-DX2J)/(DX1J+DX2J)
      CALL SIROBO (NOLD+1,NP,TH,FI,0.D0,0.D0,DBETJ)
      NNPJET (NJET) = NP-NOLD
      XX1JET (NJET) = X1J
      XX2JET (NJET) = X2J
      PPTJET (NJET) = PTJET
      PHIJET (NJET) = FI
      RETURN
      END
      SUBROUTINE JET_INI
C...Compute table of cross sections, and table of probability
C.  for the production of N (jet pairs)
C.  The OUTPUT of this routine  is the COMMON block /CCSIG/
C.  that contains  the cross sections h-p, h-Air, and the
C.  cumulative probability of n_jets.
C------------------------------------------------------------
      COMMON /S_CSIGINP/ QQ2MIN, SSQCD (51,2), SSQCD2 (51,2),SSIG0(2),
     @                 factork(2),isfchoice
      COMMON /S_CCSIG2/ SSIG_TOT(51,2),SSIG_B(51,2)
      COMMON /S_CCSIG/ NSQS, ASQSMIN, ASQSMAX, DASQS,
     +    SSIG(51,2), PJETC(0:20,51,2),SSIGN(51,2), ALINT(51,2)
      COMMON /S_RUN/ SQS, S, Q2MIN, XMIN, ZMIN , kb ,kt
      DIMENSION PJET (0:20)

      DATA CMBARN /0.389385/

      Q2MIN = QQ2MIN
      CALL FACT_INI            ! Initialise the factorial
      CALL HAD_CONV            ! Initialise profile convolution

C...spacing in energy  for the table of cross sections.
      NSQS = 51
      ASQSMIN = 1.
      ASQSMAX = 6.
      DASQS = (ASQSMAX-ASQSMIN)/FLOAT(NSQS-1)

      DO KK=1,2
         JINT = KK
         DO J=1, NSQS
            ASQS = ASQSMIN + DASQS*FLOAT(J-1)
          if (isfchoice.eq.1) then
                    SIG_QCD  =factork(1)* SSQCD(J,KK)
             else  if (isfchoice.eq.2) then
                    SIG_QCD  =factork(2)* SSQCD2(J,KK)
          endif
            SIG_SOFT = SSIG0(KK)
            CALL SIG_JET (SIG_QCD,SIG_SOFT,JINT,SIG_inel,PJET,
     +                    SIG_TOT,B_EL)
            SSIG(J,KK) = SIG_inel*CMBARN
c            SSIG_TOT(J,KK) = SIG_TOT*CMBARN
c            SSIG_B(J,KK) = B_EL
            PJETC (0,J,KK) = PJET(0)
            DO NJET=1,20
               PJETC(NJET,J,KK) = PJETC(NJET-1,J,KK) + PJET(NJET)
            ENDDO
         ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE KCODE (J,CODE,NC)
C...Produce the code for parton J
C.  Input K, Output CODE, NC=number of characters
C..................................................
      CHARACTER*5 CODE
      CHARACTER*1 NAMQ(3)
      DATA NAMQ /'U','D','S'/
      CODE = '     '
      IF(J.EQ.0)  THEN
         CODE(1:3) = 'GLU'
         NC = 3
         RETURN
      ENDIF
      JA = IABS(J)
      J1 = MOD(JA,10)
      J2 = (JA-J1)/10
      IF(JA .GT. 10) THEN
         CODE(1:1) = NAMQ(J2)
         CODE(2:2) = NAMQ(J1)
         NC = 2
      ELSE
         CODE(1:1) = NAMQ(J1)
         NC = 1
      ENDIF
      IF (J .LT. 0)  THEN
         CODE(NC+1:NC+3) = 'bar'
         NC = NC+3
      ENDIF
      RETURN
      END

      FUNCTION NJETR (K,SQS)
C...Generate a number of jet-pairs for a 'projectile'
C.  (K=1:p),(K=2:pi) interacting with a nucleon at sqrt(s)=SQS(GeV)
C..................................................................
      COMMON /S_CCSIG/ NSQS, ASQSMIN, ASQSMAX, DASQS,
     +    SSIG(51,2), PJETC(0:20,51,2),SSIGN(51,2), ALINT(51,2)
      AL = LOG10 (SQS)
      IF (AL .LT. ASQSMIN)  THEN
          NJETR = 0
          RETURN
      ENDIF
      IF (AL .GT. ASQSMAX)  THEN
          WRITE(*,*)  ' NJETR:  sqrt(s) out of bounds ', SQS
          NJETR = 0
          RETURN
      ENDIF
      J1 = (AL - ASQSMIN)/DASQS + 1
C D.H.
      J1 = MIN(J1,50)
      J1 = MAX(J1,1)

      J2 = J1+1
      T = (AL-ASQSMIN)/DASQS - FLOAT(J1-1)
      R = 0.999*RAN(0)
      DO J=0,20
         IF (R .LT. (1.-T)*PJETC(J,J1,K)+T*PJETC(J,J2,K))  GOTO 100
      ENDDO
100   NJETR = J
      RETURN
      END
      SUBROUTINE NUC1_PROFIL (AA)
C...Compute the profile function T(b)
C.  normalised as INT[d2b T(b) = 1]
C.  INPUT : AA = mass number of nucleus
C...............................................
      PARAMETER (NB=401)
      EXTERNAL DENSA
      COMMON /CC01/  B
      COMMON /CCDA/ JJA
      COMMON /CPROF/ DB, BMAX, BB(NB), TB(NB), A
      A = AA
      IA1 = INT(AA)
      IA2 = IA1 + 1
      U = AA - FLOAT(IA1)
      BMAX = 7.5
      DB = BMAX/FLOAT(NB-1)
      DO JB=1,NB
         B = DB*FLOAT(JB-1)
         BB(JB) = B
         IF (A .LE. 18.)  THEN
             T1 = PROFNUC (B, IA1)
             T2 = PROFNUC (B, IA2)
          ELSE
             JJA = IA1
             T1 = 2.*GAUSS (DENSA,0.,BMAX)
             JJA = IA2
             T2 = 2.*GAUSS (DENSA,0.,BMAX)
          ENDIF
          TB(JB) = (1.-U)*T1  + U*T2
      ENDDO
      RETURN
      END

       SUBROUTINE NUC_CONF (IA, XX, YY)
C...This routine generates the configuration  of a nucleus
C.  need an initialization call to NUC_GEOM_INI
C.
C.  INPUT  : IA = mass number of the nucleus
C.  OUTPUT : XX(1:IA), YY(1:IA) (fm) = position in impact parameter
C.                                     space of the IA nucleons
C...................................................................
      PARAMETER (IAMAX=56)
      DIMENSION XX(IAMAX), YY(IAMAX)
      PARAMETER (NB=401)
      COMMON /CPROFA/ ZMIN, DZ, BBZ(NB,IAMAX)
      DATA PI /3.1415926/
      DO J=1,IA
         Z = RAN(0)
         JZ = INT((Z-ZMIN)/DZ)+1
C D.H.
         JZ = MIN(JZ,400)
         
         T = (Z-ZMIN)/DZ - FLOAT(JZ-1)
         B = BBZ(JZ,IA)*(1.-T) + BBZ(JZ+1,IA)*T
         PHI = 2.*PI*RAN(0)
         XX(J) = B*COS(PHI)
         YY(J) = B*SIN(PHI)
      ENDDO
      RETURN
      END

      SUBROUTINE NUC_GEOM_INI
C...Initialize all nucleus profiles
      PARAMETER (NB=401)
      PARAMETER (IAMAX=56)
      COMMON /CPROF/ DB, BMAX, BB(NB), TB(NB), A
      COMMON /CPROFA/ ZMIN, DZ, BBZ(NB,IAMAX)
      DIMENSION FFB(NB), GGB(NB)
      DATA PI /3.1415926/
      CALL SHELL_INI
      CALL WOOD_SAXON_INI
      DO IA= 2,IAMAX
           JA = IA
         CALL NUC_PROFIL(JA)
         DO K=1,NB
           FFB(K) = BB(K)*TB(K) * (2.*PI)
         ENDDO
         GGB(1) = 0.
         GGB(NB) = 1.
         DO K=2,NB-1
           GGB(K) = GGB(K-1) + FFB(K-1)*DB
         ENDDO
         CALL INVERT_ARRAY(GGB,0.,DB,NB, BBZ(1,IA), ZMIN, DZ)
      ENDDO
      RETURN
      END

       SUBROUTINE NUC_NUC_INI
C...Initialization for the generation of nucleus-nucleus interactions
C.  INPUT : E0 (TeV) Energy per nucleon of the beam nucleus
C........................................................................
      CALL NUC_GEOM_INI                         ! nucleus profiles
      CALL SIGMA_INI                          ! initialize pp cross sections
      RETURN
      END
      SUBROUTINE NUC_PROFIL (JA)
C...Compute the profile function T(b)
C.  normalised as INT[d2b T(b) = 1]
C.  INPUT : JA = integer mass number of nucleus
C...............................................
      PARAMETER (NB=401)
      EXTERNAL DENSA
      COMMON /CC01/  B
      COMMON /CCDA/ JJA
      COMMON /CPROF/ DB, BMAX, BB(NB), TB(NB), A
      BMAX = 7.5
      DB = BMAX/FLOAT(NB-1)
      JJA = JA
      A = JA
      DO JB=1,NB
        B = DB*FLOAT(JB-1)
        BB(JB) = B
        IF (JA .LE. 18)  THEN
            TB(JB) = PROFNUC (B, JA)
         ELSE
            TB(JB) = 2.*GAUSS (DENSA,0.,BMAX)
         ENDIF
      ENDDO
      RETURN
      END

      FUNCTION OMEGAP (B, SIG, SLOPE, RHO)
      COMMON /CA0SH/ A0, A02
      DATA PI /3.1415926/
      ETA2 = 0.25*(A02 + SLOPE)
      F02 = SIG*SIG*(1.+RHO*RHO)/(16.*PI**2)
      ARG = -B*B/(4.*ETA2)
      OMEGAP=F02/(4.*ETA2*SLOPE)*(1.-A02/(6.*ETA2)*(1.+ARG))*EXP(ARG)
      RETURN
      END

      FUNCTION OMEGAS (B, SIG, SLOPE, RHO)
      COMMON /CA0SH/ A0, A02
      DATA PI /3.1415926/
      ETA2 = 0.25*(A02 + SLOPE)
      F02 = SIG*SIG*(1.+RHO*RHO)/(16.*PI**2)
      ARG = -B*B/(4.*ETA2)
      OMEGAS = F02/(4.*ETA2*SLOPE) *EXP(ARG)
      RETURN
      END
      BLOCK DATA PARAM_INI
C....This block data contains default values
C.   of the parameters used in fragmentation
C................................................
      COMMON /S_CZDIS/ FA, FB0
      COMMON /S_CZDISs/ FAs1, fAs2
      COMMON /S_CZLEAD/ CLEAD, FLEAD
      COMMON /S_CPSPL/ CCHIK(3,7:14)
      COMMON /S_CQDIS/ PPT0 (33),ptflag
      COMMON /S_CDIF0/ FFD, FBD, FDD
      COMMON /S_CFLAFR/ PAR(8)
      COMMON/S_cutof/stringmas0
      data stringmas0/.35/
C...Diffraction
c not used in this version.DATA FFD /0.09/, FBD /0.09/, FDD /0.04/
c      see function fdiffract
C...Longitudinal Fragmentation function
      DATA FA /0.5/, FB0 /0.8/
C...Longitudinal Fragmentation function for leading baryons
       DATA CLEAD  /0.6/, FLEAD  /0.6/
c      strange fragmentation
      data FAs1 /3./, fAs2 /3./
c      data FAs1 /0./, fAs2 /0./
C...pT of sea partons
      DATA PTFLAG /1./
      DATA PPT0 /0.30,0.30,0.450,30*0.60/
C...Splitting parameters
c      DATA CCHIK /18*2.,1.5,2.5,2.5,1.5,2.5,2.5/
      DATA CCHIK /18*2.,6*3./
C...Parameters of flavor formation
      DATA PAR /0.04,0.25,0.25,0.14,0.3,0.3,0.15,0./
      END
      SUBROUTINE PARAM_PRINT(LUN)
      COMMON /S_CZDIS/ FA, FB0
      COMMON /S_CZLEAD/ CLEAD, FLEAD
      COMMON /S_CPSPL/ CCHIK(3,7:14)
      COMMON /S_RUN/ SQS, S, Q2MIN, XMIN, ZMIN , kb ,kt
      COMMON /S_CQDIS/ PPT0 (33),ptflag
      COMMON /S_CDIF0/ FFD, FBD, FDD
      COMMON /S_CFLAFR/ PAR(8)

      WRITE (LUN, 25)
25      FORMAT( //,1x,40('-'), /
     +   ' SIBYLL MONTECARLO PROGRAM. Version 1.00',/,1x,40('-'),/
     +   ' List of parameters: ' )

      WRITE (LUN, 27) FFD, FBD, FDD
27      FORMAT(' Fraction of beam/target/double  diffraction = ',
     +         3F8.3)

      WRITE (LUN, 28) Q2MIN
28      FORMAT (' Q2min = ', F10.2, ' GeV**2 ')
      WRITE (LUN, 31) FA, FB0
31      FORMAT (' Parameters of longitudinal fragmentation: ', /,
     +          '  f(z) = (1-z)**a * exp(-b * mt**2/z) ', /,
     +          '  a = ', f9.3, 3x, ' b = ', f9.3, ' GeV**-2' )
      WRITE (LUN, 32) CLEAD, 1./FLEAD-1.
32      FORMAT (' Parameters of leading fragmentation: ', /,
     +   '  f(z) = c + (1-z)**a ', /,
     +   '  c = ',f9.3,3x,' a = ',f9.3)

      WRITE (LUN, 35) PPT0(1), PPT0(3), PPT0(11),ppt0(10)
35      FORMAT (' <pT> of sea partons ', /,
     +   2x,'<pT>(u/d) ',F8.3,2x,'<pT>(s) ',f8.3,2x,'<pT>(qq) ',f8.3,
     +     2x,'<pT>(val) ',f8.3)

      WRITE (LUN, 120) PAR
120      FORMAT (1x, 'Parameters of flavor formation: ',/,
     +   3x,'PAR(1) = Prob(qq)/Prob(q) =              ',F10.2,/,
     +   3x,'PAR(2) = Prob(s)/Prob(u)  =              ',F10.2,/,
     +   3x,'PAR(3) = Prob(us)/Prob(ud) =             ',F10.2,/,
     +   3x,'PAR(4) = Prob(ud_0)/Prob(ud_1) =         ',F10.2,/,
     +   3x,'PAR(5) = Prob(Vector)/Prob(Scalar) =     ',F10.2,/,
     +   3x,'PAR(6) = Prob(K*)/Prob(K) =              ',F10.2,/,
     +   3x,'PAR(7) = Prob(spin 3/2)/Prob(spin=1/2) = ',F10.2,/,
     +   3x,'PAR(8) = Prob(B-M-Bbar)/Prob(B-Bbar) =   ',F10.2)

      WRITE (LUN, 40)
      WRITE (LUN, 41) CCHIK (1,13), CCHIK(2,13)
40      FORMAT(' Parameters of hadron splitting ' )
41      FORMAT('   p -> [(ud) u] splitting: alpha = ', F10.3, /,
     +         '   p -> [(uu) d] splitting: alpha = ', F10.3 )

      RETURN
      END
      FUNCTION PARTON(X,L)
C...This function returns the structure function
C.   f(x) = x * [ g(x) + 4/9 *(q(x) + qbar(x)) ]
C.  for a proton. In COMMON /S_CSTR/ JSTR controls
C.  the choice of structure function
C................................................
      COMMON /S_CSTR/ JSTR, JSTRPI
C O. Palamara 27/8/1993
c      parameter beta=1.925978
      parameter (beta=1.925978)
      IF (L .EQ. 2)  GOTO 1000

C...Eichten et al.  (set 1)
100      uv = 1.78 * x**0.5 * (1.-x**1.51)**3.5
      dv = 0.67 * x**0.4 * (1.-x**1.51)**4.5
      us = 0.182 * (1.-x)**8.54
      ss = 0.081 * (1.-x)**8.54
      qq0 = uv + dv + 4.*us + 2.*ss
      glu0 = (2.62 + 9.17*x)* (1.-x)**5.90
      parton = glu0 + 4./9.*qq0
      return


1000      continue   ! goto (1100,1200),  jstrpi

C...Owens set 1   from STRF from Wisc. Pheno. group. for q2=q2_min
      AV=.4
      BV=.7
c      BETA=GGAMMA(AV)*GGAMMA(BV+1.)/GGAMMA(AV+BV+1.)  =1.925978
      uv=X**(AV)*(1.-X)**BV/BETA
      dv=uv
c
      A=.9
      BET=5.
      us=(A*(1.-X)**BET)/6.
c
      A=.888
      BET=3.11
      GA1=6.0
      glu0=A*(1.-X)**BET*(1.+GA1*X)
c   Bug Fix thanks to Sue Kashahara- correct factor in front of
c   sea quarks for Owens S.F.  5-94
      qq0 = uv + dv + 6.*us
      parton = (glu0 + 4./9.*qq0)
      return
      end

      FUNCTION PART_INT (ZMIN,L)
C...This function returns as output the integral of
C.  the parton structure function:
C.     f(x) = g(x) + 4/9 *(q(x) + qbar(x))
C.  from xmin = exp(zmin) to 1
C.  for a proton (L=1) or a pi (L=2)
C.  needs to be initialised with: CALL ZSAMPLE_INI
C.....................................................
      COMMON /S_CZGEN/ XA,XB,XMAX,ZA,ZB,ZMAX,DX,DZ,NX,NZ,APART(2),
     +   FFA(2),FFB(2),
     +   DFX(2),DFZ(2),XX(200,2),ZZ(200,2),FFX(200,2),FFZ(200,2)

C D.H.IF (ZMIN .LT. ZA)  THEN
      IF (ZMIN .LE. ZA)  THEN
         PART_INT = FFA(L) + APART(L)*(ZA-ZMIN)
C D.H.ELSE IF (ZMIN .LT. ZB) THEN
      ELSE IF (ZMIN .LE. ZB) THEN
         JZ = (ZB-ZMIN)/DZ+1
         Z0 = ZB-DZ*FLOAT(JZ-1)
         T = (Z0-ZMIN)/DZ
         PART_INT = FFZ(JZ,L)*(1.-T) + FFZ(JZ+1,L)*T
      ELSE
         X = EXP(ZMIN)
         JX = (XMAX-X)/DX+1
         X0 = XMAX-DX*FLOAT(JX-1)
         T = (X0-X)/DX
         PART_INT = FFX(JX,L)*(1.-T) + FFX(JX+1,L)*T
      ENDIF
      RETURN
      END
      SUBROUTINE PJET_PRINT (L,SQS, LUN)
C...Write the jet probability:
C.  L = particle code, SQS = sqrt(s), LUN = unit of output
C..................................................................
      COMMON /S_CCSIG/ NSQS, ASQSMIN, ASQSMAX, DASQS,
     +    SSIG(51,2), PJETC(0:20,51,2),SSIGN(51,2), ALINT(51,2)
      DIMENSION LL(7:14)
      DATA LL /6*2,2*1/
      AL = LOG10 (SQS)
      IF (AL .LT. ASQSMIN .OR. AL .GT. ASQSMAX)  RETURN

      K = LL(IABS(L))
      J1 = (AL - ASQSMIN)/DASQS + 1
      J2 = J1+1
      T = (AL-ASQSMIN)/DASQS - FLOAT(J1-1)
      R = 0.999*RAN(0)
      J = 0
      P = (1.-T)*PJETC(0,J1,K)+T*PJETC(0,J2,K)
      WRITE (LUN, 20)
      WRITE (LUN, 25) J, P
      AMED = 0.
      ASUM = P
      DO J=0,19
         PA = (1.-T)*PJETC(J,J1,K)+T*PJETC(J,J2,K)
         PB = (1.-T)*PJETC(J+1,J1,K)+T*PJETC(J+1,J2,K)
         P = PB-PA
         IF (P .GT. 1.E-04)  WRITE (LUN, 25) J+1, P
         AMED = AMED + FLOAT(J+1)*P
         ASUM = ASUM + P
      ENDDO
      WRITE (LUN, 26) AMED, ASUM
      RETURN
20    FORMAT (/,'  Minijet expected frequencies: ')
25    FORMAT ('    n(jet-pairs)   = ',i3,  F12.4)
26    FORMAT ('    <n(jet-pairs)> = ',F10.2, '   norm = ', F9.3)
      END
      FUNCTION PROFNUC (B, JA)
C...This function return
C.  the profile T(b) for a nucleus of mass number A
C.  INPUT B = impact parameter (GeV**-1)
C.        JA = integer mass number
C.  OUTPUT  (fm**-2)
C.
C.  The  density of the nucleus is the `shell model density'
C.  the parameter r0 must beinitialized in the common block
C.............................................................
      COMMON /CSHELL/ RR0(18), RR02(18)
      DATA PI /3.1415926/
      B2 = B*B
      ARG = B2/RR02(JA)
      TS = EXP(-ARG)
      TP = TS*(2.*B2+RR02(JA))/(3.*RR02(JA))
      CS = MIN(1.,4./FLOAT(JA))
      PROFNUC = (CS*TS + (1.-CS)*TP)/(PI*RR02(JA))
      RETURN
      END

      SUBROUTINE PTDIS (IFL,PX,PY)
C...Generate pT
      COMMON /S_CQDIS/ PPT0(33),ptflag
      PT = PPT0(IABS(IFL))*SQRT(-ALOG(MAX(1E-10,RAN(0))))
      PHI= 6.2831853*RAN(0)
      PX=PT*COS(PHI)
      PY=PT*SIN(PHI)
      RETURN
      END
      FUNCTION QMASS(IFL)
C...Return quark or diquark constituent masses
      DIMENSION QMAS(3)
      DATA QMAS /0.325,0.325,0.5/
      IFLA = IABS(IFL)
      IF (IFLA .LE. 3)       THEN
         QMASS = QMAS(IFLA)
      ELSE
         QMA = QMAS(IFLA/10)
         QMB = QMAS(MOD(IFLA,10))
         QMASS = QMA+QMB
      ENDIF
      RETURN
      END
       SUBROUTINE QNUM (JQ,JS,JB,JBA, NC, NF)
C...Return the quantum numbers of one event
C.  JQ = charge, JB = baryon number, JS = strangeness
C.  JBA = (number of baryons+antibaryons)
C.  NC  = number of charged particles
C.  NF  = number of final particles
C..................................................
      COMMON /S_PLIST/ NP, P(5000,5), LLIST(5000)
      COMMON /S_CHP/ ICHP(49), ISTR(49), IBAR(49)
      JQ = 0
      JB = 0
      JS = 0
      JBA= 0
      NC = 0
      NF = 0
      DO J=1,NP
          L = LLIST(J)
          LL = IABS(L)
          IF (LL .LT. 10000)  THEN
              IF(ICHP(LL) .NE. 0) NC = NC + 1
              NF = NF + 1
              JQ = JQ + ICHP(LL)*ISIGN(1,L)
              JB = JB + IBAR(LL)*ISIGN(1,L)
              JBA= JBA+ IBAR(LL)
              JS = JS + ISTR(LL)*ISIGN(1,L)
          ENDIF
      ENDDO
      RETURN
      END
      FUNCTION QUAD_INT (R,X0,X1,X2,V0,V1,V2)
c  quadratic interpolation?
      R0=R-X0
      R1=R-X1
      R2=R-X2
      S0=X0-X1
      S1=X0-X2
      S2=X1-X2
      QUAD_INT = V0*R1*R2/(S0*S1)-V1*R0*R2/(S0*S2)+V2*R0*R1/(S1*S2)
      RETURN
      END

      function rdis(idummy)
      dimension probr(20)
      data probr/
     *      0.10000, 0.15748, 0.21778, 0.28605, 0.36060,
     *      0.43815, 0.51892, 0.60631, 0.70002, 0.79325,
     *      0.88863, 0.98686, 1.10129, 1.21202, 1.32932,
     *      1.44890, 1.57048, 1.70139, 1.83417, 2.00000/
      nr = 20.*RAN(0) + 1
      if (nr .eq. 1) then
      f1 = 0.
      else
      f1 = probr(nr-1)
      endif
      dr = probr(nr) - f1
      rdis = f1 + dr*RAN(0)
      return
      end
      SUBROUTINE SAMPLE (L, X1,X2,PT)
C...Routine for the sampling the kinematical variables
C.  that determine a  jet-jet  system (x1,x2, pT)
C.  from the differential cross section:
C.     d3sigma/(dx1 dx2 dpT)
C.  This version assumes the `single parton approximation'
C.  INPUT:  L=1 incident proton, L=2  incident pi
C.  OUTPUT:  X1, X2, PT (GeV)
C.................................................................
      COMMON /S_RUN/ SQS, S, Q2MIN, XMIN, ZMIN , kb ,kt
100   Z1=ZSAMPLE (ZMIN,L)
      Z2=ZSAMPLE (ZMIN,1)
      SIG=1.-XMIN*EXP(-Z1-Z2)
      IF (SIG .LT. RAN(0))  GOTO 100
      X1=EXP(Z1)
      X2=EXP(Z2)
      Q2=Q2MIN/(1.-RAN(0)*SIG)
      PT=SQRT(Q2*(1.-Q2/(S*X1*X2)))
      RETURN
      END
      FUNCTION SHELL (R,JA)
C...Density in the shell model
      COMMON /CSHELL/ RR0(18), RR02(18)
      DATA PI /3.1415926/
      R0 = RR0(JA)
      C1 = MIN(1.,4./FLOAT(JA))
      CS = 1./(R0**3*PI**(1.5))
      CP = 2.*CS/3.
      FS = EXP(-(R/R0)**2)
      FP = (R/R0)**2 * FS
      SHELL = C1*CS*FS + (1.-C1)*CP*FP
      RETURN
      END

      SUBROUTINE SHELL_INI
C...Initialize the parameter  of the shell model
C.  for the nuclei with    6 < A < 18
C..............................................
      COMMON /CSHELL/ RR0(18), RR02(18)
      DIMENSION RR(18)
C...Data on Sqrt[<r**2>]  in fermi
      DATA RR /0.81,2.095,1.88,1.674, -1.,2.56,2.41,-1.,2.519,2.45
     +          ,2.37, 2.460, 2.440, 2.54, 2.58, 2.718, 2.662,2.789 /
      DO JA=1,18
         A = FLOAT(JA)
         RMED = RR(JA)
         IF (RMED .LE. 0.)   RMED = 0.5*(RR(JA-1) + RR(JA+1))
         C = MAX(1.5,(5./2. - 4./A) )
         R0 = RMED/SQRT(C)
         RR0 (JA) = R0
         RR02(JA) = R0*R0
      ENDDO
      RETURN
      END
      SUBROUTINE SIBLIST(LUN)
C...This routine prints the event record for the
C.  current event on unit LUN
C.................................................
      COMMON /S_PLIST/ NP, P(5000,5), LLIST(5000)
      COMMON /S_PLIST1/ LLIST1(5000)
      COMMON /S_RUN/ SQS, S, Q2MIN, XMIN, ZMIN , kb ,kt
      COMMON /S_CHIST/ NW,NJET,NNJET(15),XX1JET(100)
     +   ,XX2JET(100),PPTJET(100),PHIJET(100),NNPJET(100),NNPSTR(30)
     +   , JDIF, EMXB, EMXT
      COMMON /S_CCSTR/ X1(30),X2(30),IFLB(30),IFLT(30),PXB(30),
     +    PYB(30),PXT(30),PYT(30)
      COMMON /S_CNAM/ NAMP (0:49)
      CHARACTER*6 NAMP
      CHARACTER CODE*18, COD1*5, COD2*5
      CHARACTER*18 NAMDIF(3)
      DATA NAMDIF /'Beam diffraction  ','Target diffraction',
     +               'Double diffraction'/
*     WRITE (LUN,*)
      WRITE (LUN,*) ' Event record '
      WRITE (LUN,100)
      IF(JDIF .GT. 0)  THEN
         WRITE (LUN,*) '  ',NAMDIF(JDIF)
         GOTO 10
      ENDIF
      NA = -(NJET+2*NW+1)
      N  = 0
      DO J=1,NJET
         NA = NA+1
         DO K=1,NNPJET(J)
            N=N+1
            LLIST1(N) = NA
         ENDDO
      ENDDO
      DO J=1,2*NW
         NA = NA+1
         DO K=1,NNPSTR(J)
            N=N+1
            LLIST1(N) = NA
         ENDDO
      ENDDO
C...Jet-Jet strings
      K = 0
      JA = -(NJET+2*NW+1)
      DO J=1,NJET
         JA = JA+1
         CODE = 'Jet-Jet           '
         PX = PPTJET(J)
         PY = 0.
         PZ = SQS*(XX1JET(J)-XX2JET(J))
         EE = SQS*(XX1JET(J)+XX2JET(J))
         WRITE (LUN,120) JA, CODE, K, PX,PY,PZ,EE
      ENDDO
C...Beam strings
      DO J=1,2*NW
         JA = JA+1
         CALL KCODE(IFLT(J),COD1,NC1)
         CALL KCODE(IFLB(J),COD2,NC2)
         CODE(1:7)  = 'String '
         CODE(7+1:18) = '              '
         CODE(7+1:7+6)=COD1
         CODE(NC1+1+7:NC1+1+7) = '-'
         CODE(NC1+2+7:NC1+6+7) = COD2
         PX = PXB(J)+PXT(J)
         PY = PYB(J)+PYT(J)
         PZ = SQS*(X1(J)-X2(J))
         EE = SQS*(X1(J)+X2(J))
         WRITE (LUN,120) JA, CODE, K, PX,PY,PZ,EE
      ENDDO
C...Print particle list
10      DO J=1,NP
          L = MOD(LLIST(J),10000)
          CODE = '                  '
          CODE(1:6) = NAMP(IABS(L))
          IF (L .LT. 0) CODE(7:9) = 'bar'
          IF(IABS(LLIST(J)) .GT. 10000)   CODE(10:10) = '*'
          WRITE (LUN,120) J, CODE, LLIST1(J), (P(J,K),K=1,4)
      ENDDO
      CALL ESUM(1,NP,EE,PX,PY,PZ,NF)
      WRITE(LUN,140) PX,PY,PZ,EE
100      FORMAT(3X,'N  Particle',12X,'Ori',6x,'PX',9x,'PY',9x,'PZ'
     +         ,9x,'E', /, 3X,70('-'))
120      FORMAT(1X,I4,1X,A18,1X,I4,2X,2(F9.3,2X),2(E9.3,2X))
140      FORMAT(1X,'Tot = ',24X,2(F9.3,2X),G9.3,2X,E9.3)
      RETURN
      END
      SUBROUTINE  SIBNUC (IAB, IAT, SQS)
C...Routine that generates the interaction of a nucleus of
C.  mass number IAB with a  target nucleus  of mass IAT
C.  (IAT=0 : air).
C.  SQS (GeV) is the  center of mass energy of each
C.  nucleon - nucleon cross section
C---------------------------------------------------------------
      COMMON /S_PLIST/ NP, P(5000,5), LLIST(5000)
      COMMON /S_PLNUC/ NPA, PA(5000,5), LLA(5000)
      COMMON /S_MASS1/ AM(49), AM2(49)
      COMMON /CKFRAG/ KODFRAG
      PARAMETER (IAMAX=56)
      COMMON /CNUCMS/ B, BMAX, NTRY, NA, NB, NI, NAEL, NBEL
     +         ,JJA(IAMAX), JJB(IAMAX), JJINT(IAMAX,IAMAX)
     +         ,JJAEL(IAMAX), JJBEL(IAMAX)            
      COMMON /FRAGMENTS/ PPP(3,60)
      DIMENSION  IAF(60)
      DATA RPOX /0.3624/

C...Target mass
      IF (IAT .EQ. 0) THEN
         IATARGET = 14 + 2*INT((1.+RPOX)*RAN(0))
      ELSE
          IATARGET = IAT
      ENDIF
       
C...Single nucleon (proton) case
      IF (IAB .EQ. 1)  THEN
         NPA = 0
         CALL SIBYLL (13,IATARGET, SQS)
         CALL DECSIB
         DO J=1,NP
            LA = IABS(LLIST(J))
            IF (LA .LT. 10000)  THEN
               NPA = NPA + 1
               LLA(NPA) = LLIST(J)
               DO K=1,5
                  PA(NPA,K) = P(J,K)                                                                               (J,K)
               ENDDO
            ENDIF
         ENDDO
         RETURN
      ENDIF


C...Nuclei
      E0 = (SQS*SQS-2.*AM2(13))/(2.*AM(13))
      E0 = E0*1.E-03         ! TeV
      CALL SIGMA_PP (E0, SIGT, SIGEL, SIG0, SLOPE, RHO)
      CALL INT_NUC (IATARGET, IAB, SIG0, SIGEL) 
C...fragment the spectator nucleons
      NBT = NB + NBEL
      IF (KODFRAG .EQ. 1)  THEN
          CALL FRAGM1(IAB,NBT, NF, IAF)
      ELSE IF(KODFRAG .EQ. 2)  THEN
          CALL FRAGM2(IAB,NBT, NF, IAF)
      ELSE 
          CALL FRAGM (IATARGET, IAB, NBT,B, NF, IAF)
      ENDIF
     
C...Spectator fragments
      NPA = 0
      DO J=1,NF
         NPA = NPA+1
         LLA(NPA) = 1000+IAF(J)
         PA(NPA,1) = 0.
         PA(NPA,2) = 0.
         PA(NPA,3) = SQS/2.
         PA(NPA,4) = SQS/2.
         PA(NPA,5) = FLOAT(IAF(J))*0.5*(AM(13)+AM(14))
      ENDDO
C...Elastically scattered   fragments
      DO J=1,NBEL
         NPA = NPA+1
         LLA(NPA) = 1001
         PA(NPA,1) = 0.
         PA(NPA,2) = 0.
         PA(NPA,3) = SQS/2.
         PA(NPA,4) = SQS/2.
         PA(NPA,5) = 0.5*(AM(13)+AM(14))
      ENDDO
C...Superimpose NB  nucleon interactions
      DO JJ=1,NB
          CALL SIBYLL (13,IATARGET, SQS)
          CALL DECSIB
          DO J=1,NP
             LA = IABS(LLIST(J))
             IF (LA .LT. 10000)   THEN
                NPA = NPA + 1
                LLA(NPA) = LLIST(J)
                DO K=1,5
                    PA(NPA,K) = P(J,K)
                ENDDO
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE SIBYLL (KB, IATARG, SQS)
C...Main routine for the production of hadronic events,
C.  Generates an inelastic hadronic interaction of
C.  a `projectile particle' of code KB with a
C.  target nucleus of mass number A = IATARG (integer)
C.  IATARG = 0 is an "air" nucleus  (superposition of oxygen and nitrogen)
C.  with c.m. energy for the hadron-nucleon system SQS (GeV)
C.
C.  Allowed values of KB: 7,8,9,10,11,12,13,14,-13,-14
C.                        pi+-,K+-,KL,KS,p,n,pbar,nbar
C.
C.  The output is contained in COMMON /S_PLIST/ that contains:
C.
C.     NP           number of final particles
C.     P(1:NP, 1:5) 4-momenta + masses of the final particles
C.     LLIST (1:NP) codes of final particles.
C.  the reaction is studied in the c.m. of  hadron-nucleon system
C.
C.  The COMMON block /S_CHIST/ contains information about the
C.  the structure of the  generated event:
C.    NW   = number of wounded nucleons
C.    NJET = number of jet pairs
C.    NNJET (1:NW) = number of minijets produced in each interaction
C.    XX1JET (1:NJET) = x1  for each jet-pair
C.    XX2JET (1:NJET) = x2   "   "     "
C.    PPTJET (1:NJET) = pT   "   "     "
C.    NNPJET (1:NJET) = total number of particles in each jet pair
C.    NNPSTR (1:2*NW) = number of particles in each `beam string'
C.    JDIF  = diffraction code
C----------------------------------------------------------------------
      COMMON /S_PLIST/ NP, P(5000,5), LLIST(5000)
      COMMON /S_RUN/ SQSA, S, Q2MIN, XMIN, ZMIN , kbc ,kt
      COMMON /S_CHIST/ NW,NJET,NNJET(15),XX1JET(100),XX2JET(100)
     +   ,PPTJET(100),PHIJET(100),NNPJET(100),NNPSTR(30)
     +   ,JDIF, EMXB, EMXT
      COMMON /S_CLDIF/ LDIFF
      COMMON /S_CCSTR/ X1(30),X2(30),IFLB(30),IFLT(30),PXB(30),
     +    PYB(30),PXT(30),PYT(30)
      COMMON /S_CQDIS/ PPT0 (33),ptflag
      DIMENSION QMAS(33),X2JET(30),LL(7:14),BET(30),GAM(30),EE(30)
      DATA QMAS
     .  /2*0.35,0.6,7*0.,2*1.1,1.25,7*0.,1.25,1.1,1.25,7*0,2*1.25,1.5/
      DATA LL /6*2,2*1/
      DATA  FOX /0.257/
      COMMON/S_cutof/stringmas0

      kbc=kb
      SQSA = SQS
      S = SQS*SQS
      XMIN = 2.*Q2MIN/S
      ZMIN = LOG(XMIN)

C...`soft increase of pT'
C Setting ptflag = 0 will result in
C underestimating the P_t at high energies.
      if (ptflag.gt.0.0) then
            ptu=.3+.08*log10(sqs/30.)
            pts=.45+.08*log10(sqs/30.)
            ptqq=.6+.08*log10(sqs/30.)
            PPT0 (1) = PTU
            PPT0 (2) = PTU
            PPT0 (3) = PTS
            PPT0 (10) = PTQQ
            DO J=11,33
                PPT0(J) = PTQQ
            ENDDO
      endif

      NP = 0
      NJET = 0
      IATARGET = IATARG
C
C      Generate an 'air' interaction by choosing Nitrogen or Oxygen
C
      IF (IATARGET .EQ. 0) THEN
          R = RAN(0)
          IATARGET = 14
          IF (R .LT. FOX)  IATARGET = 16
      ENDIF
      IATARG = IATARGET
      L = LL(IABS(KB))
C
C     Generate number ow wounded nucleons, and diffraction code.
C
1000  CALL SIB_START_EV (SQS,L,IATARGET, NW,JDIF)
      IF (LDIFF .NE. 0)  THEN
         IF((LDIFF.EQ.-1) .AND. (JDIF.NE.0) ) GOTO 1000
         IF((LDIFF.EQ. 1) .AND. ((JDIF.NE.0).AND.(JDIF.NE.3)))
     +     GOTO 1000
         IF((LDIFF.EQ. 5) .AND. (JDIF.EQ.2)) GOTO 1000
         IF((LDIFF.GE. 2) .AND. (LDIFF.LE.4)) THEN
             NW = 1
             JDIF = LDIFF-1
         ENDIF
      ENDIF
C...Diffractive interactions
      IF (JDIF .NE. 0)  THEN
        CALL DIFF_GEN (KB, JDIF)
        RETURN
      ENDIF

C...Non-diffractive interactions
2000  L = LL(IABS(KB))
C...Production of minijets
3000  NP = 0
      NJET = 0
      X1JET = 0.
      DO JW=1,NW
         NNJET (JW) = NJETR (L,SQS)
         X2JET(JW) = 0.
         DO JJ=1,NNJET(JW)
           NJET=NJET+1
           NOLD=NP
           CALL SAMPLE (L,X1J,X2J,PTJET)
           CALL JET_FRAG (X1J,X2J,PTJET)
           X1JET = X1JET + X1J
           X2JET(JW) = X2JET(JW)+X2J
         ENDDO
         IF (X2JET (JW) .GT. 0.7)   GOTO 3000
      ENDDO
      IF (X1JET .GT. 0.7)           GOTO 3000
C
C ...Prepare 2*NW color strings.
C
      CALL BEAM_SPLIT (KB, NW, X1, IFLB, X1JET, LXBAD,stringmas0)
C     IF (LXBAD .EQ. 1)  GOTO 2000 Bug fix 2-4-94 PL/RSF
      IF (LXBAD .EQ. 1)  GOTO 1000
      DO J=1,NW
         J1=2*(J-1)+1
         J2=J1+1
         KT=13
         IF (IATARGET .GT. 1)  KT = 13+INT(2.*RAN(0))
         CALL HSPLI (KT,IFLT(J2),IFLT(J1))
         XMINA = 2.*stringmas0/(SQS*(1.-X2JET(J)))
C        XMINA = 2.*0.20/(SQS*(1.-X2JET(J)))  ! change RSF. 5-92
         CHI=CHIDIS (KT,IFLT(J2),IFLT(J1))
         XVAL=1.-X2JET(J)
         IF (XVAL.LT.XMINA) GOTO 3000
         X2(J2) = MAX(CHI*XVAL,XMINA)
         X2(J2) = MIN(X2(J2),XVAL-XMINA)
         X2(J1) = XVAL-X2(J2)
      ENDDO
C...Generates primordial pT for the partons
      DO J=1,NW
         J1 = 2*(J-1)+1
         J2 = J1+1
         CALL PTDIS (10,PXT(J1),PYT(J1))
         if (j.eq.1) then
            CALL PTDIS (10,PXB(J2),PYB(J2))
         else
            CALL PTDIS (IFLB(J2),PXB(J2),PYB(J2))
         endif
         PXB(J1) = -PXB(J2)
         PYB(J1) = -PYB(J2)
         PXT(J2) = -PXT(J1)
         PYT(J2) = -PYT(J1)
      ENDDO
C...Check consistency of kinematics
      DO J=1,2*NW
         EE(J) = SQS*SQRT(X1(J)*X2(J))
         XM1 = SQRT(PXB(J)**2+PYB(J)**2+QMAS(IABS(IFLB(J)))**2)
         XM2 = SQRT(PXT(J)**2+PYT(J)**2+QMAS(IABS(IFLT(J)))**2)
         IF (EE(J) .LT. XM1+XM2+0.3)  GOTO 2000
      ENDDO
C...Fragment the 2*NW color strings
      DO J=1,2*NW
         EE (J) = SQS*SQRT(X1(J)*X2(J))
         BET(J) = (X1(J)-X2(J))/(X1(J)+X2(J))
         GAM(J) = (X1(J)+X2(J))/(2.*SQRT(X1(J)*X2(J)))
         NOLD=NP
         CALL STRING_FRAG
     +     (EE(J),IFLB(J),IFLT(J),PXB(J),PYB(J),PXT(J),PYT(J),IFBAD)
         IF (IFBAD .EQ. 1)   GOTO 2000
         DO K=NOLD+1,NP
           PZ = P(K,3)
           P(K,3) = GAM(J)*(PZ+BET(J)*P(K,4))
           P(K,4) = GAM(J)*(P(K,4)+BET(J)*PZ)
         ENDDO
         NNPSTR(J) = NP-NOLD
      ENDDO

C...Check energy conservation
      CALL ESUM(1,NP,ETOT,PXT,PYT,PZT,NF)
      IF (ABS(ETOT/(0.5*SQS*FLOAT(NW+1)) - 1.) .GT. 1.E-03)  THEN
         WRITE(*,*) ' Energy non conserved. L, SQS : ',L,SQS
         WRITE(*,*) ' sqs = ', SQS, ' E_f = ', ETOT
         WRITE(*,*) ' diff/N_w/N_j = ', JDIF, NW, NJET
      ENDIF
      RETURN
      END

      SUBROUTINE SIBYLL_INI
C   Initialization routine for the   the routine
C.  SYBILL   for simulation of hadronic interactions
C.
C.  the routine fills the COMMON block /CCSIG/ that contains
C.  important information for the generation of events
C.
C*      COMMON /S_CCSIG/ NSQS, ASQSMIN, ASQSMAX, DASQS,
C*     +    SSIG(51,2), PJETC(0:20,51,2),SSIGN(51,2), ALINT(51,2)
C.
C.  NSQS = number of energy points  (51 is current version)
C.  ASQSMIN = log_10 [sqrt(s) GeV]   minimum value
C.  ASQSMIN = log_10 [sqrt(s) GeV]   maximum value
C.  DASQS   = step  in log_10[sqrt(s)]
C.            DASQS = (ASQSMAX - ASQSMIN)/(NSQS-1)
C.
C.  SSIG(J,1) inelastic cross section for pp interaction
C.            at energy: sqrt(s)(GeV) = 10**[ASQSMIN+DASQS*(J-1)]
C.  SSIG(J,2)  inelastic cross section for pi-p interaction
C.  SSIGN(J,1) inelastic cross section for p-Air interaction
C.  SSIGN(J,2) inelastic cross section for pi-Air interaction
C.
C.  PJETC(n_j,J,1) Cumulative  probability distribution
C.                 for the production of n_j (n_j=0:20) jet pairs
C.                 at sqrt(s) labeled by J, for p-p interaction
C.  PJETC(n_j,J,2) Same as above for pi-p interaction
C.  ALINT(J,1)   proton-air  interaction length (g cm-2)
C.  ALINT(J,2)   pi-air  interaction length (g cm-2)
C------------------------------------------------------------------
      WRITE(*,*) ' Initialization of the SIBYLL event  generator '
      WRITE(*,100)
 100  FORMAT(' ','====================================================',
     *     /,' ','|                                                  |',
     *     /,' ','|                 S I B Y L L  1.6                 |',
     *     /,' ','|                                                  |',
     *     /,' ','|         HADRONIC INTERACTION MONTE CARLO         |',
     *     /,' ','|                        BY                        |',
     *     /,' ','|           R.S. FLETCHER, T.K. GAISSER            |',
     *     /,' ','|               P. LIPARI, T. STANEV               |',
     *     /,' ','|                                                  |',
     *     /,' ','| LAST MODIFICATIONS: Apr  15, 1997     by D. Heck |',
     *     /,' ','====================================================',
     *     /)
      CALL JET_INI
      CALL ZSAMPLE_INI
      CALL BLOCK_INI
      CALL NUC_GEOM_INI
      CALL SIG_AIR_INI
      RETURN
      END
      SUBROUTINE sib_SIGMA_HAIR (SQS,L0,SIGINEL)
C
C      Sibyll P-air cross section
C
C...pi,p air  cross sections
C. INPUT: SQS = c.m.s. energy (GeV)
C.
C. OUTPUT:
C.         SIGINEL = inelastic cross section
C---------------------------------------------------------------------------
      COMMON /S_CCSIG/ NSQS, ASQSMIN, ASQSMAX, DASQS,
     +    SSIG(51,2), PJETC(0:20,51,2),SSIGN(51,2), ALINT(51,2)
      AL = LOG10(SQS)
      J1 = (AL - 1.)*10. + 1
C D.H.
      J1 = MAX(J1,1)
      J1 = MIN(J1,50)

      T = (AL-1.)*10. - FLOAT(J1-1)
      SIGINEL = SSIGN(J1,L0)*(1.-T) + SSIGN(J1+1,L0)*T
      RETURN
      END
      SUBROUTINE sib_SIGMA_PIP (SQS, SIGT, SIGEL, SIGINEL, SLOPE, RHO)
C...pip cross sections
C. INPUT: SQS = c.m.s. energy (GeV)
C.
C. OUTPUT: SIGT = total cross section
C.         SIGEL = elastic cross section
C.         SIGINEL = inelastic cross section
C.         SLOPE = slope of elastic scattering (GeV**-2)
C.         RHO = Imaginary/Real part of forward elastic scattering amplitude
C---------------------------------------------------------------------------
      COMMON /S_CCSIG/ NSQS, ASQSMIN, ASQSMAX, DASQS,
     +    SSIG(51,2), PJETC(0:20,51,2),SSIGN(51,2), ALINT(51,2)
      DATA PI /3.1415926/
      DATA CMBARN /0.389385/
      AL = LOG10(SQS)
      J1 = (AL - 1.)*10. + 1
C D.H.
      J1 = MAX(J1,1)
      J1 = MIN(J1,50)

      T = (AL-1.)*10. - FLOAT(J1-1)
      SIGINEL = SSIG(J1,2)*(1.-T) + SSIG(J1+1,2)*T
      CALL BLOCK(SQS,SIGT1,SIGT2,SLOP1,SLOP2,RHO1,RHO2,SIGEL1,SIGEL2)
      R = SIGEL1/SIGT1
      RHO = RHO1
      SIGT  = SIGINEL/(1.-R)
      SIGEL = SIGINEL*R/(1.-R)
      SLOPE = SIGT**2/(SIGEL * 16.*PI) * (1.+RHO1**2) /CMBARN
      RETURN
      END
      SUBROUTINE sib_SIGMA_PP (SQS, SIGT, SIGEL, SIGINEL, SLOPE, RHO)
C...pp cross sections
C. INPUT: SQS = c.m.s. energy (GeV)
C.
C. OUTPUT: SIGT = total cross section
C.         SIGEL = elastic cross section
C.         SIGINEL = inelastic cross section
C.         SLOPE = slope of elastic scattering (GeV**-2)
C.         RHO = Imaginary/Real part of forward elastic scattering amplitude
C---------------------------------------------------------------------------
      COMMON /S_CCSIG/ NSQS, ASQSMIN, ASQSMAX, DASQS,
     +    SSIG(51,2), PJETC(0:20,51,2),SSIGN(51,2), ALINT(51,2)
      COMMON /S_CCSIG2/ SSIG_TOT(51,2),SSIG_B(51,2)
      DATA PI /3.1415926/
      DATA CMBARN /0.389385/
      common/s_icr/icross_fit
      ICROSS_FIT = 1
      AL = LOG10(SQS)
      J1 = (AL - 1.)*10. + 1
C D.H.
      J1 = MAX(J1,1)
      J1 = MIN(J1,50)

      T = (AL-1.)*10. - FLOAT(J1-1)
      SIGINEL = SSIG(J1,1)*(1.-T) + SSIG(J1+1,1)*T
      IF (ICROSS_FIT.EQ.1) THEN
              CALL BLOCK(SQS,SIGT1,SIGT2,SLOP1,SLOP2,RHO1,RHO2,
     +               SIGEL1,SIGEL2)
              R = SIGEL1/SIGT1
              RHO = RHO1
              SIGT  = SIGINEL/(1.-R)
              SIGEL = SIGINEL*R/(1.-R)
              SLOPE = SIGT**2/(SIGEL * 16.*PI) * (1.+RHO1**2) /CMBARN
      ELSE
              SIGT = SSIG_TOT(J1,1)*(1.-T) + SSIG_TOT(J1+1,1)*T
              SLOPE  =  SSIG_B(J1,1) *(1.-T) + SSIG_B(J1+1,1)  *T
              RHO=0.0
              SIGEL=SIGT-SIGINEL
      ENDIF
      RETURN
      END
      SUBROUTINE SIB_START_EV (SQS,L,IA, NW,JDIF)
C=======================================================================
C. Code for the wounded nucleon distribution
C=======================================================================
C..Beginning of a SIBYLL  interaction
C. INPUT : SQS = c.m.s. energy (GeV)
C.         L = 1:proton, 2:charged pion
C.         IA = mass of target nucleon
C.
C. OUTPUT: NW    = number of wounded nucleons
C.         JDIF  = diffraction code
C.                 (0 : non diffractive)
C.                 (1 : forward diffraction)
C.                 (2 : backward diffraction)
C.                 (0 : double  diffraction)
C. Author : P.Lipari  (may 1993)
C------------------------------------------------
      COMMON /S_CNCM0/ B, BMAX, NTRY, NA
      CALL SIGMA_HP (L, SQS, SIGT, SIGEL, SIGINEL, SLOPE, RHO)
      IF (IA .GT. 1)  THEN
         CALL INT_H_NUC (IA, SIGT, SLOPE, RHO)
      ELSE
         NA = 1
      ENDIF
C...diffraction
      PF = FDIFFRACT(SQS,L)/SIGINEL
      PB = BDIFFRACT(SQS,L)/SIGINEL
      PD = DDIFFRACT(SQS,L)/SIGINEL
      P0 = 1.-PF-PB-PD
      P1 = P0 + PF
      P2 = P1 + PB
      NW = 0
      JF = 0
      JB = 0
      JD = 0
      DO K=1, NA
         R = RAN(0)
         IF (R .LT. P0)  THEN
            NW = NW + 1
         ELSE IF (R .LT. P1)  THEN
            JF = 1
         ELSE IF (R .LT. P2)  THEN
            JB = 1
         ELSE
            JD = 1
         ENDIF
      ENDDO
      JDIF = 0
      IF (NW .EQ. 0)  THEN
         NW = 1
         JDIF = 3
         IF((JF.EQ.1) .AND. (JB.EQ.0) .AND. (JD.EQ.0)) JDIF=1
         IF((JF.EQ.0) .AND. (JB.EQ.1) .AND. (JD.EQ.0)) JDIF=2
      ENDIF
      RETURN
      END
      SUBROUTINE SIGMA_AIR (IB,SIG0,SIGEL,NINT,
     +                            SIGMA,DSIGMA,SIGQE,DSIGQE)
C==========================================================================
C. Cross sections
C==========================================================================
C...Compute with a montecarlo method the "production"
C.  and "quasi-elastic" cross section for
C.  a nucleus-air  interaction
C.
C.  INPUT : IB            = mass of projectile nucleus
C.          SIG0 (mbarn)  = inelastic pp cross section
C.          NINT            = number  of interactions to generate
C.  OUTPUT : SIGMA (mbarn) = "production" cross section
C.           DSIGMA   "    = error
C.           SIGQE    "    = "quasi-elastic" cross section
C.           DSIGQE   "    = error
C.           additional output is in the common block  /CPROBAB/
C..........................................................................
      PARAMETER (IAMAX=56)
      PARAMETER (IAMAX2=3136)          ! IAMAX*IAMAX
      COMMON  /CPROBAB/ PROBA(IAMAX), DPROBA(IAMAX),
     +   PROBB(IAMAX), DPROBB(IAMAX), PROBI(IAMAX2), DPROBI(IAMAX2),
     +   P1AEL(0:IAMAX),DP1AEL(0:IAMAX),P1BEL(0:IAMAX), DP1BEL(0:IAMAX),
     +   P2AEL(0:IAMAX),DP2AEL(0:IAMAX),P2BEL(0:IAMAX), DP2BEL(0:IAMAX)
      COMMON /CNUCMS/ B, BMAX, NTRY, NA, NB, NI, NAEL, NBEL
     +         ,JJA(IAMAX), JJB(IAMAX), JJINT(IAMAX,IAMAX)
     +         ,JJAEL(IAMAX), JJBEL(IAMAX)
      DIMENSION  MMA(0:IAMAX), MMB(0:IAMAX), MMI(0:IAMAX2)
      DIMENSION  M1AEL(0:IAMAX), M1BEL(0:IAMAX)
      DIMENSION  M2AEL(0:IAMAX), M2BEL(0:IAMAX)
      DATA WOX /0.346/
      DATA PI /3.1415926/
      R2 = 0.1 * SIG0/PI
      BMAX = 15.                             ! fm
      SIGMA0 = PI*BMAX*BMAX*10.              ! mbarn
      IA = 16
      DO J=1,IA
         MMA(J) = 0
         M1AEL(J) = 0
         M2AEL(J) = 0
      ENDDO
      DO J=1,IB
         MMB(J) = 0
         M1BEL(J) = 0
         M2BEL(J) = 0
      ENDDO
      DO J=1,IA*IB
         MMI(J) = 0
      ENDDO
      NN = 0
      M = 0
      DO KK=1,NINT
         IA = 14 + 2*INT((1.+WOX)*RAN(0))
         CALL INT_NUC (IA, IB, SIG0, SIGEL)
         NN = NN + NTRY
         MMI(NI) = MMI(NI) + 1
         MMA(NA) = MMA(NA)+1
         MMB(NB) = MMB(NB)+1
         IF (NI .GT. 0)  THEN
            M = M+1
            M1AEL(NAEL) = M1AEL(NAEL)+1
            M1BEL(NBEL) = M1BEL(NBEL)+1
         ELSE
            M2AEL(NAEL) = M2AEL(NAEL)+1
            M2BEL(NBEL) = M2BEL(NBEL)+1
         ENDIF
      ENDDO
      MQE = NINT - M
      SIGMA  = SIGMA0 * FLOAT(M)/FLOAT(NN)
      DSIGMA = SIGMA0 * SQRT(FLOAT(M))/FLOAT(NN)
      SIGQE  = SIGMA0 * FLOAT(MQE)/FLOAT(NN)
      DSIGQE = SIGMA0 * SQRT(FLOAT(MQE))/FLOAT(NN)
      DO J=1,IA
         PROBA(J) = FLOAT(MMA(J))/FLOAT(M)
         DPROBA(J) = SQRT(FLOAT(MMA(J)))/FLOAT(M)
      ENDDO
      DO J=1,IB
         PROBB(J) = FLOAT(MMB(J))/FLOAT(M)
         DPROBB(J) = SQRT(FLOAT(MMB(J)))/FLOAT(M)
      ENDDO
      DO J=1,IA*IB
         PROBI(J) = FLOAT(MMI(J))/FLOAT(M)
         DPROBI(J) = SQRT(FLOAT(MMI(J)))/FLOAT(M)
      ENDDO
      DO J=0,IA
         P1AEL(J) = FLOAT(M1AEL(J))/FLOAT(M)
         DP1AEL(J) = SQRT(FLOAT(M1AEL(J)))/FLOAT(M)
         P2AEL(J) = FLOAT(M2AEL(J))/FLOAT(MQE)
         DP2AEL(J) = SQRT(FLOAT(M2AEL(J)))/FLOAT(MQE)
      ENDDO
      DO J=0,IB
         P1BEL(J) = FLOAT(M1BEL(J))/FLOAT(M)
         DP1BEL(J) = SQRT(FLOAT(M1BEL(J)))/FLOAT(M)
         P2BEL(J) = FLOAT(M2BEL(J))/FLOAT(MQE)
         DP2BEL(J) = SQRT(FLOAT(M2BEL(J)))/FLOAT(MQE)
      ENDDO
      RETURN
      END
      SUBROUTINE SIGMA_HP (L, SQS, SIGT, SIGEL, SIGINEL, SLOPE, RHO)
C--------------------------------------------------------------------------
C. Hadron-proton cross sections
C--------------------------------------------------------------------------
      IF(L .EQ. 2) THEN
         CALL sib_SIGMA_PIP(SQS,SIGT,SIGEL,SIGINEL,SLOPE,RHO)
      ELSE
         CALL sib_SIGMA_PP(SQS,SIGT,SIGEL,SIGINEL,SLOPE,RHO)
      ENDIF
      RETURN
      END

      SUBROUTINE SIGMA_INI
C...Initialize the cross section and interaction lengths  on air
        COMMON /CSAIR/ NSQS, ASQSMIN, ASQSMAX, DASQS,
     +           SSIG0(41,2),SSIGA(41,2),ALINT(41,2)
      DATA AVOG /6.0221367E-04/
      CALL BLOCK_INI
      ATARGET = 14.514
C...Loop on c.m. energy
      NSQS = 41
      SQSMIN = 10.
      SQSMAX = 1.E+05
      ASQSMIN = LOG10(SQSMIN)
      ASQSMAX = LOG10(SQSMAX)
      DASQS = (ASQSMAX-ASQSMIN)/FLOAT(NSQS-1)
      DO J=1,NSQS
         ASQS = ASQSMIN + DASQS*FLOAT(J-1)
         SQS = 10.**ASQS
         E0 = SQS*SQS/(2.*0.938) * 1.E-03
         CALL SIGMA_PP (E0, SIGT, SIGEL, SIGINEL, SLOPE, RHO)
         CALL SIG_H_AIR (SIGT, SLOPE, RHO, SSIGT, SSIGEL, SSIGQE)
         SSIGA(J,1) = SSIGT-SSIGQE
         SSIG0(J,1) = SIGINEL
         ALINT(J,1) = 1./(AVOG*SSIGA(J,1)/ATARGET)
         CALL SIGMA_PIP (E0, SIGT, SIGEL, SIGINEL, SLOPE, RHO)
         CALL  SIG_H_AIR (SIGT, SLOPE, RHO, SSIGT, SSIGEL, SSIGQE)
         SSIGA(J,2) = SSIGT-SSIGQE
         SSIG0(J,2) = SIGINEL
         ALINT(J,2) = 1./(AVOG*SSIGA(J,2)/ATARGET)
      ENDDO
      RETURN
      END

      SUBROUTINE SIGMA_MC (IA,IB,SIG0,SIGEL,NINT,
     +                            SIGMA,DSIGMA,SIGQE,DSIGQE)
C...Compute with a montecarlo method the "production"
C.  and "quasi-elastic" cross section for
C.  a nucleus-nucleus interaction
C.
C.  INPUT : IA            = mass of target nucleus
C.          IB            = mass of projectile nucleus
C.          SIG0 (mbarn)  = inelastic pp cross section
C.          NINT            = number  of interactions to generate
C.  OUTPUT : SIGMA (mbarn) = "production" cross section
C.           DSIGMA   "    = error
C.           SIGQE    "    = "quasi-elastic" cross section
C.           DSIGQE   "    = error
C.           additional output is in the common block  /CPROBAB/
C.           Prob(n_A), Prob(n_B), Prob(n_int)
C..........................................................................
      PARAMETER (IAMAX=56)
      PARAMETER (IAMAX2=3136)          ! IAMAX*IAMAX
      COMMON  /CPROBAB/ PROBA(IAMAX), DPROBA(IAMAX),
     +   PROBB(IAMAX), DPROBB(IAMAX), PROBI(IAMAX2), DPROBI(IAMAX2),
     +   P1AEL(0:IAMAX),DP1AEL(0:IAMAX),P1BEL(0:IAMAX), DP1BEL(0:IAMAX),
     +   P2AEL(0:IAMAX),DP2AEL(0:IAMAX),P2BEL(0:IAMAX), DP2BEL(0:IAMAX)
      COMMON /CNUCMS/ B, BMAX, NTRY, NA, NB, NI, NAEL, NBEL
     +         ,JJA(IAMAX), JJB(IAMAX), JJINT(IAMAX,IAMAX)
     +         ,JJAEL(IAMAX), JJBEL(IAMAX)
      DIMENSION  MMA(0:IAMAX), MMB(0:IAMAX), MMI(0:IAMAX2)
      DIMENSION  M1AEL(0:IAMAX), M1BEL(0:IAMAX)
      DIMENSION  M2AEL(0:IAMAX), M2BEL(0:IAMAX)
      DATA PI /3.1415926/
      R2 = 0.1 * SIG0/PI
      BMAX = 15.                             ! fm
      SIGMA0 = PI*BMAX*BMAX*10.              ! mbarn
      DO J=1,IA
         MMA(J) = 0
         M1AEL(J) = 0
         M2AEL(J) = 0
      ENDDO
      DO J=1,IB
         MMB(J) = 0
         M1BEL(J) = 0
         M2BEL(J) = 0
      ENDDO
      DO J=1,IA*IB
         MMI(J) = 0
      ENDDO
      NN = 0
      M = 0
      DO KK=1,NINT
         CALL INT_NUC (IA, IB, SIG0, SIGEL)
         NN = NN + NTRY
         MMI(NI) = MMI(NI) + 1
         MMA(NA) = MMA(NA)+1
         MMB(NB) = MMB(NB)+1
         IF (NI .GT. 0)  THEN
            M = M+1
            M1AEL(NAEL) = M1AEL(NAEL)+1
            M1BEL(NBEL) = M1BEL(NBEL)+1
         ELSE
            M2AEL(NAEL) = M2AEL(NAEL)+1
            M2BEL(NBEL) = M2BEL(NBEL)+1
         ENDIF
      ENDDO
      MQE = NINT - M
      SIGMA  = SIGMA0 * FLOAT(M)/FLOAT(NN)
      DSIGMA = SIGMA0 * SQRT(FLOAT(M))/FLOAT(NN)
      SIGQE  = SIGMA0 * FLOAT(MQE)/FLOAT(NN)
      DSIGQE = SIGMA0 * SQRT(FLOAT(MQE))/FLOAT(NN)
      DO J=1,IA
         PROBA(J) = FLOAT(MMA(J))/FLOAT(M)
         DPROBA(J) = SQRT(FLOAT(MMA(J)))/FLOAT(M)
      ENDDO
      DO J=1,IB
         PROBB(J) = FLOAT(MMB(J))/FLOAT(M)
         DPROBB(J) = SQRT(FLOAT(MMB(J)))/FLOAT(M)
      ENDDO
      DO J=1,IA*IB
         PROBI(J) = FLOAT(MMI(J))/FLOAT(M)
         DPROBI(J) = SQRT(FLOAT(MMI(J)))/FLOAT(M)
      ENDDO
      DO J=0,IA
         P1AEL(J) = FLOAT(M1AEL(J))/FLOAT(M)
         DP1AEL(J) = SQRT(FLOAT(M1AEL(J)))/FLOAT(M)
         P2AEL(J) = FLOAT(M2AEL(J))/FLOAT(MQE)
         DP2AEL(J) = SQRT(FLOAT(M2AEL(J)))/FLOAT(MQE)
      ENDDO
      DO J=0,IB
         P1BEL(J) = FLOAT(M1BEL(J))/FLOAT(M)
         DP1BEL(J) = SQRT(FLOAT(M1BEL(J)))/FLOAT(M)
         P2BEL(J) = FLOAT(M2BEL(J))/FLOAT(MQE)
         DP2BEL(J) = SQRT(FLOAT(M2BEL(J)))/FLOAT(MQE)
      ENDDO
      RETURN
      END

      SUBROUTINE SIGMA_PIP (E0, SIGT, SIGEL, SIGINEL, SLOPE, RHO)
C=============================================================
C.  Cross sections
C=============================================================
C...pp cross sections
C. INPUT: E0 = Laboratory Energy  (TeV)
C.
C. OUTPUT: SIGT = total cross section
C.         SIGEL = elastic cross section
C.         SIGINEL = inelastic cross section
C.         SLOPE = slope of elastic scattering (GeV**-2)
C.         RHO = Imaginary/Real part of forward elastic scattering amplitude
C...........................................................................
      DIMENSION SSIG0(41)
      DATA PI /3.1415926/
      DATA CMBARN /0.389385/
C...pi-p inelastic cross sections (mbarn)
      DATA (SSIG0(J),J=1,41) /
     +     20.28,    20.36,    20.48,    20.66,    20.91,    21.22,
     +     21.62,    22.09,    22.64,    23.27,    23.99,    24.79,
     +     25.66,    26.62,    27.65,    28.76,    29.94,    31.21,
     +     32.55,    33.97,    35.47,    37.04,    38.70,    40.46,
     +     42.29,    44.23,    46.26,    48.40,    50.64,    53.01,
     +     55.48,    58.12,    60.87,    63.75,    66.78,    69.98,
     +     73.38,    76.91,    80.62,    84.56,    88.68 /
      SQS = SQRT(2000.*0.938*E0)
      AL = LOG10(SQS)
      J1 = (AL - 1.)*10. + 1
C D.H.
      J1 = MAX(J1,1)
      J1 = MIN(J1,40)

      T = (AL-1.)*10. - FLOAT(J1-1)
      SIGINEL = SSIG0(J1)*(1.-T) + SSIG0(J1+1)*T
      CALL BLOCK(SQS,SIGT1,SIGT2,SLOP1,SLOP2,RHO1,RHO2,SIGEL1,SIGEL2)
      R = SIGEL1/SIGT1
      RHO = RHO1
      SIGT  = SIGINEL/(1.-R)
      SIGEL = SIGINEL*R/(1.-R)
      SLOPE = SIGT**2/(SIGEL * 16.*PI) * (1.+RHO1**2) /CMBARN
      RETURN
      END

      SUBROUTINE SIGMA_PP (E0, SIGT, SIGEL, SIGINEL, SLOPE, RHO)
C...pp cross sections
C. INPUT: E0 = Laboratory Energy  (TeV)
C.
C. OUTPUT: SIGT = total cross section
C.         SIGEL = elastic cross section
C.         SIGINEL = inelastic cross section
C.         SLOPE = slope of elastic scattering (GeV**-2)
C.         RHO = Imaginary/Real part of forward elastic scattering amplitude
C...........................................................................
      DIMENSION SSIG0(41)
      DATA PI /3.1415926/
      DATA CMBARN /0.389385/
C...p-p inelastic cross sections (mbarn)
      DATA (SSIG0(J),J=1,41) /
     +     32.08,    32.15,    32.26,    32.45,    32.73,    33.12,
     +     33.63,    34.28,    35.08,    36.01,    37.09,    38.31,
     +     39.67,    41.15,    42.75,    44.47,    46.29,    48.22,
     +     50.24,    52.35,    54.55,    56.81,    59.15,    61.57,
     +     64.04,    66.57,    69.17,    71.81,    74.51,    77.27,
     +     80.06,    82.93,    85.82,    88.74,    91.71,    94.76,
     +     97.86,   100.97,   104.12,   107.31,   110.54 /
      SQS = SQRT(2000.*0.938*E0)
      AL = LOG10(SQS)
      J1 = (AL - 1.)*10. + 1
C D.H.
      J1 = MAX(J1,1)
      J1 = MIN(J1,40)

      T = (AL-1.)*10. - FLOAT(J1-1)
      SIGINEL = SSIG0(J1)*(1.-T) + SSIG0(J1+1)*T
      CALL BLOCK(SQS,SIGT1,SIGT2,SLOP1,SLOP2,RHO1,RHO2,SIGEL1,SIGEL2)
      R = SIGEL1/SIGT1
      RHO = RHO1
      SIGT  = SIGINEL/(1.-R)
      SIGEL = SIGINEL*R/(1.-R)
      SLOPE = SIGT**2/(SIGEL * 16.*PI) * (1.+RHO1**2) /CMBARN
      RETURN
      END

      SUBROUTINE SIGNUC_INI (IA,E0)
C=============================================================
C.  Nucleus-nucleus cross sections
C=============================================================
C...This subroutine receives in INPUT E0 (TeV)
C.  energy per nucleon and computes the cross sections
C.  and interactions lengths for  all nuclei
C.  with A  between 2 and IA
C.  The output is contained in common block /CLENNN/
C........................................................
      COMMON /CLENNN/ SSIGNUC(60), ALNUC(60)
      DIMENSION SIGMA(5,56), SIGQE(5,56)
      DIMENSION AA(5)
      DATA NE /5/, AMIN /1./, DA /1./
      DATA AA /1.,2.,3.,4.,5./
      DATA AVOG /6.0221367E-04/
      DATA ATARGET /14.514/               ! effective masss of air
C...Data on `inelastic-production' nucleus-air cross section
      DATA (SIGMA(J, 2),J=1,5) / 396., 427., 497., 603., 702./
      DATA (SIGMA(J, 3),J=1,5) / 464., 490., 570., 680., 794./
      DATA (SIGMA(J, 4),J=1,5) / 497., 524., 600., 711., 813./
      DATA (SIGMA(J, 5),J=1,5) / 593., 629., 708., 826., 934./
      DATA (SIGMA(J, 6),J=1,5) / 701., 739., 839., 954.,1066./
      DATA (SIGMA(J, 7),J=1,5) / 708., 755., 845., 985.,1102./
      DATA (SIGMA(J, 8),J=1,5) / 777., 790., 909.,1015.,1130./
      DATA (SIGMA(J, 9),J=1,5) / 808., 845., 952.,1051.,1186./
      DATA (SIGMA(J,10),J=1,5) / 809., 862., 958.,1077.,1193./
      DATA (SIGMA(J,11),J=1,5) / 821., 870., 955.,1085.,1191./
      DATA (SIGMA(J,12),J=1,5) / 861., 897., 985.,1132.,1251./
      DATA (SIGMA(J,13),J=1,5) / 875., 909., 989.,1129.,1272./
      DATA (SIGMA(J,14),J=1,5) / 919., 952.,1043.,1202.,1315./
      DATA (SIGMA(J,15),J=1,5) / 954., 969.,1085.,1217.,1370./
      DATA (SIGMA(J,16),J=1,5) /1014.,1041.,1148.,1308.,1430./
      DATA (SIGMA(J,17),J=1,5) /1005.,1028.,1139.,1277.,1434./
      DATA (SIGMA(J,18),J=1,5) /1065.,1088.,1178.,1324.,1494./
      DATA (SIGMA(J,19),J=1,5) /1113.,1122.,1238.,1397.,1532./
      DATA (SIGMA(J,20),J=1,5) /1143.,1169.,1321.,1471.,1615./
      DATA (SIGMA(J,21),J=1,5) /1167.,1194.,1315.,1488.,1650./
      DATA (SIGMA(J,22),J=1,5) /1183.,1195.,1318.,1454.,1638./
      DATA (SIGMA(J,23),J=1,5) /1206.,1264.,1394.,1524.,1653./
      DATA (SIGMA(J,24),J=1,5) /1244.,1297.,1400.,1557.,1672./
      DATA (SIGMA(J,25),J=1,5) /1272.,1298.,1449.,1600.,1712./
      DATA (SIGMA(J,26),J=1,5) /1269.,1332.,1459.,1603.,1743./
      DATA (SIGMA(J,27),J=1,5) /1262.,1312.,1443.,1598.,1723./
      DATA (SIGMA(J,28),J=1,5) /1309.,1333.,1469.,1619.,1763./
      DATA (SIGMA(J,29),J=1,5) /1433.,1505.,1621.,1802.,1935./
      DATA (SIGMA(J,30),J=1,5) /1346.,1391.,1536.,1678.,1844./
      DATA (SIGMA(J,31),J=1,5) /1376.,1432.,1556.,1696.,1878./
      DATA (SIGMA(J,32),J=1,5) /1392.,1418.,1582.,1713.,1857./
      DATA (SIGMA(J,33),J=1,5) /1412.,1438.,1602.,1742.,1944./
      DATA (SIGMA(J,34),J=1,5) /1414.,1471.,1633.,1774.,1928./
      DATA (SIGMA(J,35),J=1,5) /1444.,1498.,1634.,1773.,1944./
      DATA (SIGMA(J,36),J=1,5) /1455.,1507.,1638.,1815.,1943./
      DATA (SIGMA(J,37),J=1,5) /1458.,1526.,1660.,1781.,1992./
      DATA (SIGMA(J,38),J=1,5) /1520.,1515.,1671.,1838.,2052./
      DATA (SIGMA(J,39),J=1,5) /1492.,1545.,1707.,1863.,2049./
      DATA (SIGMA(J,40),J=1,5) /1511.,1577.,1719.,1878.,2032./
      DATA (SIGMA(J,41),J=1,5) /1541.,1581.,1729.,1878.,2072./
      DATA (SIGMA(J,42),J=1,5) /1540.,1591.,1718.,1919.,2075./
      DATA (SIGMA(J,43),J=1,5) /1590.,1610.,1783.,1925.,2077./
      DATA (SIGMA(J,44),J=1,5) /1582.,1614.,1785.,1961.,2106./
      DATA (SIGMA(J,45),J=1,5) /1580.,1629.,1774.,1939.,2115./
      DATA (SIGMA(J,46),J=1,5) /1628.,1673.,1812.,1981.,2089./
      DATA (SIGMA(J,47),J=1,5) /1594.,1672.,1820.,1999.,2163./
      DATA (SIGMA(J,48),J=1,5) /1639.,1695.,1824.,1986.,2150./
      DATA (SIGMA(J,49),J=1,5) /1647.,1728.,1856.,2008.,2188./
      DATA (SIGMA(J,50),J=1,5) /1637.,1714.,1902.,2040.,2210./
      DATA (SIGMA(J,51),J=1,5) /1682.,1741.,1918.,2042.,2258./
      DATA (SIGMA(J,52),J=1,5) /1673.,1746.,1933.,2067.,2238./
      DATA (SIGMA(J,53),J=1,5) /1705.,1763.,1880.,2102.,2249./
      DATA (SIGMA(J,54),J=1,5) /1699.,1748.,1917.,2142.,2265./
      DATA (SIGMA(J,55),J=1,5) /1751.,1764.,1934.,2129.,2289./
      DATA (SIGMA(J,56),J=1,5) /1753.,1828.,1973.,2187.,2335./
C...Data on `quasi-elastic' nucleus-air cross section
      DATA (SIGQE(J, 2),J=1,5) /  41.,  42.,  75., 138., 236./
      DATA (SIGQE(J, 3),J=1,5) /  41.,  41.,  79., 138., 244./
      DATA (SIGQE(J, 4),J=1,5) /  39.,  41.,  78., 145., 246./
      DATA (SIGQE(J, 5),J=1,5) /  44.,  47.,  83., 152., 256./
      DATA (SIGQE(J, 6),J=1,5) /  48.,  53.,  96., 169., 288./
      DATA (SIGQE(J, 7),J=1,5) /  53.,  52.,  95., 171., 282./
      DATA (SIGQE(J, 8),J=1,5) /  53.,  52.,  95., 179., 295./
      DATA (SIGQE(J, 9),J=1,5) /  51.,  49.,  94., 180., 301./
      DATA (SIGQE(J,10),J=1,5) /  52.,  52.,  93., 183., 308./
      DATA (SIGQE(J,11),J=1,5) /  52.,  51.,  91., 179., 301./
      DATA (SIGQE(J,12),J=1,5) /  53.,  53.,  92., 179., 284./
      DATA (SIGQE(J,13),J=1,5) /  55.,  55.,  97., 184., 308./
      DATA (SIGQE(J,14),J=1,5) /  54.,  55., 102., 182., 310./
      DATA (SIGQE(J,15),J=1,5) /  57.,  53., 102., 193., 305./
      DATA (SIGQE(J,16),J=1,5) /  56.,  53., 105., 195., 331./
      DATA (SIGQE(J,17),J=1,5) /  57.,  54., 101., 192., 327./
      DATA (SIGQE(J,18),J=1,5) /  53.,  54., 107., 204., 328./
      DATA (SIGQE(J,19),J=1,5) /  59.,  63., 105., 209., 345./
      DATA (SIGQE(J,20),J=1,5) /  59.,  64., 109., 195., 343./
      DATA (SIGQE(J,21),J=1,5) /  55.,  64., 114., 212., 348./
      DATA (SIGQE(J,22),J=1,5) /  60.,  63., 117., 206., 340./
      DATA (SIGQE(J,23),J=1,5) /  63.,  68., 119., 202., 345./
      DATA (SIGQE(J,24),J=1,5) /  59.,  61., 114., 213., 354./
      DATA (SIGQE(J,25),J=1,5) /  60.,  65., 121., 216., 357./
      DATA (SIGQE(J,26),J=1,5) /  61.,  66., 124., 232., 342./
      DATA (SIGQE(J,27),J=1,5) /  63.,  61., 119., 222., 365./
      DATA (SIGQE(J,28),J=1,5) /  63.,  68., 121., 218., 354./
      DATA (SIGQE(J,29),J=1,5) /  67.,  77., 119., 239., 371./
      DATA (SIGQE(J,30),J=1,5) /  63.,  63., 120., 230., 379./
      DATA (SIGQE(J,31),J=1,5) /  67.,  66., 124., 223., 371./
      DATA (SIGQE(J,32),J=1,5) /  62.,  68., 125., 230., 357./
      DATA (SIGQE(J,33),J=1,5) /  65.,  70., 128., 227., 377./
      DATA (SIGQE(J,34),J=1,5) /  63.,  70., 120., 222., 359./
      DATA (SIGQE(J,35),J=1,5) /  66.,  71., 124., 233., 358./
      DATA (SIGQE(J,36),J=1,5) /  70.,  70., 118., 228., 376./
      DATA (SIGQE(J,37),J=1,5) /  69.,  73., 131., 209., 381./
      DATA (SIGQE(J,38),J=1,5) /  68.,  73., 128., 221., 369./
      DATA (SIGQE(J,39),J=1,5) /  68.,  72., 129., 224., 377./
      DATA (SIGQE(J,40),J=1,5) /  72.,  73., 123., 232., 384./
      DATA (SIGQE(J,41),J=1,5) /  67.,  73., 131., 240., 384./
      DATA (SIGQE(J,42),J=1,5) /  71.,  72., 131., 236., 392./
      DATA (SIGQE(J,43),J=1,5) /  69.,  76., 137., 249., 395./
      DATA (SIGQE(J,44),J=1,5) /  71.,  73., 136., 235., 385./
      DATA (SIGQE(J,45),J=1,5) /  71.,  67., 127., 236., 401./
      DATA (SIGQE(J,46),J=1,5) /  76.,  68., 133., 241., 399./
      DATA (SIGQE(J,47),J=1,5) /  73.,  69., 133., 227., 382./
      DATA (SIGQE(J,48),J=1,5) /  67.,  81., 131., 247., 402./
      DATA (SIGQE(J,49),J=1,5) /  69.,  78., 133., 247., 400./
      DATA (SIGQE(J,50),J=1,5) /  73.,  76., 131., 239., 401./
      DATA (SIGQE(J,51),J=1,5) /  73.,  75., 131., 246., 394./
      DATA (SIGQE(J,52),J=1,5) /  72.,  77., 135., 242., 408./
      DATA (SIGQE(J,53),J=1,5) /  75.,  76., 136., 236., 394./
      DATA (SIGQE(J,54),J=1,5) /  76.,  78., 137., 228., 398./
      DATA (SIGQE(J,55),J=1,5) /  75.,  82., 133., 238., 390./
      DATA (SIGQE(J,56),J=1,5) /  76.,  71., 136., 255., 408./
      ASQS = 0.5*LOG10(1.876E+03*E0)
      JE = MIN(INT((ASQS-AMIN)/DA)+1,NE-2)
      DO JA=2,IA
         ABEAM = FLOAT(JA)
         S1 = QUAD_INT(ASQS, AA(JE),AA(JE+1),AA(JE+2),
     +                   SIGMA(JE,JA),SIGMA(JE+1,JA),SIGMA(JE+2,JA))
         S2 = QUAD_INT(ASQS, AA(JE),AA(JE+1),AA(JE+2),
     +                   SIGQE(JE,JA),SIGQE(JE+1,JA),SIGQE(JE+2,JA))
         SSIGNUC(JA) = S1 + S2
         ALNUC(JA) = ATARGET/(AVOG*SSIGNUC(JA))
      ENDDO
      ALNUC(1) = FPNI(E0, 13)
      SSIGNUC(1) = ATARGET/(AVOG*ALNUC(1))
      RETURN
      END
      SUBROUTINE SIG_AIR_INI
C==========================================================================
C. Cross sections
C==========================================================================
C...Initialize the cross section and interaction lengths  on air
      COMMON /S_CCSIG/ NSQS, ASQSMIN, ASQSMAX, DASQS,
     +    SSIG(51,2), PJETC(0:20,51,2),SSIGN(51,2), ALINT(51,2)
      COMMON /S_CCSIG2/ SSIG_TOT(51,2),SSIG_B(51,2)
      COMMON /S_SIGEL/ielastic,FIN(51,2),FEL(51,2),FQE(0:6,51,2)
      DATA AVOG /6.0221367E-04/
      parameter (pi=3.1415927)
      ATARGET = 14.514
C...Loop on c.m. energy
      DO J=1,NSQS
         ASQS = ASQSMIN + DASQS*FLOAT(J-1)
         IF (J .EQ. NSQS) ASQS = ASQS-1.E-04
         SQS = 10.**ASQS
         CALL sib_SIGMA_PP (SQS, SIGT, SIGEL, SIGINEL, SLOPE, RHO)
         CALL SIG_H_AIR (SIGT, SLOPE, RHO, SSIGT, SSIGEL, SSIGQE)
         SSIGN(J,1) = SSIGT-SSIGQE
       if (ielastic.eq.1)  then
            ALINT(J,1) = 1./(AVOG*SSIGT/ATARGET)
        else
             ALINT(J,1) = 1./(AVOG*SSIGn(j,1)/ATARGET)
       endif
       ssig_tot(j,1) = sigt
       ssig_B(j,1) = slope
       FIN(J,1) = ssign(j,1)/SSIGT
       FEL(J,1) = SSIGEL/SSIGT
       FQE(0,J,1) = (SSIGQE-SSIGEL)/SSIGT
       eps=ssigt/slope/16./pi*(1+rho**2)
       FQE(1,J,1) = eps
       do nqe=2,6
          FQE(nqe,J,1) = eps**nqe/nqe + fqe(nqe-1,j,1)
       enddo
       do nqe=1,6
          FQE(nqe,J,1) = fqe(nqe,j,1)/fqe(6,j,1)
       enddo

         CALL sib_SIGMA_PIP (SQS, SIGT, SIGEL, SIGINEL, SLOPE, RHO)
         CALL  SIG_H_AIR (SIGT, SLOPE, RHO, SSIGT, SSIGEL, SSIGQE)
         SSIGN(J,2) = SSIGT-SSIGQE
       if (ielastic.eq.1)  then
            ALINT(J,2) = 1./(AVOG*SSIGT/ATARGET)
        else
             ALINT(J,2) = 1./(AVOG*SSIGn(j,2)/ATARGET)
       endif
       ssig_tot(J,2) = sigt
       ssig_B(J,2) = slope

       FIN(j,2) = ssign(J,2)/SSIGT
       FEL(j,2) = ssigEL/SSIGT
       FQE(0,J,2) = SSIGQE/SSIGT
       eps=ssigt/slope/16./pi*(1+rho**2)
       FQE(1,J,2) = eps
       do nqe=2,6
          FQE(nqe,J,2) = eps**nqe/nqe + fqe(nqe-1,j,2)
       enddo
       do nqe=1,6
          FQE(nqe,J,2) = fqe(nqe,j,2)/fqe(6,j,2)
       enddo

      ENDDO
      RETURN
      END
      BLOCK DATA  SIG_DAT
C...Precalculated table of cross sections
      COMMON /S_CSIGINP/ QQ2MIN, SSQCD (51,2), SSQCD2 (51,2),
     @                       SSIG0(2), factork(2),isfchoice
C...Structure functions
      COMMON /S_CSTR/ JSTR, JSTRPI
C...EHLQ set 1 for proton Duke-Owens set 1 for pion
      DATA JSTR /1/, JSTRPI /1/
C...Q2min (GeV**2) for minijets
      DATA QQ2MIN /5.0/
C...Soft cross section in the eikonal factor
      DATA SSIG0 /123., 73./
      DATA factork/1.7,2.2/
      data isfchoice/1/
C...Sigma_qcd (GeV**-2) for p-p computed with EHLQ set 1 Q2min = 5. GeV**2
c... with scaling violations. THis is the default choice in SIBYLL.
c... A K-factor of 1.7 should be used to fit. the Tevatron data.
      DATA (SSQCD(K,1),K=1,51)  /
     @  1.1983993E-02, 4.4127252E-02, 0.1238399, 0.2868755, 0.5772045,
     @   1.045294,    1.744142,    2.731708,    4.065390,    5.807725,
     @   8.020122,    10.77380,    14.13659,    18.20087,    23.08322,
     @   28.82062,    35.58009,    43.48313,    52.64074,    63.20496,
     @   75.42126,    89.40395,    105.5904,    123.8777,    144.6305,
     @   168.2220,    195.0619,    225.1132,    259.1954,    297.7083,
     @   340.8559,    389.3647,    443.2799,    503.9778,    571.3537,
     @   647.0132,    731.9388,    825.9186,    932.3583,    1049.230,
     @   1178.351,    1320.911,    1479.186,    1656.306,    1851.798,
     @   2071.153,    2311.224,    2576.411,    2869.631,    3194.693,
     @   3556.603/
C...Sigma_qcd (GeV**-2) for pi-p computed with EHLQ set 1  for p
C                                Owens set 1 for pi Q2min = 5. GeV**2
c... with scaling violations. THis is the default choice in SIBYLL.
c... A K-factor of 1.7 should be used to fit. the Tevatron data.
      DATA (SSQCD(K,2),K=1,51)  /
     @ 2.6713109E-02, 7.6703623E-02, 0.1787110, 0.3587718, 0.6452169,
     @ 1.067511,   1.656302,   2.442088,   3.459795,   4.740401,
     @ 6.328330,   8.265375,   10.59707,   13.38726,   16.70245,
     @ 20.60534,   25.20829,   30.61100,   36.92186,   44.27321,
     @ 52.88524,   62.90113,   74.63335,   88.24413,   104.0870,
     @ 122.5886,   144.2660,   169.4464,   199.1269,   233.9357,
     @ 274.6917,   322.6675,   378.8712,   445.5984,   524.2751,
     @ 617.7631,   729.1132,   860.6794,   1019.997,   1208.843,
     @ 1433.871,   1703.056,   2026.311,   2417.850,   2889.604,
     @ 3463.152,   4152.619,   4989.879,   6001.615,   7231.195,
     @ 8747.169/
C...Sigma_qcd (GeV**-2) for pi-p computed with EHLQ set 1  for p
C                                Owens set 1 for pi Q2min = 5. GeV**2
c... WITHOUT scaling violations. THis is an alternate choice in SIBYLL.
c... A K-factor of 2.2 should be used to fit. the Tevatron data.
      DATA (SSQCD2(K,1),K=1,51)  /
     @ 1.4302040E-02, 5.3822853E-02,  0.1535475, 0.3596057, 0.7277440,
     @ 1.318651,  2.191311,  3.401969,  4.996748,  7.020068,
     @ 9.494884,  12.44863,  15.89774,  19.85705,  24.34084,
     @ 29.32943,  34.83667,  40.86320,  47.40503,  54.46183,
     @ 62.03970,  70.14349,  78.79037,  87.91505,  97.56121,
     @ 107.7228,  118.4118,  129.5977,  141.3069,  153.5433,
     @ 166.2887,  179.5711,  193.3551,  207.6443,  222.4576,
     @ 237.7850,  253.7659,  270.1528,  287.0669,  304.5056,
     @ 322.4221,  340.8359,  359.7802,  379.2549,  399.2404,
     @ 419.7521,  440.7868,  462.3503,  484.4333,  507.0548,
     @ 530.2106/
      DATA (SSQCD2(K,2),K=1,51)  /
     @  3.2613490E-02, 9.5264249E-02, 0.2243170, 0.4523652, 0.8127463,
     @  1.336795,  2.052137,  2.981797,  4.145481,  5.557004,
     @  7.229480,  9.172240,  11.39208,  13.89822,  16.69263,
     @  19.78566,  23.18154,  26.88788,  30.90883,  35.25125,
     @  39.92667,  44.94619,  50.30739,  56.02631,  62.11592,
     @  68.57966,  75.43336,  82.67727,  90.33365,  98.41272,
     @  106.9134,  115.8630,  125.2634,  135.1346,  145.4902,
     @  156.3337,  167.7068,  179.5899,  192.0124,  204.9890,
     @  218.5388,  232.6741,  247.4235,  262.8056,  278.8209,
     @  295.4990,  312.8557,  330.9224,  349.6995,  369.2270,
     @  389.5247/
      END

      SUBROUTINE SIG_H_AIR (SSIG, SLOPE, ALPHA,  SIGT, SIGEL, SIGQE)
C...Subroutine to compute hadron-air cross sections
C.  according to:
C.  R.J. Glauber and G.Matthiae  Nucl.Phys. B21, 135, (1970)
C.
C.  Air is a linear combination of Nitrogen and oxygen
C.
C.  INPUT :  SSIG  (mbarn) total pp cross section
C.           SLOPE (GeV**-2)  elastic scattering slope for pp
C.           ALPHA    real/imaginary part of the forward pp elastic
C.                                               scattering amplitude
C.  OUTPUT : SIGT  = Total cross section
C.           SIGEL = Elastic cross section
C.           SIGQEL  = Elastic + Quasi elastic cross section
C......................................................................
      DATA  FOX /0.257/
      CALL GLAUBER(14,SSIG,SLOPE,ALPHA,SIG1,SIGEL1,SIGQE1)
      CALL GLAUBER(16,SSIG,SLOPE,ALPHA,SIG2,SIGEL2,SIGQE2)
      SIGT  = (1.-FOX)*SIG1   + FOX*SIG2
      SIGEL = (1.-FOX)*SIGEL1 + FOX*SIGEL2
      SIGQE = (1.-FOX)*SIGQE1 + FOX*SIGQE2
      RETURN
      END

      SUBROUTINE SIG_JET (SIG_QCD, SIG_SOFT, JINT, SIG_inel, PJET,
     +                    SIG_TOT,B_EL)
C...This subroutine  receives in INPUT:
C.       sig_qcd (GeV-2)
C.       sig_soft (GeV-2)
C.       JINT (1 = pp interaction)    (2 pi-p interaction)
C.
C.  and returns as output:
C.       SIG_inel
C.       and PJET (1:20)   probability of n-jets
C.
C.  USES THE OLD GEOMETRY OF:
C   L.Durand and H.Pi,
c
C....................................................................
      COMMON /S_CFACT/ FACT (0:20), CO_BIN(0:20,0:20)
      COMMON /S_CHDCNV/NB,DB,ABPP(200),ABPIP(200),ABPPH(200),
     +        ABPIPH(200)
c      COMMON /S_CHDCNV/NB,DB,ABPP(200),ABPIP(200)
      DIMENSION PJET (0:20)
      DATA PI /3.1415926/

      DO J=1,20
          PJET(J) = 0.
      ENDDO
      SUM = 0.
      SUM_tot = 0.
      SUM_B = 0.

      DO JB=1,NB
         B = DB*FLOAT(JB-1)
         IF (JINT .EQ. 1)  THEN
            ABSOFT = ABPP   (JB)
            ABHARD = ABPPh (JB)
         ENDIF
         IF (JINT .EQ. 2)  THEN
            ABSOFT = ABPIP   (JB)
            ABHARD = ABPIPh (JB)
         ENDIF
         F1 = EXP(-ABHARD*SIG_QCD)
         F2 = EXP(-ABSOFT*SIG_SOFT)
         F = B*(1.-F1*F2)
         SUM = SUM+F
       f4= sqrt(f1*f2)
       F_tot=B*(1-f4)
       sum_tot=sum_tot+f_tot
       F_B=B**3*(1-f4)
       sum_B=sum_b+f_b
         PJET(0) = PJET(0) + (1.-F2)*F1*B
         G = SIG_QCD*ABHARD
         F3 = G*F1*B
         PJET(1) = PJET(1) + F3
         DO J=2,20
            F3 = F3*G
            PJET (J) = PJET(J) + F3
         ENDDO
      ENDDO
      SIG_inel = SUM*2.*PI*DB
      sig_tot= SUM_TOT*4.*PI*DB
      B_EL= SUM_B*PI*DB/sig_tot*2.
      SA = 0.
      DO J=0,20
         SA = SA + PJET(J)/FACT(J)
      ENDDO
      DO J=0,20
         PJET(J) = PJET(J)/FACT(J)/SA
      ENDDO

      RETURN
      END

      SUBROUTINE SINCO(S,C)
      DATA PI /3.1415926/
      F = 2.*PI*RAN(0)
      C = COS (F)
      S = SIN (F)
      RETURN
      END


       SUBROUTINE SIROBO( NBEG, NEND, THE, PHI, DBEX, DBEY, DBEZ)
C **********************************************************************
C   THIS IS A SLIGHTLY ALTERED VERSION OF "LUROBO" [JETSET63.PYTHIA]   *
C SET TO WORK IN THE SIBYL ENVIROMENT. THE TRANSFORMATION IS PERFORMED *
C ON PARTICLES NUMBER FROM NBEG TO NEND. COMMON BLOCKS CHANGED.        *
C                                      TSS,   Oct '87                  *
C  modification  use directly BETA in double precision in input (PL)   *
C **********************************************************************
      COMMON /S_PLIST/ NP, PLIST(5000,5), LLIST(5000)
      DIMENSION ROT(3,3),PV(3)
      DOUBLE PRECISION DP(4),DBEX,DBEY,DBEZ,DGA,DBEP,DGABEP
      IF(THE**2+PHI**2 .LE. 1E-20) GO TO 131
C...ROTATE (TYPICALLY FROM Z AXIS TO DIRECTION THETA,PHI)
       ROT(1,1)=COS(THE)*COS(PHI)
       ROT(1,2)=-SIN(PHI)
       ROT(1,3)=SIN(THE)*COS(PHI)
       ROT(2,1)=COS(THE)*SIN(PHI)
       ROT(2,2)=COS(PHI)
       ROT(2,3)=SIN(THE)*SIN(PHI)
       ROT(3,1)=-SIN(THE)
       ROT(3,2)=0.
       ROT(3,3)=COS(THE)
       DO 120 I=NBEG,NEND
       DO 100 J=1,3
 100   PV(J)=PLIST(I,J)
       DO 110 J=1,3
 110   PLIST(I,J)=ROT(J,1)*PV(1)+ROT(J,2)*PV(2)+ROT(J,3)*PV(3)
 120   CONTINUE
 131    IF(DBEX**2+DBEY**2+DBEZ**2 .LE. 1D-20) GO TO 151
C...LORENTZ BOOST (TYPICALLY FROM REST TO MOMENTUM/ENERGY=BETA)
       DGA=1D0/DSQRT(1D0-DBEX**2-DBEY**2-DBEZ**2)
       DO 140 I=NBEG, NEND
       DO 130 J=1,4
 130   DP(J)=PLIST(I,J)
       DBEP=DBEX*DP(1)+DBEY*DP(2)+DBEZ*DP(3)
       DGABEP=DGA*(DGA*DBEP/(1D0+DGA)+DP(4))
       PLIST(I,1)=DP(1)+DGABEP*DBEX
       PLIST(I,2)=DP(2)+DGABEP*DBEY
       PLIST(I,3)=DP(3)+DGABEP*DBEZ
       PLIST(I,4)=DGA*(DP(4)+DBEP)
 140   CONTINUE
 151   RETURN
      END
      SUBROUTINE SSLOPE (S, BP, BM)
      COMMON /BLOCKD/ CP, DP, EP, CM, DM
      AL = LOG(S)
      BP = CP + DP*AL + EP*AL*AL
      BM = CM + DM*AL
      RETURN
      END

      SUBROUTINE STRING_FRAG(E0,IFL1,IFL2,PX1,PY1,PX2,PY2,IFBAD)
C...This routine fragments a string of energy E0
C.  the ends of the strings  have flavors IFL1 and IFL2
C.  the particles produced are in the  jet-jet frame
C.  with IFL1 going in the +z direction
C.     E0 = total energy in jet-jet system
C.  This version consider also a primordial pT attached
C.  to the ends of the string PX1,PY1,  PX2,PY2
C.  OUTPUT:  IFBAD =1  kinematically impossible decay
c
c      Modified Nov. 91.  RSF and TSS to fragment symetrically
c      ie forward and backward are fragmented as leading.
c      Change- Dec. 92  RSF.  call to ptdis moved- to use flavor
c      of NEW quark in fragmentation.
C...........................................................
      COMMON /S_PLIST/ NP, P(5000,5), LLIST(5000)
      COMMON /S_MASS1/ AM(49), AM2(49)
      COMMON /S_diagnostics/ntry,Iflag(5000),xm(5000),zst(5000)
      DIMENSION WW(2,2), PTOT(4), PX(3),PY(3),IFL(3)
      DIMENSION LPOINT(3000), PMQ(3)
      LOGICAL LRANK
      DATA LRANK/.true./

C...initialise
      NTRY = 0
      IFBAD = 0
200      NTRY = NTRY + 1
      IF (NTRY .GT. 50)  THEN
         IFBAD = 1
         RETURN
      ENDIF
      I = NP
      DO K=1,2
         WW(K,1) = 1.
         WW(K,2) = 0.
      ENDDO
      PX(1) = PX1
      PY(1) = PY1
      PX(2) = PX2
      PY(2) = PY2
      PX(3) = 0.
      PY(3) = 0.
      PTOT (1) = PX1+PX2
      PTOT (2) = PY1+PY2
      PTOT (3) = 0.
      PTOT (4) = E0
      IFL(1) = IFL1
      IFL(2) = IFL2
      PMQ(1) = QMASS(IFL(1))
      PMQ(2) = QMASS(IFL(2))

      IBLEAD = 0
C
C      SET FLAG FOR GENERATION OF LEADING PARTICLES.
C      "AND" IS FOR PPBAR ( DIQUARK AT BOTH ENDS)
C      "OR" IS FOR PP, PPI, ( DIQUARK AT ONE END.)
C
      IF (IABS(IFL1) .GT. 10 .AND. IABS(IFL2) .GT. 10)  THEN
         IBLEAD = 2
         I = I+1
         JT = 1.5+RAN(0)
         GOTO 350
      ENDIF
      IF (IABS(IFL1) .GT. 10 .OR. IABS(IFL2) .GT. 10)  THEN
         IBLEAD = 1
         I = I+1
         JT = 1
         IF (IABS(IFL2) .GT. 10) JT = 2
         GOTO 350
      ENDIF

C...produce new particle: side, pT
300      I=I+1
      IF (IBLEAD .GT. 0)  THEN
           JT = 3 - JT
           GO TO 350
       ENDIF
c
 349     continue
         JT=1.5+RAN(0)
 350      JR=3-JT
      LPOINT(I) = JT
      Iflag(i)=0
c      old call to Ptdis. pre Dec. 92
c      CALL PTDIS (IFL(JT), PX(3),PY(3))

C...particle ID and pt.
 999        continue
      CALL IFLAV (IFL(JT), 0, IFL(3), LLIST(I))
 991    continue
      PMQ(3) = QMASS(IFL(3))
      P(I,5) = AM(IABS(LLIST(I)))
      CALL PTDIS (IFL(3), PX(3),PY(3))
C...fill transverse momentum
      P(I,1) = PX(JT) + PX(3)
      P(I,2) = PY(JT) + PY(3)
      XMT2 = P(I,5)**2+P(I,1)**2+P(I,2)**2


C...test end of fragmentation

      WREM2 = PTOT(4)**2-PTOT(1)**2-PTOT(2)**2-PTOT(3)**2
      IF (WREM2 .LT. 0.1)  GOTO 200
      WMIN = PMQ(1)+PMQ(2)+2.*PMQ(3)+ 1.1 + (2.*RAN(0)-1.)*0.2
c      WMIN = PMQ(jr)+sqrt(xmt2)+pmq(3)+ 1.1 +(2.*RAN(0)-1.)*0.2
c      IF (WREM2 .LT. WMIN**2) goto 400
      IF (WREM2 .LT. WMIN**2)    Then!   goto 400
         if (abs(ifl(3)).ne.3) GOTO 400
          goto 200
      endif

c
C...Choose z
      xm(i)=xmt2
      IF (IBLEAD .GT. 0.and.abs(ifl(jt)).gt.10)  THEN
c        Special frag. for leading Baryon only
         Z = ZBLEAD (IABS(LLIST(I)))
         IBLEAD = IBLEAD - 1
      ELSE
         Z = ZDIS (IFL(3),ifl(jt),XMT2)
      ENDIF
c       store z for spliting
      if (z.le.0) WRITE (6,*) 'z less than 0 =',z
      zst(i)=z
      WW(JT,2) = Z*WW(JT,1)
      WW(JR,2) = XMT2/(WW(JT,2)*E0**2)

      P(I,3) = WW(1,2)*0.5*E0 - WW(2,2)*0.5*E0
      P(I,4) = WW(1,2)*0.5*E0 + WW(2,2)*0.5*E0

      DO J=1,4
         PTOT (J) = PTOT(J) - P(I,J)
      ENDDO
      DO K=1,2
         WW(K,1) = WW(K,1) - WW(K,2)
      ENDDO

C...Reset pT and flavor at ebds of the string
      PX(JT) = -PX(3)
      PY(JT) = -PY(3)
      IFL(JT) =-IFL(3)
      PMQ(JT) = PMQ(3)
      GOTO 300

C...Final two hadrons
400      IF (IFL(JR)*IFL(3) .GT. 100)  GOTO 200
c   debug- output ptot
      iflag(i)=1
      iflag(i+1)=1
      do iii=1,4
        p(4999,iii)=ptot(iii)
      enddo
c     p(4999,5)=sqrt(wrem2)
      CALL IFLAV (IFL(JR), -IFL(3), IFLA, LLIST(I+1))
      P(I+1,5) = AM(IABS(LLIST(I+1)))
      P(I,1)   = PX(JT)+PX(3)
      P(I,2)   = PY(JT)+PY(3)
      I1 = I+1
      P(I+1,1) = PX(JR)-PX(3)
      P(I+1,2) = PY(JR)-PY(3)
      XM1 = P(I,5)**2+P(I,1)**2+P(I,2)**2
      XM2 = P(I1,5)**2+P(I1,1)**2+P(I1,2)**2
      IF (SQRT(XM1)+SQRT(XM2) .GT. SQRT(WREM2)) GOTO 200
      WREM = SQRT(WREM2)
      EA1 = (WREM2+XM1-XM2)/(2.*WREM)
      PA2 = (EA1**2-XM1)
      if (pa2.gt.0)  then
            PA = SQRT(PA2)
      else
            goto 200
      endif
      BA = PTOT(3)/PTOT(4)
      GA = PTOT(4)/WREM
      S = FLOAT(3-2*JT)
      P(I,3) = GA*(BA*EA1+S*PA)
      P(I,4) = GA*(EA1+BA*S*PA)
      P(I+1,3) = PTOT(3)-P(I,3)
      P(I+1,4) = PTOT(4)-P(I,4)
      NA= NP+1
      NP=I+1

C...reorder  particles along chain (in rank)
      IF (LRANK)  THEN
      N1 = NA-1
      N2 = 0
      DO J=NA,NP
         IF(LPOINT(J) .EQ. 2)  THEN
            N2=N2+1
            LLIST (NP+N2) = LLIST(J)
            DO K=1,5
               P(NP+N2,K)=P(J,K)
            ENDDO
         ELSE
            N1= N1+1
            IF (N1.LT.J)   THEN
               LLIST(N1) = LLIST(J)
               DO K=1,5
                  P(N1,K) = P(J,K)
               ENDDO
            ENDIF
         ENDIF
      ENDDO
      JJ=N1
      DO J=NP+N2,NP+1,-1
         JJ= JJ+1
         LLIST(JJ) = LLIST(J)
         DO K=1,5
             P(JJ,K) = P(J,K)
         ENDDO
      ENDDO
      ENDIF

      RETURN
      END
      FUNCTION WOOD_SAXON (R, JA)
C....Woods Saxon nuclear density (normalised to 1)
C.   for a nucleus of mass number A.
C.   INPUT R =  (fm)
C.         JA = mass number
C.   OUTPUT (fm**-3)
C......................................................
      COMMON /CWOOD/ RR0(19:56), AA0(19:56), CC0(19:56)
      WOOD_SAXON = CC0(JA)/(1.+EXP((R-RR0(JA))/AA0(JA)))
      RETURN
      END

      SUBROUTINE WOOD_SAXON_INI
      COMMON /CWOOD/ RR0(19:56), AA0(19:56), CC0(19:56)
      DATA PI /3.1415926/
C...Wood-Saxon parameters from  table 6.2   of Barrett and Jackson
      RR0 (19) = 2.59
      AA0 (19) = 0.564
      RR0 (20) = 2.74
      AA0 (20) = 0.569
      RR0 (22) = 2.782
      AA0 (22) = 0.549
      RR0 (24) = 2.99
      AA0 (24) = 0.548
      RR0 (27) = 2.84
      AA0 (27) = 0.569
      RR0 (28) = 3.14
      AA0 (28) = 0.537
      RR0 (29) = 3.77
      AA0 (29) = 0.52
      RR0 (48) = 3.912
      AA0 (48) = 0.5234
      RR0 (56) = 3.98
      AA0 (56) = 0.569
      DO J=19, 56
         IF (RR0(J) .LE. 0.)  THEN
            RR0(J) = 1.05*FLOAT(J)**0.333333
            AA0(J) = 0.545
         ENDIF
         CC0(J)=3./(4.*PI*RR0(J)**3)/(1.+((AA0(J)*PI)/RR0(J))**2)
      ENDDO
      RETURN
      END

      FUNCTION ZBLEAD (LB)
C...fragmentation function for leading baryon
C.  simple form:  f(z) = a + x**b
C   INPUT : LB = particle code.
C..................................................
      COMMON /S_CZLEAD/ CLEAD, FLEAD
c      COMMON /S_SZLEAD/ CLEADs, FLEADs
      COMMON /S_CHP/ ICHP(49), ISTR(49), IBAR(49)

            IC = ICHP(Lb)*ISIGN(1,Lb)

      if (lb.ge.34.and.lb.le.39)  then  ! Lambda's and Sigma's
  665               ZBLEAD = RAN(0)
                if (zblead.le..01) goto 665
c          zblead=zdisn(1) ! blead**2   ! soft
      else if (ic.eq.0)     then
          zblead=zdisn(1)   ! blead**2   !soft
      else if (ic.eq.1)  then  ! fast protons only
            if (abs(lb).eq.13) then
              IF (RAN(0) .LT. CLEAD)  THEN
  666               ZBLEAD = RAN(0)
                if (zblead.le..01) goto 666
              ELSE
                  zblead=1.-zdisn(1)  ! zblead**2   !hard
              ENDIF
            continue
           else
               zblead=zdisn(1)  ! zblead**2   !hard
           endif
      else if (ic.eq.2)  then  ! fast delta++
          zblead=1.- zdisn(1)  ! (zblead)**.3333
      else
               zblead=RAN(0) ! zdisn(1)     !hard
      endif
       RETURN
      END
      FUNCTION ZDIS (IFL1,ifl2, XMT2)
C...z distribution
      COMMON /S_CZDIS/ FAin, FB0in
      COMMON /S_CZDISs/ FAs1, fAs2
      COMMON /S_RUN/ SQS, S, Q2MIN, XMIN, ZMIN , kb ,kt
      fa=fain
      fb0=fb0in
C  following statement corrected by D.H. may 10, 1996
      if (abs(kb).ge.13) then   ! baryons only
          if (abs(ifl2).eq.3)  fa=fain+fas2
          if (abs(ifl1).eq.3)  fa=fain+fas1
      endif
      FB = FB0*XMT2
      IF(FA.GT.0.01.AND.ABS(FA-1.)/FB.LE.0.01) ZMAX=FB/(1.+FB)+
     +  (1.-FA)*FB**2/(1.+FB)**3
      IF(FA.GT.0.01.AND.ABS(FA-1.)/FB.GT.0.01) ZMAX=0.5*(1.+FB-
     +  SQRT((1.-FB)**2+4.*FA*FB))/(1.-FA)
      IF(ZMAX.LT.0.1)  ZDIV=2.75*ZMAX
      IF(ZMAX.GT.0.85)
     +     ZDIV=ZMAX-0.6/FB**2+(FA/FB)*ALOG((0.01+FA)/FB)
C...Choice if z, preweighted for peaks at low or high z
100      Z=RAN(0)
      IDIV=1
      FPRE=1.
      IF (ZMAX.LT.0.1)  THEN
         IF(1..LT.RAN(0)*(1.-ALOG(ZDIV)))  IDIV=2
         IF (IDIV.EQ.1)  Z=ZDIV*Z
         IF (IDIV.EQ.2)  Z=ZDIV**Z
         IF (IDIV.EQ.2)  FPRE=ZDIV/Z
      ELSEIF (ZMAX.GT.0.85)  THEN
         IF(1..LT.RAN(0)*(FB*(1.-ZDIV)+1.)) IDIV=2
         IF (IDIV.EQ.1)  Z=ZDIV+ALOG(Z)/FB
         IF (IDIV.EQ.1)  FPRE=EXP(FB*(Z-ZDIV))
         IF (IDIV.EQ.2)  Z=ZDIV+Z*(1.-ZDIV)
      ENDIF
C...weighting according to the correct formula
      IF (Z.LE.FB/(50.+FB).OR.Z.GE.1.)  GOTO 100
      FVAL=(ZMAX/Z)*EXP(FB*(1./ZMAX-1./Z))
      IF(FA.GT.0.01)  FVAL=((1.-Z)/(1.-ZMAX))**FA*FVAL
      IF(FVAL.LT.RAN(0)*FPRE)  GOTO 100
      ZDIS=Z
      RETURN
      END
      FUNCTION ZDISN (n)
C...Generate (1-x)**n
      continue
666   rmin=1.1
      do i=1,n+1
         R1=RAN(0)
         IF (R1.LE.RMIN) RMIN=R1
      ENDDO
      ZDISn=RMIN
      if (zdisn.le..01) goto 666
      if (zdisn.ge..99) goto 666
      END
      FUNCTION ZSAMPLE (ZMIN,L)
C...This function returns as output a value z=log(x)
C.  distributed as f(x) = g(x) + 4/9 *(q(x) + qbar(x))
C.  from a minimum value ZMIN to 0,
C.  for a proton (L=1) or a pi (L=2)
C.  needs to be initialised with: CALL ZSAMPLE_INI
C.....................................................
      COMMON /S_CZGEN/ XA,XB,XMAX,ZA,ZB,ZMAX,DX,DZ,NX,NZ,APART(2),
     +   FFA(2),FFB(2),
     +   DFX(2),DFZ(2),XX(200,2),ZZ(200,2),FFX(200,2),FFZ(200,2)

      F = PART_INT(ZMIN,L)*RAN(0)
C D.H.
      IF (F .GE. FFA(L))  THEN
         ZSAMPLE = ZA - (F-FFA(L))/APART(L)
C D.H.
      ELSE IF (F .GE. FFB(L))  THEN
         JF = (F-FFB(L))/DFZ(L) + 1
         F0 = FFB(L) + DFZ(L)*FLOAT(JF-1)
         T = (F-F0)/DFZ(L)
         ZSAMPLE = ZZ(JF,L)*(1.-T)+ZZ(JF+1,L)*T
      ELSE
         JF = F/DFX(L)+1
         F0 = DFX(L)*FLOAT(JF-1)
         T = (F-F0)/DFX(L)
         X = XX(JF,L)*(1.-T)+XX(JF+1,L)*T
         ZSAMPLE = LOG(X)
      ENDIF
      RETURN
      END
      SUBROUTINE ZSAMPLE_INI
C...This subroutine initialise the generation of
C.  z = log(x)  for the generation  of z according
C.  to the structure functions
C..................................................
      COMMON /S_CZGEN/ XA,XB,XMAX,ZA,ZB,ZMAX,DX,DZ,NX,NZ,APART(2),
     +   FFA(2),FFB(2),
     +   DFX(2),DFZ(2),XX(200,2),ZZ(200,2),FFX(200,2),FFZ(200,2)

      XA = 1.E-04
      XB = 1.E-01
      XMAX = 0.80
      ZA = LOG(XA)
      ZB = LOG(XB)
      ZMAX = LOG(XMAX)
      NX = 200
      NZ = 200
      DX = (XMAX-XB)/FLOAT(NX-1)
      DZ = (ZB-ZA)/FLOAT(NZ-1)

      DO L=1,2
C         very small x:  f(x) = A/x
         APART(L) = PARTON(0.,L)

C         large x: interpolation in x
         FFX(1,L) = 0.
         DO J=2,NX
            X = XMAX - DX*(FLOAT(J)-0.5)
             G = PARTON(X,L)/X
            FFX(J,L) = FFX(J-1,L)+G*DX
         ENDDO
         CALL INVERT_ARRAY (FFX(1,L),XMAX,-DX,NX,XX(1,L),FMIN,
     +                        DFX(L))

C         small x: interpolation in log(x)
         FFZ(1,L) = FFX(NX,L)
         DO J=2,NZ
            Z = ZB - DZ*(FLOAT(J)-0.5)
            X = EXP(Z)
            G = PARTON(X,L)
            FFZ(J,L) = FFZ(J-1,L)+G*DZ
         ENDDO
         CALL INVERT_ARRAY (FFZ(1,L),ZB,-DZ,NZ,ZZ(1,L),FMIN,DFZ(L))
         FFA(L) = FFZ(NZ,L)
         FFB(L) = FFX(NX,L)
      ENDDO
      RETURN
      END
      BLOCK DATA DATDEC
      COMMON /S_CSYDEC/ IDB(49), CBR(102), KDEC(612), LBARP(49)
      COMMON /S_MASS1/ AM(49), AM2(49)
      COMMON /S_CHP/ ICHP(49), ISTR(49), IBAR(49)
      COMMON /S_CNAM/ NAMP (0:49)
      CHARACTER NAMP*6
      DATA CBR /3*1.,0.,1.,1.,0.6351,0.8468,0.9027,0.9200,0.9518,1.,
     +   0.6351,0.8468,0.9027,0.9200,0.9518,1.,0.2160,0.3398,0.4748,
     +   0.6098,0.8049,1.,0.6861,1.,3*0.,0.5,1.,0.5,1.,
     +   0.3890,0.7080,0.9440,0.9930,1.,0.,0.4420,0.6470,0.9470,0.9770,
     +   0.9990,4*1.,0.6670,1.,9*0.,0.6670,1.,0.6670,1.,0.6670,1.,
     +   0.8880,0.9730,1.,0.4950,0.8390,0.9870,1.,0.5160,5*1.,0.6410,1.,
     +   1.,0.67,1.,0.33,1.,1.,0.88,0.94,1.,0.88,0.94,1.,0.88,0.94,1.,
     +   0.33,1.,0.67,1.,0.678,0.914,1./
      DATA AM / 0.,2*0.511E-3, 2*0.10566, 0.13497, 2*0.13957,
     +   2*0.49365, 2*0.49767, 0.93827, 0.93957, 4*0.,0.93827,
     +   0.93957, 2*0.49767, 0.54880,0.95750,2*0.76830,0.76860,
     +   2*0.89183,2*0.89610,0.78195,1.01941,1.18937,1.19255,
     +   1.19743,1.31490,1.32132,1.11563,1.23100,1.23500,
     +   1.23400,1.23300,1.38280,1.38370,1.38720,
     +   1.53180,1.53500,1.67243 /
      DATA AM2 /0.,2*2.61121E-07,2*0.011164,0.018217,0.019480,
     + 0.019480,0.243690,0.243690,0.247675,0.247675,0.880351,0.882792,
     + 0.000000,0.000000,0.000000,0.000000,0.880351,0.882792,0.247675,
     + 0.247675,0.301181,0.916806,0.590285,0.590285,0.590746,0.795361,
     + 0.795361,0.802995,0.802995,0.611446,1.039197,1.414601,1.422176,
     + 1.433839,1.728962,1.745887,1.244630,1.515361,1.525225,1.522765,
     + 1.520289,1.912136,1.914626,1.924324,2.346411,2.356225,2.797022/
      DATA IDB /
     +    0,0,0,1,2,3,5,6,7,13,19,25,8*0,30,32,34,40,46,47,48,49,60,62,
     +    64,66,69,73,75,76,77,78,79,81,82,84,86,87,90,93,96,98,100/
      DATA KDEC /
     + 3,1,15,2,18,0,3,1,16,3,17,0,2,0,1,1,8*0,2,0,4,17,0,0,2,0,5,18,0,
     + 0,2,0,4,17,0,0,2,0,7,6,0,0,3,0,7,7,8,0,3,0,7,6,6,0,3,1,17,4,6,0,
     + 3,1,15,2,6,0,2,0,5,18,0,0,2,0,8,6,0,0,3,0,8,8,7,0,3,0,8,6,6,0,3,
     + 1,18,5,6,0,3,1,16,3,6,0,3,0,6,6,6,0,3,0,7,8,6,0,3,1,18,5,7,0,3,
     + 1,17,4,8,0,3,1,16,3,7,0,3,1,15,2,8,0,2,0,7,8,0,0,2,0,6,6,20*0,1,
     + 0,11,3*0,1,0,12,0,0,0,1,0,11,0,0,0,1,0,12,0,0,0,2,0,1,1,0,0,3,0,
     + 6,6,6,0,3,0,7,8,6,0,3,0,1,7,8,0,3,0,1,3,2,7*0,3,0,7,8,23,0,3,0,6
     + ,6,23,0,2,0,1,27,0,0,2,0,1,32,0,0,2,0,1,1,0,0,3,0,6,6,6,0,2,0,7,
     + 6,0,0,2,0,8,6,0,0,2,0,7,8,0,0,2,0,21,7,0,0,2,0,9,6,0,0,54*0,2,0,
     + 22,8,0,0,2,0,10,6,0,0,2,0,9,8,0,0,2,0,21,6,0,0,2,0,10,7,0,0,
     + 2,0,22,6,0,0,3,0,7,8,6,0,2,0,1,6,0,0,2,0,7,8,0,0,2,0,9,10,0,
     + 0,2,0,11,12,0,0,3,0,7,
     + 8,6,0,2,0,1,23,0,0,2,0,13,6,0,0,2,0,14,7,0,0,2,0,39,1,0,0,2,
     + 0,14,8,0,0,2,0,39,6,0,0,2,0,39,8,0,0,2,0,13,8,0,0,2,0,
     + 14,6,0,0,2,0,13,7,0,0,2,0,13,6,
     + 0,0,2,0,14,7,0,0,2,0,13,8,0,0,2,0,14,6,0,0,2,0,14,8,0,0,2,0,
     + 39,7,0,0,2,0,34,6,0,0,2,0,35,7,0,0,2,0,39,6,0,0,2,0,34,8,0,0,
     + 2,0,36,7,0,0,2,0,39,8,0,0,2,
     + 0,35,8,0,0,2,0,36,6,0,0,2,0,37,6,0,0,2,0,38,7,0,0,2,0,
     + 37,8,0,0,2,0,38,6,0,0,2,0,39,10,0,0,2,0,37,8,0,0,2,0,38,6,0,0/
      DATA LBARP/1,3,2,5,4,6,8,7,10,9,11,12,-13,-14,16,15,18,17,13,14,
     +  22,21,23,24,26,25,27,29,28,31,30,32,33,-34,-35,-36,-37,-38,-39,
     +  -40,-41,-42,-43,-44,-45,-46,-47,-48,-49/
      DATA ICHP /0,1,-1,1,-1,0,1,-1,1,-1,0,0,1,0,4*0,-1,0,4*0,
     +    1,-1,0,1,-1,4*0,1,0,-1,0,-1,0,2,1,0,-1,1,0,-1,0,-1,-1/
      DATA ISTR /8*0,-1,+1,10,10,8*0,-1,+1,5*0,-1,+1,-1,+1,2*0,
     +           3*1,2*2,1,4*0,3*1,2*2,3 /
      DATA IBAR /12*0,2*1,4*0,2*-1,13*0,16*1/
      DATA NAMP /
     +     '     ','gam   ','e+','e-','mu+','mu-','pi0',
     +     'pi+','pi-','k+', 'k-', 'k0l','k0s',
     +     'p', 'n', 'nue', 'nueb', 'num', 'numb', 'pbar', 'nbar',
     +     'k0', 'k0b', 'eta', 'etap', 'rho+', 'rho-','rho0',
     +     'k*+','k*-','k*0','k*0b','omeg', 'phi', 'SIG+', 'SIG0',
     +     'SIG-','XI0','XI-','LAM','DELT++','DELT+','DELT0','DELT-',
     +     'SIG*+ ','SIG*0','SIG*-', 'XI*0', 'XI*-', 'OME*-'/
      END
      SUBROUTINE DECPAR (LA,P0,ND,LL,P)
C...This subroutine generates the decay of a particle
C.  with ID = LA, and 5-momentum P0(1:5)
C.  into ND particles of 5-momenta P(j,1:5) (j=1:ND)
C.
C.  If the initial particle code is LA=0
C.  then ND and LL(1:ND) are considered as  input and
C.  the routine generates a phase space decay into ND
C.  particles of codes LL(1:nd)
C.
C.  june 1992
C.  This version  contains the decay of polarized muons
C.  The muon codes are  L =  4 : mu+ R
C.                          -4 : mu+ L
C.                           5 : mu- L
C.                          -5 : mu- R
C------------------------------------------------------
      COMMON /S_CSYDEC/ IDB(49), CBR(102), KDEC(612), LBARP(49)
      COMMON /S_MASS1/ AM(49), AM2(49)
      DIMENSION P0(5), LL(10), P(10,5)
      DIMENSION PV(10,5), RORD(10), UE(3),BE(3), FACN(3:10)
      DATA FACN /2.,5.,15.,60.,250.,1500.,12000.,120000./
      DATA PI /3.1415926/

C...c.m.s. Momentum in two particle decays
      PAWT(A,B,C) = SQRT((A**2-(B+C)**2)*(A**2-(B-C)**2))/(2.*A)

C...Phase space decay into the particles in the list
      IF (LA .EQ. 0)  THEN
          MAT = 0
          MBST = 0
          PS = 0.
          DO J=1,ND
             P (J,5) = AM(IABS(LL(J)))
             PV(J,5) = AM(IABS(LL(J)))
             PS = PS+P(J,5)
          ENDDO
          DO J=1,4
             PV(1,J) = P0(J)
          ENDDO
          PV(1,5) = P0(5)
          GOTO 140
      ENDIF

C...Choose decay channel
      L = IABS(LA)
      ND=0
      IDC = IDB(L)-1
      IF (IDC+1 .LE.0)  RETURN
      RBR = RAN(0)
110   IDC=IDC+1
      IF(RBR.GT.CBR(IDC))  GOTO 110

      KD =6*(IDC-1)+1
      ND = KDEC(KD)
      MAT= KDEC(KD+1)
      MBST=0
      IF (MAT .GT.0 .AND. P0(4) .GT. 20*P0(5)) MBST=1
      IF (MAT .GT.0 .AND. MBST .EQ. 0)
     +        BETA = SQRT(P0(1)**2+P0(2)**2+P0(3)**2)/P0(4)
      PS = 0.
      DO J=1,ND
         LL(J) = KDEC(KD+1+J)
         P(J,5)  = AM(LL(J))
         PV(J,5) = AM(LL(J))
         PS = PS + P(J,5)
      ENDDO
      DO J=1,4
         PV(1,J) = 0.
         IF (MBST .EQ. 0)  PV(1,J) = P0(J)
      ENDDO
      IF (MBST .EQ. 1)  PV(1,4) = P0(5)
      PV(1,5) = P0(5)

140   IF (ND .EQ. 2) GOTO 280

      IF (ND .EQ. 1)  THEN
         DO J=1,4
            P(1,J) = P0(J)
         ENDDO
         RETURN
      ENDIF

C...Calculate maximum weight for ND-particle decay
      WWTMAX = 1./FACN(ND)
      PMAX=PV(1,5)-PS+P(ND,5)
      PMIN=0.
      DO IL=ND-1,1,-1
         PMAX = PMAX+P(IL,5)
         PMIN = PMIN+P(IL+1,5)
         WWTMAX = WWTMAX*PAWT(PMAX,PMIN,P(IL,5))
      ENDDO

C...generation of the masses, compute weight, if rejected try again
240   RORD(1) = 1.
      DO 260 IL1=2,ND-1
        RSAV = RAN(0)
        DO 250 IL2=IL1-1,1,-1
          IF(RSAV.LE.RORD(IL2))   GOTO 260
250     RORD(IL2+1)=RORD(IL2)
260     RORD(IL2+1)=RSAV
      RORD(ND) = 0.
      WT = 1.
      DO 270 IL=ND-1,1,-1
      PV(IL,5)=PV(IL+1,5)+P(IL,5)+(RORD(IL)-RORD(IL+1))*(PV(1,5)-PS)
270   WT=WT*PAWT(PV(IL,5),PV(IL+1,5),P(IL,5))
      IF (WT.LT.RAN(0)*WWTMAX)   GOTO 240

C...Perform two particle decays in respective cm frame
280   DO 300 IL=1,ND-1
      PA=PAWT(PV(IL,5),PV(IL+1,5),P(IL,5))
      UE(3)=2.*RAN(0)-1.
      PHI=2.*PI*RAN(0)
      UT = SQRT(1.-UE(3)**2)
      UE(1) = UT*COS(PHI)
      UE(2) = UT*SIN(PHI)
      DO 290 J=1,3
      P(IL,J)=PA*UE(J)
290   PV(IL+1,J)=-PA*UE(J)
      P(IL,4)=SQRT(PA**2+P(IL,5)**2)
300   PV(IL+1,4)=SQRT(PA**2+PV(IL+1,5)**2)

C...Lorentz transform decay products to lab frame
      DO 310 J=1,4
310   P(ND,J)=PV(ND,J)
      DO 340 IL=ND-1,1,-1
      DO 320 J=1,3
320   BE(J)=PV(IL,J)/PV(IL,4)
      GA=PV(IL,4)/PV(IL,5)
      DO 340 I=IL,ND
      BEP = BE(1)*P(I,1)+BE(2)*P(I,2)+BE(3)*P(I,3)
      DO 330 J=1,3
330   P(I,J)=P(I,J)+GA*(GA*BEP/(1.+GA)+P(I,4))*BE(J)
340   P(I,4)=GA*(P(I,4)+BEP)

C...Weak decays
      IF (MAT .EQ. 1)  THEN
         F1=P(2,4)*P(3,4)-P(2,1)*P(3,1)-P(2,2)*P(3,2)-P(2,3)*P(3,3)
         IF (MBST.EQ.1)  THEN
C          WT = P0(5)*P(1,4)*F1
           WT = P0(5)*(P(1,4)+FLOAT(LA/L)*P(1,3))*F1
         ENDIF
         IF (MBST.EQ.0)  THEN
           WT=F1*(P(1,4)*P0(4)-P(1,1)*P0(1)-P(1,2)*P0(2)-P(1,3)*P0(3))
           WT= WT-FLOAT(LA/L)*(P0(4)*BETA*P(1,4)-P0(4)*P(1,3))*F1
         ENDIF
         WTMAX = P0(5)**4/8.
         IF(WT.LT.RAN(0)*WTMAX)   GOTO 240
      ENDIF

C...Boost back for rapidly moving particle
      IF (MBST .EQ. 1)   THEN
         DO 440 J=1,3
440      BE(J)=P0(J)/P0(4)
         GA= P0(4)/P0(5)
         DO 460 I=1,ND
         BEP=BE(1)*P(I,1)+BE(2)*P(I,2)+BE(3)*P(I,3)
         DO 450 J=1,3
450         P(I,J)=P(I,J)+GA*(GA*BEP/(1.+GA)+P(I,4))*BE(J)
460         P(I,4)=GA*(P(I,4)+BEP)
      ENDIF

C...labels for antiparticle decay
      IF (LA .LT. 0 .AND. L .GT. 18)  THEN
           DO J=1,ND
            LL(J) = LBARP(LL(J))
         ENDDO
      ENDIF

      RETURN
      END
      SUBROUTINE DECPR (LUN)
C...Print on unit LUN the list of particles and decay channels
      COMMON /S_CSYDEC/ IDB(49), CBR(102), KDEC(612), LBARP(49)
      COMMON /S_MASS1/ AM(49), AM2(49)
      COMMON /S_CNAM/ NAMP (0:49)
      CHARACTER*6 NAMP
      DIMENSION LL(3)

      DO L=1,49
         IDC = IDB(L)-1
         NC = 0
         WRITE (LUN,10) L,NAMP(L), AM(L)
         IF(IDC+1 .GT. 0)  THEN
            CB = 0.
110         IDC=IDC+1
            NC = NC+1
            CBOLD = CB
            CB = CBR(IDC)
            BR = CB-CBOLD
            KD = 6*(IDC-1)+1
            ND = KDEC(KD)
            MAT= KDEC(KD+1)
            DO J=1,ND
              LL(J) = KDEC(KD+1+J)
            ENDDO
            WRITE (LUN,15) NC,BR,ND,MAT, (NAMP(LL(J)),J=1,ND)
            IF (CB .LT. 1.)  GOTO 110
         ENDIF
      ENDDO
      RETURN
10    FORMAT(1X,I3,2X,A6,3X,F10.4)
15    FORMAT(5X,I2,2X,F9.4,I4,I4,2X,3(A6,2X))
      END
      SUBROUTINE DECSIB
C----------------------------------------------------------------------------
C  Code for Decay  developed for the SIBYLL montecarlo
C----------------------------------------------------------------------------
C...Decay all unstable particle in Sibyll
C.  decayed particle have the code increased by 10000
      COMMON /S_CSYDEC/ IDB(49), CBR(102), KDEC(612), LBARP(49)
      COMMON /S_PLIST/ NP, P(5000,5), LLIST(5000)
      COMMON /S_PLIST1/ LLIST1(5000)
      DIMENSION P0(5), LL(10), PD(10,5)
      NN = 1
      DO J=1,NP
         LLIST1(J) = 0
      ENDDO
      DO WHILE (NN .LE. NP)
         L= LLIST(NN)
         IF (IDB(IABS(L)) .GT. 0)  THEN
            DO K=1,5
              P0(K) = P(NN,K)
            ENDDO
            CALL DECPAR (L,P0,ND,LL,PD)
            LLIST(NN) = LLIST(NN)+ISIGN(10000,LLIST(NN))
            DO J=1,ND
               DO K=1,5
                  P(NP+J,K) = PD(J,K)
               ENDDO
               LLIST(NP+J)=LL(J)
               LLIST1(NP+J)=NN
            ENDDO
            NP=NP+ND
         ENDIF
         NN = NN+1
      ENDDO
      RETURN
      END
      SUBROUTINE DEC_DEBUG (L,P0, ND, LL, PD)
      COMMON /S_CNAM/ NAMP (0:49)
      CHARACTER*6 NAMP
      DIMENSION P0(5), LL(10), PD(10,5)
      ETOT = 0.
      DO J=1,ND
         ETOT = ETOT + PD(J,4)
      ENDDO
      WRITE(*,*)  NAMP(IABS(L)),' -> ', (NAMP(IABS(LL(J))),J=1,ND)
      WRITE(*,*)  ' Ei, Ef = ', P0(4), ETOT, ' L = ', L
      RETURN
      END
