## -*- octave -*-

## focal length of the frame, in cm
##
##   m(i,1) = i;
##   m(i,2) = s;
##   m(i,3) = rho;
##   m(i,4) = theta;
##   m(i,5) = x;
##   m(i,6) = y;
##   m(i,7) = z;
##   m(i,8) = thetan;
##   m(i,9) = phin;
##   m(i,10) = xm;
##   m(i,11) = ym;
##   m(i,12) = zm;


global focal = 488; 

mirror = 32;

xcentro=m(mirror,5:7)

x=[xcentro(1),xcentro(2),0.0]
r=[ 0.0, 0.0, 1.0 ]

thetaCT=0.0
phiCT=0.0

omegaCT=omega(thetaCT,phiCT);
omegaICT=omegaI(thetaCT,phiCT);

xCT = (omegaCT * x')'
rCT = (omegaCT * r')'

thetaN=m(mirror,8)
phiN=m(mirror,9)

[thetaN,phiN]*180.0/pi
m(mirror,10:12)

omegaM=omega(thetaN,phiN);
omegaIM=omegaI(thetaN,phiN);

xm = (omegaM * (xCT-xcentro)')'
rm = (omegaM * rCT')'

a = 1.0
b = 2.0 * (xm*rm' - 2.0*focal*rm(3))
c = (xm*xm' - 4.0*focal*xm(3))

t = (-b - sqrt(b*b - 4.0*a*c)) / (2.0*a)

xcut = xm + rm*t
rnor = 2.0*xcut - [0.0,0.0,4.0*focal]
rnor = -rnor / norm(rnor)
(omegaIM * rnor')'

calpha = abs(rnor*rm')

rrefl = 2.0*rnor*calpha - rm
rrefl = rrefl / norm(rrefl)

xcutCT = (xcut*omegaIM)+xcentro
rreflCT = (rrefl*omegaIM)

t = (focal - xcutCT(3)) / rreflCT(3)
xcam = xcutCT + rreflCT*t
phi = asin (rreflCT(3))

#xcam = [(rreflCT(1)/rreflCT(3)) * (focal-xcutCT(3)) + xcutCT(1),
#	(rreflCT(2)/rreflCT(3)) * (focal-xcutCT(3)) + xcutCT(2),
#	focal]
#phi = asin (rreflCT(3))

