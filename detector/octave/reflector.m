## -*- octave -*-

global nmirror

if (1 == 1),
  system("rm reflector.diary")
  diary reflector.diary
  diary on
endif

disp("start");

load -ascii -force m.oc

mirror=m(nmirror,:)'

theta = .2;printf("theta = %f\n", theta*180/pi);
phi = 0.;printf("phi   = %f\n", phi*180/pi);

x = mirror(5:7);x(3)=0;
x(1) = x(1)-mirror(7)*tan(theta);
x
r = [sin(theta) 0 cos(theta)]'

focal=488.;

omegact = omega(theta,phi)
omegaIct = omegaI(theta,phi)

xct = omegact*x
rct = omegact*r

omegamirror = omega(mirror(8),mirror(9))
omegaImirror = omegaI(mirror(8),mirror(9))

xmm = xct - mirror(5:7)
xm = omegamirror*xmm
rm = omegamirror*rct
rm=rm./norm(rm)

a = 1.0; # (rm(1)^2 + rm(2)^2 + rm(3)^2) == 1.0
b = 2.0*(xm(1)*rm(1) + xm(2)*rm(2) + xm(3)*rm(3) - 2*focal*rm(3));
c = (xm(1)^2 + xm(2)^2 + xm(3)^2 - 4*focal*xm(3));

t1=(-b+sqrt(b*b-4.0*a*c))/(2*a);
t2=(-b-sqrt(b*b-4.0*a*c))/(2*a);
t=t2;
## xm + rm * t1
## xm + rm * t2
xcut = xm + rm * t

rnor = [2*xcut(1) 2*xcut(2) -2*(2*focal-xcut(3))]';
rnor = -rnor./norm(rnor)

calpha = abs(rnor'*rm);
printf("\nalpha = %f\n",acos(calpha)*180/pi);

rnor=rnor.*calpha

rrefl=2*rnor - rm;
rrefl=rrefl./norm(rrefl)

rnor=rnor./norm(rnor);

xcutct = omegaImirror*xcut + mirror(5:7)
rreflct = omegaImirror*rrefl
t=(focal-xcutct(3))/rreflct(3);
xcam = xcutct + rreflct*t

angle=90-abs(asin(rreflct(3))*180/pi)

disp("\n\n>>>>>>>>>>>>>>> resultados <<<<<<<<<<<<<<<\n");
disp("\nrm =\n");disp(rm');
disp("\nomegaImirror*rnor =\n");disp((omegaImirror*rnor)');
disp("\nmirror(10:12) =\n");disp(mirror(10:12)');
disp("\nrreflct =\n");disp(rreflct');
disp("\nxcam =\n");disp(xcam');

disp("end");
