## -*-octave-*-

print=1;

r2d=180/pi;
d2r=1./r2d;

d1=36;
d2=11;
d=1./d1;

theta1=0:.0001:pi/4;
ntheta=max(size(theta1));
ctheta1=cos(theta1);
mu=ctheta1;

figure(0)
delta=mu.^4+mu.^2*(1-d^2);

gamma1=(-d+sqrt(delta))./(mu.^2-1);
gamma2=(-d-sqrt(delta))./(mu.^2-1);

gamma2(1)=2.*gamma2(2); # avoid infinity 

gamma=gamma2;

beta=sqrt(1-1./gamma.^2);

ctheta2=(-d1+d2+d1*beta.*ctheta1)./(d2*beta);
theta2=acos(ctheta2);

d2=gamma.*(1-beta.*ctheta2);
d2=1./d2;

if (print>0),
  gset output "df3.ps"
  gset term postscript
end

gset multiplot
gset title "Doppler-Factor Studies"

subplot(2,2,1);grid();
settitle("Variation of Lorenz factor with assumed theta1",
         "theta1","log10(gamma)",0);
#plot(theta1*r2d,log10(gamma))
semilogy(theta1*r2d,gamma)

#pause();

subplot(2,2,3);grid();
settitle("Variation of velocity with assumed theta1",
         "theta1","beta",0);
plot(theta1*r2d,beta,"1");

#pause();

subplot(2,2,2);grid();
settitle("Value of theta2 from assumed theta1",
         "theta1","theta2",0);
plot(theta1*r2d,theta2*r2d,'1');

#pause();

subplot(2,2,4);grid();
settitle("Displacement of theta2 from assumed theta1",
         "theta1","theta2-theta1",0);
plot(theta1*r2d,(theta2-theta1)*r2d,'1');

gset nomultiplot

if (print>0),
  gset term x11
  gset output
end

