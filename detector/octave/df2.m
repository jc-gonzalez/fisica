## -*-octave-*-

r2d=180/pi;
d2r=1./r2d;

d1=36;
d2=11;
d=1./d1;

maxtheta=asin(d)+.1;

theta1=0:.0001:maxtheta;
ctheta1=cos(theta1);
mu=ctheta1;

beta1=(mu+d*sqrt(d^2+mu.^2-1))./(d^2+mu.^2);
beta2=(mu-d*sqrt(d^2+mu.^2-1))./(d^2+mu.^2);

figure(0)
plot(theta1*r2d,beta1,'r',theta1*r2d,beta2,'b')

pause();

delta=mu.^4+mu.^2*(1-d^2);

gamma1=(-d+sqrt(delta))./(mu.^2-1);
gamma2=(-d-sqrt(delta))./(mu.^2-1);

figure(0)
plot(theta1*r2d,gamma1)
pause();
beta1=sqrt(1-1./gamma1.^2);
plot(theta1*r2d,beta1)


return

beta=beta1

ctheta2=(-d1+d2+d1*beta.*ctheta1)./(d2*beta);
theta2=acos(ctheta2);


#pause();

figure(1)
grid();
xlabel("theta1");ylabel("beta");
title("variation of velocity with assumed theta1");
plot(theta1*r2d,beta,"1");

#pause();

figure(2)
grid();
xlabel("theta1");ylabel("theta2");
title("theta2 from assumed theta1");
plot(theta1*r2d,theta2*r2d,'1');

#pause();

figure(3)
grid();
xlabel("theta1");ylabel("theta2-theta1");
title("displacement of theta2 from assumed theta1");
plot(theta1*r2d,(theta2-theta1)*r2d,'1');

