## -*-octave-*-

theta1=0:.001:pi/12;
ctheta1=cos(theta1);

d1=36;
d2=11;

r2d=180/pi;

# betaf1=(d1^2*ctheta1);
# betaf2=sqrt(1-d1^2+d1^2*ctheta1.^2);
# betaf3=(1+d1^2*ctheta1.^2);

# beta1=(betaf1+betaf2)./betaf3;
# beta2=(betaf1-betaf2)./betaf3;
# beta=beta1;

a=(1-ctheta1.^2);
b=(-2/d1).*ones(size(a));
c=(1/d1^2+ctheta1.^2);
gamma=zeros(size(a));

n=max(size(a));

for i=1:n;
  aa=a(i);
  bb=b(i);
  cc=c(i);
  if (aa>0),
    gamma(i) = (-bb-sqrt(bb^2-4*aa*cc))/(2*aa);
  else
    gamma(i) = -cc/bb;
  end
end

beta=sqrt(1-1./gamma.^2);

ctheta2=(-d1+d2+d1*beta.*ctheta1)./(d2*beta);
theta2=acos(ctheta2);

grid();

xlabel("theta1");ylabel("beta");
title("variation of velocity with assumed theta1");
plot(theta1*r2d,beta,'1');
pause();

xlabel("theta1");ylabel("theta2");
title("theta2 from assumed theta1");
plot(theta1*r2d,theta2*r2d,'1');

pause();

xlabel("theta1");ylabel("theta2");
title("displacement of theta2 from assumed theta1");
plot(theta1*r2d,(theta2-theta1)*r2d,'1');

