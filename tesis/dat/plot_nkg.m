s = 0.5:0.1:1.6;
r = 0.01:0.01:1.5;

rho = zeros(size(r));

## La Palma
rm=106; # meters
t=800/37;
ne=10^(logne(15,t));

gset logscale y

for i=1:max(size(s)),
  
  rho = (ne/rm^2).*nkg(s(i),r);

  data=[r*106;rho]';
  gplot data w l 

  hold on

  pause();

end

hold off
