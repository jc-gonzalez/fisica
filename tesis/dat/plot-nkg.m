s = 0.5:0.1:1.6;
r = 0:0.1:1.5;

rho = zeros(size(r));

for i=1:max(size(s)),
  
  rho = nkg(s(i),r);

  plot(r, rho)

  pause();

end
