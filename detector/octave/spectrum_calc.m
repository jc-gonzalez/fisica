## -*- octave -*-

nshowers = 200000;

lx=(0:.01:log10(30000))';
x=10.^lx;

## Integrate[ E^(-1.5), {x,1,30000} ] = 

function y=eint(a,b)
  y = (a^(-2.5) - b^(-2.5)) / 1.5;
endfunction

integ = eint(1,30000);

y=x.^(-2.5);

ypor = y ./ y(1) .* 100;
lypor = log10(ypor);

ynor = ypor ./ 100 .* nshowers;
lynor = log10(ynor);

#plot(lx,lynor,"ro");
plot(lx,ypor,"r-");

#data=[x,ypor];
#gset logscale xy;
#gset grid;
#gplot data;



