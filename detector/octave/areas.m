## -*- octave -*-

## number of ellipses in the simulation
npts = 1000;

## define sigma for determination of orientation of ellipse
sigma = 5.;  # degrees

## generate random alphas
alpha = rand(npts,1)*sigma;
alpha = alpha*pi/180;

## define positions of center
maxx = 100;
minx = -maxx;
maxy = 100;
miny = -maxy;

x = rand(npts,1)*2+minx;
y = rand(npts,1)*2+miny;

## define my grid 100x100 
x1 = -60;
x2 = -x1;
y1 = -60;
y2 = -y1;

nx = 100;
ny = 100;

ix= (x2-x1)/nx;
iy= (y2-y1)/ny;

fsp5 = zeros([nx,ny]); # fspN  = > False Source Plot with alpha<N

## plot all the ellipses
function plotellipse(x,y,alpha,axislength)
  plot(x,y,'o');
  theta=atan2(y,x)+alpha;
  tantheta=tan(theta);
  dx= [ x-axislength             x+axislength         ];
  dy= [ y-axislength*tantheta    y+axislength*tantheta];
  plot(dx,dy,'r-');
endfunction

plot([minx maxx],[miny maxy],".");
hold

for i=1:npts;
  plotellipse(x(i),y(i),alpha(i),2); 
end

hold off


