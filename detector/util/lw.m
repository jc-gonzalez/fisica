##-*-octave-*-

d2r = pi/180;
r2d = 180/pi;

## create cloud of random points around an origin (x0,y0),
## with a given rotation, and a given weight

n = 1000;
angle = 90*d2r;
x0 = 0;
y0 = 10;
ampl=2;
offsetx = 2;
offsety = 1;

dtmp = randn(n,2);

dtmp(:,1) = dtmp(:,1)*ampl+offsetx;
dtmp(:,2) = dtmp(:,2)+offsety;
d = dtmp*rotation(angle);
dx = d(:,1);
dy = d(:,2);
w = 1000./(dx.^2+dy.^2+1);
dx = dx+x0;
dy = dy+y0;
d(:,1) = dx;
d(:,2) = dy;

##plot(dx,w,"@b");
##pause();
##plot(dy,w,"@m");
##pause();

grid();
plot([-10 30],[0 0],"r-",[0 0],[-10 30],"r-");
hold
gplot d w dots title 'Data points'

## calculate centroid

sw = sum(w);

xm = sum(dx.*w);
ym = sum(dy.*w);

x2m = sum(dx.*dx.*w);
xym = sum(dx.*dy.*w);
y2m = sum(dy.*dy.*w);

x3m = sum(dx.^3.*w);
x2ym = sum(dx.^2.*dy.*w);
xy2m = sum(dx.*dy.^2.*w);
y3m = sum(dy.^3.*w);

xm = xm/sw;
ym = ym/sw;

pm = [xm ym]

x2m = x2m/sw;
xym = xym/sw;
y2m = y2m/sw;

sx2 = x2m - xm.^2;
sxy = xym - xm.*ym;
sy2 = y2m - ym.^2;

## calculation of Hillas' parameters

d = sy2 - sx2;
zz = sqrt( d^2 + 4*sxy^2 );;
zu = 1.0 + d / zz;
zv = 2.0 - zu;

a = (d + zz) / (2*sxy);
b = ym - a * xm;

length = sqrt( (sx2 + 2*a*sxy + a*a*sy2) / (1+a*a) )
width  = sqrt( (sx2 - 2*a*sxy + a*a*sy2) / (1+a*a) )
dist   = sqrt( xm^2 + ym^2 )
##xdist  = sqrt( xmax^2 + ymax^2 )
azw    = sqrt( xm^2*y2m - 2*xm*ym*xym + x2m*ym^2 )
miss   = abs( b / sqrt(1 + a^2) )
alfa   = asin(miss/dist);
alpha  = alfa*r2d

gplot pm w p 0 72 title 'Center of gravity'

chi=atan2(ym,xm);
phi=alfa+chi;
ll=length*5;
ww=width*5;
xaxis=[xm-ll, ym-ll*tan(phi);  xm+ll, ym+ll*tan(phi)];  
yaxis=[xm-ww*tan(phi), ym+ww;  xm+ww*tan(phi), ym-ww];  
gplot xaxis w lines title "main axis", yaxis w lines title "secondary axis"

linetocm=[0,0;xm,ym];
gplot linetocm w lines title "line to CM"

## calculate maximum

nmax=10;

[sow, idx]=sort(-w);
sow=-sow;
xmax=0;
ymax=0;
smax=0;
for i=1:nmax,
  xmax=xmax+dx(idx(i))*sow(i);
  ymax=ymax+dy(idx(i))*sow(i);
  smax=smax+sow(i);
end
xmax=xmax/smax;
ymax=ymax/smax;
pmax=[xmax ymax]

gplot pmax w p 0 74 title 'Maximum'

## calculate displacement of pmax with respect to pm

pdis=(pm-pmax)*rotation(phi)
printf("Ratio: %f  (orig.: %f)\n", pdis(2)/pdis(1), offsety/offsetx);


hold off
