##-*-octave-*-

clear

print=1

d2r = pi/180;
r2d = 180/pi;

## create cloud of random points around an origin (x0,y0),
## with a given rotation, and a given weight

n = 2000;
angle = 45*d2r;
x0 = 8;
y0 = 4;
amplx=4;
amply=2;
offsetx = 2;
offsety = 0.5;

dtmp = randn(n,2);

dtmp(:,1) = dtmp(:,1)*amplx+offsetx;
dtmp(:,2) = dtmp(:,2)*amply+offsety;
d = dtmp*rotation(angle);
dx = d(:,1);
dy = d(:,2);
w = 1000./(dx.^2+dy.^2+1);
dx = dx+x0;
dy = dy+y0;
d(:,1) = dx;
d(:,2) = dy;

hold off

#figure(0)
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

zd = sy2 - sx2;
zz = sqrt( zd^2 + 4*sxy^2 );;
zu = 1.0 + zd ./ zz;
zv = 2.0 - zu;

a = (zd + zz) / (2*sxy);
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

chi = atan2(ym,xm);
phi = alfa+chi

ll = length*5;
ww = width*5;
## calculate vectors for axis x' and y'
vx = [1 0]*rotation(phi)
vy = [0 1]*rotation(phi)

## calculate vectors for axis x' and y'
xaxis1 = pm - ll*vx;
xaxis2 = pm + ll*vx;
yaxis1 = pm - ww*vy;
yaxis2 = pm + ww*vy;

xaxis = [xaxis1; xaxis2];
yaxis = [yaxis1; yaxis2];
gplot xaxis w lines title "main axis", yaxis w lines title "secondary axis"

linetocm = [0,0;xm,ym];
gplot linetocm w lines title "line to CM"

## calculate maximum

nmax = 100;

[sow, idx] = sort(-w);
sow = -sow;
xmax = 0;
ymax = 0;
smax = 0;
for i = 1:nmax,
  xmax = xmax+dx(idx(i))*sow(i);
  ymax = ymax+dy(idx(i))*sow(i);
  smax = smax+sow(i);
end
xmax = xmax/smax;
ymax = ymax/smax;
pmax = [xmax ymax]

gplot pmax w p 0 74 title 'Maximum'

if (print!=0), 
  gset output "octave0.ps"
  gset term postscript
  replot
end

## calculate displacement of pmax with respect to pm

pdis = (pm-pmax)*rotation(phi)

## calculate coordinates of points in new system

p = d;
p(:,1) = p(:,1)-pmax(1);
p(:,2) = p(:,2)-pmax(2);
p = p*rotation(phi);

## plot some information

figure(1);

subplot(2,2,1)
plot(dx,w,"@b");
subplot(2,2,2)
plot(dy,w,"@m");
subplot(2,2,3)
hist(p(:,1),40)
subplot(2,2,4)
hist(p(:,2),40)

if (print!=0), 
  gset output "octave1.ps"
  gset term postscript
  replot
end

multiplot(0,0);


## plot nice view of the quarters

hold off;

grid();
plot([-10 30],[0 0],"b-",[0 0],[-10 30],"b-");
hold

for i=1:n;

  xside = side(d(i,:), pmax+vx, pmax-vx);
  yside = side(d(i,:), pmax+vy, pmax-vy);


  if (xside>0 && yside>0), q1(i,:)=d(i,:);
  elseif (xside<0 && yside>0), q2(i,:)=d(i,:);
  elseif (xside<0 && yside<0), q3(i,:)=d(i,:);
  elseif (xside>0 && yside<0), q4(i,:)=d(i,:); end

end

gplot q1 w d title '1st Quarter'
gplot q2 w d title '2nd Quarter'
gplot q3 w d title '3rd Quarter'
gplot q4 w d title '4th Quarter'

xaxis1 = pmax - ll*vx;
xaxis2 = pmax + ll*vx;
yaxis1 = pmax - ww*vy;
yaxis2 = pmax + ww*vy;

xaxis = [xaxis1; xaxis2];
yaxis = [yaxis1; yaxis2];

gplot pm w p 0 72 title 'Center of gravity'
gplot linetocm w lines title "line to CM"
gplot xaxis w lines title "main axis", yaxis w lines title "secondary axis"
gplot pmax w p 0 74 title 'Maximum'


hold off

if (print!=0), 
  gset output "octave2.ps"
  gset term postscript
  replot
end

