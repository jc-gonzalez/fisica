##-*-octave-*-

d2r = pi/180;
r2d = 180/pi;

## create cloud of random points around an origin (x0,y0),
## with a given rotation, and a given weight

n = 3000;
angle = 30*d2r;
x0 = 4;
y0 = 0.1;
ampl=2;
mux1 = -2;
muy1 = -1;
sigx1 = 1;
sigy1 = 0.5;
sigx2 = 2;
sigy2 = 1;

rx1 = randn(2*n/3,1).*sigx1+mux1;
rx2 = randn(n/3,1).*sigx2+0;
ry1 = randn(2*n/3,1).*sigy1+muy1;
ry2 = randn(n/3,1).*sigy2+0;

dtmp=[rx1 , ry1; rx2 , ry2];

d = dtmp*rotation(angle);
dx = d(:,1);
dy = d(:,2);
w = ones(size(dx));
dx = dx+x0;
dy = dy+y0;
d(:,1) = dx;
d(:,2) = dy;

grid();
plot([-5 15],[0 0],"r-",[0 0],[-5 15],"r-");
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
zu = 1.0 + zd / zz;
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

pause()

## convert points to new system

newd = d;
newd(:,1) = newd(:,1) - xmax;
newd(:,2) = newd(:,2) - ymax;
newd = newd*rotation(-phi);

gplot newd w dots title 'new Data points'

s=[0 0 0 0];
s2=[0 0 0 0];
st2=[0 0 0 0];
sw=[0 0 0 0];
for i=1:n,

  x = newd(i,1);
  y = newd(i,2);
  wei = w(i);

  ## X+
  if (x>0), 
    sw(1) = sw(1) + wei;
    s(1) = s(1) + wei*x;
    s2(1) = s2(1) + wei*x*x;
    st2(1) = st2(1) + 2*wei*x*x;
  end

  ## X-
  if (x<0), 
    sw(2) = sw(2) + wei;
    s(2) = s(2) + wei*x;
    s2(2) = s2(2) + wei*x*x;
    st2(2) = st2(2) + 2*wei*x*x;
  end

  ## Y+
  if (y>0), 
    sw(3) = sw(3) + wei;
    s(3) = s(3) + wei*y;
    s2(3) = s2(3) + wei*y*y;
    st2(3) = st2(3) + 2*wei*y*y;
  end

  ## Y-
  if (y<0), 
    sw(4) = sw(4) + wei;
    s(4) = s(4) + wei*y;
    s2(4) = s2(4) + wei*y*y;
    st2(4) = st2(4) + 2*wei*y*y;
  end

end

##std = (s2./sw) - (s./sw).^2
std = sqrt(st2./sw);

l1 = std(1);
l2 = std(2);
w1 = std(3);
w2 = std(4);

hold off

plot([-5 15],[0 0],"r-",[0 0],[-5 15],"r-");
hold on

gplot newd w p 0 0 title 'new Data points'

trapezoid = [l1 0;0 w1;-l2 0;0 -w2;l1 0];
trapezoid2 = trapezoid * 2;
trapezoid4 = trapezoid * 4;
trapezoid8 = trapezoid * 8;
 
gplot trapezoid w lines 
gplot trapezoid2 w lines 
gplot trapezoid4 w lines 
gplot trapezoid8 w lines 

hold off
