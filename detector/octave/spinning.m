## -*- octave -*-

disp("start");

format long

## data

z=zeros(91,1);

#for i=5:90,

theta  = d2r( 10 );
phi    = d2r( 0.0 );
thetas = d2r( 2.0 );
phis   = d2r( 45.4385485866 );

## obtain thetap and phip

ct=cos(theta);
st=sqrt(1-ct*ct);
cts=cos(thetas);
sts=sqrt(1-cts*cts);

#thetap = acos(ct*cts + st*sts*cos(phis));
#phip   = asin(sin(thetas)*sin(phis)/sin(thetap));

u = atan(tan(thetas)*cos(phis));
phip = atan2(tan(phis)*sin(u),sin(theta-u));
thetap = atan2(tan(theta-u),cos(phip));

##

printf("Theta   = %f\nPhi     = %f\n", r2d(theta ), r2d(phi ));
printf("Theta_s = %f\nPhi_s   = %f\n", r2d(thetas), r2d(phis));
printf("Theta_p = %f\nPhi_p   = %f\n", r2d(thetap), r2d(phip));

#z(i+1) = r2d(phip);

#end

disp("end");

