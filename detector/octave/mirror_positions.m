## -*- octave -*-

##
## CT1-parabolic
## this is for a hypothetical CT1 with parabolic shape.
## DON'T USE IT FOR THE SIMULATION!!
##

##define apothema and radius
a = 30.0;
r = a/cos(30*pi/180);

m=zeros(33,12);
lines=zeros(33,2);
alpha=30*pi/180;
beta=30*pi/180;

function y=norm(x)
y=sqrt(x*x');
endfunction

## define mirrors for CT1
mirrors = [2*a   0;         # first ring
		   a     1.5*r;
		   -a    1.5*r;
		   -2*a  0;
		   -a    -1.5*r;
		   a     -1.5*r;
		   4*a   0;         # second ring
		   3*a   1.5*r;
		   2*a   3*r;
		   0     3*r;
		   -2*a  3*r;
		   -3*a  1.5*r;
		   -4*a  0;
		   -3*a  -1.5*r;
		   -2*a  -3*r;
		   0     -3*r;
		   2*a   -3*r;
		   3*a   -1.5*r;
		   5*a   1.5*r;     # third (incomplete) ring
		   4*a   3*r;
		   3*a   4.5*r;
		   1*a   4.5*r;
		   -1*a  4.5*r;
		   -4*a  3*r;
		   -5*a  1.5*r;
		   -6*a  0;
		   -5*a  -1.5*r;
		   -4*a  -3*r;
		   -1*a  -4.5*r;
		   1*a   -4.5*r;
		   3*a   -4.5*r;
		   4*a   -3*r;
		   5*a   -1.5*r
];

relativ_coords = [a 0.5*r;
				  0 r;
				  -a 0.5*r;
				  -a -0.5*r;
				  0 -r;
				  a -0.5*r
];

f1=fopen("mirror-positions.dat","w");

## focal length of the frame, in cm
global focal = 488; 

## define s as global variable
global s

## define h_enf as global variable
global h_enf = 1000000  # 10 Km in cm

## define arc
function y=arc(x),
  global focal
  y=sqrt(1+x^2/(4*focal^2)); 
endfunction

## loop over mirrors

[n,m] = size(mirrors);

for i=1:n,

  s = norm(mirrors(i,:));

  ## define function f to f-solve
  function y=f(r), 
	global s
	y=quad("arc",0,r)-s; 
  endfunction

  ## solve for rho

  rho = fsolve("f", s);
  theta=atan2(mirrors(i,2),mirrors(i,1));

  x = rho*cos(theta);
  y = rho*sin(theta);
  z = (x^2+y^2)/(4*focal);

  fprintf(f1,"%4d %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f ", 
		  i, s, rho, theta, x, y, z); 
  
  m(i,1) = i;
  m(i,2) = s;
  m(i,3) = rho;
  m(i,4) = theta;
  m(i,5) = x;
  m(i,6) = y;
  m(i,7) = z;

  psi=atan2(rho, h_enf - z);
  fi=atan2(rho, focal - z);
  xi=(psi+fi)/2;
  [psi fi xi]*180/pi;

  xm = -x;
  ym = -y;
  zm = rho/tan(xi);
  rm = sqrt(xm^2 + ym^2 +zm^2);
  xm = xm / rm;
  ym = ym / rm;
  zm = zm / rm;

  thetan = acos( zm );
  phin = atan2( ym/sin(thetan) , xm/sin(thetan) );

  fprintf(f1,"%11.8f %11.8f %11.8f %11.8f %11.8f\n", thetan, phin, xm, ym, zm);
  m(i,8) = thetan;
  m(i,9) = phin;
  m(i,10) = xm;
  m(i,11) = ym;
  m(i,12) = zm;

  lines(i,1) = ym*cos(beta) - xm*cos(alpha);
  lines(i,2) = zm - ym*sin(beta) - xm*sin(alpha);

end

fclose(f1);
