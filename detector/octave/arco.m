##define apothema and radius
a = 30.0;
r = a/cos(30*pi/180);

## define mirrors
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
];
		   
## focal length of the frame, in cm
global focal = 488; 

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

  printf("%4d %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f\n", 
		 i, s, rho, theta, x, y, (x^2+y^2)/(4*focal)); 

end
