1;

format long

f = 1700;

h = input('Enter h (km): ');

h = h * 1e5;

for i=1:920;

  d = scanf("%f",[12,1])';

  n  = d( 1);
  a  = d( 2); b  = d( 3); c  = d( 4);
  x  = d( 5); y  = d( 6); z  = d( 7);
  t  = d( 8); p  = d( 9);
  xn = d(10); yn = d(11); zn = d(12);

  vnormal = [xn, yn, zn];

  rho = norm([x, y]);

  if (h>0)

    l = h - z;
    theta = atan2(rho, l);
  
    d = f - z;
    beta = atan2(d, rho);

    alpha = (pi/2 - theta - beta) / 2;
    xi = alpha + theta;
  
  else

    d = f - z;
    beta = atan2(d, rho);

    xi = (pi/2 - beta) / 2;

  endif

  ux = -sin(xi) * x / rho;
  uy = -sin(xi) * y / rho;
  uz =  cos(xi);
 
  u = [ux, uy, uz];
 
 
 
#   t = -x/ux;
#   cz = t*uz+z;
#   c = [0,0,cz];
#   norm(c-[x,y,z]);
#   radius = norm(c-[x,y,z]);
#   fmirr = radius/2;
 
#  printf("%11.4g%11.4g%11.4g\n", u(1)/xn, u(2)/yn, u(3)/zn);
 
  printf("%3d %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f %12.8f %12.8f %12.8f %12.8f %12.8f\n",
           n, a, b, c, x, y, z, xi, p, u(1), u(2), u(3));

endfor
