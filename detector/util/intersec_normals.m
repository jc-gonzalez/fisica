## Calculates intersections of reflected vectors (from rays coming from the
## infinity) with several camera planes, at different distances.

## Author: J C Gonzalez <gonzalez@gae.ucm.es>
## Created: December 2000

1;

heights = 1680:1720;

for i=1:920;

  d = scanf("%f",[12,1])';

  n  = d( 1);
  a  = d( 2); b  = d( 3); c  = d( 4);
  x  = d( 5); y  = d( 6); z  = d( 7);
  t  = d( 8); p  = d( 9);
  xn = d(10); yn = d(11); zn = d(12);

  vnormal = [xn, yn, zn];

  ##------------------------------------------------------------

  vinf = [0, 0, 1];
  
  h10km = 1.0e6; ## 10 km = 10^6 cm
  r = norm([x, y]);
  theta10km = atan2(r, h10km - z);
  v10km = [-sin(theta10km) * x / r, ...
	   -sin(theta10km) * y / r, ...
	    cos(theta10km) ];

  ##------------------------------------------------------------

  vincident = v10km;

  vrot = qrotate(vnormal, vincident, pi);

  ##------------------------------------------------------------

  j = 0;
  for h=heights;
    j++;
    visec = vrot * (h - z) / vrot(3) + [x, y, z];
    printf("%3d%11.4g%11.4g%11.4g\n", j, visec(1), visec(2), visec(3));
  endfor

endfor

## Local Variables:
## mode:octave
## End:
