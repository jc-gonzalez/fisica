## Usage: q = qrotate(u, v, theta)
##
## Rotates the vector v around the vector u an angle theta (in rad)
##
## u is assumed to be normalized
##
## if u is a vector, and theta the angle to rotate, we can associate 
## a quaternion to this rotation
##
##   q = [cos(theta/2), sin(theta/2) u] 
##
## and associating the pure quaternion to v
##
##   V = [0, v]
##
## then the rotated quaternion is
##
##   W = q^-1 V q = [0, w]
##
## being w the rotated vector around u an angle theta
##

## Author: J C Gonzalez <gonzalez@gae.ucm.es>
## Created: December 2000

function w=qrotate(u, v, theta) 

  q.s = cos(theta/2);
  q.v = sin(theta/2) * u;

  vv.s = -1;
  vv.v = v;

  ww = qproduct(q, qproduct(vv, qinverse(q)));

  w = ww.v;

## Local Variables:
## mode:octave
## End:
