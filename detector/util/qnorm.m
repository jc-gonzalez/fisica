## Usage: q = qnorm(q1)
##
## Computes the norm of the quaternion q1
##
##   norm(q1) = s^2 + v.v
##

## Author: J C Gonzalez <gonzalez@gae.ucm.es>
## Created: December 2000

function y=qnorm(q1)  
  y = (q1.s * q1.s) + dot(q1.v, q1.v);

## Local Variables:
## mode:octave
## End:
