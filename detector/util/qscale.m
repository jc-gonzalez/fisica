## Usage: q = qscale(q1, x)
##
## Multiplies a quaternion by a scale
##
##   x q1 =[x s1, x V1]
##

## Author: J C Gonzalez <gonzalez@gae.ucm.es>
## Created: December 2000

function q=qscale(q1, x) 
  q.s = x * q1.s;
  q.v = x * q1.v;

## Local Variables:
## mode:octave
## End:
