## Usage: q = qconjugate(q1, q2)
##
## Computes the conjugate of the quaternion q1
##
##   conj(q1) =[s1, -V1]
##

## Author: J C Gonzalez <gonzalez@gae.ucm.es>
## Created: December 2000

function q=qconjugate(q1) 
  q.s =  q1.s;
  q.v = -q1.v;

## Local Variables:
## mode:octave
## End:
