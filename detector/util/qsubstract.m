## Usage: q = qsubstract(q1, q2)
##
## Computes the difference of the quaternions q1 and q2
##
##   q1 - q2 =[s1-s2, V1-V2]
##

## Author: J C Gonzalez <gonzalez@gae.ucm.es>
## Created: December 2000

function q=qsubstract(q1, q2) 
  q.s = (q1.s - q2.s);
  q.v = (q1.v - q2.v);

## Local Variables:
## mode:octave
## End:
