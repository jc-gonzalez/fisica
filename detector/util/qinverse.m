## Usage: q = qproduct(q1)
##
## Computes the inverse of a quaternion
##
##   q^-1 =[s, -v] / |q|
##  

## Author: J C Gonzalez <gonzalez@gae.ucm.es>
## Created: December 2000

function q=qinverse(q1)

  f = qabs(q1);
  q.s =  q1.s / f;
  q.v = -q1.v / f;

## Local Variables:
## mode:octave
## End:
