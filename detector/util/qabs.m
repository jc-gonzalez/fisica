## Usage: q = qabs(q1)
##
## Computes the absolute value of the quaternion q1
##
##   abs(q1) = sqrt(norm(q1))
##

## Author: J C Gonzalez <gonzalez@gae.ucm.es>
## Created: December 2000

function y=qabs(q1)  
  y = sqrt(qnorm(q1));

## Local Variables:
## mode:octave
## End:
