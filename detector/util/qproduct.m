## Usage: q = qproduct(q1, q2)
##
## Computes the product of the quaternions q1 and q2
##
##   q1 q2 =[s1s2-V1oV2,(s1V2+s2V1+V1xV2)]
##  
##   where V1oV2 stands for VECTOR-SCALAR_PRODUCT
##   and   V1xV2 stands for VECTOR-CROSS-PRODUCT
##  
##   expanded out into components:
##  
##   s1s2 = a0b0, V1oV2 = a1b1+a2b2+a3b3,
##  
##   s1s2-V1oV2 = a0b0-a1b1-a2b2-a3b3
##  
##   s1V2 =  a0b1,a0b2,a0b3 ,
##   s2V1 =  b0a1,b0a2,b0a3 ,
##   V1xV2 = <(a2b3-b2a3),(b1a3-a1b3),(a1b2-b1a2)> = EijkV1jV2k

## Author: J C Gonzalez <gonzalez@gae.ucm.es>
## Created: December 2000

function q=qproduct(q1, q2) 

  q1q2 = dot(q1.v, q2.v);
  q1xq2 = cross(q1.v, q2.v);
  
  q.s = (q1.s * q2.s) - q1q2;
  q.v = (q1.s * q2.v) + (q2.s * q1.v) + q1xq2;

## Local Variables:
## mode:octave
## End:
