## -*- octave -*-
##
## Luminosity distance, for $q_0$=1/2 and $\Lambda$=0 cosmology
##
function y=dl(z,H0)
c=299792.4580;
y=2*c*(z+1-sqrt(z+1))/H0;
return

