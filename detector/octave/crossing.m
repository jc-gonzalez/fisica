##-*-octave-*-
function pcross=crossing(a,b,c,d)
w=(b(1)-a(1))*(d(2)-c(2))-(b(2)-a(2))*(d(1)-c(1));
r=(a(2)-c(2))*(d(1)-c(1))-(a(1)-c(1))*(d(2)-c(2));
##s=(a(2)-c(2))*(b(1)-a(1))-(a(1)-c(1))*(b(2)-a(2));
pcross=a+r*(b-a)/w;
endfunction