##-*-octave-*-
function s=side(p,p1,p2)
v=p2-p1;
vp=p-p1;
s=sign(v(1)*vp(2) - vp(1)*v(2));
endfunction