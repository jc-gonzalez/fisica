## -*- octave -*-
function r=rotx(alpha)
r=[1 0 0;0 cos(alpha) -sin(alpha);0 sin(alpha) cos(alpha)];
