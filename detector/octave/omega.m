## -*- octave -*-
function r=omega(theta,phi)
r=rotz(phi)*roty(theta);