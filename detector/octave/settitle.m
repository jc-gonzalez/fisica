#-*-octave-*-
function y=settitle(tit,x,y,doit)
title(tit);
xlabel(x);
ylabel(y);
if (doit>0),
  replot
end
end
