##-*-octave-*-
function s=polysurf(p)
  n=max(size(p));
  s=(p(n,1)*p(1,2)-p(n,2)*p(1,1));
  for i=1:n-1,
    s=s+(p(i,1)*p(i+1,2)-p(i,2)*p(i+1,1));
  end
  s=abs(s)/2;
endfunction