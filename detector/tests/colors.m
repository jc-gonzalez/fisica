## -*- octave -*-

highC=45;lowC=10;

q=(0:100);

collin=zeros(size(q));
collog=zeros(size(q));

for i=1:max(size(q))
  if (q(i)<lowC)
    qq=lowC; 
  elseif (q(i)>=highC)
    qq=highC;
  else 
    qq=q(i);
  end
  collin(i) = ((31*(qq-lowC))/(highC-lowC));
  collog(i) = ((31*(log10(qq+1.)-log10(lowC+1.0)))
               /(log10(highC+1.0)-log10(lowC+1.0)));
end

plot(q,collin,"r-",q,collog,"b--")
[min(collin),max(collin)]
[min(collog),max(collog)]