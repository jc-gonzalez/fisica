## -*- octave -*-

s=0.4:0.1:1.8;
ns=max(size(s));
sconstant=zeros(ns,11);

for i=1:11;
  x=4+2*i;
  tt=(2*x)./(3./s-1);
  sconstant(:,i)=logne(x,tt)';
end

plot(tt,sconstant);
