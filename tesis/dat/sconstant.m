## -*- octave -*-

s=0.4:0.1:1.8;
ns=max(size(s));
sconst=zeros(ns,11);
tsconst=zeros(ns,11);

for j=1:ns;
  sj=s(j);
  tt=zeros(11,1);
  sc=tt;
  for i=1:11;
    x=4+2*i;
    tt(i)=(2*x)/((3/sj)-1);
    sc(i)=logne(x,tt(i))';
  end
  tsconst(j,:) = tt';
  sconst(j,:) = sc';
end

for j=1:ns;
  plot(tsconst(j,:),sconst(j,:),"r-");
end
