## -*- octave -*-

1;

dat=readcol("longdist.param",6);

s=prod(size(dat));
nshw=s/6;

for i=1:nshw,
  
  p=dat(i,:)'

  if (p(2) != p(3)),

    t=10:10:800;
    
    n=longdist(t,p);
    
    plot(t,n);
    
    pause();
    
  end

end


  


