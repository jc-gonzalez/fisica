## -*- octave -*-

theta=input("Theta? ");

phi  =input("Phi?   ");

ialpha=1:.5:10;
ibeta=0:5:360;

d2r=pi/180;

t  = theta*d2r;
p  = phi*d2r;
inct = ialpha*d2r;
incp = ibeta*d2r;

n=max(size(ibeta));
m=max(size(ialpha));
l=n*m;
k=zeros([n,4]);
q=zeros([l,3]);

for i=1:m,

  ia = inct(i);

  for j=1:n,

    ib = incp(j);

    if (ib < pi/2),
      beta1 = pi/2 - ib;
      iaf = -1;
      ibf = +1;
    elseif (ib < pi),
      beta1 = ib - pi/2;
      iaf = +1;
      ibf = +1;
    elseif (ib < 3*pi/2),
      beta1 = 3*pi/2 - ib;
      iaf = +1;
      ibf = -1;
    else
      beta1 = ib - 3*pi/2;
      iaf = -1;
      ibf = -1;
    endif
    
    beta2 = pi/2 - beta1;

    itheta = asin( sin(ia) * sin(beta1) );
    deltap = asin( sin(ia) * sin(beta2) );

    newtheta = t + itheta * iaf;

    iphi = deltap / sin( newtheta );
    newphi = p + iphi * ibf;
    
##    printf("%6.2f %6.2f -> %8.4f %8.4f\n", 
##           ia./d2r, ib./d2r, itheta./d2r, iphi./d2r);

    k(j,1) = ia./d2r;
    k(j,2) = ib./d2r;
    k(j,3) = itheta./d2r;
    k(j,4) = iphi./d2r;
    
    l=(i-1)*n+j;
    q(l,1) = sin( newtheta ) * cos( newphi );
    q(l,2) = sin( newtheta ) * sin( newphi );
    q(l,3) = cos( newtheta );

  end

end

save -ascii q.oc q
