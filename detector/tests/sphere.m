## -*- octave -*-

itheta=0:5:180;
iphi=0:5:360;

d2r=pi/180;

t  = theta*d2r;
p  = phi*d2r;

inct = itheta*d2r;
incp = iphi*d2r;

n=max(size(iphi));
m=max(size(itheta));
l=n*m;
q=zeros([l,3]);

for i=1:m,

  it = inct(i);

  for j=1:n,

    ip = incp(j);

    l=(i-1)*n+j;

    q(l,1) = sin( it ) * cos( ip );
    q(l,2) = sin( it ) * sin( ip );
    q(l,3) = cos( it );

  end

end

