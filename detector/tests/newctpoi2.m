## -*- octave -*-

theta=input("Theta? ");

phi  =input("Phi?   ");

ialpha=[1 5 10];
ibeta=0:5:360;

d2r=pi/180;

t  = theta*d2r;
p  = phi*d2r;
inct = ialpha*d2r;
incp = ibeta*d2r;

n=max(size(ibeta));
m=max(size(ialpha));
l=n*m;
q=zeros([l,3]);

for i=1:m,

  ia = inct(i);

  for j=1:n,

    ib = incp(j);

    cos_newtheta = cos(t)*cos(ia) + sin(t)*sin(ia)*cos(ib);
    newtheta = acos( cos_newtheta );
    sin_iphi = sin(ia)*sin(ib) / sin(newtheta);
    cos_iphi = ( cos(ia) - cos_newtheta * cos(t) ) / ...
               ( sin(newtheta) * sin(t) );
    
    if ( (cos_iphi>=0) && (sin_iphi>=0) ),
      iphi = asin( sin_iphi );
    elseif ( (cos_iphi>0) && (sin_iphi<0) ),
      iphi = 2*pi - asin( sin_iphi ) ;
    elseif ( (cos_iphi<0) && (sin_iphi>0) ),
      iphi = pi - asin( sin_iphi );
    else
      iphi = pi - asin( abs(sin_iphi) );
    end
    
    if ( ib < pi ),
      ibf = +1;
    else
      ibf = -1;
    end

    newphi = p + iphi * ibf;
    
##    printf("%6.2f %6.2f -> %8.4f %8.4f\n", 
##           ia./d2r, ib./d2r, newtheta./d2r, newphi./d2r);

    l=(i-1)*n+j;
    q(l,1) = sin( newtheta ) * cos( newphi );
    q(l,2) = sin( newtheta ) * sin( newphi );
    q(l,3) = cos( newtheta );

  end

end

save -ascii q89.oc q
