## -*- octave -*-

disp("START");

global x_hadr s_hadr x_gamm s_gamm

x_hadr=0.35957;
s_hadr=0.11657;
x_gamm=0.26166;
s_gamm=0.030186;

function y=hadr(x)
  global x_hadr s_hadr
  y=zeros(size(x));
  for i=1:max(size(x));
    if (x(i)>x_hadr)
      y(i)=1;
    else
      y(i)=exp(-(x(i)-x_hadr).^2./(2*s_hadr^2));
    end
  end
endfunction

function y=gamm(x)
  global x_gamm s_gamm
  y=zeros(size(x));
  for i=1:max(size(x));
    if (x(i)<x_gamm)
      y(i)=1;
    else
      y(i)=exp(-(x(i)-x_gamm).^2./(2*s_gamm^2));
    end
  end
endfunction


shw=1;

fractions=0.5;
q=zeros(size(fractions));

npts=10000;

for k=1:max(size(fractions)),

  fr = fractions(k);
  x=(0.:.005:1.)';
  
  y1=gamm(x);
  y2=hadr(x);  

  if (shw==1); 
    plot(x,y1,"r",x,y2,"b"); 
    hold
    plot(x,exp(-(x-x_gamm).^2./(2*s_gamm^2)),"r.",
	 x,exp(-(x-x_hadr).^2./(2*s_hadr^2)),"b."); 
    hold off
  end
  
  ngamm = npts;
  nhadr = npts;

  ## hadrons
  clear rn
  rn=randn(nhadr,1)';
  xdat=x_hadr;
  sdat=s_hadr;
  d1=rn*sdat+xdat;
  
  ## gammas
  clear rn
  rn=randn(ngamm,1)';
  xdat=x_gamm;
  sdat=s_gamm;
  d2=rn*sdat+xdat;
  
  doff=d1;
  don=d2;
  
  [nnon,xxon]=hist(don,x);
  [nnoff,xxoff]=hist(doff,x);
  printf("%f %f\n", max(nnon),max(nnoff));
  maxx=max(max(nnon),max(nnoff));
  nnon=nnon./maxx;
  nnoff=nnoff./maxx;

  if (shw==1), 
    hold; 
    stairs(xxon,nnon); 
    stairs(xxoff,nnoff); 
    hold off, 
  end
  
  ## do it with on
  
  ymu1=gamm(don);
  ymu2=1.-hadr(don);
  y=zeros(size(don));
  for i=1:max(size(y)); y(i)=max(0,ymu1(i)+ymu2(i)-1); end
  yon=y;
  
  ## do it with off
  
  ymu1=gamm(doff);
  ymu2=1.-hadr(doff);
  y=zeros(size(doff));
  for i=1:max(size(y)); y(i)=max(0,ymu1(i)+ymu2(i)-1); end
  yoff=y;
  
  
  if (shw==1), hold; plot(don,yon,"r+",doff,yoff,"b+"); hold off; end
  
  ndon=don; nyon=zeros(size(yon));
  ndoff=doff; nyoff=zeros(size(yoff));
  nokon=0; nokoff=0;
  
  for i=1:npts
    if ( yon(i) > 0 ),
      nyon(i)=yon(i);
      nokon=nokon+1;
    end
    if ( yoff(i) > 0 ),
      nyoff(i)=yoff(i);
      nokoff=nokoff+1;
    end
  end
  
  if (shw==1), 
    hold; plot(ndon,nyon/2+.25,"go",ndoff,nyoff/2+.25,"co"); hold off; 
  end
  
  q(k) = (nokon / ngamm) / sqrt(nokoff / nhadr);

  printf("%f : (%f/%f) / SQRT( %f/%f ) = %f \n", fr,
	 nokon,  ngamm, 
	 nokoff, nhadr, 
	 q(k));
  
end
