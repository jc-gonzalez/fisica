## -*- octave -*-

disp("START");

global x_hadr s_hadr x_gamm s_gamm

x_hadr=.8;
s_hadr=.15;
x_gamm=1.0;
s_gamm=.05;

function y=hadr(x)
  global x_hadr s_hadr
  y=zeros(size(x));
  for i=1:max(size(x));
    if (x(i)<x_hadr)
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
    if (x(i)>x_gamm)
      y(i)=1;
    else
      y(i)=exp(-(x(i)-x_gamm).^2./(2*s_gamm^2));
    end
  end
endfunction


shw=1;

fractions=0.1;
q=zeros(size(fractions));

npts=10000;

for k=1:max(size(fractions)),

  fr = fractions(k);
  x=(0.4:.005:1.4)';
  
  y1=gamm(x);
  y2=hadr(x);

  

  if (shw==1); 
    plot(x,y1,"r",x,y2,"b"); 
    hold
    plot(x,exp(-(x-x_gamm).^2./(2*s_gamm^2)),"r.",
	 x,exp(-(x-x_hadr).^2./(2*s_hadr^2)),"b."); 
    hold off
  end
  
  ##  fr=.3;
  ngamm=npts*fr;
  nhadr=npts-ngamm;
  
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
  
  ## on
  don=[d1,d2];
  
  ## off
  clear rn
  rn=randn(npts,1)';
  xdat=x_hadr;
  sdat=s_hadr;
  doff=rn*sdat+xdat;
  
  [nnon,xxon]=hist(don,100);
  [nnoff,xxoff]=hist(doff,100);
  ##  maxx=1.1*max(max(nnon),max(nnoff));
  ##  nnon=nnon./maxx;
  ##  nnoff=nnoff./maxx;
  nnon=nnon./sum(nnon);
  nnoff=nnoff./sum(nnoff);
  nnon=nnon./(1.1*max(nnon));
  nnoff=nnoff./(1.1*max(nnoff));

  if (shw==1), 
    ngamm
    nhadr
    npts
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
    if ( yon(i) > 0.5 ),
      nyon(i)=yon(i);
      nokon=nokon+1;
    end
    if ( yoff(i) > 0.5 ),
      nyoff(i)=yoff(i);
      nokoff=nokoff+1;
    end
  end
  
  if (shw==1), 
    hold; plot(ndon,nyon/2+.25,"go",ndoff,nyoff/2+.25,"co"); hold off; 
  end
  
  q(k) = (nokon) / sqrt(nokon + nokoff);

  printf("%f : %f / SQRT( %f ) = %f \n", fr,
	 nokon, 
	 nokoff, 
	 q(k));
  
end
