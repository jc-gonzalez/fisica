## -*- octave -*-

disp("START");

function y=ishadr(x)
  y=max(1-abs(x+1)./1,0);
endfunction

function y=isgamm(x)
  y=max(1-abs(x-1)./1,0);
endfunction

global x_Lhadr s_Lhadr x_Lgamm s_Lgamm

x_Lhadr=.8;
s_Lhadr=.10;
x_Lgamm=1.0;
s_Lgamm=.05;

function y=Lhadr(x)
  global x_Lhadr s_Lhadr
  y=exp(-(x-x_Lhadr).^2./(2*s_Lhadr^2));
endfunction

function y=Lgamm(x)
  global x_Lgamm s_Lgamm
  y=exp(-(x-x_Lgamm).^2./(2*s_Lgamm^2));
endfunction

function y=lLhadr(x)
  global x_Lhadr s_Lhadr
  y=Lhadr(x+s_Lhadr);
endfunction

function y=rLhadr(x)
  global x_Lhadr s_Lhadr
  y=Lhadr(x-s_Lhadr);
endfunction

function y=lLgamm(x)
  global x_Lgamm s_Lgamm
  y=Lgamm(x+s_Lgamm);
endfunction

function y=rLgamm(x)
  global x_Lgamm s_Lgamm
  y=Lgamm(x-s_Lgamm);
endfunction

global x_Whadr s_Whadr x_Wgamm s_Wgamm

x_Whadr=.8;
s_Whadr=.10;
x_Wgamm=1.0;
s_Wgamm=.05;

function y=Whadr(x)
  global x_Whadr s_Whadr
  y=exp(-(x-x_Whadr).^2./(2*s_Whadr^2));
endfunction

function y=Wgamm(x)
  global x_Wgamm s_Wgamm
  y=exp(-(x-x_Wgamm).^2./(2*s_Wgamm^2));
endfunction

function y=lWhadr(x)
  global x_Whadr s_Whadr
  y=Whadr(x+s_Whadr);
endfunction

function y=rWhadr(x)
  global x_Whadr s_Whadr
  y=Whadr(x-s_Whadr);
endfunction

function y=lWgamm(x)
  global x_Wgamm s_Wgamm
  y=Wgamm(x+s_Wgamm);
endfunction

function y=rWgamm(x)
  global x_Wgamm s_Wgamm
  y=Wgamm(x-s_Wgamm);
endfunction

alpha1=zeros(6,6);
alpha2=zeros(6,6);
z=zeros(6,1);
yl=[-.5 -1 -.5 0.5 1 0.5]';

shw=0;

fractions=0.02:.02:.98;
q=zeros(size(fractions));

npts=10000;

gset term postscript
gset output "foo.ps"

for k=1:max(size(fractions)),

  fr = fractions(k);
  x=(0.4:.005:1.4)';
  
  y1=Wgamm(x);
  y2=Whadr(x);
  yr1=rWgamm(x);
  yr2=rWhadr(x);
  yl1=lWgamm(x);
  yl2=lWhadr(x);

  if (shw==1); 
    plot(x,y1,"r",x,y2,"b"); 
    hold
    plot(x,yr1,"r+",x,yr2,"b+"); 
    plot(x,yl1,"rx",x,yl2,"bx"); 
    hold off
  end

  pause(10);

  ##  fr=.3;
  ngamm=npts*fr;
  nhadr=npts-ngamm;
  
  ## Lhadrons
  clear rn
  rn=randn(nhadr,1)';
  xdat=x_Lhadr;
  sdat=s_Lhadr;
  d1=rn*sdat+xdat;
  
  ## Lgammas
  clear rn
  rn=randn(ngamm,1)';
  xdat=x_Lgamm;
  sdat=s_Lgamm;
  d2=rn*sdat+xdat;
  
  ## on L
  donL=[d1,d2];

  ## Whadrons
  clear rn
  rn=randn(nhadr,1)';
  xdat=x_Whadr;
  sdat=s_Whadr;
  d1=rn*sdat+xdat;
  
  ## Wgammas
  clear rn
  rn=randn(ngamm,1)';
  xdat=x_Wgamm;
  sdat=s_Wgamm;
  d2=rn*sdat+xdat;
  
  ## on W
  donW=[d1,d2];

  defuzz=zeros(size(donL));

  for i=1:npts,
    xl=donL(i);
    xw=donW(i);
    for n=1:6,
      if (n==1) alpha1(:,n)=rLhadr(xl); end
      if (n==2) alpha1(:,n)=Lhadr(xl); end
      if (n==3) alpha1(:,n)=lLhadr(xl); end
      if (n==4) alpha1(:,n)=rLgamm(xl); end
      if (n==5) alpha1(:,n)=Lgamm(xl); end
      if (n==6) alpha1(:,n)=lLgamm(xl); end

      if (n==1) alpha2(n,:)=rWhadr(xw); end
      if (n==2) alpha2(n,:)=Whadr(xw); end
      if (n==3) alpha2(n,:)=lWhadr(xw); end
      if (n==4) alpha2(n,:)=rWgamm(xw); end
      if (n==5) alpha2(n,:)=Wgamm(xw); end
      if (n==6) alpha2(n,:)=lWgamm(xw); end

    end
    
    alpha=diag(alpha1.*alpha2);
    results=alpha;

    defuzz(i)=sum(results.*yl)./sum(results);

  end
  
  hist(donL,0:.2:1.6);
  hold; hist(donW,0:.2:1.6); hold off
  pause(5);
  hist(defuzz,100);
  pause(5);
  plot(donW.*defuzz,"x")
  [nn,xx]=hist(defuzz,[-1.5 -.5 .5 1.5]);
  pause(5);

  ng = nn(3)+nn(4);
  np=abs(ngamm-ng);
  q(k) = (ng/ngamm) / sqrt(np/ngamm);

  printf("%g : (%g / %g) / SQRT( %g / %g ) = %g \n", fr,
	 ng,ngamm, 
	 np,ngamm, 
	 q(k));
  
end

gset grid
gset yrange [0:8]
plot (fractions,q,"bo");
gset term x11
gset output
