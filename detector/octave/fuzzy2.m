## -*- octave -*-
######################################################################
##
## fuzzy
##
## @file        fuzzy.m
## @title       Studies on fuzzy logic for $\gamma$-h separation
## @subtitle    Use of Adaptive Fuzzy Systems
## @author      J C Gonz\'alez
## @email       gonzalez@mppmu.mpg.de
##
## @maintitle
######################################################################

## @tableofcontents 

## @section Studies on fuzzy logic for $\gamma$-h separation

## @text
## The aim of this small program is to study the possiblity of
## using Adaptive Fuzzy Systems to develope new methods on gamma-hadron
## separation, for Cherenkov Telescopes observation mainly.
## @endtext

## @code

## @section The code

disp("START");

## @subsection Definition of parameters of the simulation

## @code
shw=1;
npts=1000;
npts_lookup=5000;
fractions=0.2:.2:.8;
ntimes=1;
NL=4; 
NW=4; 
## @endcode

## @subsection Definition of global variables

## @code
global x_Lhadr s_Lhadr x_Lgamm s_Lgamm
global x_Whadr s_Whadr x_Wgamm s_Wgamm

x_Lhadr=.8;
s_Lhadr=.12;
x_Lgamm=1.0;
s_Lgamm=.10;

x_Whadr=.5;
s_Whadr=.10;
x_Wgamm=.4;
s_Wgamm=.08;
## @endcode

## @subsection Definition of functions

## @text
## \paragraph{Triangular function}
## Our membership functions ($\mu(x)$) will be {\bf triangular}
## (such a kind of membership function is shown in figure \ref{fig:triang}), 
## with $x_0-x_1 = a = b = x_2-x_0$ and $\mu(x_0)=1$.
## \begin{figure}[htbp]
##  \begin{center}
##    \includegraphics[width=0.4\textwidth]{triang.eps}
##    \caption{Triangular membership function.}
##    \label{fig:triang}
##  \end{center}
## \end{figure}
## @endtext

## @code
## triangular function

function y=tri(x,x0,a)
  y=max(1-abs(x-x0)./a,0);
endfunction
## @endcode

## @text
## \paragraph{Distributions of input variables}
## For this studies, we use two input variables, both for gammas and
## for hadrons. These variables are assigned the labels {\bf L} and 
## {\bf W} (yes, you are right, we were thinking on Length and Width\ldots\
## but that's another story).
## The distributions for these variables are taken to be 
## {\bf gaussian}, with the parameters defined in the section 
## for global variables.
## @endtext

## @code
## distributions of L for g and h

function y=Lhadr(x)
  global x_Lhadr s_Lhadr
  y=exp(-(x-x_Lhadr).^2./(2*s_Lhadr^2));
endfunction

function y=Lgamm(x)
  global x_Lgamm s_Lgamm
  y=exp(-(x-x_Lgamm).^2./(2*s_Lgamm^2));
endfunction

## distributions of W for g and h

function y=Whadr(x)
  global x_Whadr s_Whadr
  y=exp(-(x-x_Whadr).^2./(2*s_Whadr^2));
endfunction

function y=Wgamm(x)
  global x_Wgamm s_Wgamm
  y=exp(-(x-x_Wgamm).^2./(2*s_Wgamm^2));
endfunction

## first, create pairs for lookup table

## number of membership functions for each variable (in L and W)
## we calculate also the semi-width and the center of the triangle-
## membership functions
x1a=0.2; x1b=1.5;
NNL=2*NL+1;
xLwidth=(x1b-x1a)/(2*NL);
xLcenter=x1a+(0:2*NL)*xLwidth;

x2a=0.0; x2b=1.0;
NNW=2*NW+1;
xWwidth=(x2b-x2a)/(2*NW);
xWcenter=x2a+(0:2*NW)*xWwidth;

ya=-1; yb=1;
NRES=1; NNRES=2*NRES+1;
xRESwidth=(yb-ya)/(2*NRES);
xREScenter=ya+(0:2*NRES)*xRESwidth;

## define lookup table
lookup=zeros(NNL,NNW);
lookup_rules=zeros(NNL,NNW);

####################
## GAMMAS

## gammas L
clear rn
rn=randn(npts_lookup,1)';
dL=rn*s_Lgamm+x_Lgamm;
## gammas W
clear rn
rn=randn(npts_lookup,1)';
dW=rn*s_Wgamm+x_Wgamm;

if (shw==1)
  gset xrange [0:1.5]
  gset grid 
  hist(dL,0:.05:1.5);
  hold
  hist(dW,0:.05:1.5);
  hold off
endif

fsL=zeros(size(dL));
fsW=zeros(size(dW));
fsy=ones(size(dL));

fsy(:) = 3;

for k=1:npts_lookup,
  
  maxx=0.;
  x=dL(k);
  for l=1:NNL,
    t=tri(x,xLcenter(l),xLwidth);
    if (maxx < t),
      maxx = t;
      fsL(k) = l;
    end
  end
  maxx=0.;
  x=dW(k);
  for w=1:NNW,
    t=tri(x,xWcenter(w),xWwidth);
    if (maxx < t),
      maxx = t;
      fsW(k) = w;
    end
  end

  l=fsL(k);
  w=fsW(k);
  tt=(tri(dL(k),xLcenter(l),xLwidth)*
      tri(dW(k),xWcenter(w),xWwidth)*
      tri(xREScenter(fsy(k)),xREScenter(fsy(k)),xRESwidth));
  if (tt>lookup(l,w)),
    lookup(l,w) = tt;
    lookup_rules(l,w) = fsy(k);
  end
end

####################
## HADRONS

## hadrons L
clear rn
rn=randn(npts_lookup,1)';
dL=rn*s_Lhadr+x_Lhadr;
## hadrons W
clear rn
rn=randn(npts_lookup,1)';
dW=rn*s_Whadr+x_Whadr;

if (shw==1)
  hold
  hist(dL,0:.05:1.5);
  hist(dW,0:.05:1.5);
  hold off
endif

fsL=zeros(size(dL));
fsW=zeros(size(dW));
fsy=ones(size(dL));

fsy(:) = 1;

for k=1:npts_lookup,
  
  maxx=0.;
  x=dL(k);
  for l=1:NNL,
    t=tri(x,xLcenter(l),xLwidth);
    if (maxx < t),
      maxx = t;
      fsL(k) = l;
    end
  end
  maxx=0.;
  x=dW(k);
  for w=1:NNW,
    t=tri(x,xWcenter(w),xWwidth);
    if (maxx < t),
      maxx = t;
      fsW(k) = w;
    end
  end

  l=fsL(k);
  w=fsW(k);
  tt=(tri(dL(k),xLcenter(l),xLwidth)*
      tri(dW(k),xWcenter(w),xWwidth)*
      tri(xREScenter(fsy(k)),xREScenter(fsy(k)),xRESwidth));
  if (tt>lookup(l,w)),
    lookup(l,w) = tt;
    lookup_rules(l,w) = fsy(k);
  end
end

if (shw==1),
  lookup
  lookup_rules
end

## @comment
## \paragraph{hola}
## At this point we already have the lookup table.
## Let's start playing around
## @endcomment

qplus=zeros(max(size(fractions))*ntimes,1);
qminus=qplus;
ffr=qplus;

for k=1:max(size(fractions)),

  fr = fractions(k);

  for t=1:ntimes;

    kk = (k-1)*ntimes + t;

    ## generate set of events
    
    ngamm = npts*fr;
    nhadr = npts - ngamm;

    LL=[randn(ngamm,1).*s_Lgamm+x_Lgamm;
	randn(nhadr,1).*s_Lhadr+x_Lhadr];
    WW=[randn(ngamm,1).*s_Wgamm+x_Wgamm;
	randn(nhadr,1).*s_Whadr+x_Whadr];
    RES=zeros(size(LL));

    [m,n] = size(lookup);

    for p=1:npts,

      smuy=0;
      smu=0;

      for i=1:m,
	for j=1:n,
	  
	  lk=lookup(i,j);
	  lkr=lookup_rules(i,j);

	  if (lkr>0),
	    muo=(tri(LL(p),xLcenter(i),xLwidth)*
		 tri(WW(p),xWcenter(j),xWwidth));
	    smu = smu + muo;
	    smuy = smuy + muo*xREScenter(lkr);
	  endif

	end
      end
    
      RES(p) = smuy./smu;

      if (RES(p)>0),
	qplus(kk) = qplus(kk) + 1;
      else
	qminus(kk) = qminus(kk) + 1;
      end

    end

    ffr(kk) = fr;
    
    if (shw==1)
      printf("%10.6f %10d %10d %10d %10d\n", 
	     fr, qplus(kk), ngamm, qminus(kk), nhadr);
    end

  end
end

## @endcode