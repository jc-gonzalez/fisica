% warningcheck:=0;

verbatimtex
\documentclass[a4paper,12pt]{article}
%\usepackage[T1]{fontenc}
%\usepackage[latin1]{inputenc}
\usepackage{amsmath,amssymb}
\begin{document}
etex

vardef TEX primary s =
  write "verbatimtex"                    to "mptextmp.mp";
  write "\documentclass[12pt]{article}"  to "mptextmp.mp";
  write "\usepackage[T1]{fontenc}"       to "mptextmp.mp";
  write "\usepackage{amsmath,amssymb}"   to "mptextmp.mp";
  write "\usepackage[latin1]{inputenc}"  to "mptextmp.mp";
  write "\begin{document}"               to "mptextmp.mp";
  write "etex"                           to "mptextmp.mp";
  write "btex "&s&" etex"                to "mptextmp.mp";
  write EOF                              to "mptextmp.mp";
  scantokens "input mptextmp"
enddef;

vardef gamma(expr l,n,m) =
  save u;
  pair A,B,C,D;
  pair u,v;
  path g;

  A := (    0,0);
  B := (l*1cm,0);
  xpart D = (xpart B) / n;
  ypart C = (xpart D) / m; 
  xpart C = ypart D = 0;

  u = A;
  v = u shifted D;

  g := u{right} .. (1/4[u,v]+C){right} .. 1/2[u,v]{(1,-1)} 
                .. (3/4[u,v]-C){right} .. v{(1,1)};   
  u := v;
  v := v shifted D;

  for i=2 upto n-1:
    g := g .. (1/4[u,v]+C){right} .. 1/2[u,v]{(1,-1)} 
           .. (3/4[u,v]-C){right} .. v{(1,1)}; 
    u := v;
    v := v shifted D;
  endfor;

  g .. (1/4[u,v]+C){right} .. 1/2[u,v]{(1,-1)} 
         .. (3/4[u,v]-C){right} .. v{right} -- (v+(.2cm,0))
enddef;

vardef nuclei(expr n) =
  save u,v;
  save nucl;
  picture nucl;
  nucl := nullpicture;
  u := 2mm;
  pair v;
  for i=1 upto n:
    v := (u*uniformdeviate(1), u*uniformdeviate(1)) - (u/2,u/2);
    addto nucl contour fullcircle scaled 2mm shifted v withcolor white*uniformdeviate(1);
    addto nucl doublepath fullcircle scaled 2mm shifted v withpen pencircle scaled 0.1pt;
  endfor; 
  nucl
enddef;

vardef thefigure =

begingroup

  u := 1cm;

  path g[];
  g[1] := gamma(1,4,6);
  g[2] := gamma(1,8,3);
  g[3] := gamma(1,16,1.5);

  path f[];

  %for i=1 upto 8:
  %  drawarrow g[1] shifted (1cm,0) rotated (45*i) withpen pencircle;
  %  drawarrow g[2] shifted (2.5cm,0) rotated (45*i) withpen pencircle;
  %  drawarrow g[3] shifted (4cm,0) rotated (45*i) withpen pencircle;
  %endfor;

  pair a[];

  a[0] := (0,0); 

  a[1] = a[0] + (0,-3u);        %% Primary

  a[2] = a[1] + (-2u,-u);       %% subnuclei 1
  a[3] = a[1] + (-.8u,-2u);     %% pi+
  a[4] = a[1] + (1u,-2u);       %% subnuclei 2
  a[5] = a[1] + (2.5u,-1.8u);        %% pi-

  a[6] = a[2] + (-.5u,-1.2u);   %% pi0, gamma, gamma
  f[0] := g[1] rotated -130 shifted a[6];
  f[1] := g[2] rotated  -70 shifted a[6];

  a[7] := point length(f[0]) of f[0]; %% 
  a[8] := point length(f[1]) of f[1]; %% 
  a[9]  := a[7] + (0,-u) rotated -35;
  a[10] := a[7] + (0,-1.1u) rotated -5;
  a[11] := a[8] + (0,-1.6u) rotated -10;
  a[12] := a[8] + (0,-u) rotated 15;

  f[2] := g[2] rotated 110 shifted a[7];
  f[3] := g[1] rotated  80 shifted a[8];

  a[20] := a[3] + (.2u,-1.6u);      %% mu+
  a[21] := a[3] + (-.6u,-.8u);      %% numu

  a[22] := a[20] + ((0,-u) rotated -30);      %% anumu
  a[23] := a[20] + ((0,-1.6u) rotated 0);      %% e
  a[24] := a[20] + ((0,-1.1u) rotated 25);      %% nue

  a[30] := (a[4]+((0,-1.5u) rotated -10));  

  a[40] := a[5] + (1.5u,-.6u);      %% anumu
  a[41] := a[5] + (u,-1.8u);    %% mu-

  a[42] := a[41] + ((0,-1.1u) rotated 35);      %% anue
  a[43] := a[41] + ((0,-1.6u) rotated 10);      %% e
  a[44] := a[41] + ((0,-u) rotated -20);      %% numu

  xpart a[1001] = xpart 1/2[a[10],a[11]]; 
  ypart a[1001] = ypart 1/2[a[10],a[11]] - 1.5u;

  xpart a[1002] = xpart a[23]; 
  ypart a[1002] = ypart a[23] - 1.5u;
  
  xpart a[1003] = xpart a[43]; 
  ypart a[1003] = ypart a[43] - 1.5u;

  xpart a[1004] = xpart a[4]; 
  ypart a[1004] = ypart a[4] - 3.5u;

  picture n[];
  n[0] := nuclei(12);
  n[1] := nuclei(5);
  n[2] := nuclei(7);

  g[0] := a[0]--a[1];

  vardef drawmidarrow(expr p) =
    drawarrow subpath(0,0.5) of p;
    draw      subpath(0.5,1) of p;
  enddef;

  vardef drawdashedmidarrow(expr p) =
    drawarrow subpath(0,0.5) of p dashed evenly;
    draw      subpath(0.5,1) of p dashed evenly;
  enddef;

  drawmidarrow( a[0]--a[1] );    
  drawmidarrow( a[1]--a[2] );
  drawmidarrow( a[1]--a[3] );
    label.lft(btex $\pi^+$ etex, 1/2[a[1],a[3]]);
  drawmidarrow( a[1]--a[4] );
  drawmidarrow( a[1]--a[5] );
    label.urt(btex $\pi^-$ etex, 1/2[a[1],a[5]]);

  drawmidarrow( a[2]--a[6] );  
    label.lft(btex $\pi^0$ etex, 1/2[a[2],a[6]]);  
  drawarrow f[0];
    label.ulft(btex $\gamma$ etex, point length(f[0])/2 of f[0]);  
  drawarrow f[1];
    label.urt(btex $\gamma$ etex, point length(f[1])/2 of f[1]);  

  draw f[2] withcolor .5white;
  draw f[3] withcolor .5white;

  draw a[2]--(a[2]+(-u,-.5u));
  draw a[2]--(a[2]+(-1.2u,-.7u));
  draw a[2]--(a[2]+(.4u,-.5u));
  draw a[2]--(a[2]+(.3u,-.6u));

  drawmidarrow( a[7]--a[9] );
    label.lft(btex $e^-$ etex, 1/2[a[7],a[9]]);  
  drawmidarrow( a[7]--a[10] );
    label.rt(btex $e^+$ etex, 1/2[a[7],a[10]]);  
  drawmidarrow( a[8]--a[11] );
    label.lft(btex $e^+$ etex, 1/2[a[8],a[11]]);  
  drawmidarrow( a[8]--a[12] );
    label.rt(btex $e^-$ etex, 1/2[a[8],a[12]]);  
 
  drawmidarrow( a[3]--a[20] );
    label.rt(btex $\mu^+$ etex, 1/2[a[3],a[20]]);  
  drawdashedmidarrow( a[3]--a[21] );
    label.lft(btex $\nu_\mu$ etex, 1/2[a[3],a[21]]);  

  drawdashedmidarrow( a[20]--a[22] );
    label.lft(btex $\bar{\nu_\mu}$ etex, 1/2[a[20],a[22]]);  
  drawmidarrow( a[20]--a[23] );
    label.lft(btex $e^+$ etex, .8[a[20],a[23]]);  
  drawdashedmidarrow( a[20]--a[24] );
    label.rt(btex $\nu_e$ etex, 1/2[a[20],a[24]]);  

  draw a[4]--(a[4]+((0,-u) rotated -30));
  draw a[4]--a[30];
  draw a[4]--(a[4]+((0,-1.4u) rotated 25));
  draw a[4]--(a[4]+((0,-1.7u) rotated 5));
  draw a[30]--(a[30]+((0,-.7u) rotated -10));
  draw a[30]--(a[30]+((0,-u) rotated 5));
  draw a[30]--(a[30]+((0,-.8u) rotated 10));

  drawdashedmidarrow( a[5]--a[40] );
    label.urt(btex $\bar{\nu_\mu}$ etex, 1/2[a[5],a[40]]);  
  drawmidarrow( a[5]--a[41] );
    label.llft(btex $\mu^-$ etex, 1/2[a[5],a[41]]);  

  drawdashedmidarrow( a[41]--a[42] );
    label.rt(btex $\nu_\mu$ etex, 1/2[a[41],a[42]]);  
  drawmidarrow( a[41]--a[43] );
    label.lft(btex $e^-$ etex, .8[a[41],a[43]]);  
  drawdashedmidarrow( a[41]--a[44] );
    label.lft(btex $\bar{\nu_e}$ etex, 1/2[a[41],a[44]]);  

  draw n[0] shifted a[1];
  draw n[1] shifted a[2];
  draw n[2] shifted a[4];

  for i=0 upto 16:
    draw ((3mm,0)--(4.5mm,0)) rotated uniformdeviate(360) shifted a[1];
  endfor;

endgroup;
enddef;

beginfig(1)
		
  thefigure;
 
  label.lft(btex Primary etex rotated 90, 1/2[a[0],a[1]]);
  label.rt(btex cosmic ray etex rotated 90, 1/2[a[0],a[1]]);

  label(btex \footnotesize\vbox{\hbox{Electromagnetic}\hbox{~~~subshowers}} 
        etex rotated 90, a[1001]);
		 
  label(btex \footnotesize\vbox{\hbox{Electromagnetic}\hbox{~~~subshower}} 
        etex rotated 90, a[1002]);
		 
  label(btex \footnotesize\vbox{\hbox{Electromagnetic}\hbox{~~~subshower}} 
        etex rotated 90, a[1003]);
		 
  label(btex \footnotesize\vbox{\hbox{~Hadronic}\hbox{subshowers}} 
        etex rotated 90, a[1004]);

endfig;

beginfig(2)

  thefigure;
		 
  label.lft(btex Rayo c\'osmico etex rotated 90, 1/3[a[0],a[1]]);
  label.rt(btex primario etex rotated 90, 1/3[a[0],a[1]]);

  label(btex \footnotesize
        \vbox{\hbox{~~~~Subcascadas}\hbox{electromagn{\'{e}}ticas}} 
        etex rotated 90, a[1001]);
		 
  label(btex \footnotesize
        \vbox{\hbox{~~~~Subcascada}\hbox{electromagn{\'{e}}tica}} 
        etex rotated 90, a[1002]);
		 
  label(btex \footnotesize
        \vbox{\hbox{~~~~Subcascada}\hbox{electromagn{\'{e}}tica}} 
        etex rotated 90, a[1003]);
		 
  label(btex \footnotesize
        \vbox{\hbox{Subcascadas}\hbox{hadr{\'{o}}nicas}} 
        etex rotated 90, a[1004]);

endfig;
end.
