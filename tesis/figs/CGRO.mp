beginfig(1)

numeric u;
u=2cm;
z.a=(0,0);
z.b=(7u,0);
z.c=(7u,2u);
z.d=(0,2u);
z11=.25[z.a,z.d]; z12=.5[z.a,z.d]; z13=.75[z.a,z.d]; 
z21=.25[z.b,z.c]; z22=.5[z.b,z.c]; z23=.75[z.b,z.c];

x0=0;    x1=2.2u;
x2=1u;   x3=3u;
x4=2u;   x5=3.6u;
x6=3.5u; x7=6.5u;

fill z.a--z.b--z.c--z.d--cycle
withcolor (1.00,1.00,0.94);  % ivory1

fill (x0,y.d)--(x1,y.d)--(x1,y13)--(x0,y13)--cycle
withcolor (1.00,0.65,0.00);  % orange
fill (x2,y13)--(x3,y13)--(x3,y12)--(x2,y12)--cycle
withcolor (0.20,0.80,0.20);  % LimeGreen
fill (x4,y12)--(x5,y12)--(x5,y11)--(x4,y11)--cycle
withcolor (0.28,0.82,0.80);  % MediumTurquoise
fill (x6,y11)--(x7,y11)--(x7,y.a)--(x6,y.a)--cycle 
withcolor (0.48,0.41,0.93);  % MediumSlateBlue

draw z11--z21 withcolor 0.5 white; 
draw z12--z22 withcolor 0.5 white; 
draw z13--z23 withcolor 0.5 white; 

draw (x0,y.d)--(x1,y.d)--(x1,y13)--(x0,y13)--cycle withcolor 0.2 white;
draw (x2,y13)--(x3,y13)--(x3,y12)--(x2,y12)--cycle withcolor 0.2 white;
draw (x4,y12)--(x5,y12)--(x5,y11)--(x4,y11)--cycle withcolor 0.2 white;
draw (x6,y11)--(x7,y11)--(x7,y.a)--(x6,y.a)--cycle withcolor 0.2 white;

label.rt("BATSE",   .5[(x1,y.d),(x1,y13)]);
label.rt("OSSE",    .5[(x3,y13),(x3,y12)]);
label.rt("COMPTEL", .5[(x5,y12),(x5,y11)]);
label.lft("EGRET",  .5[(x6,y11),(x6,y.a)]);

z51=(0 , y.d+.08*u); draw (x51, y.d)--z51;
label.top(btex \vbox{ \hbox{$\displaystyle 10^4$ eV}\hbox{10 keV}} etex, z51);
z52=(1u, y.d+.08*u); draw (x52, y.d)--z52;
label.top(btex \vbox{ \hbox{$\displaystyle 10^5$ eV}\hbox{100 keV}} etex,z52);
z53=(2u, y.d+.08*u); draw (x53, y.d)--z53;
label.top(btex \vbox{ \hbox{$\displaystyle 10^6$ eV}\hbox{1 MeV}} etex,  z53);
z54=(3u, y.d+.08*u); draw (x54, y.d)--z54;
label.top(btex \vbox{ \hbox{$\displaystyle 10^7$ eV}\hbox{10 MeV}} etex, z54);
z55=(4u, y.d+.08*u); draw (x55, y.d)--z55;
label.top(btex \vbox{ \hbox{$\displaystyle 10^8$ eV}\hbox{100 MeV}} etex,z55);
z56=(5u, y.d+.08*u); draw (x56, y.d)--z56;
label.top(btex \vbox{ \hbox{$\displaystyle 10^9$ eV}\hbox{1 GeV}} etex,  z56);
z57=(6u, y.d+.08*u); draw (x57, y.d)--z57;
label.top(btex \vbox{ \hbox{$\displaystyle 10^{10}$ eV}\hbox{10 GeV}} etex, z57);
z58=(7u, y.d+.08*u); draw (x58, y.d)--z58;
label.top(btex \vbox{ \hbox{$\displaystyle 10^{11}$ eV}\hbox{100 GeV}} etex,z58);

pickup pencircle scaled 1.5;

draw z.a--z.b--z.c--z.d--cycle;

endfig;

beginfig(2)

numeric u;
u=2cm;
z.a=(0,0);
z.b=(7u,0);
z.c=(7u,2u);
z.d=(0,2u);
z11=.25[z.a,z.d]; z12=.5[z.a,z.d]; z13=.75[z.a,z.d]; 
z21=.25[z.b,z.c]; z22=.5[z.b,z.c]; z23=.75[z.b,z.c];

x0=0;    x1=2.2u;
x2=1u;   x3=3u;
x4=2u;   x5=3.6u;
x6=3.5u; x7=6.5u;

fill z.a--z.b--z.c--z.d--cycle
withcolor (1.00,1.00,0.94);  % ivory1

fill (x0,y.d)--(x1,y.d)--(x1,y13)--(x0,y13)--cycle
withcolor (1.00,0.65,0.00);  % orange
fill (x2,y13)--(x3,y13)--(x3,y12)--(x2,y12)--cycle
withcolor (0.20,0.80,0.20);  % LimeGreen
fill (x4,y12)--(x5,y12)--(x5,y11)--(x4,y11)--cycle
withcolor (0.28,0.82,0.80);  % MediumTurquoise
fill (x6,y11)--(x7,y11)--(x7,y.a)--(x6,y.a)--cycle 
withcolor (0.48,0.41,0.93);  % MediumSlateBlue

draw z11--z21 withcolor 0.5 white; 
draw z12--z22 withcolor 0.5 white; 
draw z13--z23 withcolor 0.5 white; 

draw (x0,y.d)--(x1,y.d)--(x1,y13)--(x0,y13)--cycle withcolor 0.2 white;
draw (x2,y13)--(x3,y13)--(x3,y12)--(x2,y12)--cycle withcolor 0.2 white;
draw (x4,y12)--(x5,y12)--(x5,y11)--(x4,y11)--cycle withcolor 0.2 white;
draw (x6,y11)--(x7,y11)--(x7,y.a)--(x6,y.a)--cycle withcolor 0.2 white;

label.rt("BATSE",   .5[(x1,y.d),(x1,y13)]);
label.rt("OSSE",    .5[(x3,y13),(x3,y12)]);
label.rt("COMPTEL", .5[(x5,y12),(x5,y11)]);
label.lft("EGRET",  .5[(x6,y11),(x6,y.a)]);

z51=(0 , y.d+.08*u); draw (x51, y.d)--z51;
label.top(btex \vbox{ \hbox{$\displaystyle 10^4$ eV}\hbox{10 keV}} etex, z51);
z52=(1u, y.d+.08*u); draw (x52, y.d)--z52;
label.top(btex \vbox{ \hbox{$\displaystyle 10^5$ eV}\hbox{100 keV}} etex,z52);
z53=(2u, y.d+.08*u); draw (x53, y.d)--z53;
label.top(btex \vbox{ \hbox{$\displaystyle 10^6$ eV}\hbox{1 MeV}} etex,  z53);
z54=(3u, y.d+.08*u); draw (x54, y.d)--z54;
label.top(btex \vbox{ \hbox{$\displaystyle 10^7$ eV}\hbox{10 MeV}} etex, z54);
z55=(4u, y.d+.08*u); draw (x55, y.d)--z55;
label.top(btex \vbox{ \hbox{$\displaystyle 10^8$ eV}\hbox{100 MeV}} etex,z55);
z56=(5u, y.d+.08*u); draw (x56, y.d)--z56;
label.top(btex \vbox{ \hbox{$\displaystyle 10^9$ eV}\hbox{1 GeV}} etex,  z56);
z57=(6u, y.d+.08*u); draw (x57, y.d)--z57;
label.top(btex \vbox{ \hbox{$\displaystyle 10^{10}$ eV}\hbox{10 GeV}} etex, z57);
z58=(7u, y.d+.08*u); draw (x58, y.d)--z58;
label.top(btex \vbox{ \hbox{$\displaystyle 10^{11}$ eV}\hbox{100 GeV}} etex,z58);

pickup pencircle scaled 1.5;

draw z.a--z.b--z.c--z.d--cycle;

endfig;

end;