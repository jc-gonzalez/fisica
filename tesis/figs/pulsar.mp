input boxes

myfontscale := 2;

ahlength := 7;

%vardef cuta(suffix a,b) expr p =
%  draw p cutbefore bpath.a cutafter bpath.b;
%%  drawarrow p cutbefore bpath.a cutafter bpath.b;
%  point .5*length p of p
%enddef;

vardef cuta(suffix a,b) expr p =
  path q;
  q = p cutbefore bpath.a cutafter bpath.b;
  drawarrow subpath (0,1) of q;
  draw      subpath (1,2) of q;
%  drawarrow p cutbefore bpath.a cutafter bpath.b;
  point .5*length p of p
enddef;

vardef self@# expr p =
  cuta(@#,@#) @#.c{curl.6}.. 
        tension .76 ..@#.c+p{down}.. tension .76 
        ..{curl.6}@#.c
enddef;



beginfig(1);

verbatimtex \def\stk#1#2{$\displaystyle{\matrix{#1\cr#2\cr}}$} etex

circleit.aa(btex \vbox{\hbox{Neutron}\hbox{~~~Star}} etex);
fill bpath.aa withcolor 0.05[white,black];
drawboxed(aa);

for i=30pt,40pt,55pt,75pt,110pt,160pt :
  draw self.aa(i,0);
  draw self.aa(-i,0);
endfor;

numeric a, b, c, d;
z0 = aa.c;

z0 = z21 + (0, 220pt) = z22 - (0, 220pt);
draw z21--z22 dashed evenly scaled 2;

path q.a;
for i=2 upto 6:
  c:=86+i*1;
  d:=30+i*15;  % = 90 for i=4
  a:=200*(cosd d);
  b:=200*(sind d);
%  drawarrow z0{dir c}..z0+(a,b) cutbefore bpath.aa;
%  draw z0{dir c}..z0+(a,b) cutbefore bpath.aa;
  q.a:=z0{dir c}..z0+(a,b) cutbefore bpath.aa;
  drawarrow subpath (0,.5) of q.a;
  draw      subpath (.5,1) of q.a;
  d:=30+i*15+180;
  a:=200*(cosd d);
  b:=200*(sind d);
%  drawarrow z0+(a,b)..z0{dir c} cutafter bpath.aa;
%  draw z0+(a,b)..z0{dir c} cutafter bpath.aa;
  q.a:=z0+(a,b)..z0{dir c} cutafter bpath.aa;
  drawarrow subpath (0,.5) of q.a;
  draw      subpath (.5,1) of q.a;
endfor;

z3 = (-200pt,-50pt);
z1 = z0 + z3; z0 = .5[z1, z2]; 

numeric k;
k:=angle -z3;

path p, q;
p = z1--z0--z2;
q = z0{curl.6}.. tension .76 ..z0+(-160pt,0){down}.. tension .76 ..{curl.6}z0;

z.b=p intersectionpoint q;
z0 = .5[z.a, z.b];

transform T;
T = identity rotatedaround(z0,-k);

picture pic;
pic = currentpicture transformed T;
currentpicture := nullpicture;

draw pic;

z0 = z4 + (0, 230pt) = z5 - (0, 230pt);
draw z4--z5 dashed evenly;

z.c = z.a transformed T;
z.d = z.b transformed T;

%dotlabel.urt(btex P etex scaled myfontscale,z.c);
draw z.c withpen pencircle scaled 8bp withcolor .5[white,black];
label.urt(btex P etex scaled myfontscale,z.c);

%dotlabel.llft(btex Q etex scaled myfontscale,z.d);
draw z.d withpen pencircle scaled 8bp withcolor 0.5[white,black];
label.llft(btex Q etex scaled myfontscale,z.d);

z.c = z6 + (0, 200pt) = z7 - (0, 200pt);
z.d = z8 + (0, 200pt) = z9 - (0, 200pt);
draw z6--z7 dashed evenly scaled 4;
draw z8--z9 dashed evenly scaled 4;

label.rt(btex Light Cylinder etex scaled myfontscale rotated -90, .7[z.c,z7]);

z.e = z0 + (0, 150pt);
z.f = z.e transformed T;

drawdblarrow z.e{right}..z.f dashed evenly;

label.urt(btex $\theta$ etex scaled myfontscale, .5[z.e,z.f]);

drawarrow z0+(-5pt,180pt)..z0+(-10pt,175pt)..
        z0+(0,169pt)..z0+(10pt,175pt)..z0+(5pt,180pt);

label.urt(btex $\Omega$ etex scaled myfontscale, z0+(5pt,180pt));

endfig;



beginfig(2);

verbatimtex \def\stk#1#2{$\displaystyle{\matrix{#1\cr#2\cr}}$} etex

circleit.aa(btex \vbox{\hbox{Estrella de}\hbox{neutrones}} etex);
fill bpath.aa withcolor 0.05[white,black];
drawboxed(aa);

for i=30pt,40pt,55pt,75pt,110pt,160pt :
  draw self.aa(i,0);
  draw self.aa(-i,0);
endfor;

numeric a, b, c, d;
z0 = aa.c;

z0 = z21 + (0, 220pt) = z22 - (0, 220pt);
draw z21--z22 dashed evenly scaled 2;

path q.a;
for i=2 upto 6:
  c:=86+i*1;
  d:=30+i*15;  % = 90 for i=4
  a:=200*(cosd d);
  b:=200*(sind d);
%  drawarrow z0{dir c}..z0+(a,b) cutbefore bpath.aa;
%  draw z0{dir c}..z0+(a,b) cutbefore bpath.aa;
  q.a:=z0{dir c}..z0+(a,b) cutbefore bpath.aa;
  drawarrow subpath (0,.5) of q.a;
  draw      subpath (.5,1) of q.a;
  d:=30+i*15+180;
  a:=200*(cosd d);
  b:=200*(sind d);
%  drawarrow z0+(a,b)..z0{dir c} cutafter bpath.aa;
%  draw z0+(a,b)..z0{dir c} cutafter bpath.aa;
  q.a:=z0+(a,b)..z0{dir c} cutafter bpath.aa;
  drawarrow subpath (0,.5) of q.a;
  draw      subpath (.5,1) of q.a;
endfor;

z3 = (-200pt,-50pt);
z1 = z0 + z3; z0 = .5[z1, z2]; 

numeric k;
k:=angle -z3;

path p, q;
p = z1--z0--z2;
q = z0{curl.6}.. tension .76 ..z0+(-160pt,0){down}.. tension .76 ..{curl.6}z0;

z.b=p intersectionpoint q;
z0 = .5[z.a, z.b];

transform T;
T = identity rotatedaround(z0,-k);

picture pic;
pic = currentpicture transformed T;
currentpicture := nullpicture;

draw pic;

z0 = z4 + (0, 230pt) = z5 - (0, 230pt);
draw z4--z5 dashed evenly;

z.c = z.a transformed T;
z.d = z.b transformed T;

%dotlabel.urt(btex P etex scaled myfontscale,z.c);
draw z.c withpen pencircle scaled 8bp withcolor .5[white,black];
label.urt(btex P etex scaled myfontscale,z.c);

%dotlabel.llft(btex Q etex scaled myfontscale,z.d);
draw z.d withpen pencircle scaled 8bp withcolor 0.5[white,black];
label.llft(btex Q etex scaled myfontscale,z.d);

z.c = z6 + (0, 200pt) = z7 - (0, 200pt);
z.d = z8 + (0, 200pt) = z9 - (0, 200pt);
draw z6--z7 dashed evenly scaled 4;
draw z8--z9 dashed evenly scaled 4;

label.rt(btex Cilindro de Luz etex scaled myfontscale rotated -90, .7[z.c,z7]);

z.e = z0 + (0, 150pt);
z.f = z.e transformed T;

drawdblarrow z.e{right}..z.f dashed evenly;

label.urt(btex $\theta$ etex scaled myfontscale, .5[z.e,z.f]);

drawarrow z0+(-5pt,180pt)..z0+(-10pt,175pt)..
        z0+(0,169pt)..z0+(10pt,175pt)..z0+(5pt,180pt);

label.urt(btex $\Omega$ etex scaled myfontscale, z0+(5pt,180pt));

endfig;


end;