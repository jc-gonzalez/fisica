\input boxes

vardef cuta(suffix a,b) expr p =
  draw p cutbefore bpath.a cutafter bpath.b;
%  drawarrow p cutbefore bpath.a cutafter bpath.b;
  point .5*length p of p
enddef;

vardef self@# expr p =
  cuta(@#,@#) @#.c{curl.6}.. 
        tension .76 ..@#.c+p{down}.. tension .76 
        ..{curl.6}@#.c
enddef;

beginfig(1);

verbatimtex \def\stk#1#2{$displaystyle{\matrix{#1\cr#2\cr}}$} etex

circleit.aa(btex \vbox{\hbox{Neutron}\hbox{~~~Star}} etex);
drawboxed(aa);

for i=30pt,40pt,55pt,75pt,110pt,160pt :
  draw self.aa(i,0);
  draw self.aa(-i,0);
endfor;

numeric a, b, c, d;
z0 = aa.c;

for i=2 upto 6:
  c:=86+i*1;
  d:=30+i*15;  % = 90 for i=4
  a:=200*(cosd d);
  b:=200*(sind d);
%  drawarrow z0{dir c}..z0+(a,b) cutbefore bpath.aa;
  draw z0{dir c}..z0+(a,b) cutbefore bpath.aa;
  d:=30+i*15+180;
  a:=200*(cosd d);
  b:=200*(sind d);
%  drawarrow z0+(a,b)..z0{dir c} cutafter bpath.aa;
  draw z0+(a,b)..z0{dir c} cutafter bpath.aa;
endfor;

z0 = z21 + (0, 50pt) = z22 - (0, 50pt);
draw z21--z22 dashed evenly scaled 2;

z3 = (-200pt,-50pt);
z1 = z0 + z3; z0 = .5[z1, z2]; 

numeric k;
k:=angle -z3;

show k;

show z1,z0,z2;

path p, q;
p = z1--z0--z2;
q = z0{curl.6}.. tension .76 ..z0+(-160pt,0){down}.. tension .76 ..{curl.6}z0;

z.b=p intersectionpoint q;
z0 = .5[z.a, z.b];

dotlabel.urt("P",z.a);
dotlabel.llft("Q",z.b);

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

z.c = z6 + (0, 200pt) = z7 - (0, 200pt);
z.d = z8 + (0, 200pt) = z9 - (0, 200pt);
draw z6--z7 dashed evenly scaled 4;
draw z8--z9 dashed evenly scaled 4;

endfig;
end;