angle_radius=8pt;

def draw_angle(expr a, b, c) =
  begingroup
  save s, p, q; path p,q;
  p = unitvector(a-b){(a-b)rotated 90}..unitvector(c-b);
  s = 10pt/length(point 1 of p - point 0 of p);
  show s, angle_radius;
  if s<angle_radius: s:=angle_radius; fi
  q = p scaled s shifted b;
  draw q;
  endgroup
enddef;

vardef label_angle@#(expr a, b, c, n, t) =
  begingroup
  save s, p, q; path p, q;
  p = unitvector(a-b){(a-b)rotated 90}..unitvector(c-b);
  s = 10pt/length(point 1 of p - point 0 of p);
  show s, angle_radius;
  if s<angle_radius: s:=angle_radius; fi
  q = p scaled s shifted b;
  draw q;
  label@#(t, point n of q);
  endgroup
enddef;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vardef thefigure(expr a, b, c) = 

begingroup

numeric #;
#=6pt;

path r[];
r0 = fullcircle scaled 4# xscaled 8;

numeric n;
n = length r0;

r1 = subpath (0,4) of r0;
r2 = subpath (4,8) of r0;

path l;
l = (0,27#)--(0,-10#);

z1 = (0,-5#);

numeric t;
path u[];
pair v;
for t=.25 step .25 until .75:

  u1 := r1 scaled t shifted t[z1,(0,0)];
  u2 := r2 scaled t shifted t[z1,(0,0)];
  draw u1 dashed evenly;
  draw u2;

  v := t[(0,-5#),(0,35#)];

  for h=.2,.3,.4,.5,.7,.9,1.1,1.3,1.6,2.4,2.7,2.9,3.1,3.3,3.5,3.6,3.7,3.8:
    draw v-- point h of u2 withcolor (.7,.7,1.);
  endfor

  u2 := v-- point 4 of u1;
  drawarrow subpath (0,.5) of u2;
  draw subpath (.5,1) of u2;

  u2 := v-- point 0 of u1;
  drawarrow subpath (0,.5) of u2;
  draw subpath (.5,1) of u2;

endfor

pair h[];

pickup pencircle scaled .5bp;
for t=0 step .5 until 4:
  h[t] = point t of r0;
  draw h[t]--z1 dashed evenly withcolor (.6,.6,.6);
endfor

pickup pencircle scaled 1pt;
draw r1 withcolor (0,0,.8);

pickup pencircle scaled 2pt;
numeric s;
s := ahlength;
ahlength := 12pt; ahangle := 20;
drawarrow l withcolor (.8,0,0);
ahlength := s; ahangle := 45;

pickup pencircle scaled 1pt;
linejoin := rounded;
draw r2 withcolor (0,0,.8);

pickup pencircle scaled .1bp;
for t=4.5 step .05 until 7.5:
  h[t] = point t of r0;
  draw h[t]--z1 withcolor (.4,.4,.8);
endfor

transform T;
T = identity rotated -15;

picture pic;
pic = currentpicture transformed T;

currentpicture := pic;

label_angle.bot(z1 transformed T,
                v transformed T,
                point 0 of u1 transformed T, 
                .7, btex $\theta_c$ etex);

label.rt(a, point .3 of u2 transformed T);
label.llft(b, point 4 of u1 transformed T);
label.lrt(c, point .95 of l transformed T);

endgroup;
enddef;

beginfig(1)
thefigure(btex \vbox{\hbox{~~~Radiated}\hbox{Cherenkov light}} etex,
          btex \vbox{\hbox{Cherenkov}\hbox{~~Front}} etex,
          btex \vbox{\hbox{Particle}\hbox{Velocity}} etex);
endfig;

beginfig(2)
thefigure(btex \vbox{\hbox{Luz Cherenkov}\hbox{~~~emitida}} etex,
          btex \vbox{\hbox{Frente de luz}\hbox{~~Cherenkov}} etex,
          btex \vbox{\hbox{Velocidad de}\hbox{la part{\'{\i}}cula}} etex);
endfig;

end; 