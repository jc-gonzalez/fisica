angle_radius = 12pt;
 
vardef draw_angle(expr a, b, c) =
  save s, p, q; path p,q;
  p = unitvector(a-b){(a-b)rotated 90}..unitvector(c-b);
  s = 10pt/length(point 1 of p - point 0 of p);
  show s, angle_radius;
  if s<angle_radius: s:=angle_radius; fi;
  q = p scaled s shifted b;
  draw q;
  point 1 of (b--(point .5 of q))
enddef;
 
vardef label_angle@#(expr a, b, c, n, t) =
  save s, p, q; path p, q;
  p = unitvector(a-b){(a-b)rotated 90}..unitvector(c-b);
  s = 10pt/length(point 1 of p - point 0 of p);
  show s, angle_radius;
  if s<angle_radius: s:=angle_radius; fi
  q = p scaled s shifted b;
  draw q;
  label@#(t, point n of q);
  point 1 of (b--(point .5 of q))
enddef;

beginfig(1);

numeric u; u := 1cm;
numeric uu; uu := .3u;

pair c;
c := (2u,1u);

z0 = (0, 0);
z1 = z0 + (0, 20u);
z2 = z1 + (6u, 0);

z3 = point .97 of (z0 -- z1);
z4 = point .97 of (z0 -- z2);

path earth, sealevel, obslevel;
earth = subpath (0,.3) of ( (0, 1u){dir 0} .. (1u, 0){dir -90} );

sealevel = earth scaled 15;
obslevel = earth scaled 16;

z5 = point 0 of obslevel;
z6 - z5 = z4 - z3;

z7 = point 0 of sealevel;
z8 - z7 = z4 - z3;

draw z0 -- z1;
draw z0 -- z2;

draw z3+(-uu,0) -- z4+(uu,0) dashed evenly;
draw z5         -- z6+(uu,0) dashed evenly;
draw z7+(-uu,0) -- z8+(uu,0) dashed evenly;

draw z5 -- z4 dashed evenly;
draw z7 -- z4;

drawdblarrow z7+(-uu/2,0) -- z3+(-uu/2,0);
drawdblarrow z8+( uu/2,0) -- z6+( uu/2,0);

path hv;
hv := ((z0 -- z4) cutbefore sealevel) rotatedaround(z0,-.8);
drawdblarrow hv;

linecap := butt;
draw sealevel withpen pencircle scaled 3bp;
draw obslevel withpen pencircle scaled 1.5bp dashed evenly scaled 2;
linecap := rounded;

path adot; 
adot := fullcircle scaled .15u;

fill adot shifted z5 withcolor white;
draw adot shifted z5;

fill adot shifted z7 withcolor white;
draw adot shifted z7;

fill adot shifted z4 withcolor white;
draw adot shifted z4;

pair angle[];
 
angle[0] := draw_angle(z2, z0, z1);
angle[1] := draw_angle(z4, z5, z3);
angle[2] := draw_angle(z4, z7, z3);
 
pair uvector;
uvector:=(.5u,0);
 
label(btex $\alpha$ etex scaled 1.75, angle[0]+(2*uvector rotated 80));
label(btex $\theta$ etex scaled 1.75, angle[1]+(uvector rotated 60));
label(btex $\omega$ etex scaled 1.75, angle[2]+(.8uvector rotated 60));
 
label(btex A etex scaled 1.75, z5+(-2uu,0));
label(btex O etex scaled 1.75, z7+(-2uu,0));
label(btex P etex scaled 1.75, z4+(-uu,uu));

label(btex $R$ etex scaled 1.75, .5[z0,z4]+(uvector));
label(btex $R$ etex scaled 1.75, .5[z0,z3]+(uvector));
label(btex $h_v$ etex scaled 1.75, (point .5 of hv) + uvector);
label(btex $h_C$ etex scaled 1.75, (point .5 of (z7--z3)) - 1.2uvector);
label(btex $h_0$ etex scaled 1.75, (point .5 of (z8--z6)) + 1.1uvector);


%---------------------------------

picture pic;
pic := currentpicture shifted c;

currentpicture := pic;

endfig;


end;
