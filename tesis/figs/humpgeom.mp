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

beginfig(1)

numeric u; u := 1cm;

numeric cher; cher := 15;
numeric za;   za := 30;

pair a[];

a[0] := (0,0);
a[1] := (0,7u);
a[2] := (0,-2u);

path r[];
r[0]    := (a[1] -- a[2]);
r[1]    := r[0] rotatedabout( a[1],  cher );
r[2]    := r[0] rotatedabout( a[1], -cher );
r[1000] := (a[1]+(0,1u) -- a[2]);

path g, gg, righta;
g := (-4u, 0) -- (4u, 0);
gg := (-4u, 0) -- (4u, 0) -- (4u, -.5u) -- (-4u, -.5u) -- cycle;
righta := (-.2u,0) -- (-.2u,.2u) -- (.2u,.2u) -- (.2u,0);

r[10] := r[1000] rotatedabout( a[0], -za );
r[11] := r[1]    rotatedabout( a[0], -za );
r[12] := r[2]    rotatedabout( a[0], -za );
r[99] := g       rotatedabout( a[0], -za );
r[100]:= righta  rotatedabout( a[0], -za );

numeric t[];

t[11] := xpart (r[11] intersectiontimes g);
t[12] := xpart (r[12] intersectiontimes g);
t[91] := xpart (r[11] intersectiontimes r[99]);
t[92] := xpart (r[12] intersectiontimes r[99]);

pair pt[];

pt[11] := point t[11] of r[11];
pt[12] := point t[12] of r[12];
pt[91] := point t[91] of r[11];
pt[92] := point t[92] of r[12];

pt[0] := a[1] rotated -za;

fill gg withcolor .80white;
draw g withpen pencircle scaled 1.5bp withcolor .4white;
draw r[0] dashed dashpattern(on 3bp off 3bp on .5bp off 3bp on 3bp);

r[999] := r[99] 
          cutbefore point (ypart (r[12] intersectiontimes r[99]))-.1 of r[99]
          cutafter  point (ypart (r[11] intersectiontimes r[99]))+.1 of r[99];

drawoptions(withpen pencircle scaled 1bp);

draw r[11] cutafter point (t[91]+.05) of r[11];
draw r[12];

ahlength:=14bp; ahangle:=30;
drawarrow point 0 of r[10] -- point .4 of r[10] withpen pencircle scaled 2bp;
draw point .4 of r[10] -- point 1 of r[10] withpen pencircle scaled 2bp;

draw r[999] dashed evenly;
draw r[100];

drawoptions(withpen pencircle scaled 1bp withcolor white);

fill fullcircle scaled 4bp;
fill fullcircle scaled 4bp shifted pt[11];
fill fullcircle scaled 4bp shifted pt[12];
fill fullcircle scaled 4bp shifted pt[91];
fill fullcircle scaled 4bp shifted pt[92];
                                        
drawoptions();                          
                                        
draw fullcircle scaled 4bp;             
draw fullcircle scaled 4bp shifted pt[11];
draw fullcircle scaled 4bp shifted pt[12];
draw fullcircle scaled 4bp shifted pt[91];
draw fullcircle scaled 4bp shifted pt[92];

pair angle[];

angle[0] := draw_angle(point 0 of r[10],origin,a[1]);
angle[1] := draw_angle(origin,pt[0],pt[11]);
angle[2] := draw_angle(pt[12],pt[0],origin);

%picture pic;
%pic := currentpicture shifted (10cm,10cm);
%currentpicture :=  pic;

pair uvector;
uvector:=(.6u,0);

label(btex B' etex scaled 1.75, pt[12]+(uvector rotated 125));
label(btex A' etex scaled 1.75, pt[11]+(uvector rotated 115));
label(btex B  etex scaled 1.75, pt[92]+(uvector rotated 90));
label(btex A  etex scaled 1.75, pt[91]+(uvector rotated 200));

label(btex $\Theta$ etex scaled 1.75, ((0,1.4u) rotated (-za/2)));

label(btex $\theta_c$ etex scaled 1.75, (angle[1]+(uvector rotated -15)));
label(btex $\theta_c$ etex scaled 1.75, (angle[2]+(uvector rotated 135)));

label.ulft(btex \bf s etex scaled 1.75, point .5 of r[10]);
label.ulft(btex \bf g etex scaled 1.75, point 1 of g);
label.urt (btex \bf p etex scaled 1.75, point 1 of r[999]);

endfig;
end.