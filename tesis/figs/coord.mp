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

beginfig(1)

numeric u;
u=.5cm;

z0 = (20u,25u);
 
z1 = z0 + (5u,-.5u); z2 = z0 + (10u,-1u);
z3 = z0 + (-4u,-2u); z4 = z0 + (-8u,-4u);
z5 = z0+ (0,16u);

path q[];

%% axes
q1 = z0--z2;
q2 = z0--z4;
q3 = z0--z5;

%% trayectory of the incoming photon
z6 = (z0+(5u,15u));
q4 = z0--z6;

%% where the CT is pointing to
z7 = (z0+(2u,12u));
q5 = z0--z7;

z8 = z0+(0,14u);
z9 = point .8 of q4;

q6 = z0--z8;
q7 = z0--z9;

drawarrow q1;
drawarrow q2;
drawarrow q3;

drawarrow q4;
drawarrow q5;

draw z7{dir 120}..z8;
draw z7{dir 15}..z9;

label_angle.top(z7, z0, z5, .5, btex $\Theta$ etex);
label_angle.top(z6, z0, z7, .5, btex $\theta_p$ etex);

endfig;
end;