beginfig(1);

numeric u; u := 1cm;

pair c;
c := (10u,10u);

path adot;
adot := fullcircle scaled .6u shifted c;

fill adot withcolor black;
draw adot;

pair o[], off[];

off[1] := (-.5u,  .866u);
off[2] := ( .5u,  .866u);
off[3] := (  -u,   0);
off[4] := (   u,   0);
off[5] := (-.5u, -.866u);
off[6] := ( .5u, -.866u);

o[1] := c shifted off[1];
o[2] := c shifted off[2];
o[3] := c shifted off[3];
o[4] := c shifted off[4];
o[5] := c shifted off[5];
o[6] := c shifted off[6];

drawoptions(dashed evenly);

draw o[1] -- o[2];
draw o[1] -- o[3];
draw o[2] -- o[4];
draw o[3] -- o[5];
draw o[4] -- o[6];
draw o[5] -- o[6];

draw o[1] -- o[6];
draw o[2] -- o[5];
draw o[3] -- o[4];

drawoptions();

draw c+(-1.6u,-1.6u) -- c+(-1.6u,1.6u) -- 
     c+(1.6u,1.6u) -- c+(1.6u,-1.6u) -- cycle withcolor .8white;

fill adot withcolor black; 
draw adot;

for i=1 upto 6:
  fill adot shifted off[i] withcolor 0.5white;
  draw adot shifted off[i];
endfor

endfig;


end;
