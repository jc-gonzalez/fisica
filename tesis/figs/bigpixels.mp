beginfig(1);

numeric u; u := 1cm;

pair c;
c := (10u,10u);

path adot;
adot := fullcircle scaled .6u shifted c;

pair o[], off[];

off[1] := (-.5u,  .866u);
off[2] := ( .5u,  .866u);
off[3] := (   u,   0);
off[4] := ( .5u, -.866u);
off[5] := (-.5u, -.866u);
off[6] := (  -u,   0);

o[1] := c shifted off[1];
o[2] := c shifted off[2];
o[3] := c shifted off[3];
o[4] := c shifted off[4];
o[5] := c shifted off[5];
o[6] := c shifted off[6];

path hex[];
hex[1] := off[1] -- off[2] -- off[3] -- off[4] -- off[5] -- off[6] -- cycle;

hex[2] := hex[1] shifted (off[2]+off[3]);
hex[3] := hex[1] shifted (off[5]+off[6]);
hex[4] := hex[1] shifted (off[1]+off[2]);
hex[5] := hex[1] shifted (off[6]+off[1]);
hex[6] := hex[1] shifted (off[3]+off[4]);
hex[7] := hex[1] shifted (off[4]+off[5]);
hex[8] := hex[1] shifted 3(off[1]+off[3]);
hex[9] := hex[1] shifted 3(off[5]+off[1]);

path shex, smallhex[];
shex := hex[1] scaled .5;

smallhex[1] := shex shifted .5off[2];
smallhex[2] := shex shifted .5off[4];
smallhex[3] := shex shifted .5off[6];

smallhex[4] := shex shifted off[1];
smallhex[5] := shex shifted off[3];
smallhex[6] := shex shifted off[5];

numeric g[];
g[1] := .8;
g[2] := .6;
g[3] := .7;

for i=1 upto 3:
  fill smallhex[i] withcolor g[i]*white;
  draw smallhex[i];
endfor;

for i=4 upto 6:
  fill smallhex[i] withcolor 0.95*white;
  draw smallhex[i] dashed evenly;
endfor;

for i=2 upto 9:
  draw hex[i] withcolor .3white;
endfor;

draw hex[1] withpen pencircle scaled 2bp;

label("426", center hex[1]);

label("427", center hex[2]);
label("425", center hex[3]);
label("464", center hex[4]);
label("463", center hex[5]);
label("419", center hex[6]);
label("418", center hex[7]);
label("465", center hex[8]);
label("462", center hex[9]);

%---------------------------------

picture pic;
pic := currentpicture shifted c;

currentpicture := pic;

endfig;


end;
