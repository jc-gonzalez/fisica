beginfig(1);

numeric i,h,c;

z0 = (0, 0);
z1 = (0, 10cm);

for i=1 upto 9:
  c := 20-i;
  h := i*c*3.1416/180 cm;
  draw (-h,0) -- (i/10)[z0,z1] -- (h,0)
    withpen pencircle scaled 1bp
    withcolor blue;
endfor;

draw z0--z1   
  withpen pencircle scaled 2bp
  withcolor 0.5[blue,black];

transform T;
picture pic;
T = identity rotatedaround(z0,-15);
pic = currentpicture transformed T;
currentpicture := nullpicture;
draw pic;

clip currentpicture to 
  (-3cm,.65cm)--(3cm,.65cm)--(3cm,20cm)--(-3cm,20cm)--cycle;

for i=-3.5cm step 2mm until 3.5cm:
  draw (i,.65cm)--(i-2mm,2mm)
    withpen pencircle scaled 2bp
    withcolor 0.5[white,black];
endfor;

draw (-3cm,.65cm)--(3cm,.65cm)
  withpen pencircle scaled 3bp;

clip currentpicture to 
  (-3cm,2mm)--(3cm,2mm)--(3cm,20cm)--(-3cm,20cm)--cycle;

transform T;
picture pic;
T = identity scaled 2 shifted (10cm,1cm);
pic = currentpicture transformed T;
currentpicture := nullpicture;
draw pic;

draw ( 6.0cm,2.7cm) ..
     ( 6.5cm,3.0cm) ..
     ( 8.0cm,5.0cm) .. 
     ( 8.5cm,4.7cm) ..
     (10.5cm,4.6cm) ..
     (12.5cm,4.7cm) .. 
     (13.0cm,5.0cm) ..
     (14.0cm,3.0cm) ..
     (14.5cm,2.7cm)
  withpen pencircle scaled 3bp
  withcolor red;

endfig;

end;