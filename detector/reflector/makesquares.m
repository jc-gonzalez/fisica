1;

d = readcol("mirrors.dat", 5);

nmirrors = 920;

for i=1:nmirrors,

  o = omega(d(i, 4), d(i, 5));
        
  v1 = [ 50,  50, 0]';
  v2 = [-50,  50, 0]';
  v3 = [-50, -50, 0]';
  v4 = [ 50, -50, 0]';

  w1 = o * v1;
  w2 = o * v2;
  w3 = o * v3;
  w4 = o * v4;

  c = (w1 + w2 + w3 + w4) / 4;

  printf ("%f %f %f\n", c(1), c(2), c(3));

endfor




