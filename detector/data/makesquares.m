1;

d = readcol("mirrors.dat", 5);

nmirrors = 920;

pts3d=zeros([nmirrors,3]);
pts=zeros([nmirrors,2]);

printf("null -1000 1000 -1000 1000 AB\n");

inclination = 35; # degrees

incrad = inclination * pi / 180;

for i=1:nmirrors,

  o = omegai(-d(i, 4), -d(i, 5));
  
  v = d(i, 1:3)';
        
  v1 = [ 24.5,  24.5, 1]';
  v2 = [-24.5,  24.5, 1]';
  v3 = [-24.5, -24.5, 1]';
  v4 = [ 24.5, -24.5, 1]';

  w1 = o * v1 + v;
  w2 = o * v2 + v;
  w3 = o * v3 + v;
  w4 = o * v4 + v;

#  c3d = (w1 + w2 + w3 + w4) / 4;
#  c = proy3d(c3d, incrad, incrad);
#  
#  pts3d(i, 1) = c3d(1);
#  pts3d(i, 2) = c3d(2);
#  pts3d(i, 3) = c3d(3);
#
#  pts(i, 1) = c(1);
#  pts(i, 2) = c(2);

  c1 = proy3d(w1, incrad, incrad);
  c2 = proy3d(w2, incrad, incrad);
  c3 = proy3d(w3, incrad, incrad);
  c4 = proy3d(w4, incrad, incrad);

  printf("v/cre x(4) r %f %f %f %f\n", c1(1), c2(1), c3(1), c4(1));
  printf("v/cre y(4) r %f %f %f %f\n", c1(2), c2(2), c3(2), c4(2));
  printf("graph 4 x y f\n");

endfor

printf("wait\n");



