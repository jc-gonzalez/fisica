function v=proy3d(u,alpha,beta)
  x = u(1);
  y = u(2);
  z = u(3);
  v=[y*cos(beta)-x*cos(alpha), z-y*sin(beta)-x*sin(alpha)];
endfunction
