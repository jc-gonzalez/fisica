function y=omega(theta,phi)

  y = zeros([3,3]);
  ct = cos(theta);
  st = sin(theta);
  cp = cos(phi);
  sp = sin(phi);
  
  y(1,1) =  cp*ct;
  y(1,2) =  sp*ct; 
  y(1,3) = -st; 
     
  y(2,1) = -sp;
  y(2,2) =  cp;
  y(2,3) =  0;      

  y(3,1) =  cp*st;
  y(3,2) =  sp*st; 
  y(3,3) =  ct;         

endfunction
