function y=omegai(theta,phi)

  y = zeros([3,3]);
  ct = cos(theta);
  st = sin(theta);
  cp = cos(phi);
  sp = sin(phi);
  
  y(1,1) =  cp*ct;
  y(1,2) = -sp;    
  y(1,3) =  cp*st;
                  
  y(2,1) =  sp*ct;
  y(2,2) =  cp;   
  y(2,3) =  sp*st;  
                  
  y(3,1) = -st;   
  y(3,2) =  0;     
  y(3,3) =  ct; 

endfunction
