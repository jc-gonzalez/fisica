beginfig(1);

input neighborhoods

draw c shifted (-u,-u) -- c shifted (+u,+u) dashed evenly; 
draw c shifted (-u,+u) -- c shifted (+u,-u) dashed evenly; 

for i=-1 upto 1:

  for j=-1 upto 1:

    if (i=0) and (j=0):
      fill adot withcolor black;
      draw adot;
    else:
      fill adot shifted (i*u, j*u) withcolor 0.5white;
      draw adot shifted (i*u, j*u);
    fi;

  endfor;

endfor;

endfig;


end;