beginfig(1);

input neighborhoods

for i=-1 upto 1:

  for j=-1 upto 1:

    if (i=0) and (j=0):
      fill adot withcolor black;
      draw adot;
    elseif (i*j)=0:
      fill adot shifted (i*u, j*u) withcolor 0.5white;
      draw adot shifted (i*u, j*u);
    else:
      fill adot shifted (i*u, j*u) withcolor white;
      draw adot shifted (i*u, j*u) dashed evenly;
    fi;

  endfor;

endfor;

endfig;


end;