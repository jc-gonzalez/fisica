
numeric u; u := 1cm;

pair c;
c := (10u,10u);

path adot;
adot := fullcircle scaled .6u shifted c;

for i=-1 upto 1:
  draw c shifted (i*u,-u) -- c shifted (i*u,+u) dashed evenly; 
  draw c shifted (-u,i*u) -- c shifted (+u,i*u) dashed evenly;
endfor;

draw c+(-1.6u,-1.6u) -- c+(-1.6u,1.6u) -- 
     c+(1.6u,1.6u) -- c+(1.6u,-1.6u) -- cycle withcolor .8white;
