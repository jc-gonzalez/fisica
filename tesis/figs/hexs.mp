beginfig(1);

vardef hex(expr z) =
path h;
h = z+(u*cosd  30,u*sind  30)--z+(u*cosd  90,u*sind  90)--
    z+(u*cosd 150,u*sind 150)--z+(u*cosd 210,u*sind 210)--
    z+(u*cosd 270,u*sind 270)--z+(u*cosd 330,u*sind 330)--cycle;
draw h;
enddef;

input kk

endfig;
end;