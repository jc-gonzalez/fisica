function f1=difdiv(x,f,x1)
#% Interpolación mediante diferencias divididas.
#% Parámetros de entrada:
#%    - x,f Vectores con puntos conocidos
#%    - x1  Vector con puntos para interpolar

n=size(x,1)+size(x,2)-1;
a=f;
for j=2:n
   for i=j:n
      a(i)=(a(i)-f(i-1))/(x(i)-x(i-j+1));
   end
   f([j:n])=a([j:n]);
   f=a;
end
a
f1=a(1);
for k=1:n-1
   pr=1;
   for i=1:k
      pr=pr.*(x1-x(i));
   end
   f1=f1+a(k+1).*pr;
end
