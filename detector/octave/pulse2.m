//-*-octave-*-

1;

b=(0:0.5:100)';

num=max(size(b));

n=exp(-0.5*(b-30).^2/20)*10+rand(b);
//n=floor(n);  

xp=(0:0.5:10)';
yp=pulse(xp,10,0.5,20);
yp=yp/max(yp);yp=yp*10;

nump=max(size(yp));

//n=zeros(b);
//n(20)=10;

yf=zeros(n);

for i=1:num-nump,
  
  yf(i:i+nump-1) = yf(i:i+nump-1) + yp*n(i);
  
end

ya=n;
yb=yf;

xbasc();

r=[0 0 100 max(ya)*1.2];
plotframe(r,[4 10 2 5],...
          [%f,%f],['Original','t (ns)','N.ph.e-'],...
          [0 0.5 1 0.5]);
plot2d3("onn",b,ya,2,"000");

r=[0 0 100 max(yb)*1.2];
plotframe(r,[4 10 2 5],...
          [%f,%f],['Smoothed','t (ns)','q (mV)'],...
          [0 0 1 0.5]);
plot2d2("onn",b,yb,5,"000");

