1;

b=(0:0.5:100)';

num=max(size(b));

n=exp(-0.5*(b-30).^2/20)+rand(b)/5;
n=n*20;
n=floor(n);  

xp=0:0.5:10;
yp=pulse(xp,10,0.5,20);

nump=max(size(yp));

//n=zeros(b);
//n(20)=10;

xbasc();plot2d3("onn",b,n,2)
ix=b(2)-b(1);

yf=zeros(n);

for i=1:num-,
  
  nx=n(i);

  for k=1:nx,

    x0=b(i);

    for j=i+1:num,

      xx=(b(j)-x0);
      yf(j)=yf(j)+pulse(xx,10,0.5,20);
    
    end

  end

end

ya=n/max(n);
yb=yf/max(yf);

xbasc();plot2d3("onn",b,ya,2);plot2d2("onn",b,yb,5)
