1;


n=100000;
x=randn(n,1)*5;
y=randn(n,1)*3;
alpha=15*pi/180;
xx=x*cos(alpha)-y*sin(alpha);yy=x*sin(alpha)+y*cos(alpha);
xxx=xx+1;yyy=yy+2;
plot(xxx,yyy,'b.',mean(xxx),mean(yyy),'r*')
Sx=[cov(x,x),cov(x,y);cov(x,y),cov(y,y)]
Sxx=[cov(xx,xx),cov(xx,yy);cov(xx,yy),cov(yy,yy)]
Sxxx=[cov(xxx,xxx),cov(xxx,yyy);cov(xxx,yyy),cov(yyy,yyy)]
[vx,lx]=eig(Sx);dlx=sqrt(diag(lx));
[vxx,lxx]=eig(Sxx);dlxx=sqrt(diag(lxx));
[vxxx,lxxx]=eig(Sxxx);dlxxx=sqrt(diag(lxxx));
[dlx';dlxx';dlxxx']
[det(Sx),det(Sxx),det(Sxxx)]
vx,vxx,vxxx