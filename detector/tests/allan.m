####
##
##
####

data=readcol("oo1",2);x1=data(:,1);y1=data(:,2);
data=readcol("oo2",2);x2=data(:,1);y2=data(:,2);

lx1=log10(x1);ly1=log10(y1);
lx2=log10(x2);ly2=log10(y2);

idx=10:200;
[coef1,y1f]=polyfit(lx1(idx),ly1(idx),1);
[coef2,y2f]=polyfit(lx2(idx),ly2(idx),1);

ly1f=polyval(coef1,lx1);
ly2f=polyval(coef2,lx2);

gset grid
gset title "Studing low-frequency terms : Mkn 501 data - Allan Variance Plot"
gset xlabel "Order"
gset ylabel "Variance"

data1=[lx1,ly1];data2=[lx2,ly2-5];
data3=[lx1,ly1f];data4=[lx2,ly2f-5];

gset term postscript
gset output "allan-mkn501.ps"

gplot data1 w lines t "ID=1", data2 w lines t "ID=2", \
    data3 w lines t "fit(10:200)", data4 w lines t "fit(10:200)"

gset output
gset term x11

replot

pause()