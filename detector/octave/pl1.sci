//## -*- octave -*-

nshowers = 200000;

lx=(0:.1:log10(30000))';
x=10.^lx;

//## Integrate[ E^(-1.5), {x,1,30000} ] = 

deff('[y]=eint(a,b,index)',['y = (b^(-(index-1)) - a^(-(index-1))) / (-index);']);

//function y=eint(a,b,index)
//  y = (b^(index+1) - a^(index+1)) / (index);
//endfunction

//integ05 = abs(eint(1,30000,0.5))
//integ10 = abs(eint(1,30000,1.0))
integ15 = abs(eint(1,30000,1.5))

//y05=x.^(0.5);
//y10=x.^(-0.0);
y15=x.^(-0.5);

//ypor05 = y05 ./ max(y05) .* 100; ynor05 = ypor05 ./ 100 .* nshowers;
//lypor05 = log10(ypor05);lynor05 = log10(ynor05);

//ypor10 = y10 ./ max(y10) .* 100; ynor10 = ypor10 ./ 100 .* nshowers;
//lypor10 = log10(ypor10);lynor10 = log10(ynor10);

ypor15 = y15 ./ max(y15) .* 100; ynor15 = ypor15 ./ 100 .* nshowers;
lypor15 = log10(ypor15);lynor15 = log10(ynor15);

xset("window",0);
xbasc(0);
xset("thickness",1);
xset("font",2,1);
xset("dashes",1);
//plot2d1("oll",x,[ypor05,ypor10,ypor15],[0,0,0],"021");
plot2d1("oll",x,ypor15,[0],"021");
xgrid(31);
xset("thickness",3);
//plot2d1("oll",x,[ypor05,ypor10,ypor15],[1,2,3],"100",..
plot2d1("oll",x,ypor15,[3],"100",..
"Dif.Index=-1.5");

//xtitle("Percentage of showers above a given energy","Energy (GeV)","%");
xset("font",4,4);
xtitle("Percentage of showers above a given energy");
xset("font",3,1);
//xtitle("","Energy (GeV)","%");
xstring(20000,4e-2,"Energy (GeV)");
xstring(.4,80,"%");

xset("window",1);
xbasc(1);
xset("thickness",1);
xset("font",2,1);
xset("dashes",1);
//plot2d1("oll",x,[ynor05,ynor10,ynor15],[0,0,0],"021");
plot2d1("oll",x,ynor15,[0],"021");
xgrid(31);
xset("thickness",3);
//plot2d1("oll",x,[ynor05,ynor10,ynor15],[1,2,3],"100",..
plot2d1("oll",x,ynor15,[3],"100",..
"Dif.Index=-1.5");

//xtitle("Number of showers above a given energy ","Energy (GeV)","%");
xset("font",4,4);
xtitle("Number of showers above a given energy");
xset("font",3,1);
//xtitle("","Energy (GeV)","%");
xstring(20000,3.5e2,"Energy (GeV)");
xstring(.3,6e5,"N");

