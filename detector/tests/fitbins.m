
h=rand(200000,1);

a=zeros([2,16]);

bins=[1 2 3 4 5 6 8 10 20 30 50 80 100 200 250 500]*3;


pslope=-1.5;
llimit=1;
ulimit=1000;

ll=llimit^(pslope+1.0);
ul=ulimit^(pslope+1.0);
slex=1.0/(pslope+1.0);

e=(h.*ul+(1-h).*ll).^slex;
le=log10(e);

gset grid

for i=1:16
  nb=bins(i);
  [nn,xx]=hist(le,nb);
  unos = 1 .- (nn & (ones(size(nn))));
  lnn=log10(nn+unos);
  coefs=polyfit(xx,lnn,1);
  [s,errmsg,status] = sprintf("Ajuste con %d bines/decada (index=%.1f)", nb/3, pslope);
  title(s);
  xlabel("X = log10(x)");
  ylabel("Y = log10(y)");
  data = [xx;lnn;polyval(coefs,xx)]';
  gplot data using 1:2 title "datos" with points, data using 1:3 title "ajuste" with lines
##  plot(xx,lnn,"rx",xx,polyval(coefs,xx),"b")
  a(:,i)=coefs;
end,

a
