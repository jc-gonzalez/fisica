e1a = 1; 
e2a = 100;
alphaa = -1.5;
na = 5000;
ta = 2.08;

## e1a = 1; 
## e2a = 100;
## alphaa = -1.5;
## na = 1100;
## ta = 0.55; 

e1b = 300; 
e2b = 3000;
alphab = -1.5;
nb = 500;
tb = 22.617;

aa = (na * (alphaa + 1)) / (e2a^(alphaa+1) - e1a^(alphaa+1));
ab = (nb * (alphab + 1)) / (e2b^(alphab+1) - e1b^(alphab+1));

beta = 0.:.01:2.005;

num = (e2b.^(alphab+beta+1) - e1b.^(alphab+beta+1));
den = (e2a.^(alphaa+beta+1) - e1a.^(alphaa+beta+1));

f = (num ./ den) - (tb ./ ta);

nanf = find(isnan(f));
f(nanf) = (f(nanf-1) + f(nanf+1)) / 2;

af = abs(f);
lf = log(af);

plot(beta, lf);

[bestlf, bestbetaind] = min(lf);

bestbeta = beta(bestbetaind)

chia = (ta * (alphaa + bestbeta + 1) / aa);
chia = chia / (e2a.^(alphaa+bestbeta+1) - e1a.^(alphaa+bestbeta+1))

chib = (tb * (alphab + bestbeta + 1) / ab);
chib = chib / (e2b.^(alphab+bestbeta+1) - e1b.^(alphab+bestbeta+1))

x = (0:.2:4.01)';
ene = 10.^x;

emeanloga = 1;
emeanlogb = 3;

chi  = (chib - chia) / (emeanlogb - emeanloga) * (x - emeanloga) + chia;
chi0 = - (chib - chia) ./ (emeanlogb - emeanloga) * emeanloga + chia
chi1 = (chib - chia) ./ (emeanlogb - emeanloga)

timegen = chi .* ene.^bestbeta;

# grid on
loglog(ene, timegen, 'r-');

n = 1000;
alpha = -1.5;
a = (n * (alpha + 1)) ./ ((10^(alpha+1) - 1) * ene.^(alpha+1));

cte = a .* chi ./ (alpha + bestbeta + 1);
timegen10 = cte .* (10^(alpha+bestbeta+1) - 1) .* ene.^(alpha+bestbeta+1);

pause;

loglog(ene, timegen10, 'b-');
