## -*- octave -*-

1;

clear

## initial values

nevents = 200000;
histobins = -1:.025:5;

llimit = 1;
ulimit = 10000;

global res1 = .5;
global res2 = .5;

diff_spectral_index = -1.5;

## generate set of uniform random numbers (0:1000)

urnd = rand(nevents,1);

pslope = diff_spectral_index;
ll   = llimit.^(pslope + 1);
ul   = ulimit.^(pslope + 1);
slex = 1 / (pslope + 1);
energy = ( urnd.*ul + ( 1-urnd ).*ll ).^slex;

[nn,xx] = hist(log10(energy),histobins);

x10 = 10.^xx;
nn1=nn.*(x10.^(-diff_spectral_index-1));
data = [xx;nn1]';
gset grid
gset logscale y
gset nolabel
gset title 'Simulated Spectrum'
gset xlabel 'log(E)'
gset ylabel 'dN(E)/dE'
gplot data with steps

## our events have energies stored in the vector sprnd

## now we assume that our resolution with the energy
## follows a linear function from res1=res(log10(E1))
## till like res2=res(log10(E2))

global le1 = log10(llimit);
global le2 = log10(ulimit);

global reslope = (res2-res1)/(le2-le1)

function y=res(le)
  global res1 
  global res2  
  global le1  
  global le2  
  global reslope
  y=reslope.*(le-le1)+res1;
endfunction
 
x=le1:.2:le2;
y=res(x);

gset nolabel
gset title 'Assumed Energy Resolution'
gset xlabel 'log(E)'
gset ylabel 'dE/E'
#plot(x,y,"2-")

## we assume that the estimated energy is calculated as
## Ecalc = N(E, sigmaE)
lenergy = log10(energy);
sigmares = energy.*res(lenergy);

ecalc = randn(nevents,1).*sigmares + energy;

[nn,xx] = hist(log10(ecalc),histobins);

x10 = 10.^xx;
nn2=nn.*(x10.^(-diff_spectral_index-1));
data = [xx;nn1]';
gset grid
gset nologscale 
gset title 'Original vs. Restored Simulated Spectrum'
gset xlabel 'log(E)'
gset ylabel 'dN(E)/dE'
gplot data with steps t 'Original Spectrum'
hold
data = [xx;nn2]';
gplot data with steps t 'Restored Spectrum'
hold off

gset output 'binning.ps'
gset term postscript
replot
gset term x11
gset output

