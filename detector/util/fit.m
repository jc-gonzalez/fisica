#!/usr/local/bin/octave -q

1;

function y=readcol(file, col)
  f1=fopen(file,"r");
  y=fscanf(f1,"%f",[col,Inf])';
  fclose(f1);
endfunction

## 

filename = argv(1,:);
degree = str2num(argv(2,:));

modex = str2num(argv(3,:));
modey = str2num(argv(4,:));

npts = str2num(argv(5,:));
xa = str2num(argv(6,:));
xb = str2num(argv(7,:));

fprintf(stderr, "Fitting data in file %s with poly. of degree %d.\n",
        filename, degree);

data = readcol(filename,2);
x = data(:,1);
y = data(:,2);

if (modex == 1),
  x = log10(x);
endif
if (modex == 2),
  x = 10.^x;
endif


if (modey == 1),
  y = log10(y);
endif
if (modey == 2),
  y = 10.^y;
endif

[coefs,yfit] = polyfit(x, y, degree);
chisq = sum((y - yfit).^2);

h = (xb - xa) / (npts - 1);
newx = (xa:h:xb)';
newy = polyval(coefs, newx);
newdata = [newx, newy];

disp(newdata)
