## -*- octave -*-
############################################################
##
##
##
############################################################

f1=fopen("pixels.dat","r");
data=fscanf(f1,"%f %f %f %f %f",[5,Inf])';
fclose(f1);

np=max(size(data));

pn=zeros(40,40);
px=zeros(40,40);
py=zeros(40,40);

## calculating the pixels array

for i=1:np,

  n=data(i,1);
  ix=data(i,2)+20;
  iy=data(i,3)+20;
  x=data(i,4);
  y=data(i,5);
  
  pn(ix,iy) = n;
  px(ix,iy) = x;
  py(ix,iy) = y;
  
end  

## processing the shower data

ctpixsize = 3.;              # in cm
ctapot = ctpixsize / 2.;     # in cm
ct2apot = 2.*ctapot;         # in cm
sin60=sin(60*pi/180)
cos60=cos(60*pi/180)

nn=zeros(1000,1);

f2=fopen("shower.dat","r");
data=fscanf(f2,"%f %f %f %f %f",[5,Inf])';
fclose(f2);

##plot([-4 -4 4 4],[-4 4 -4 4],"g.");
##hold on
##plot(data(:,2),data(:,3),"b.");
##hold off

np = max(size(data));

for i=1:np

  cx = data(i,2);
  cy = data(i,3);

  phi = atan2(cy,cx);
  if (phi<0.0), phi = phi+2*pi; end
  phi = 60*pi/180 - phi;

  ci = floor(cx / ctpixsize + 0.5);  
  cj = floor( (-cx*cos60/sin60 + cy/sin60) / ctpixsize + 0.5);  

  ici = ci;
  icj = cj;
  
  iici = ici+20;
  iicj = icj+20;

  if ((iici>0) && (iici<41) && (iicj>0) && (iicj<=41)),

    n = pn(iici, iicj);
    x = px(iici, iicj);
    y = py(iici, iicj);
    
    nn(n) = nn(n) + 1;
    
  end

end






