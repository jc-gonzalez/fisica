##-*- octave -*-

d2r=pi/180.;
r2d=180/pi;

a=1;
r=a/cos(30*d2r);

num=6;

ang=pi/6;
polygon=zeros(num,2);
ox = 1;
oy = 0;

for i=1:num,
  polygon(i,:)=[r*cos(2*pi*i/num+ang)+ox r*sin(2*pi*i/num+ang)+oy];
end

#polygon=[a r/2 ; 0 r ; -a r/2 ; -a -r/2 ; 0 -r ; a -r/2]; 

axisline=[-2 -1 ; 2 1.5];

n=max(size(polygon));

px=zeros(n+1,1);
py=zeros(n+1,1);

px(1:n)=polygon(:,1);px(n+1)=px(1);
py(1:n)=polygon(:,2);py(n+1)=py(1);

lx=axisline(:,1);
ly=axisline(:,2);

paxisline1=axisline(1,:);
paxisline2=axisline(2,:);

p=[px,py];

grid();

gplot p t 'hexagon' w l, axisline t 'axis line' w l

# for i=1:6,
#   dist=distpl( [px(i) py(i)], [lx(1) ly(1)], [lx(2) ly(2)] ); 
#   printf("dist[line,(%f,%f)] = %f\n", px(i), py(i), dist);
# end
# dist=distpl( [0 0], [lx(1) ly(1)], [lx(2) ly(2)] ); 
# printf("\ndist[line,(%f,%f)] = %f\n", 0, 0, dist);

vleft=zeros(1,2);
vright=zeros(1,2);
nvleft=0;
nvright=0;

vi=polygon(n,:);

for j=1:n,
  
  vj = polygon(j,:);

  sidei = side(vi, paxisline1, paxisline2);
  sidej = side(vj, paxisline1, paxisline2);
  iscross = 0;

  ## if sidei !=  sidej, the segment and the line cross each other
  if (sidei !=  sidej), 
    printf("crossing for points %d and %d ...\n", j-1, j);
    iscross = 1;
  end

  ## put the i in the list
  if (sidei < 0)
    nvleft++;
    vleft(nvleft,:) = vi;
  else
    nvright++;
    vright(nvright,:) = vi;
  end

  ## calculate cross-point, if any
  if (iscross > 0),
    c = crossing(vi, vj, paxisline1, paxisline2);
    nvleft++;
    vleft(nvleft,:) = c;
    nvright++;
    vright(nvright,:) = c;
  end

  vi = vj;

end

vleft
vright

vl=vleft;
vr=vright;

vlc=mean(vleft);
vrc=mean(vright);

vl(nvleft+1,:)=vl(1,:);
vr(nvright+1,:)=vr(1,:);

vl(:,1)=(vl(:,1)-vlc(1))*0.95+vlc(1);
vl(:,2)=(vl(:,2)-vlc(2))*0.95+vlc(2);
vr(:,1)=(vr(:,1)-vrc(1))*0.95+vrc(1);
vr(:,2)=(vr(:,2)-vrc(2))*0.95+vrc(2);

hold
gplot vl t 'vleft' w l, vr t 'vr' w l
hold off

## we already have TWO separated polygons in the form of
## two lists of vertices, vleft and vright

## now we calculate the surface for each one

s=polysurf(polygon);
s1=polysurf(vleft);
s2=polysurf(vright);

printf("Surface of initial polygon: %f\n", s);
printf("Surface of polygon 1 .....: %f\n", s1);
printf("Surface of polygon 2 .....: %f\n", s2);










