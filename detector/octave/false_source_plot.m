## -*- octave -*-

## number of ellipses in the simulation
npts = 50;

## define sigma for determination of orientation of ellipse
sigma = 5.;  # degrees

## generate random alphas
alpha = rand(npts,1)*sigma;
alpha = alpha*pi/180;

## define positions of center
maxx = 100;
minx = -maxx;
maxy = 100;
miny = -maxy;

x = rand(npts,1)*2*maxx+minx;
y = rand(npts,1)*2*maxx+miny;

## define my grid 100x100 
x1 = -60;
x2 = -x1;
y1 = -60;
y2 = -y1;

nx = 100;
ny = 100;

ix= (x2-x1)/nx;
iy= (y2-y1)/ny;

fsp5 = zeros([nx,ny]); # fspN  = > False Source Plot with alpha<N

## plot all the ellipses
function plotellipse(x,y,alpha,axislength)
  plot(x,y,'o');
  theta=atan2(y,x)+alpha;
  tantheta=tan(theta);
  dx= [ x-axislength             x+axislength         ];
  dy= [ y-axislength*tantheta    y+axislength*tantheta];
  plot(dx,dy,'r-');
endfunction

#plot([minx maxx],[miny maxy],".");
#hold

#for i=1:npts;
#  plotellipse(x(i),y(i),alpha(i),50); 
#end

#hold off

## for each event, fill in the histogram (grid)

## main loop

for i=1:npts;

  ## get x, y, alpha

  cx = x(i);  cy = y(i);  calpha = alpha(i);

  ## define straight line ( tan(theta) )

  theta=atan2(cy,cx)+calpha;
  tantheta=tan(theta)

  ## if abs(tantheta) > 1, invert axis and go in Y, if not go in X

  if ( abs(tantheta) < 1 ), # normal algorithm

    it = ix;

    ## both directions
    for k=1:2;

      px = cx; 
      py = cy;
      vx = cos(theta);
      vy = sin(theta);
      t = it;

      while ( (px < maxx) && (px > minx) ),

        ## define point in axis
        px = cx + vx * t;
        py = cy + vy * t;
        
        ## identify cell
        cellx = round( (px - x1) / ix );      
        celly = round( (py - y1) / iy );

        printf("%d %d\n", cellx, celly);
        
        ## go to the next point
        t = t + it;
        
      end
      
      it = -it;

    end

  end

end

