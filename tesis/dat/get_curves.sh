#! /bin/sh

file=$1


awktmp=tmpgc.awk

cat <<EOF > $awktmp
BEGIN {
  naxis = 0;
  ncurve = 0;
  npoints = 0;
  np = 0;
  axesfile="tmpaxes.dat"
  curvesfile="tmpcurves.dat"
  printf "" > axesfile
  printf "" > curvesfile
}

(/# Axis:/) {
  naxis = naxis+1;
  u[naxis] = \$3;
  v[naxis] = \$4;
  getline; getline;
  x[naxis] = \$1;    
  y[naxis] = \$2;
  print u[naxis],v[naxis],x[naxis],y[naxis] >> axesfile
}

(/# Curve:/) {
  name=substr(\$0, 10); 
  ncurve = ncurve+1;
  getline; 
  npoints = \$NF;
  print ncurve  >> curvesfile
  print name    >> curvesfile
  print npoints >> curvesfile
  np = 0;
  while (np < npoints) {
    getline;
    for (i=1; i<=NF; i+=2) {
      cx = \$i; cy = \$(i+1);
      print cx,cy >> curvesfile
      np+=1;
    }
  }
}

END {
  print -1 >> curvesfile
}
EOF

awk -v xlog=0 -v ylog=1 -f $awktmp < $file

./gc 0 1 curves |tee gc.log 