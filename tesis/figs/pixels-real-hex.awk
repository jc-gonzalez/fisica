BEGIN {
    ox = 10000;
    oy = 10000;

    pixsize = 1;

    scale = 400;
    scale2 = scale/2/(sqrt(3)/2);

    for(i=0;i<6;i++) {
	x[i] = scale2*sin(2*3.1415926535/6*i);
	y[i] = scale2*cos(2*3.1415926535/6*i);
    }    

    hexline = "2 3 0 1 0 7 10 0 -1 0.000 0 0 0 0 0 7";

    print "#FIG 3.2";
    print "Landscape";
    print "Center";
    print "Metric";
    print "Letter  ";
    print "100.00";
    print "Single";
    print "-2";
    print "1200 2";
    print "6 1500 1500 18500 18500";
}

($1==398) {
    hexline = "2 3 0 1 0 7 20 0 -1 0.000 0 0 0 0 0 7";
    print "-6";
    print "6 1000 1000 19000 19000";
}

($1==416) {
    hexline = "2 3 0 1 0 7 30 0 -1 0.000 0 0 0 0 0 7";
    print "-6";
    print "6 500 500 19500 19500";
    pixsize = 2;
    getline;getline;
}

($1>595) {
    print "-6";
    printf "1 3 1 1 0 7 50 0 -1 4.000 1 0.0000 ";
    printf "10000 10000 6000 6000 10000 10000 4000 10000\n";
    exit;
}

{
    print hexline;
    printf "\t";
    xx = ox + scale * $5;
    yy = ox + scale * $6;
    for (i=0;i<6;i++) {
	px = x[i] * pixsize + xx;
	py = y[i] * pixsize + yy;
	printf " %d %d",px,py;
    }
    px = x[0] * pixsize + xx;
    py = y[0] * pixsize + yy;
    printf "\n\t %d %d\n",px,py;
    printf "1 3 1 1 0 7 9 0 -1 4.000 1 0.0000 ";
    printf "%d %d %d %d %d %d %d %d\n",
	xx, yy,
	scale * 0.45  * pixsize, scale * 0.45 * pixsize,
	xx, yy,
	xx + scale2 * pixesize, yy;
}

