#!/usr/local/bin/perl

$henf=1.0e6;
$magic=0;
$f=488.0;

while (<>) {
    print;
    last if ( /define_mirrors/ );
}

if ($magic==0) {
    print STDERR "using focal ",$f;
}

while (<>) {
    last if ( /EOF/ );
    @line=split;
    ($i,$a,$b,$c,$x,$y,$z,$t,$p,$xn,$yn,$zn)=@line;

    $rho = sqrt($x^2+$y^2);

    if ($magic==1) {
	$f = $a;
	print STDERR "using focal ",$f;
    }
    $t = (atan2($rho,$henf-$z)+atan2($rho,$f-$z))/2.0;
    $p = atan2($y,$x);
    $xn = -sin($t) * cos($p);
    $yn = -sin($t) * sin($p);
    $zn = -cos($t);
    
    printf("%3d %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f %12.8f %12.8f %12.8f %12.8f %12.8f\n",
	   $i,$a,$b,$c,$x,$y,$z,$t,$p,$xn,$yn,$zn);
}

print "# EOF\n";

