BEGIN {
    print "u:=10pt;";
}

($1==398) {
    print "";
}

($1==416) {
    print "u:=20pt;";
    getline;getline;
}

($1>595) {
    exit;
}

{
    xx = $5*1pt;
    yy = $6*1pt;
    printf "hex( (%du,%du) );\n", xx, yy;
}

