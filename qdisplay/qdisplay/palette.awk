BEGIN {
    i=0;
}

(/^#/) {
    print "# Palette ",name,", J C Gonzalez, 2000";
    next;
}

(NR<5) {
    print;
}

(NR>4) {

    r=$1;    getline;
    g=$1;    getline;
    b=$1;

    i++;
#    printf("%3d %3d %3d      #%2d : 0x%06x\n", r, g, b, i, r*256*256+g*256+b);
    printf("%3d %3d %3d\n", r, g, b);
}

END {
    print "#EOF";
}
