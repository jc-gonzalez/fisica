function max(a,b) {
    if (a>b) { return a; } else { return b; }
}

(/Warning:/)  {
    print "    " $0 >> "warning";
}

(/^Chapter/) {
    n=split(prev,a,"/");
    file=a[n];
}

(/^CHAPTER/) {
    gsub("CHAPTER:", "");
    print prev,$0,"(" file ")" >> "warning";
}

(/^ \*File List\*/) {
    flist = 1;
    next;
}

(/^ \*\*\*\*\*\*\*\*\*\*\*/) {
    flist = 0;
}

(/erful/) {
    print >> "overfuls";
}

{
    if (flist == 1) {
        if (NF == 1) {
            printf(" %-25s\n",$1) >> "filelist";
        } else {
            text=substr($0, max(length($1),12)+1);
            printf(" %-25s  %s\n",
                   $1,text) >> "filelist";
        }
    }

    prev=$0;
}
