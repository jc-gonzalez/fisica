(/^%/) {
    next;
}

(NF>0) {
    item=$0;
    getline; eng=$0;
    getline; esp=$0;
    if (lang=="esp") {
        print item," &\n",esp," \\\\";
    } else {
        print item," &\n",eng," \\\\";
    }
}


        
