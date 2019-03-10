#!/bin/bash

curdir=$(pwd)
pkg=$(basename $curdir)

for i in README AUTHORS INSTALL THANKS BUGS NEWS TODO FAQ license ; do

    echo ""
    
    if [ "$i" == "license" ]; then

	echo ""
	echo "License"
	echo "-------"
	echo ""
	cat MIT-LICENSE.txt

    else

	cat $i

    fi

    echo ""

done > doc/${pkg}.md

