#!/bin/sh

numevt=100000000
bines="4000 2000 1000 500 400 200 100 50 40 30 20 10 6 4 3 2 1"
#bines="500 400 200 100 50 40 30 20 10 6 4 3 2 1"


index=-1.5

for i in $bines; do

	nbins=`expr $i \* 4`

	fits 1. 10000. ${nbins} ${index} $numevt} 1 > index:${index}:${nbins}

done

index=-2.0

for i in $bines; do

	nbins=`expr $i \* 4`

	fits 1. 10000. ${nbins} ${index} $numevt} 1 > index:${index}:${nbins}

done

index=-2.5

for i in $bines; do

	nbins=`expr $i \* 4`

	fits 1. 10000. ${nbins} ${index} $numevt} 1 > index:${index}:${nbins}

done

index=-3.0

for i in $bines; do

	nbins=`expr $i \* 4`

	fits 1. 10000. ${nbins} ${index} $numevt} 1 > index:${index}:${nbins}

done

