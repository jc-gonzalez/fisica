#!/bin/sh
#set -x
for e in 10 20 40 60 100 500 1000; do
    rm *000*
    sed "s/#E#/$e/g" in-sh > in-shx
    time c520-osf < in-shx 1> out$e 2> time$e
    ls -l cer*00* |awk '{s+=$5}END{print s}' >> time$e
    ls -l dat*00* |awk '{s+=$5}END{print s}' >> time$e
done

