#! /bin/sh

release="$1"
full_dist=`cat FILES`
code_dist=`cat FILES-c520`

release_path="mmcs-${release}"
full_dist_release="mmcs-${release}.tar.gz"
code_dist_release="mmcs-c520-${release}.tar.gz"

rm -r ${release_path}
mkdir ${release_path}
cd ${release_path}

for i in ${full_dist}; do
    echo "Linking $i . . ."    
    ln -s ../$i ./
done

cd ..

echo "Packing full distribution . . . "

gtar cvzhf ${full_dist_release} ${release_path}/*

rm ${release_path}/*
cd ${release_path}

for i in ${code_dist}; do
    echo "Linking $i . . ."
    ln -s ../$i ./
done

cd ..

echo "Packing code distribution . . . "
 
gtar cvzhf ${code_dist_release} ${release_path}/*

rm -r ${release_path}

exit 0


