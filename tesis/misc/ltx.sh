#! /bin/bash
########################################################
##
## ltx.sh
##
## Bash shell script to process the thesis files
## in both versions english (eng => -i) and
## spanish (esp => -e).
##
## Copyright (c) 1998, J C Gonzalez
##
########################################################

#set -x

TEXINPUTS=:.//
TESIS=common/tesis
TESISCVS=/hd62/gonzalez/tesis-cvs
export TEXINPUTS
export TESIS
export TESISCVS

latex="latex"
dopdf="n"

THIS=`pwd`
export THIS

#-- run latex
do_latex () {
    $latex tesis-$1
}

#-- run bibtex
do_bibtex () {
    bibtex tesis-$1
}

#-- run both latex and bibtex - stops at errors
do_fulltex () {
    $latex tesis-$1 && bibtex tesis-$1 && $latex tesis-$1 && $latex tesis-$1
}

do_fullidxtex () {
    $latex tesis-$1 && bibtex tesis-$1 && makeindex tesis-$1 && $latex tesis-$1 && $latex tesis-$1
}

#-- select either "esp" or "eng" version
selecciona () {
    TEXINPUTS=:${THIS}:${THIS}/common:${THIS}/figs:${THIS}/$1
    export TEXINPUTS
    rm ${THIS}/eng.tex ${THIS}/esp.tex  1> /dev/null 2>&1
    cat << EOF > ${THIS}/$1.tex
%% TESIS version $1
\typeout{^^J^^J=======================================================}
\typeout{Compiling TESIS, version "$1".}
\typeout{=======================================================^^J^^J}
\endinput
%%EOF
EOF
}

#-- initialize and run
run () {
    mkdir scratch 2>/dev/null || echo ''
    cd scratch
    #for i in aux lof lot toc log; do
    #  rm *.$i  > /dev/null
    #done
    for i in tesis.tex tesis.bib jc.bst; do
        cp ${THIS}/common/$i ./
    done
    mv tesis.tex tesis-$2.tex
    $1 $2
    cd ../

#    if [ "$latex" = "pdflatex" ]; then
#        ln -sf scratch/tesis-$2.pdf .
#    else
#        ln -sf scratch/tesis-$2.dvi .
#    fi

    if [ "$dopdf" = "y" ]; then
        cp scratch/tesis-$2.dvi figs/
        cd figs
        dvips tesis-$2.dvi -o tesis-$2.ps
        ps2pdf tesis-$2.ps tesis-$2.pdf
        mv tesis-$2.pdf ../scratch
        rm tesis-$2.dvi
        cd ..
        ln -sf scratch/tesis-$2.pdf .
    else
        ln -sf scratch/tesis-$2.dvi .
    fi
}

#
# MAIN
#

args=$@
if [ -z "$args" ]; then
    echo ''
    echo 'ltx.sh'
    echo ''
    echo 'options:'
    echo '          -e     espan~ol'
    echo '          -i     ingles'
    echo '          -f     full compilation'
    echo '          -x     full compilation + index'
    echo '          -p     generated PDF'
    echo ''
    exit 0
fi

set -- `getopt eipfx $*`
if [ $? -ne 0 ]; then
    exit 1
fi

esp=0
eng=0
full=0
full2=0

#-- see options

while [ $1 != -- ]; do
    case $1 in
        -e)  # selecciona esp (espa~nol)
             esp=1;;
        -i)  # selecciona eng (ingles)
             eng=1;;
        -p)  # selecciona eng (ingles)
             dopdf="y";;
             #latex="pdflatex";;
        -f)  # do full compilation
             full=1;;
        -x)  # do full compilation
             full2=1;;
    esac
    shift
done

#-- compile

# if esp
if [ $esp -eq 1 ]; then

    selecciona esp

    if [ $full -eq 1 ]; then
        run do_fulltex esp
    elif [ $full2 -eq 1 ]; then
        run do_fullidxtex esp
    else
        run do_$latex esp
    fi

fi

# if eng
if [ $eng -eq 1 ]; then

    selecciona eng

    if [ $full -eq 1 ]; then
        run do_fulltex eng
    elif [ $full2 -eq 1 ]; then
        run do_fullidxtex eng
    else
        run do_$latex eng
    fi

fi



