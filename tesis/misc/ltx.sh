#! /bin/sh 
#set -x

TEXINPUTS=:.//
TESIS=common/tesis
TESISCVS=/hd62/gonzalez/tesis-cvs
export TEXINPUTS
export TESIS
export TESISCVS

THIS=`pwd`
export THIS

#-- run latex
do_latex () {
    latex tesis
}

#-- run bibtex
do_bibtex () {
    bibtex tesis
}

#-- run both latex and bibtex - stops at errors
do_fulltex () {
    latex tesis && bibtex tesis && latex tesis && latex tesis
}
    
#-- select either "esp" or "eng" version
select () {
    TEXINPUTS=:${THIS}/common:${THIS}/figs:${THIS}/$1
    export TEXINPUTS
}

#-- checkout either "esp" or "eng" version
checkout () {
    cvs -d $TESISCVS checkout common figs $1
}

#-- initialize and run
run () {
    mkdir scratch || echo "\c"
    cd scratch
    for i in tesis.tex tesis.bib my-utphys.bst; do 
	ln -sf ${THIS}/common/$i ./
    done
    $1
    for i in tesis.tex tesis.bib my-utphys.bst; do 
	rm $i
    done
    cd ../
    ln -sf scratch/tesis.dvi .
}

ltx () {
    run do_latex
}

fullrun () {
    run do_fulltex
}

args=$@
set -- `getopt eifg $*`
if [ $? -ne 0 ]; then
  exit 1
fi

esp=0
eng=0
full=0
get=0

#-- see options

while [ $1 != -- ]; do
  case $1 in
    -e)  # select esp (espa~nol)
         esp=1
    -i)  # select eng (ingles)
         eng=1
    -f)  # do full compilation
         full=1
    -o)  # set the output file name
         get=1
  esac
  shift
done

#-- compile

# if esp
if [ $esp -eq 1 ] then
  select esp
  if [ $get -eq 1 ] then
    checkout esp
  fi
  if [ $full -eq 1 ] then
    fullrun
  else
    ltx
  fi
fi

# if eng
if [ $eng -eq 1 ] then
  select eng
  if [ $get -eq 1 ] then
    checkout eng
  fi
  if [ $full -eq 1 ] then
    fullrun
  else
    ltx
  fi
fi



