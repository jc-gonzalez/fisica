########################################################################
##
## Makefile for 'tesis'
##
## Filename:       $RCSfile$ 
## Revision:       $Revision$
## Date:           $Date$ 
##----------------------------------------------------------------------
## $Id$
########################################################################

#----------------------------------------------------------------------
# destination directory
#
DESTDIR = .

#----------------------------------------------------------------------
# LaTeX compiler
#
LATEX = latex

#----------------------------------------------------------------------
# directories
#
TESISCOMMON = common
TESISESP = tesis-esp
TESISENG = tesis-eng

#----------------------------------------------------------------------
# these files will be checked out from the common directory
#
BIBFILE = tesis.bib
BIBLIOFILES = biblio.tex my-utphys.bst $(BIBFILE)
STYLEFILES = my-accents.tex my-bib.tex my-defs.tex my-hyphen.tex my-style.tex 
INDEXESFILES = contents.tex 
FIGURESFILES = figures.tex equations.tex tables.tex 
LOGINFO = log-info.tex 
PACKAGES = packages.tex 
MAIN = tesis.tex        
COMMON_FILES =  $(BIBLIOFILES) \
		$(STYLEFILES) \
		$(INDEXESFILES) \
		$(FIGURESFILES) \
		$(LOGINFO) \
		$(PACKAGES) \
		$(MAIN)

#----------------------------------------------------------------------
# these files will be checked out from the language directory
#
TITLE = title.tex
INTRO = intro.tex
CHAPTERS = ch01.tex
FIGURELABELS = figureslab.tex equationslab.tex tableslab.tex
APENDIXES = 
ACKNOWLEDGMENTS = 
DEDICATEDTO = 
ABSTRACT =        
LANGUAGE_FILES = $(TITLE) \
		 $(INTRO) \
		 $(CHAPTERS) \
		 $(FIGURELABELS) \
		 $(APENDIXES) \
		 $(ACKNOWLEDGMENTS) \
		 $(DEDICATEDTO) \
		 $(ABSTRACT)

#----------------------------------------------------------------------
# all files
#
ALL = $(COMMON_FILES) $(LANGUAGE_FILES)

#----------------------------------------------------------------------
# getting common files
#
get-common-title:
	@echo "Getting common files . . ."

$(COMMON_FILES):
	@(ln -fs $(TESISCOMMON)/$@ $@ && \
	echo " - " $@ " will be a symbolic link") || \
	echo " - " $@ " is a real file"

get-common-files: get-common-title $(COMMON_FILES)

#----------------------------------------------------------------------
# getting language files
#
get-lang-title:
	@echo "Getting files for [" $(LANG) "] . . ."

$(LANGUAGE_FILES):
	@(ln -fs $(TESISLANG)/$@ $@ && \
	echo " - " $@ " will be a symbolic link") || \
	echo " - " $@ " is a real file"

get-lang-files: get-lang-title $(LANGUAGE_FILES)

#----------------------------------------------------------------------
# set language to [espa~nol]
#
set-esp:
	LANG=espa~nol TESISLANG=$(TESISESP)

set-eng:
	LANG = english  \
	TESISLANG = $(TESISENG)

#----------------------------------------------------------------------
# compile the source files
#
compile-latex: dolatex dobib dolatex dolatex

dolatex: 
	$(LATEX) $(MAIN)

dobib:
	$(BIBTEX) $(BIBFILE)

#----------------------------------------------------------------------
# make all 
#
esp: set-esp get-lang-files get-common-files compile-latex

eng: set-eng get-lang-files get-common-files compile-latex

all:
