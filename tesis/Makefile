##################################################################
#
# makefile -- Makefile for processing Ph.D.Thesis
#
# $RCSfile$
# $Revision$
# $Author$ 
# $Date$
#
##################################################################
#

# NEEDS GNUMake !!!

THIS=$(shell pwd)

RUN=texis
RUNPDF=pdf$(RUN)

RUNBIB=bibtex

RUNIDX=makeindex

DVIPS=dvips

DOC=tesis
DOCPDF=pdf$(DOC)

OUTDVI=$(TESIS).dvi
OUTPDF=$(TESIS).pdf
OUTPS=$(TESIS).ps

COMMON_PATH=$(THIS)/common
SCRATCH_PATH=$(THIS)/scratch
FIGS_PATH=$(THIS)/figs
ENG_PATH=$(THIS)/eng
ESP_PATH=$(THIS)/esp

ADD_TEXINPUTS_ENG=$(THIS):$(COMMON_PATH):$(FIGS_PATH):$(ENG_PATH)
ADD_TEXINPUTS_ESP=$(THIS):$(COMMON_PATH):$(FIGS_PATH):$(ESP_PATH)

LOGFILE=$(SCRATCH_PATH)/$(DOC).out
WRNFILE=$(SCRATCH_PATH)/warning
FULFILE=$(SCRATCH_PATH)/overfuls
LSTFILE=$(SCRATCH_PATH)/filelist
MEMFILE=$(SCRATCH_PATH)/memory

ANALYZELOG=$(COMMON_PATH)/log.awk

DOCSRC=$(COMMON_PATH)/$(DOC).tex
BIBSRC=$(COMMON_PATH)/jc.bst
BIBLIO=$(COMMON_PATH)/tesis.bib $(COMMON_PATH)/magic.bib
ENGINES=$(COMMON_PATH)/$(RUN) $(COMMON_PATH)/$(RUNPDF)
FORMATS1=$(COMMON_PATH)/$(RUN).fmt $(COMMON_PATH)/$(RUNPDF).fmt
FORMATS2=$(COMMON_PATH)/supp-mis.tex $(COMMON_PATH)/supp-pdf.tex 
FORMATS=$(FORMATS1) $(FORMATS2)

COMMON_FILES=$(DOCSRC) $(BIBSRC) $(BIBLIO) $(ENGINES) $(FORMATS)

.PHONY: eng esp ps pdf dvi all prepare banner log
.PHONY: clean mrproper redo
.PHONY: clearlang lang
.PHONY: engpdf pdfeng esppdf pdfesp
.PHONY: engps pseng espps psesp
.PHONY: virtex 
.PHONY: common $(COMMON_FILES)
.PHONY: $(OUTDVI) $(OUTPDF) $(OUTPS)

#----------------------------------------------------------------------

engpdf pdfeng: eng pdf

esppdf pdfesp: esp pdf

engps pseng: eng ps

espps psesp: esp ps

esp eng ps pdf::
	echo $@ > .$@

all: ps pdf

ps:: dvi $(OUTPS) clearlang

dvi: prepare $(OUTDVI) log clearlang

pdf:: prepare $(OUTPDF) log clearlang

prepare: banner common lang

virtex: 
	cd $(COMMON_PATH);\
	cp $(COMMON_PATH)/$(RUN).tex $(COMMON_PATH)/$(RUNPDF).tex;\
	initex '&latex '$(COMMON_PATH)/$(RUN).tex' \dump' && \
	pdfinitex '&pdflatex '$(COMMON_PATH)/$(RUNPDF).tex' \dump';\
	ln -sf `which virtex` $(COMMON_PATH)/$(RUN) && \
	ln -sf `which pdfvirtex` $(COMMON_PATH)/$(RUNPDF)

common: $(COMMON_FILES)
	if [ -f .esp ]; then \
	  ln -sf $(SCRATCH_PATH)/$(DOC).tex $(SCRATCH_PATH)/$(DOC)-esp.tex;\
	else \
	  ln -sf $(SCRATCH_PATH)/$(DOC).tex $(SCRATCH_PATH)/$(DOC)-eng.tex;\
	fi

$(COMMON_FILES):
	@echo Making symbolic link for $@ . . .
	@ln -sf $@ $(SCRATCH_PATH)/

lang:
	if [ -f .ps ]; then \
	  echo "%% TESIS version PostScript" > language.tex;\
	else \
	  echo "%% TESIS version PDF" > language.tex;\
	fi
	echo "\newif\ifenglish" >> language.tex
	if [ -f .eng ]; then \
	  echo "\englishtrue" >> language.tex;\
	else \
	  echo "\englishfalse" >> language.tex;\
	fi
	( echo "\typeout{^^J===================================}";\
	echo "\ifenglish" ;\
	echo "\typeout{Compiling THESIS, English version...}";\
	echo "\else ";\
	echo "\typeout{Compilando TESIS, version Español...}";\
	echo "\fi";\
	echo "\typeout{===================================^^J}";\
	echo "\ifenglish ";\
	echo "\usepackage[english]{babel}";\
	echo "\else ";\
	echo "\usepackage[spanish,activeacute]{babel}";\
	echo "\fi";\
	echo "\endinput" ) >> language.tex

$(OUTDVI):
	if [ -f .esp ];then \
	  TEXINPUTS=.:$(ADD_TEXINPUTS_ESP):$${TEXINPUTS};\
	else \
	  TEXINPUTS=.:$(ADD_TEXINPUTS_ENG):$${TEXINPUTS};\
	fi;\
	TEXPSHEADERS=$${TEXINPUTS};\
	export TEXINPUTS;\
	export TEXPSHEADERS;\
	cd $(SCRATCH_PATH);\
	for run in $(RUN) $(RUN) $(RUNBIB) $(RUNIDX) $(RUN); do \
	  if [ -f .esp ]; then \
	    $$run $(DOC)-esp.tex 2>&1 | tee $(LOGFILE);\
	  else \
	    $$run $(DOC)-eng.tex 2>&1 | tee $(LOGFILE);\
	  fi; \
	done;\
	cd -;\
	if [ -f .esp ]; then \
	  ln -sf {$(SCRATCH_PATH),$(THIS)}/$(DOC)-esp.dvi;\
	else \
	  ln -sf {$(SCRATCH_PATH),$(THIS)}/$(DOC)-eng.dvi;\
	fi

$(OUTPS): $(OUTDVI)
	cd $(FIGS_PATH); \
	if [ -f .esp ]; then \
	  $(DVIPS) -o $(THIS)/$(DOC)-esp.{ps,dvi};\
	else \
	  $(DVIPS) -o $(THIS)/$(DOC)-eng.{ps,dvi};\
	fi; \
	cd -

$(OUTPDF):
	if [ -f .esp ];then \
	  TEXINPUTS=.:$(ADD_TEXINPUTS_ESP):$${TEXINPUTS};\
	else \
	  TEXINPUTS=.:$(ADD_TEXINPUTS_ENG):$${TEXINPUTS};\
	fi;\
	TEXPSHEADERS=$${TEXINPUTS};\
	export TEXINPUTS;\
	export TEXPSHEADERS;\
	cd $(SCRATCH_PATH);\
	for run in $(RUNPDF) $(RUNPDF) $(RUNBIB) $(RUNIDX) $(RUNPDF); do \
	  if [ -f .esp ]; then \
	    $$run $(DOC)-esp 2>&1 | tee $(LOGFILE);\
	  else \
	    $$run $(DOC)-eng 2>&1 | tee $(LOGFILE);\
	  fi; \
	done;\
	cd -;\
	if [ -f .esp ]; then \
	  ln -sf {$(SCRATCH_PATH),$(THIS)}/$(DOC)-esp.pdf;\
	else \
	  ln -sf {$(SCRATCH_PATH),$(THIS)}/$(DOC)-eng.pdf;\
	fi

log:
	-rm -f $(FULFILE)
	-rm -f $(LSTFILE)
	-rm -f $(WRNFILE)
	cd $(SCRATCH_PATH);\
	awk -f $(ANALYZELOG) $(LOGFILE);\
	if [ -f .esp ]; then \
	  tail -30 $(DOC)-esp.log | grep "^ [0-9]" > $(MEMFILE);\
	else \
	  tail -30 $(DOC)-eng.log | grep "^ [0-9]" > $(MEMFILE);\
	fi; \
	cd -
	echo "";echo "#### Over/Underfuls ########################"
	cat $(FULFILE)
	echo "";echo "#### List of files #########################"
	cat $(LSTFILE)
	echo "";echo "#### Warnings ##############################"
	cat $(WRNFILE)

banner:
	@echo "----------------------------------------"
	@echo " Generating Ph.D.Thesis output" 
	@echo ""
	@echo " Language:      "$(DOCLANG)
	@echo " Output format: "$(OUTFMT)
	@echo "----------------------------------------"

clearlang:
	cd $(THIS)
	-rm -rf .eng .esp .pdf .ps language.tex

clean: clearlang
	@echo "Cleanning..."
	-rm -f $(SCRATCH_PATH)/* 

mrproper: clean
	@echo "Mr.Proper in action . . ."
	-find . -name "*~" -exec rm -f {} \;
	-rm -f $(THIS)/tesis-{eng,esp}.{ps,dvi,pdf}

redo: clean all

#EOF