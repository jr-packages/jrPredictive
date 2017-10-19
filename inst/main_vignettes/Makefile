#Make and Makefiles:
#A Quick Introduction
#Ross Ihaka

## $* = filename without extension
## $@ = the output file
## $< = the input file

.SUFFIXES: .tex .pdf .Rnw .R

PKG = jrPredictive
DIR = vignettes

PRACS = practical1 practical2 practical3
SOLS = solutions1 solutions2 solutions3
OTHER = practical4
ALL =  $(PRACS)  $(SOLS) $(OTHER)

SOLSPDF = $(SOLS:=.pdf)
PRACSPDF = $(PRACS:=.pdf)
OTHERPDF = $(OTHER:=.pdf)
ALLPDF = $(ALL:=.pdf)

PRACSRNW =  $(PRACS:=.Rnw)
SOLSRNW =  $(SOLS:=.Rnw)

.PHONY: force sols all clean commit cleaner

#force:
#	make -C $(DIR) -f ../Makefile all

check.txt: $(PRACSPDF) $(OTHERPDF)
	make solutions*.Rnw
	make sols
	cp -fvr knitr_figure graphics ../../vignettes
	touch check.txt



#R: $(RFILES)
solutions%.Rnw: $(PRACSPDF)
	## Copy practical file to solutions
	$(foreach var, \
		$(PRACSRNW), \
		cp $(var) $(subst practical, solutions, $(var);))

	## Updated to show results
	$(foreach var, \
		$(SOLSRNW), \
		sed -i "5s/.*/results='show';echo=TRUE/" $(var);)

sols: $(SOLSPDF)


view: $(MAIN).pdf
	xpdf $(MAIN).pdf &

#.Rnw.R: TO FIX
#	R CMD Stangle $<

build:
	make all
#	make cleaner
	cd ../../../ && R CMD build $(PKG)

check:
	make build
	cd ../../../ && R CMD check --as-cran $(PKG)_*.tar.gz

.Rnw.pdf:
	sed -i "1s/.*/%\\\VignetteIndexEntry{$*}/" $*.Rnw
	Rscript  -e "require(knitr); knit('$*.Rnw', output='$*.tex')"
	pdflatex $*.tex
	pdflatex $*.tex
	cp -fvr $*.tex ../../vignettes/$*.Rnw


clean:
	rm -fvr knitr_figure figure graphics cache
	rm -fvr $(ALLPDF)  *.tex *.synctex.gz \
		*.aux *.dvi *.log *.toc *.bak *~ *.blg *.bbl *.lot *.lof \
		 *.nav *.snm *.out *.pyc \#*\# _region_* _tmp.* *.vrb \
		Rplots.pdf example.RData d.csv.gz mygraph.* \
		knitr_* knitr.sty .Rhistory ex.data check.txt
	cd ../../vignettes && rm -fvr *.aux *.out knitr_cache/ *.R
cleaner:
	make clean
	rm -fvr auto/
#	cd ../ && rm -fvr $(PKG).Rcheck $(PKG)_*.tar.gz


