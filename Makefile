# Name of the main output
TEXFILE = paper

# Names of all figures to include
FIGURES := correlation.pdf us_debt.pdf

############################################################

# Main target
$(TEXFILE).pdf : $(TEXFILE).tex $(FIGURES)
	pdflatex $(TEXFILE).tex

# A recipe describing how to make .pdf files
# Here foo.R generates foo.pdf
%.pdf : %.R
	R CMD BATCH $<

view : 
	open $(TEXFILE).pdf

clean :
	rm *.aux *.log *.nav *.out *.snm *.toc *.vrb *.Rout
