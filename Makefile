# Name of the main output
TEXFILE = paper

# Names of all figures to include
FIGURES := correlation.pdf us_debt.pdf na_plot_KOR.pdf

############################################################

# Main target
# Running twice to make sure to get the figure references
$(TEXFILE).pdf : $(TEXFILE).tex $(FIGURES)
	pdflatex $(TEXFILE).tex
	pdflatex $(TEXFILE).tex

# A recipe describing how to make .pdf files
# Here foo.R generates foo.pdf
%.pdf : %.R
	R CMD BATCH $<

view : 
	open $(TEXFILE).pdf

clean :
	rm *.aux *.log *.nav *.out *.snm *.toc *.vrb *.Rout
