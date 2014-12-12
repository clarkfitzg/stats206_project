# Name of the main output
TEXFILE = paper

# Names of all figures to include
FIGURES := correlation.pdf us_debt.pdf na_plot_KOR.pdf 

#scatterplot.pdf

############################################################

# Main target
# Running twice to make sure to get the figure references
$(TEXFILE).pdf : $(TEXFILE).tex $(FIGURES) country.Rda
	pdflatex $(TEXFILE).tex
	pdflatex $(TEXFILE).tex

# Local cache of the data as returned from Quandl
cache.Rda : download.R quandl_template.csv
	R CMD BATCH $<

country.Rda : preprocess.R cache.Rda functions.R
	R CMD BATCH $<

# A recipe describing how to make .pdf files
# Here foo.R generates foo.pdf
%.pdf : %.R
	R CMD BATCH $<

analysis : analysis.R
	R CMD BATCH $<

view : 
	open $(TEXFILE).pdf

clean :
	rm *.aux *.log *.nav *.out *.snm *.toc *.vrb *.Rout
