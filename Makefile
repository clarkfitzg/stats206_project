# Names of all figures to include
FIGURES := sin.pdf lines.pdf

############################################################

# Main target
paper.pdf : paper.tex $(FIGURES)
	pdflatex paper.tex

# A recipe describing how to make .pdf files having no other recipes
# Here foo.R generates foo.pdf
%.pdf : %.R
	R CMD BATCH $<

clean :
	rm *.aux *.log *.nav *.out *.snm *.toc *.vrb *.Rout
