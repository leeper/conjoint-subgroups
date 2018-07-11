all: manuscript.pdf

manuscript.pdf: manuscript.Rnw references.bib apsa-leeper.bst
	Rscript -e "knitr::knit2pdf('manuscript.Rnw')"
