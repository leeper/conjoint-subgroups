all: si.pdf article.pdf

manuscript.pdf: manuscript.Rnw references.bib apsa-leeper.bst data/ballard-rosa.dta data/bechtel_scheve_pnas.dta data/hainmueller-candidate.dta data/hainmueller-immigrant.dta data/teele.dta Makefile
	Rscript -e "knitr::knit2pdf('manuscript.Rnw')"

si.pdf: manuscript.pdf
	pdftk manuscript.pdf cat 28-62 output si.pdf

article.pdf: manuscript.pdf
	pdftk manuscript.pdf cat 1-27 output article.pdf
