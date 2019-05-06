---
title: "Measuring Subgroup Preferences in Conjoint Experiments"
author: Thomas J. Leeper, Sara B. Hobolt, and James Tilley
date: May 06, 2019
output: github_document
---

This repository contains the following files:

 - manuscript.Rnw: a knitr document containing the manuscript and all analysis reported in the paper
 - manuscript.tex: an intermediate LaTeX document generated from `manuscript.Rnw`
 - references.bib: a BibTeX reference library used in manuscript.Rnw
 - apsa-leeper.bst: a BibTeX style file used in manuscript.Rnw
 - Makefile: a makefile for compiling the final manuscript from the above files
 - manuscript.pdf: a PDF containing the final manuscript and all analysis
 - data/ballard-rosa.dta: data for the Ballard-Rosa et al. study reproduced in the paper
 - data/bechtel_scheve_pnas.dta: data for the Bechtel and Scheve study reproduced in the paper
 - data/hainmueller-candidate.dta: data for the Hainmueller et al. candidate study reproduced in the paper
 - data/hainmueller-immigrant.dta: data for the Hainmueller et al. immigration study reproduced in the paper
 - data/teele.dta: data for the Teele et al. study reproduced in the paper

Running `make` on the command line will reproduce all results and the final `manuscript.pdf` from the above.
