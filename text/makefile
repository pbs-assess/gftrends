REFS = refs

all: gf2022-sopo.pdf

gf2022-sopo.pdf: gf2022-sopo.Rmd $(REFS).bib csas.csl ../figs/ridges.png ../figs/ts-summary.png ../figs/stock-vs-indices.png
	Rscript -e "rmarkdown::render('gf2022-sopo.Rmd')"
continuous:
	while true; do make --silent; sleep 0.25; done
