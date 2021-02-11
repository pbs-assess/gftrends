TEXT = gf2021-sopo
REFS = refs

all: $(TEXT).pdf

%.pdf: %.Rmd $(REFS).bib csas.csl ../figs/ridges.pdf ../figs/ts-summary.pdf
	Rscript -e "rmarkdown::render('gf2021-sopo.Rmd')"
continuous:
	while true; do make --silent; sleep 0.25; done
