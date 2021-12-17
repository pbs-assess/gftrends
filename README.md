# Trends in Pacific Canadian groundfish stock status

An analysis of trends in Pacific Canadian groundfish stock status for the annual [State of the Pacific Ocean](https://www.dfo-mpo.gc.ca/oceans/soto-rceo/pacific-pacifque/index-eng.html) conference/report and eventually an associated manuscript.

The model extends the one described in the appendix section *Estimating regional trends in abundance, fishing pressure and catch* of the paper [*Effective fisheries management instrumental in improving fish stock status*](https://doi.org/10.1073/pnas.1909726116) by Hilborn et al. The original model was developed by Cóilín Minto. This version:
* is Bayesian and fit with Stan
* allows for measurement error on stock status

<!-- * may ultimately allow for a hierarchy of stocks with correlated random walks-->

This repository contains:
* a script to go from raw data (MCMC samples usually; stored locally) to cleaned and processed data (`data-raw-process/process-raw-data.R`)
* the processed data sets themselves as `.rds` files (`data-generated`)
* the model in Stan `analysis/rw-ss.stan`
* a simulation test of the model (`analysis`)
* code to fit the model and make figures (`analysis`)
* text for the preprint (`preprint`)
