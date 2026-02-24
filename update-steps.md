This repository contains:

* a script to go from raw data (MCMC samples usually; stored locally) to cleaned and processed data (`data-raw/process-raw-data.R`)
* the processed data sets themselves as `.rds` files (`data-generated`)
* the model in Stan `analysis/rw-ss.stan`
* a simulation test of the model (`analysis`)
* code to fit the model and make figures (`analysis`)
* text for the preprint (`preprint`)

### A. Adding and processing the latest assessment data

1. Use the `data-raw/process-raw-dat.R` file to clean any newly added/updated assessment data (raw data can be placed in `data-raw/model-output`). This script saves outputs as: `data-raw/species-region-mcmc-year.rds`, where species name is often abbreviated and year is the last year with included data.
    - Contact people for the latest MCMCs of the biomass trend and Bmsy (or other LRP and USR MCMC's). LRP and USR are generally 0.4 * Bmsy and 0.8 * Bmsy (e.g., see: [A fishery decision-making framework incorporating the precautionary approach](https://www.dfo-mpo.gc.ca/reports-rapports/regs/sff-cpd/precaution-eng.htm)) or some fraction of B_0 or historical biomass. Double check the stock assessment document to confirm. 
    - For example: Bmsy is not used for Arrowtooth Flounder [e.g., see pg 5](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2023/2023_042-eng.pdf), Pacific Cod [e.g., see pg 4](https://publications.gc.ca/collections/collection_2021/mpo-dfo/fs70-7/Fs70-7-2021-002-eng.pdf), Walleye Pollock [e.g., see pg 3](https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40987395.pdf), or Dogfish

If the LRP and USR are based on fractions of Bmsy, process new data to get dataframe of the format: 

| year | b    | bmsy | lrp | usr | species             | region | iter | run |
| year | b    | bmsy | lrp | usr | species             | region | iter | run |
| ---- | ---- | ---- | --- | --- | ------------------- | ------ | ---- | --- |
| 2018 | 1370 | 523  | 209 | 418 | pacific-ocean-perch  | 3CD    | 1    | 1   |
| 2019 | 1400 | 523  | 209 | 418 | pacific-ocean-perch  | 3CD    | 1    | 1   |
| 2020 | 1530 | 523  | 209 | 418 | pacific-ocean-perch  | 3CD    | 1    | 1   |

If the LRP and USR are not based on fractions of Bmsy, the dataframe will look like this:

| year | blrp | busr | species   | region | iter | run |
| year | blrp | busr | species   | region | iter | run |
| ---- | ---- | ---- | --------- | ------ | ---- | --- |
| 1918 | 7.26 | 3.63 | quillback | 4B     | 1    | 1   |
| 1918 | 9.00 | 4.50 | quillback | 4B     | 2    | 1   |
| 1918 | 8.34 | 4.17 | quillback | 4B     | 3    | 1   |


- If there is no `run` column, you will need to add a placeholder, e.g., `mutate(run = 1)`; `run` has been used in the past as column for identifying different scenarios that are run if the final status is generated from an ensemble.


2. Add any new stocks (if any) to the `analysis/stock_df.R` and `data-raw/species-regions-tofit.csv` (see [Section C](#a.-adding-and-processing-the-latest-assessment-data))


3. Update the `data-raw/last-assess-years.csv`, grab the info from the latest stock assessments/stock assessment drafts in review. `mcmc_year` is the last year MCMC samples in the raw MCMC data -- which should be the status year (this is often after the last year of catch as population dynamics defines the biomass the next year before fishing has occured). The `last_data_year` is the last year to which data was fitted, and `plus_one` is how many years in between the last data year and the status year `mcmc_year`. This `plus_one` can probably be removed next year. 

4. Update the file `data-raw/surveys_to_assessments.csv` if needed.

### B. Analyse the assessment data

The scripts contained in `analysis` summarise the **assessment data**. The following scripts should be run in order (as numbered) and should not need to be modified year to year. With the exception of updating `end_year` in `analysis/01-stitch-data.R` L.5 and `analysis/stock_df.R`.

1. `analysis/01-stitch-data.R` can be run once the MCMC data files have been updated, collates the processed assessment data. **UPDATE THE `end_year` at the top of the file**. This script also outputs a figure that you can use to check that all the B status ratios loaded properly and were updated properly before you fit the Stan model.

2. `analysis/02-fit-models.R` compiles and fits the Stan model.

3. `analysis/03-plot-models.R` plots model fits of the assessment time-series.

4. `analysis/04-ridges.R` plots posterior distributions of estimated biomass over limit reference points

5. `analysis/05-combine-plots` --> archive

6. `analysis/06-values.R` --> archive


### C. Analyse the survey data

The scripts contained in `analysis-survey` summarise the **survey data**. The following scripts should be run in order (as numbered) and should not need to be modified year to year. 

If new species or assessment regions are added, you will need to update:
`data-raw/species-regions-tofit.csv` and `analysis-survey/get-all-spp.R`.

1. `analysis-survey/00-get-all-spp.R` is run to get the latest data on all the species of interest. This list may or may not need to be updated. Note this queries GFBio and will need to access to the DFO network.

2. `analysis-survey/02-render-indices.R` 
    - `analysis-survey/01-index-new-deltas.Rmd` is not manually run. It generates html files for each species and fits the delta-gamma model. If the delta-gamma fails to converge, the Tweedie is then fit. In 2023 all delta-gamma models converged. 
    - `analysis-survey/00-make-grids.R` is also sourced by `02-render-indices.R`. If prediction grids are updated this can be done here.

3. `analysis-survey/03-calc-slopes.R` calculates the slopes of indices since 2000 so they can be ordered in Figure 3.

4. `analysis-survey/04-join.R` joins the assessment data with survey indices and scales by the geometric mean.

5. `analysis-survey/05-plot-assess-surveys.R` plots the latest assessments overlaid with the survey indices by species-region. 

6. `analysis-survey/06-sopo-text.Rmd` can be rendered to get an updated draft. Values used in the text are calculated at the top of the document, use those and manually update last year's word document (update with what SOPO sends as the template/version from the previous year), or choose to use the markdown document. Currently, references are not included and are added manually. 

7. `analysis-survey/06-plot-presentation-figs.R` generates the figures for the presentation. 
