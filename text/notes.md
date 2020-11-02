# Notes

* Add uncertainty on response
* Overall RW average
* Trend by flatfish rockfish etc.? Correlated random walks?
* Overall weighted equally or by biomass/catch?
* Kobe sliced by decade plot?
* Probability of increase or decline by decade?
* What to do about coastwide vs split up stocks?
* Gaussian process on x_t trend?
* Data collation side benefit; get into a proper database eventually?
* What to do when multiple scenarios per stock? Likely ensemble first?
* What about when MSE? Use OMs and ensemble first?
* Could also ensemble within the model, but adds some complexity and maybe doesn’t add much
* But keep separate and ensembles in dataset?
* What other metadata needed? Keep minimal. ID (ResDoc/SAR?), species, reference ID, Type of model... look at RAM Legacy for ideas, but keep minimal.

Data/metadata:
* By scenario/model for all models included in advice: B/Bmsy: mean of B/Bmsy; mean of log(B/Bmsy), SD/SE of log(B/Bmsy)
* Ensemble of all models included in advice (weighted if was weighted, but I don't think this has happened officially): same as above
* Same as above for F/Fmsy
* If available, estimate of mean SSB by year, again by model/scenario and ensembled
* Catch by year as entered into assessment, again by model/scenario and ensembled
* Meta-data:
  - citation
  - a unique ID (e.g., 'POP-2012')
  
Common formatted .csv files for now. Create example.

Or, maybe just record ensemble values for now (or 1 if 1 was chosen)

Instead of mean/SD, could save posterior samples of each scenario in `.rds` files so they can be summarized later.

E.g.:
* Folder: `POP-2012`
* Files: `bbmsy-1.rds`, `bbmsy-2.rds`, etc., each contains: `year`, `value`
* Files: `ffmsy-1.rds`, `ffmsy-2.rds`, etc., each contains: `year`, `value`
* Files: `ssb-1.rds`, `ssb-2.rds`, etc., each contains: `year`, `value`
* Files: `catch-1.rds`, `catch-2.rds`, etc., each contains: `year`, `value`
* File:  `citation.bib` BibTeX entry

Then write basic R code to read all into dataset. Can be summarized and input into SQL database from there.

If need to keep simple for now, just do B/Bmsy + basic metadata
