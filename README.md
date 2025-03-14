# proof
This repository contains code and data to accompany [Proof: The Uncertain Science of Certainty](https://proof.kucharski.io/) by Adam Kucharski.

If you use any of the code/analysis under the GNU license, please cite 'Kucharski AJ, The Rules of Contagion: Why Things Spread - and Why They Stop, Wellcome Collection/Profile Books, 2020' as well as any relevant data sources and packages (listed below).

### Guide to files

The main script to reproduce the analysis is in `scripts/main_plot_figures.R`. This script calls the following files:

> `R/plotting_functions.R` - Loads and plots data

> `R/set_plot.R` - Custom plot settings

As you'd expect, data is the the `data` folder and plots are output in the `plots` folder.

### References for datasets and packages

#### Chapter 2

* Koch snowflake plot [geostats package](XX)

#### Chapter 5

* PHE Variants of Concern Technical Briefing 10: [Supporting dataset](https://www.gov.uk/government/publications/investigation-of-novel-sars-cov-2-variant-variant-of-concern-20201201)
* outbreakinfo variant proportions in India: [Variant explorer dataset](https://outbreak.info/)
* covidregionaldata R package: [Palmer et al., (2021). covidregionaldata: Subnational data for COVID-19 epidemiology. Journal of Open Source Software, 6(63), 3290](https://joss.theoj.org/papers/10.21105/joss.03290)

#### Chapter 6

* Perceptions of: [GitHub code](https://github.com/zonination/perceptions)
* Effect sizes in clinical trials over time: [Kaplan and Irvin, PLOS ONE, 2015](10.1371/journal.pone.0132382)

