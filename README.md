
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DabomPriestRapidsCoho

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/BiomarkABS/DabomPriestRapidsCoho/master?urlpath=rstudio)

This repository contains the data and code for running the **D**am
**A**dult **B**ranch **O**ccupancy **M**odel
([DABOM](https://github.com/BiomarkABS/DABOM)) for adult Coho returning
above Priest Rapids Dam in the Upper Columbia River. This model
estimates escapement past various locations in the Upper Columbia using
detections of PIT tagged fish. The full methods are described in the
paper:

> Waterhouse, L., White, J., See, K.E., Murdoch, A. R. and Semmens,
> B.X., (2020). *A Bayesian nested patch occupancy model to estimate
> steelhead movement and abundance*. Ecological Applications
> <https://doi.org/10.1002/eap.2202>

### How to cite

Please cite this compendium as:

> See, K.E., (2021). \_Compendium of R code and data for Upper Columbia
> DABOM Coho Accessed 13 Sep 2021.

## Contents

The **analysis** directory contains:

  - [:file\_folder: data](/analysis/data): Data used in the analysis.
  - [:file\_folder: R\_scripts](/analysis/R_scripts): R scripts run as
    part of these analyses.
  - [:file\_folder: figures](/analysis/figures): Plots and other
    illustrations
  - [:file\_folder:
    supplementary-materials](/analysis/supplementary-materials):
    Supplementary materials including notes and other documents prepared
    and collected during the analysis.

The **outgoing** directory contains:

  - [:file\_folder: PITcleanr](/outgoing/PITcleanr): Initial output from
    `PITcleanr` with blank spaces in the `user_keep_obs` column that
    need to be filled in by the user with `TRUE` or `FALSE` to determine
    if that detection should be kept for `DABOM`.
  - [:file\_folder: estimates](/outgoing/estimates): Spreadsheets with
    tabs for abundance estimates, detection probability estimates and
    other biological summaries.

## How to run in your broswer or download and run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

The simplest way to explore the text, code and data is to click on
[binder](https://mybinder.org/v2/gh/BiomarkABS/DabomPriestRapidsCoho/master?urlpath=rstudio)
to open an instance of RStudio in your browser, which will have the
compendium files ready to work with. Binder uses rocker-project.org
Docker images to ensure a consistent and reproducible computational
environment. These Docker images can also be used locally.

You can download the compendium as a zip from from this URL:
[master.zip](/archive/master.zip). After unzipping: - open the `.Rproj`
file in RStudio - run `devtools::install()` to ensure you have the
packages this analysis depends on (also listed in the
[DESCRIPTION](/DESCRIPTION) file). - finally, open
`analysis/paper/paper.Rmd` and knit to produce the `paper.docx`, or run
`rmarkdown::render("analysis/paper/paper.Rmd")` in the R console

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.

# Project Notes
