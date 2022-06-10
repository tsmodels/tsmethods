# tsmethods [![R](https://github.com/tsmodels/tsmethods/workflows/R/badge.svg)](https://github.com/tsmodels/tsmethods/actions?query=workflow%3AR) [![Build Status](https://travis-ci.org/tsmodels/tsmethods.svg?branch=master)](https://travis-ci.org/tsmodels/tsmethods) [![Build status](https://ci.appveyor.com/api/projects/status/github/tsmodels/tsmethods?branch=master)](https://ci.appveyor.com/project/kthohr/tsmethods/branch/master)
Foundation packages with general methods/classes used by other **tsmodels** packages as well as plotting for prediction and distribution classes conforming to the current probabilistic framework. Also includes functions for ensembling using a copula methodology.

## Installation

For installation instructions and vignettes for **tsmodels** packages, see https://tsmodels.github.io/.

## To Do

[x] Create tsequation for each of the packages to generate latex output of both the generic model equation used as well as option for populating with actual parameter values.

[x] Switch to using either ggplot2 or plotly for plotting output of tsmodel.predict and tsmodel.distribution.

[x] Check consistency of colors and output for plots across packages (for estimated object).

[x] Check consistency of summary output across packages (for estimated object).

[x] Create tsanimate method for animating walkthrough in tsbacktest output.

[x] Convert documentation to roxygen [ongoing]

[x] Enhance tsconvert method to generate SEM model from MEM and add appendix in vignette for proof based on Casals et al. paper (James Nesbit).
