
[![R-CMD-check](https://github.com/tsmodels/tsmethods/workflows/R-CMD-check/badge.svg)](https://github.com/tsmodels/tsmethods/actions)
[![Last-changedate](https://img.shields.io/badge/last%20change-2023--03--23-yellowgreen.svg)](/commits/master)
[![packageversion](https://img.shields.io/badge/Package%20version-0.3.2-orange.svg?style=flat-square)](commits/master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/tsmethods)](https://cran.r-project.org/package=tsmethods)

# tsmethods

Foundation packages with general methods/classes used by other
**tsmodels** packages as well as plotting for prediction and
distribution classes conforming to the current probabilistic framework.
Also includes functions for ensembling using a copula methodology.

## Installation

For installation instructions and vignettes for **tsmodels** packages,
see <https://tsmodels.github.io/>.

## To Do

<input type="checkbox" unchecked></input> Switch to using either ggplot2
or plotly for plotting output of tsmodel.predict and
tsmodel.distribution.

<input type="checkbox" unchecked></input> Check consistency of colors
and output for plots across packages (for estimated object).

<input type="checkbox" unchecked></input> Check consistency of summary
output across packages (for estimated object).

<input type="checkbox" unchecked></input> Create tsanimate method for
animating walkthrough in tsbacktest output.

<input type="checkbox" unchecked></input> Merge ad (tsetsad, tsissmad,
tsvetsad) packages into main packages and remove non-ad versions.
Rewrite optimization to make use of scaling and implement sandwich
estimators if possible across all models.

<input type="checkbox" unchecked></input> Change tsbacktest to allow
rolling estimation with filtering.

<input type="checkbox" unchecked></input> Move all metrics and test into
new package and create unified print method for console and flextable
output.

<input type="checkbox" checked></input> Convert documentation to
roxygen.

<input type="checkbox" checked></input> Create tsequation for each of
the packages to generate latex output of both the generic model equation
used. \[Done for tsgarch and tsarma\].
