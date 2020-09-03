\name{tsensemble.ensemble.spec}
\alias{tsensemble.ensemble.spec}
\alias{tsensemble}
\title{
Ensembles predictions using their posterior predictive or simulated distribution.
}
\description{
Ensembles posterior predictive or simulated distributions spanning the same forecast horizon.
}
\usage{
tsensemble(object, weights, \dots)
\method{tsensemble}{ensemble.spec}(object, weights, \dots)
}
\arguments{
\item{object}{
Currently only dispatches based on objects of class \dQuote{ensemble.spec} which
have been validated for ensembling.
}
\item{weights}{
A numeric vector of length equal to the length of the ensembling predictions, 
summing to 1, which represent the ensembling weights.
}
\item{\dots}{
Any additional arguments required (none at present)
}
}
\details{
Returns the weighted distribution, under the assumption that the predictions were generated
using a joint error distribution whose values were passed to the \code{innov} argument of
the predict function used for each model.
}
\value{
An object of class \dQuote{tsmodel.predict} or \dQuote{tsmodel.distribution} depending on
the input class in \code{\link{ensemble_modelspec}}.
}
\author{
Alexios Galanos
}
