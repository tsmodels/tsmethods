\name{ensemble_modelspec}
\alias{ensemble_modelspec}
\title{
Ensemble specification
}
\description{
Creates and validates an ensemble specification.
}
\usage{
ensemble_modelspec(\ldots)
}
\arguments{
\item{\ldots}{
Objects of either all of class \dQuote{tsmodel.predict} or \dQuote{tsmodel.distribution} 
representing the probabilistic forecasts for the same horizon of optionally different 
models on the same series and with same number of draws. It is expected that the predictive 
distributions are based on joint simulated draws passed to the \code{innov} argument in
the \code{predict} function of the supporting models. Instead of \ldots it is also possible
to pass a list of the objects.
}
}
\details{
Creates and validates the objects to be passed to the \link{tsensemble} method.
}
\value{
An object of class \dQuote{ensemble.spec}.
}
\author{
Alexios Galanos
}
