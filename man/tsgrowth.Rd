\name{tsgrowth}
\alias{tsgrowth}
\alias{tsgrowth.tsmodel.predict}
\title{
Growth Calculation
}
\description{
Calculates the growth distribution from a predicted object.
}
\usage{
tsgrowth(object, \ldots)
\method{tsgrowth}{tsmodel.predict}(object, d = 1, 
type = c("diff","simple","log"),  \dots)
}
\arguments{
\item{object}{
An object of some S3 class.
}
\item{d}{
The period back to look at for growth calculations.
}
\item{type}{
The type of growth calculation. \dQuote{diff} is simply the
difference in values over n periods, \dQuote{simple} if the
rate of change and \dQuote{log} the difference in logs.
}
\item{\ldots}{
Additional arguments
}
}
\value{
For the implemented method on class \dQuote{tsmodel.predict} will also 
return an object of \dQuote{tsmodel.predict} with the original series
now in the requested growth rate and the distribution similarly transformed
to represent the growth.
}
\author{
Alexios Galanos
}
