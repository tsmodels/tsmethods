\name{tsfilter}
\alias{tsfilter}
\title{
Online Time Series Filter
}
\description{
Filters a model with new data.
}
\usage{
tsfilter(object, y, newxreg = NULL, \dots)
}
\arguments{
\item{object}{
An object of some appopriate S3 class.
}
\item{y}{
New outcome data to filter on.
}
\item{newxreg}{
New optional regressors to filter on.
}
\item{\dots}{
Additional arguments to specific classes.
}
}
\details{
Using tsfilter instead of filter to avoid conflicts with
stats filter function which is not a method.
}
\author{
Alexios Galanos
}
