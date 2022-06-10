#' Estimation Method
#'
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases estimate
#' @rdname summary
#' @export
#'
#'
estimate <- function(object, ...)
{
  UseMethod("estimate")
}

#' Time Series Model Backtesting
#'
#' @description Backtesting of a time series model.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsbacktest
#' @rdname tsbacktest
#' @export
#'
#
tsbacktest <- function(object, ...)
{
  UseMethod("tsbacktest")
}

#' Ensembles predictions using their posterior predictive or simulated distribution
#'
#' @description Ensembles posterior predictive or simulated distributions 
#' spanning the same forecast horizon.
#' @param object currently only dispatches based on objects of 
#' class \dQuote{ensemble.spec} which have been validated for ensembling.
#' @param weights a numeric vector of length equal to the length of 
#' the ensembling predictions which represent the ensembling weights.
#' @param ... additional parameters passed to the method.
#' @aliases tsensemble
#' @rdname tsensemble
#' @export
#'
#
tsensemble <- function(object, weights = NULL, ...)
{
  UseMethod("tsensemble")
}


tsdistribution <- function(object, ...)
{
  UseMethod("tsdistribution")
}

#' Extract diagnostic model information
#'
#' @description Extracts the diagnostics from an object.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsdiagnose
#' @rdname tsdiagnose
#' @export
#'
#
tsdiagnose <- function(object, ...)
{
  UseMethod("tsdiagnose")
}

#' Online Time Series Filter
#'
#' @description Extracts the diagnostics from an object.
#' @param object an object.
#' @param y new outcome data to filter on.
#' @param newxreg new optional regressors to filter on.
#' @param ... additional parameters passed to the method.
#' @aliases tsfilter
#' @rdname tsfilter
#' @export
#'
#
tsfilter <- function(object, y, newxreg = NULL, ...)
{
  UseMethod("tsfilter")
}

#' Time Series Model Decomposition
#'
#' @description Decomposition of an estimated time series object.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsdecompose
#' @rdname tsdecompose
#' @export
#'
#
tsdecompose <- function(object, ...)
{
  UseMethod("tsdecompose")
}

#' Time Series Performance Metrics Method
#'
#' @description Generates time series performance metrics.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsmetrics
#' @rdname tsmetrics
#' @export
#'
#
tsmetrics <- function(object, ...)
{
  UseMethod("tsmetrics")
}

#' Covariance matrix.
#'
#' @description Extracts the covariance matrix from a multivariate model or the
#' autocovariance matrix of a univariate model.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tscov
#' @rdname tscov
#' @export
#'
#
tscov <- function(object, ...)
{
  UseMethod("tscov")
}

#' Correlation matrix.
#'
#' @description Extracts the correlation matrix from a multivariate model or the
#' autocorrelation matrix of a univariate model.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tscor
#' @rdname tscor
#' @export
#'
#
tscor <- function(object, ...)
{
  UseMethod("tscor")
}

#' Report Method
#'
#' @description Generates reports in different output formats.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsreport
#' @rdname tsreport
#' @export
#'
#
tsreport <- function(object, ...)
{
  UseMethod("tsreport")
}

#' Profile a time series model
#'
#' @description Profiles a time series model by simulating from the estimated 
#' DGP and measuring the performance under different levels of system noise.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsprofile
#' @rdname tsprofile
#' @export
#'
#
tsprofile <- function(object , ...)
{
  UseMethod("tsprofile")
}

#' Extract specification object from estimation object
#'
#' @description Extracts the specification object from an S3 object.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsspec
#' @rdname tsspec
#' @export
#'
#
tsspec <- function(object , ...)
{
  UseMethod("tsspec")
}


#' Time Series Model Benchmarking
#'
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsbenchmark
#' @rdname tsbenchmark
#' @export
#'
#
tsbenchmark <- function(object, ...)
{
  UseMethod("tsbenchmark")
}

#' Time Series Aggregation
#'
#' @description General aggregation method for both observed series as well 
#' as certain models such as homogeneous coefficients whose dynamics aggregate (see 
#' tsvets for an example).
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsaggregate
#' @rdname tsaggregate
#' @export
#'
#
tsaggregate <- function(object, ...)
{
  UseMethod("tsaggregate")
}

#' Conversion method from one model to another
#'
#' @description Special purpose function for converting one model to another.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsconvert
#' @rdname tsconvert
#' @export
#'
#
tsconvert <- function(object, ...)
{
  UseMethod("tsconvert")
}


#' Model Equation Extractor
#'
#' @description Extract the equation of a model into either latex or some 
#' other format.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsequation
#' @rdname tsequation
#' @export
#'
#
tsequation <- function(object, ...)
{
  UseMethod("tsequation")
}

#' Growth Calculation
#'
#' @description Calculates the growth distribution from a predicted object.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsgrowth
#' @rdname tsgrowth
#' @export
#'
#
tsgrowth <- function(object, ...)
{
  UseMethod("tsgrowth")
}

#' Estimation method using AD
#'
#' @description Estimates a model using automatic differentiation.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases estimate_ad
#' @rdname estimate_ad
#' @export
#'
#
estimate_ad <- function(object, ...)
{
  UseMethod("estimate_ad")
}

#' Prediction Calibration method
#'
#' @description Calibrates a model targeting specific objectives.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tscalibrate
#' @rdname tscalibrate
#' @export
#'
#
tscalibrate <- function(object, ...)
{
  UseMethod("tscalibrate")
}

#' Analytic Forecast Moments
#'
#' @description here available, returns the analytic (or approximated) moments 
#' of the forecast (usually mean and variance).
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @aliases tsmoments
#' @rdname tsmoments
#' @export
#'
#
tsmoments <- function(object, ...)
{
  UseMethod("tsmoments")
}