#' Model Estimation
#'
#' @description Generic method for estimating (fitting) a model.
#' @param object an object of the model specification.
#' @param ... additional parameters passed to the method.
#' @returns An object holding the results of the estimated model.
#' @aliases estimate
#' @rdname estimate
#' @export
#'
#'
estimate <- function(object, ...)
{
  UseMethod("estimate")
}

#' Time Series Model Backtesting
#'
#' @description Generic method for backtesting of a time series model.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns An object holding the results of the backtest.
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
#' @description Generic method for ensembles of posterior predictive or simulated distributions 
#' spanning the same forecast horizon.
#' @param object currently only dispatches based on objects of 
#' class \dQuote{ensemble.spec} which have been validated for ensembling.
#' @param weights a numeric vector of length equal to the length of 
#' the ensembling predictions which represent the ensembling weights.
#' @param ... additional parameters passed to the method.
#' @returns The ensembled distribution.
#' @aliases tsensemble
#' @rdname tsensemble
#' @export
#'
#
tsensemble <- function(object, weights = NULL, ...)
{
  UseMethod("tsensemble")
}


#' Extract diagnostic model information
#'
#' @description Generic method for extracting the diagnostics from an object.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The model diagnostics.
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
#' @description Generic method for (online) time series filtering.
#' @param object an object.
#' @param y new outcome data to filter on.
#' @param newxreg new optional regressors to filter on.
#' @param ... additional parameters passed to the method.
#' @returns The filtered value(s).
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
#' @description Generic method for decomposition of an estimated time series object.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns An object of the model decomposition.
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
#' @description Generic method for generating time series performance metrics.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The performance metrics.
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
#' @description Generic method for extracting the covariance matrix from a multivariate model or the
#' autocovariance matrix of a univariate model.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns A covariance matrix or array of matrices (time varying) or other custom object related to this.
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
#' @description Generic method for extracting the correlation matrix from a multivariate model or the
#' autocorrelation matrix of a univariate model.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns A correlation matrix or array of matrices (time varying) or other custom object related to this.
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
#' @description Generic method for generating reports in different output formats.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns An object holding a ready to parse report.
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
#' @description Generic method for profiling a time series model by simulating from the estimated 
#' data generating process and measuring the performance under different levels of system noise.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The profiled model with information such as root mean squared error per parameter.
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
#' @description Generic method for extracting the specification object from an S3 object.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns A specification object extracted from an estimated or other
#' model which retains this information.
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
#' @description Generic method for benchmarking models.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns An object holding the model benchmark which can be printed
#' and referenced.
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
#' @description Generic method for aggregating of both observed series as well 
#' as certain models such as homogeneous coefficients whose dynamics aggregate (such
#' as in the Vector ETS model).
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The aggregated distribution.
#' @aliases tsaggregate
#' @rdname tsaggregate
#' @export
#'
#
tsaggregate <- function(object, ...)
{
  UseMethod("tsaggregate")
}

#' Conversion method from one object to another
#'
#' @description Generic special purpose method for converting one object to another.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @details This method can be used to convert one model to another when those
#' models are related, one set of parameters to a different parameterization, or
#' any other use case which involves some meaningful conversion in the context
#' of the model being implemented for.
#' @returns The converted object.
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
#' @description Generic method for extracting the equation of a model into either 
#' latex or some other format.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The model equation(s) in a specified format.
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
#' @description Generic method for calculating the growth distribution from an object.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The growth distribution.
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
#' @description Generic method for estimating a model using automatic differentiation. This
#' may be deprecated in the future in favor of just using the estimate method.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The estimated autodiff model.
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
#' @description Generic method for calibrating a model, targeting specific objectives.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns An object with details of the calibrated target.
#' @aliases tscalibrate
#' @rdname tscalibrate
#' @export
#'
#
tscalibrate <- function(object, ...)
{
  UseMethod("tscalibrate")
}

#' Analytic Moments
#'
#' @description Generic method for generating the analytic (or approximated) moments 
#' of the model or forecast (usually mean and variance).
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The moments of the class it was applied to (e.g. estimated or predicted object).
#' @aliases tsmoments
#' @rdname tsmoments
#' @export
#'
#
tsmoments <- function(object, ...)
{
  UseMethod("tsmoments")
}

#' Unconditional Value
#'
#' @description Generic method for extracting the unconditional value of a model.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The unconditional value or values (e.g. mean and variance).
#' @aliases unconditional
#' @rdname unconditional
#' @export
#'
#
unconditional <- function(object, ...)
{
    UseMethod("unconditional")
}

#' Probability Integral Transform (PIT)
#'
#' @description Generic method for calculating the conditional probability integral
#' transform given the data and estimated density
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The conditional probabilities.
#' @details The PIT is essentially the probabilities returned from the cumulative
#' distribution function (*p) given the data and estimated value of the mean,
#' conditional standard deviation and any other distributional parameters.
#' @aliases pit
#' @rdname pit
#' @export
#'
#
pit <- function(object, ...)
{
    UseMethod("pit")
}

#' Half Life
#'
#' @description Generic method for calculating the half-life of a model.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The half life of the model.
#' @details The half life is defined as the period it
#' takes a series to reach half its long-term average values.
#' @aliases halflife
#' @rdname halflife
#' @export
#'
#
halflife <- function(object, ...)
{
    UseMethod("halflife")
}

#' Convolution of Distributions
#'
#' @description Generic method for convolution of conditional distribution.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The convolved density.
#' @details The method is meant to apply the Fast Fourier Transform
#' to the characteristic function of a distribution. Applications of this can
#' be found in the Independent Factor Conditional Density Models.
#' @aliases tsconvolve
#' @rdname tsconvolve
#' @export
#'
#
tsconvolve <- function(object, ...)
{
    UseMethod("tsconvolve")
}

#' Co-Skewness
#'
#' @description Generic method for the co-skewness of a model.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The co-skewness tensor (folded or unfolded). 
#' @details The method calculates the conditional co-skewness of a model.
#' Applications of this can be found in the Independent Factor Conditional 
#' Density Models.
#' @aliases tscoskew
#' @rdname tscoskew
#' @export
#'
#
tscoskew <- function(object, ...)
{
    UseMethod("tscoskew")
}


#' Co-Kurtosis
#'
#' @description Generic method for the co-kurtosis of a model.
#' @param object an object.
#' @param ... additional parameters passed to the method.
#' @returns The co-kurtosis tensor (folded or unfolded). 
#' @details The method calculates the conditional co-kurtosis of a model.
#' Applications of this can be found in the Independent Factor Conditional 
#' Density Models.
#' @aliases tscokurt
#' @rdname tscokurt
#' @export
#'
#
tscokurt <- function(object, ...)
{
    UseMethod("tscokurt")
}