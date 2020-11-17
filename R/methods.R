estimate <- function(object, ...)
{
  UseMethod("estimate")
}
tsbacktest <- function(object, ...)
{
  UseMethod("tsbacktest")
}
tsensemble <- function(object, weights = NULL, ...)
{
  UseMethod("tsensemble")
}
tsdistribution <- function(object, ...)
{
  UseMethod("tsdistribution")
}
tsdiagnose <- function(object, ...)
{
  UseMethod("tsdiagnose")
}
tsfilter <- function(object, y, newxreg = NULL, ...)
{
  UseMethod("tsfilter")
}
tsdecompose <- function(object, ...)
{
  UseMethod("tsdecompose")
}
tsmetrics <- function(object, ...)
{
  UseMethod("tsmetrics")
}
tscov <- function(object, ...)
{
  UseMethod("tscov")
}
tscor <- function(object, ...)
{
  UseMethod("tscor")
}
tsreport <- function(object, ...)
{
  UseMethod("tsreport")
}
tsprofile <- function(object , ...)
{
  UseMethod("tsprofile")
}
tsspec <- function(object , ...)
{
  UseMethod("tsspec")
}
tsautoselect <- function(object , ...)
{
  UseMethod("tsautoselect")
}
tsbenchmark <- function(object, ...)
{
  UseMethod("tsbenchmark")
}
tsaggregate <- function(object, ...)
{
  UseMethod("tsaggregate")
}
tsconvert <- function(object, ...)
{
  UseMethod("tsconvert")
}
tsequation <- function(object, ...)
{
  UseMethod("tsequation")
}
tsgrowth <- function(object, ...)
{
  UseMethod("tsgrowth")
}