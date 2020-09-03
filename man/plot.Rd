\name{plot}
\alias{plot}
\alias{plot.tsmodel.distribution}
\alias{plot.tsmodel.predict}
\title{
Plots of predictive distributions
}
\description{
Plots for objects generated from probabilistic models returning a forecast
distribution.
}
\usage{
\method{plot}{tsmodel.distribution}(x, y = NULL, median_color = "black",
median_type = 1, median_width = 3, interval_quantiles = c(0.025, 0.975),
gradient_color = "steelblue", interval_color = "cyan", interval_type = 2,
interval_width = 2, ylim = NULL, ylab="", n_x = NCOL(x), x_axes = TRUE,
add = FALSE, zero_out = FALSE, date_class = "Date", ...)
\method{plot}{tsmodel.predict}(x, y = NULL, plot_original = TRUE,
median_color = "black", median_type = 1, median_width = 3,
interval_quantiles = c(0.025, 0.975), gradient_color = "steelblue",
interval_color = "cyan", interval_type = 2, interval_width = 2,
ylim = NULL, ylab="", n_original = NULL, x_axes = TRUE,
zero_out = FALSE, ...)
}
\arguments{
\item{x}{
An object of class \dQuote{tsmodel.distribution} or \dQuote{tsmodel.predict}.
}
\item{y}{
Not used.
}
\item{plot_original}{
Whether to include the original dataset in the plot.
}
\item{median_color}{
The color used for plotting the median value.
}
\item{median_type}{
The line type for the median.
}
\item{median_width}{
The width of the median line.
}
\item{interval_quantiles}{
The quantiles to include in the plot.
}
\item{gradient_color}{
The gradient color to use for the distribution.
}
\item{interval_color}{
The color of the quantile lines
}
\item{interval_type}{
The line type for the quantiles.
}
\item{interval_width}{
The width of the quantile lines.
}
\item{ylim}{
User specified limits for y.
}
\item{ylab}{
User specified label for y.
}
\item{n_x}{
The number of time periods from the end to plot for x.
}
\item{n_original}{
The number of time periods from the end to plot for the original series.
Defaults to plotting the whole series.
}
\item{x_axes}{
Whether to print the x-axis (usually time/date).
}
\item{add}{
Whether to overlay another \dQuote{tsmodel.distribution} on top of current
plot. This will only plot the median and quantiles and not the full
distribution with gradient color.
}
\item{zero_out}{
Whether to zero any negative value in the prediction intervals.
}
\item{date_class}{
When overlaying (add argument) one distribution (\dQuote{tsmodel.distribution} 
on top of another, particularly if it is added to a plot based 
on \dQuote{tsmodel.predict}, then in order for this to work correctly 
the two date classes have to be the same. The \dQuote{tsmodel.predict} plot 
method infers the class from the original time series which is contained in
the object. Since the \dQuote{tsmodel.distribution} carries no additional
information other than the column names of the date/time stamps, then it
is upto the user to supply what this should be.
}
\item{\dots}{
Not currently used
}
}
\note{
Any matrix representing a distribution of values at points in time, with time in the columns (date labels in columns)
and the distribution in rows can be set to class \dQuote{tsmodel.distribution} and then passed to the plot function which
can generate a nice distribution plot. The \dQuote{tsmodel.predict} is a list with the posterior predictive (or simulated) distribution
(the \dQuote{tsmodel.distribution}) in addition to the original series (original.series) of class zoo or xts.
}
\author{
Alexios Galanos
}

