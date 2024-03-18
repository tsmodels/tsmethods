#' Plots of predictive distributions
#'
#' @description Plots for objects generated from probabilistic models returning 
#' a forecast distribution.
#' @param x an object of class \dQuote{tsmodel.distribution} or \dQuote{tsmodel.predict}.
#' @param y not used.
#' @param median_color the color used for plotting the median value.
#' @param median_type the line type for the median.
#' @param median_width the width of the median line.
#' @param interval_quantiles the quantiles to include in the plot.
#' @param gradient_color the gradient color to use for the distribution.
#' @param interval_color the color of the quantile lines.
#' @param interval_type the line type for the quantiles.
#' @param interval_width the width of the quantile lines.
#' @param ylim user specified limits for the y-axis.
#' @param ylab user specified label for y-axis.
#' @param n_x the number of time periods from the end to plot for x.
#' @param x_axes whether to print the x-axis (usually time/date).
#' @param add whether to overlay another \dQuote{tsmodel.distribution} on top 
#' of a current plot. This will only plot the median and quantiles and not the full 
#' distribution with gradient color.
#' @param zero_out whether to zero any negative value in the prediction intervals.
#' @param date_class when overlaying (add argument) one distribution 
#' (\dQuote{tsmodel.distribution} on top of another, particularly if it is added 
#' to a plot based on \dQuote{tsmodel.predict}, then in order for this to work 
#' correctly the two date classes have to be the same. The \dQuote{tsmodel.predict} 
#' plot method infers the class from the original time series which is contained in 
#' the object. Since the \dQuote{tsmodel.distribution} carries no additional 
#' information other than the column names of the date/time stamps, then it is 
#' upto the user to supply what this should be.
#' @note Any matrix representing a distribution of values at points in time, 
#' with time in the columns (date labels in columns) and the distribution in rows 
#' can be set to class \dQuote{tsmodel.distribution} and then passed to the plot 
#' function which can generate a nice distribution plot. The \dQuote{tsmodel.predict} 
#' is a list with the posterior predictive (or simulated) distribution 
#' (the \dQuote{tsmodel.distribution}) in addition to the original series 
#' (original.series) of class zoo or xts.
#' @returns a plot of the predicted distribution.
#' @method plot tsmodel.distribution
#' @rdname plot
#' @aliases tsmodel.distribution tsmodel.predict
#' @export
#'
#
plot.tsmodel.distribution <- function(x, y = NULL, median_color = "black", median_type = 1, median_width = 3,
                                   interval_quantiles = c(0.025, 0.975), gradient_color = "steelblue",
                                   interval_color = "cyan", interval_type = 2, interval_width = 2,
                                   ylim = NULL, ylab="", n_x = NCOL(x), x_axes=TRUE, add = FALSE,
                                   zero_out = FALSE, date_class = "Date", ...)
{
  N = NCOL(x)
  prediction <- list()
  prediction$distribution = x[,pmax(1,N - n_x):N]
  prediction$interval <- apply(prediction$distribution, 2, quantile, interval_quantiles)
  prediction$median <- apply(prediction$distribution, 2, median)
  qtl <- seq(interval_quantiles[1], interval_quantiles[2], by = 0.01)
  quantile_matrix <- t(apply(prediction$distribution, 2, quantile, probs = qtl, na.rm = TRUE))
  if (!is.null(attr(x, "date_class"))) {
    date_class <- attr(x, "date_class")
    date_form <- match.fun(paste0("as.", date_class))
  } else {
    date_form <- match.fun(paste0("as.", date_class))
  }
  if (is.null(ylim)) {
    ylim <- range(quantile_matrix)
  }
  if (is.null(colnames(prediction$distribution))) {
    dt <- 1:ncol(prediction$distribution)
  } else {
    dt <- date_form(colnames(prediction$distribution))
  }
  if (!add) {
    .plot.dynamic.distribution(curves = prediction$distribution, timestamps = dt, add = add, ylim = ylim, 
                               gradient_color = gradient_color, ylab = ylab, interval_quantiles = interval_quantiles, 
                               x_axes = x_axes, zero_out = zero_out, ...)
  }
  lines(dt, prediction$median, col = median_color, lty = median_type, lwd = median_width, ...)
  if (zero_out) {
    for (j in 1:nrow(prediction$interval)) {
      if (any(prediction$interval[j,] <= 0, na.rm = TRUE)) {
        ix <- which(prediction$interval[j,] <= 0)
        prediction$interval[j,ix] <- 0
      }
    }
  }
  for (i in 1:nrow(prediction$interval)) {
    lines(dt, prediction$interval[i, ], col = interval_color, lty = interval_type, lwd = interval_width, ...)
  }
  grid()
  return(invisible(NULL))
}


#' @param plot_original whether to include the original dataset in the plot.
#' @param n_original the number of time periods from the end to plot for the 
#' original series. Defaults to plotting the whole series.
#' @param ... additional arguments to the plot.default function.
#' @method plot tsmodel.predict
#' @rdname plot
#' @aliases plot.tsmodel.distribution tsmodel.predict
#' @examples
#' library(xts)
#' months <- c("01","02","03","04","05","06","07","08","09","10","11","12")
#' dates <- as.Date(paste0(sort(rep(1973:1978, 12)),"-",rep(months, 6), "-",rep("01",12*6)))
#' y <- xts(as.numeric(USAccDeaths), dates)
#' samples <- do.call(cbind, lapply(1:12, 
#' function(i){sample(as.numeric(y[format(index(y),"%m") == months[i]]), 100, replace = TRUE)}))
#' predict_dates <- as.Date(paste0(rep(1979, 12),"-",months, "-",rep("01",12)))
#' expected_value <- colMeans(samples)
#' p <- list()
#' colnames(samples) <- as.character(predict_dates)
#' class(samples) <- "tsmodel.distribution"
#' p$original_series <- y
#' p$distribution <- samples
#' p$mean <- xts(expected_value, predict_dates)
#' class(p) <- "tsmodel.predict"
#' actuals_available <- c(7798,7406,8363,8460,9217,9316)
#' plot(p, main = "USAccDeaths Resample Based Forecast", n_original = 12*3, 
#' gradient_color = "orange", interval_color = "deepskyblue", median_width = 1.5)
#' points(predict_dates[1:6], actuals_available, col = "green", cex = 1.5)
#' @export
plot.tsmodel.predict <- function(x, y = NULL, plot_original = TRUE, median_color = "black", median_type = 1,
                              median_width = 3, interval_quantiles = c(0.025, 0.975),
                              gradient_color = "steelblue", interval_color = "cyan",
                              interval_type = 2, interval_width = 2, ylim = NULL, ylab = "",
                              n_original = NULL, x_axes = TRUE, zero_out = FALSE, ...)
{
  if (is.null(x$original_series)) {
    # dispatch to tsmodel.distribution method instead
      plot(x$distribution)
      return(invisible(NULL))
  }
  prediction <- x
  prediction$interval <- apply(prediction$distribution, 2, quantile, interval_quantiles)
  prediction$median   <- apply(prediction$distribution, 2, median)

  original_series <- prediction$original_series
  if (!is.null(n_original)) {
    original_series = tail(original_series, n_original)
  }
  if (is.numeric(plot_original)) {
    original_series <- tail(original_series, plot_original)
    plot_original <- TRUE
  }
  if (!is.null(attr(x$distribution, "date_class"))) {
    date_class <- attr(x$distribution, "date_class")
    date_form <- match.fun(paste0("as.", date_class))
  } else {
    if (class(index(original_series))[1] == "Date") {
      date_form <- as.Date
    } else if (class(index(original_series))[1] == "POSIXct") {
      date_form <- as.POSIXct
    } else{
      stop("\nunregognized date format")
    }
  }
  qtl <- seq(interval_quantiles[1], interval_quantiles[2], by = 0.01)
  quantile_matrix <- t(apply(prediction$distribution, 2, quantile, probs = qtl, na.rm = TRUE))

  time <- date_form(index(original_series))
  lasttimebeforeprediction = max(which(time < date_form(colnames(prediction$distribution))[1]))

  if (is.null(ylim)) {
    ylim <- range(quantile_matrix, original_series, na.rm = TRUE)
  }
  if (plot_original) {
    # Pre-append original series date
    pred_time <- c(time[lasttimebeforeprediction], date_form(colnames(prediction$distribution))) 
    if (!x_axes) xx <- "n" else xx <- "s"
    plot(time, as.numeric(original_series), type = "l", xlim = range(time, pred_time), ylim = ylim, ylab = ylab, xaxt = xx, ...)
  }
  else {
    pred_time <- date_form(colnames(prediction$distribution))
  }
  curve_dat <- prediction$distribution
  if (plot_original) {
    curve_dat <- cbind(as.numeric(original_series[lasttimebeforeprediction]), curve_dat)
    colnames(curve_dat)[1] <- as.character(time[lasttimebeforeprediction])
    prediction$median <- c(as.numeric(original_series[lasttimebeforeprediction]), prediction$median)
    names(prediction$median)[1] <- as.character(time[lasttimebeforeprediction])
    if (!is.null(prediction$mean)) {
      prediction$mean <- c(as.numeric(original_series[lasttimebeforeprediction]), prediction$mean)
      names(prediction$mean)[1] <- as.character(time[lasttimebeforeprediction])
    }
    prediction$interval <- cbind(as.numeric(original_series[lasttimebeforeprediction]), prediction$interval)
    colnames(prediction$interval)[1] <- as.character(time[lasttimebeforeprediction])
  }
  if (zero_out) {
    for (j in 1:nrow(prediction$interval)) {
      if (any(prediction$interval[j,] <= 0, na.rm = TRUE)) {
        ix = which(prediction$interval[j,] <= 0)
        prediction$interval[j,ix] <- 0
      }
    }
  }
  .plot.dynamic.distribution(curves = curve_dat, timestamps = pred_time, add = plot_original, 
                             ylim = ylim, gradient_color = gradient_color, ylab = ylab, 
                             interval_quantiles = interval_quantiles, x_axes = x_axes, 
                             zero_out = zero_out, ...)
  lines(pred_time, prediction$median, col = median_color, lty = median_type, lwd = median_width, ...)

  for (i in 1:nrow(prediction$interval)) {
    lines(pred_time, prediction$interval[i, ], col = interval_color, lty = interval_type, lwd = interval_width, ...)
  }

  if (!is.null(prediction$mean)) lines(pred_time, as.numeric(prediction$mean), col = transparent_color("violet"))
  if (plot_original) {
    lines(time, as.numeric(original_series), col = "red")
  }
  grid()
  return(invisible(NULL))
}

.plot.dynamic.distribution = function(curves, timestamps = NULL, quantile_step = 0.01, xlim = NULL, xlab = "Time", 
                                      ylim = range(curves, na.rm = TRUE), ylab = "distribution", 
                                      add = FALSE, x_axes = TRUE, gradient_color = "gray", 
                                      interval_quantiles = c(0.025, 0.975), zero_out = FALSE, ...)
{
  stopifnot(ncol(curves) >= 1)
  qtl <- seq(interval_quantiles[1], interval_quantiles[2], by = quantile_step)
  quantile_matrix <- t(apply(curves, 2, quantile, probs = qtl, na.rm = TRUE))
  nc <- ncol(quantile_matrix)
  number_of_quantile_steps <- (nc + 1)/2
  if (number_of_quantile_steps < 3) { stop("'quantile.step' is too large in PlotDynamicDistribution") }
  lower_quantile <- quantile_matrix[, 1]
  upper_quantile <- quantile_matrix[, nc]
  if (is.null(timestamps)) {
    timestamps <- tryCatch(as.POSIXct(colnames(curves)),error = function(e) {
      return(NULL)
    })
    if (is.null(timestamps)) {
      timestamps <- 1:ncol(curves)
    }
  }
  if (is.null(xlim)) {
    xlim <- range(timestamps)
  }
  if (gradient_color == "gray") {
    gcolor = gray(1 - 1/number_of_quantile_steps)
  } else {
    colfunc <- colorRampPalette(c(gradient_color,"white"))
    gcolor <- colfunc(200)[175:200]
  }
  if (zero_out) {
    if (any(lower_quantile <= 0, na.rm = TRUE)) {
      ix = which(lower_quantile <= 0)
      lower_quantile[ix] <- 0
    }
  }
  .filled.plot(timestamps, cbind(lower_quantile, upper_quantile), poly_color = gcolor, axes = FALSE, 
               add = add, xlim = xlim, xlab = xlab, ylim = ylim, ylab = ylab, ...)
  box()
  if (x_axes) {
    if (inherits(timestamps, "Date")) {
      axis.Date(1, timestamps, xpd = NA)
    } else if (inherits(timestamps, "POSIXt")) {
      axis.POSIXct(1, as.POSIXct(timestamps), xpd = NA)
    } else {
      axis(1, xpd = NA)
    }
  }
  axis(2, xpd = NA)
  for (i in 2:(number_of_quantile_steps - 1)) {
    if (gradient_color == "gray") {
      gcolor = gray(1 - i/number_of_quantile_steps)
    } else {
      colfunc <- colorRampPalette(c(gradient_color, "white"))
      gcolor <- colfunc(200)
      gcolor <- gcolor[125 + i]
    }
    lower_quantile <- quantile_matrix[, i]
    upper_quantile <- quantile_matrix[, nc + 1 - i]
    .filled.plot(timestamps, cbind(lower_quantile, upper_quantile), add = TRUE, poly_color = gcolor)
  }
  return(NULL)
}

.filled.plot <- function(timestamps, quantile_matrix, poly.color, add = FALSE, xlab, ylab, ylim, xlim, poly_color = NULL, ...) {
  ylo <- quantile_matrix[, 1]
  yhi <- quantile_matrix[, 2]
  stopifnot(length(timestamps) == nrow(quantile_matrix))
  if (any(yhi < ylo, na.rm = TRUE)) { warning("second column of quantile_matrix must be >= the first") }
  if (!add) {
    plot(timestamps, ylo, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, type = "n", ...)
  }
  observed_values <- !(is.na(ylo) | is.na(yhi))
  observed_rle <- rle(observed_values)
  number_of_contiguous_regions <- length(observed_rle$lengths)
  for (i in 1:number_of_contiguous_regions) {
    if (observed_rle$values[i]) {
      if (i == 1) {
        start <- 1
      } else {
        start <- sum(observed_rle$lengths[1:(i - 1)])
      }
      finish <- start + observed_rle$lengths[i] - 1
      index <- seq(start, finish)
      X <- c(timestamps[index], rev(timestamps[index]), timestamps[index][1])
      Y <- c(ylo[index], rev(yhi[index]), ylo[index][1])
      polygon(X, Y, border = NA, col = poly_color)
    }
  }
}

transparent_color <- function(col, alpha = 100)
{
  tcolor <- col2rgb(col)
  apply(tcolor, 2, function(x){
    rgb(red = x[1], green = x[2], blue = x[3], alpha = alpha, 
        maxColorValue = 255)
    })
}
